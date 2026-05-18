/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include "splitcombobox.h"

#include "externalicon.h"
#include "usericon.h"
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <QStandardItemModel>
#include <vector>

SplitComboBox::SplitComboBox(QWidget* parent, std::filesystem::path userPath,
                             std::string userHeader, std::filesystem::path hardcodedPath,
                             std::string hardcodedHeader, std::string specialFirst,
                             std::function<bool(const std::filesystem::path&)> fileFilter,
                   std::function<std::string(const std::filesystem::path&)> createTooltip)
    : QComboBox(parent)
    , _userPath(std::move(userPath))
    , _userHeader(std::move(userHeader))
    , _hardCodedPath(std::move(hardcodedPath))
    , _hardCodedHeader(std::move(hardcodedHeader))
    , _specialFirst(std::move(specialFirst))
    , _fileFilter(std::move(fileFilter))
    , _createTooltip(std::move(createTooltip))
{
    setCursor(Qt::PointingHandCursor);

    connect(
        this,
        QOverload<int>::of(&QComboBox::currentIndexChanged),
        [this](int index) {
            std::string path = itemData(index).toString().toStdString();
            if (!_specialFirst.empty() && index == 0 && path.empty()) {
                // We have a special entry which is at the top of the list and that entry
                // was selected
                emit selectionChanged(std::nullopt);
            }
            else {
                emit selectionChanged(std::move(path));
            }
        }
    );
}

void SplitComboBox::populateList(const std::string& preset) {
    // We don't want any signals to be fired while we are manipulating the list
    blockSignals(true);

    // Clear the previously existing entries since we might call this function again
    clear();

    // Create "icons" that we use to indicate whether an item is built-in, user content
    // (in user folder) or external content (user content outside of user folder)
    QIcon iconUser = userIcon();
    QIcon iconExternal = externalIcon();

    //
    // Special item (if it was specified and if it exists)
    if (!_specialFirst.empty()) {
        const std::optional<std::filesystem::path> specialPath = unrollPath(
            _specialFirst
        );

        if (specialPath.has_value()) {
            const std::filesystem::path& p = *specialPath;
            if (p.string().starts_with(_userPath.string())) {
                addItem(
                    iconUser,
                    QString::fromStdString(guiText(p)),
                    QString::fromStdString(p.string())
                );
            }
            else if (p.string().starts_with(_hardCodedPath.string())) {
                addItem(
                    QString::fromStdString(guiText(p)),
                    QString::fromStdString(p.string())
                );
            }
            else {
                addItem(
                    iconExternal,
                    QString::fromStdString(guiText(p)),
                    QString::fromStdString(p.string())
                );
            }

            const QString toolTip = QString::fromStdString(std::format(
                "<p>{}</p> <p style='white-space: nowrap;'>{}</p>",
                _createTooltip(p).empty() ? "(no description)" : _createTooltip(p),
                p.generic_string()
            ));
            setItemData(0, toolTip, Qt::ToolTipRole);
        }
    }


    //
    // User entries
    addItem(QString::fromStdString(_userHeader));
    qobject_cast<QStandardItemModel*>(model())->item(count() - 1)->setEnabled(false);

    std::vector<std::filesystem::path> userFiles = ghoul::filesystem::walkDirectory(
        _userPath,
        ghoul::filesystem::Recursive::Yes,
        ghoul::filesystem::Sorted::Yes,
        _fileFilter
    );
    for (const std::filesystem::path& p : userFiles) {
        std::filesystem::path relPath = std::filesystem::relative(p, _userPath);
        relPath.replace_extension();

        // Display the relative path, but store the full path in the user data segment
        addItem(
            iconUser,
            QString::fromStdString(relPath.generic_string()),
            QString::fromStdString(p.string())
        );

        const QString toolTip = QString::fromStdString(std::format(
            "<p>{}</p> <p style='white-space: nowrap;'>{}</p>",
            _createTooltip(p).empty() ? "(no description)" : _createTooltip(p),
            p.generic_string()
        ));
        setItemData(count() - 1, toolTip, Qt::ToolTipRole);
    }


    //
    // Hardcoded entries
    addItem(QString::fromStdString(_hardCodedHeader));
    qobject_cast<QStandardItemModel*>(model())->item(count() - 1)->setEnabled(false);

    std::vector<std::filesystem::path> hcFiles = ghoul::filesystem::walkDirectory(
        _hardCodedPath,
        ghoul::filesystem::Recursive::Yes,
        ghoul::filesystem::Sorted::Yes,
        _fileFilter
    );
    for (const std::filesystem::path& p : hcFiles) {
        std::filesystem::path relPath = std::filesystem::relative(p, _hardCodedPath);
        relPath.replace_extension();

        // Display the relative path, but store the full path in the user data segment
        addItem(
            QString::fromStdString(relPath.generic_string()),
            QString::fromStdString(p.string())
        );

        const QString toolTip = QString::fromStdString(std::format(
            "<p>{}</p> <p style='white-space: nowrap;'>{}</p>",
            _createTooltip(p).empty() ? "(no description)" : _createTooltip(p),
            p.generic_string()
        ));
        setItemData(count() - 1, toolTip, Qt::ToolTipRole);
    }


    // Now we are ready to send signals again
    blockSignals(false);


    //
    // Reset selection and then find the provided preset and set it as the current one
    const std::optional<std::filesystem::path> presetPath = unrollPath(preset);
    if (presetPath.has_value()) {
        setCurrentIndex(-1);

        const std::filesystem::path& p = *presetPath;
        const int idx = findData(QString::fromStdString(p.string()));
        if (idx != -1) {
            setCurrentIndex(idx);
            return;
        }

        // File exists but it's not in the list, so we add it
        insertItem(
            0,
            iconExternal,
            QString::fromStdString(guiText(p)),
            QString::fromStdString(p.string())
        );
        setCurrentIndex(0);

        const QString toolTip = QString::fromStdString(std::format(
            "<p>{}</p> <p style='white-space: nowrap;'>{}</p>",
            _createTooltip(p).empty() ? "(no description)" : _createTooltip(p),
            p.generic_string()
        ));
        setItemData(0, toolTip, Qt::ToolTipRole);
    }
    else {
        // The file did not exist
        std::string text = std::format("Cannot find '{}'", preset);
        insertItem(0, QString::fromStdString(text), "");
        setCurrentIndex(0);

        const QString toolTip = QString::fromStdString(
            std::format("<p style='white-space: nowrap;'>{}</p>", text)
        );
        setItemData(0, toolTip, Qt::ToolTipRole);
    } 
}

std::pair<std::string, std::string> SplitComboBox::currentSelection() const {
    return {
        currentText().toStdString(),
        currentData().toString().toStdString()
    };
}

std::optional<std::filesystem::path> SplitComboBox::unrollPath(std::string pathString) {
    // Creates path based on system preference (forward slash or backslash)
    const std::filesystem::path inPath =
        std::filesystem::path(pathString).make_preferred();

    // Determine if realtive of absolute path
    if (inPath.is_relative()) {
        // Check type of relative path
        const size_t beginning = pathString.find("${");
        const size_t ending = pathString.find('}');
        if (beginning == 0 && ending != std::string::npos) {
            const std::string sub = pathString.substr(beginning, ending + 1);
            if (FileSys.hasRegisteredToken(sub)) {
                const auto file = validatePath(absPath(pathString));
                if (file.has_value()) {
                    return *file;
                }
            }

            // Variable expansion path cannot be expanded as it does not exist
            return std::nullopt;
        }
        else {
            const auto uFilePath = validatePath(absPath(_userPath / inPath));
            const auto hcFilePath= validatePath(absPath(_hardCodedPath / inPath));

            if (uFilePath.has_value()) {
                return *uFilePath;
            }

            if (hcFilePath.has_value()) {
                return *hcFilePath;
            }
        }
    }
    else {
        const auto file = validatePath(inPath);
        if (file.has_value()) {
            return *file;
        }
    }

    // We could not confirm that file exists
    return std::nullopt;
}

std::optional<std::filesystem::path> SplitComboBox::validatePath(
                                                           const std::filesystem::path &p)
{

    if (std::filesystem::is_directory(p)) {
        return std::optional<std::filesystem::path>(std::nullopt);
    }

    if (p.has_extension()) {
        return std::filesystem::is_regular_file(p) ?
            std::optional<std::filesystem::path>(p) :
            std::optional<std::filesystem::path>(std::nullopt);
    }

    // Handle file check for paths without file extension
    const std::string name = p.stem().string();
    if (std::filesystem::exists(p.parent_path())) {
        for (const auto& f : std::filesystem::directory_iterator(p.parent_path())) {
            if (f.is_regular_file() && f.path().stem() == name) {
                return std::optional<std::filesystem::path>(f);
            }
        }
    }

    // Didn't find anything
    return std::optional<std::filesystem::path>(std::nullopt);
}


std::string SplitComboBox::guiText(std::filesystem::path absolutePath) {
    if (absolutePath.string().starts_with(_userPath.string())) {
        auto uPath = std::filesystem::relative(absolutePath, _userPath);
        return uPath.replace_extension().generic_string();
    }

    if (absolutePath.string().starts_with(_hardCodedPath.string())) {
        auto hcPath = std::filesystem::relative(absolutePath, _hardCodedPath);
        return hcPath.replace_extension().generic_string();
    }

    return absolutePath.stem().generic_string();
}
