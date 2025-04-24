/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include "usericon.h"
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <QPainter>
#include <QStandardItemModel>

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
            if (!_specialFirst.empty() && index == 0) {
                // We have a special entry which is at the top of the list and that entry
                // was selected
                emit selectionChanged(std::nullopt);
            }
            else {
                std::string path = itemData(index).toString().toStdString();
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

    // Create "icons" that we use to indicate whether an item is built-in or user content
    QIcon icon = userIcon();

    //
    // Special item (if it was specified)
    if (!_specialFirst.empty()) {
        if (_specialFirst.starts_with(_userPath.string())) {
            addItem(
                icon,
                QString::fromStdString(_specialFirst),
                QString::fromStdString(_specialFirst)
            );
        }
        else {
            addItem(
                QString::fromStdString(_specialFirst),
                QString::fromStdString(_specialFirst)
            );
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
            icon,
            QString::fromStdString(relPath.string()),
            QString::fromStdString(p.string())
        );

        const QString toolTip = QString::fromStdString(std::format(
            "<p>{}</p>",
            _createTooltip ? _createTooltip(p) : "(no description available)"
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
            QString::fromStdString(relPath.string()),
            QString::fromStdString(p.string())
        );

        std::string tooltipDescription = "(no description available)";
        if (_createTooltip) {
            tooltipDescription = _createTooltip(p);
        }
        const QString toolTip = QString::fromStdString(
            std::format("<p>{}</p>", tooltipDescription)
        );
        setItemData(count() - 1, toolTip, Qt::ToolTipRole);
    }


    // Now we are ready to send signals again
    blockSignals(false);


    //
    // Find the provided preset and set it as the current one
    const int idx = findText(QString::fromStdString(preset));
    if (idx != -1) {
        // Found the preset, set it as the current index
        setCurrentIndex(idx);
    }
    else {
        // The preset wasn't found, so we add it to the top
        std::string text = std::format("'{}' not found", preset);
        insertItem(0, QString::fromStdString(text));
        setCurrentIndex(0);
    }
}

std::pair<std::string, std::string> SplitComboBox::currentSelection() const {
    return {
        currentText().toStdString(),
        currentData().toString().toStdString()
    };
}
