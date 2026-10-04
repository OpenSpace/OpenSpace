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

#include "customicons.h"
#include <openspace/filesystem/filesystem.h>
#include <openspace/format.h>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QStandardItemModel>
#include <QStyle>
#include <QStyleOptionComboBox>
#include <QTreeView>
#include <QWheelEvent>
#include <algorithm>
#include <iterator>
#include <map>
#include <vector>

namespace {
    // The indentation of a single level in the tree. The popup is only as wide as the
    // combo box itself, so this is a bit tighter than the default of most styles
    constexpr int TreeIndentation = 16;

    // Compares two relative paths such that, at the first component in which the two
    // differ, a path that continues into a subfolder is sorted before a path that ends
    // in that component. All other components are compared alphabetically. The result is
    // an ordering in which the subfolders of every folder are listed before its files
    bool isPathLess(const std::filesystem::path& lhs, const std::filesystem::path& rhs) {
        std::filesystem::path::const_iterator lIt = lhs.begin();
        std::filesystem::path::const_iterator rIt = rhs.begin();
        for (; lIt != lhs.end() && rIt != rhs.end(); lIt++, rIt++) {
            if (*lIt == *rIt) {
                continue;
            }

            // The components differ, so whichever of the two has at least one more
            // component left is a folder and thus comes first
            const bool lIsFolder = std::next(lIt) != lhs.end();
            const bool rIsFolder = std::next(rIt) != rhs.end();
            if (lIsFolder != rIsFolder) {
                return lIsFolder;
            }

            return lIt->string() < rIt->string();
        }

        // One of the paths is fully contained in the other, which happens if a file has
        // the same name as a folder next to it. The folder comes first
        return lIt != lhs.end();
    }

    // A file together with its path relative to the base folder of its section
    struct Entry {
        std::filesystem::path path;
        std::filesystem::path relative;
    };
} // namespace

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
    , _model(new QStandardItemModel(this))
    , _treeView(new QTreeView(this))
{
    setCursor(Qt::PointingHandCursor);
    setModel(_model);

    // The popup shows a tree so that the folders in which the files are stored can be
    // collapsed. Handing the view to the combo box has to happen before configuring it
    // as the combo box overwrites most of the settings of the view while taking it over
    setView(_treeView);
    _treeView->setObjectName("configpopup");
    _treeView->setHeaderHidden(true);
    _treeView->setUniformRowHeights(true);
    _treeView->setAllColumnsShowFocus(true);
    _treeView->setExpandsOnDoubleClick(false);
    _treeView->setIndentation(TreeIndentation);

    // A style sheet that is set on a widget does not reach a popup since the popup is a
    // window of its own rather than a child widget. So the view has to be given the
    // style sheet of the launcher itself; the rules in there that do not concern the
    // view are simply ignored
    _treeView->setStyleSheet(window()->styleSheet());

    // The event filters have to be installed after the view was handed over since the
    // combo box installs filters of its own in there and the filter that was installed
    // last is the one that is called first
    _treeView->installEventFilter(this);
    _treeView->viewport()->installEventFilter(this);

    // `activated` is only sent when the user picks an entry, which is the only case in
    // which we are not the ones changing the selection in the first place
    connect(
        this, &QComboBox::activated,
        [this]() { emitSelectionChanged(Force::No); }
    );
}

void SplitComboBox::populateList(const std::string& preset) {
    // We don't want any signals to be fired while we are manipulating the list
    blockSignals(true);

    // Clear the previously existing entries since we might call this function again
    _files.clear();
    _model->clear();

    // Create "icons" that we use to indicate whether an item is built-in, user content
    // (in user folder) or external content (user content outside of user folder)
    const QIcon iconUser = userIcon();
    const QIcon iconExternal = externalIcon();

    //
    // Special item (if it was specified and if it exists)
    if (!_specialFirst.empty()) {
        const std::optional<std::filesystem::path> specialPath = unrollPath(
            _specialFirst
        );

        if (specialPath.has_value()) {
            const std::filesystem::path& p = *specialPath;
            const std::string pStr = p.string();

            QIcon icon;
            if (pStr.starts_with(_userPath.string())) {
                icon = iconUser;
            }
            else if (!pStr.starts_with(_hardCodedPath.string())) {
                icon = iconExternal;
            }

            // This entry is not sorted into any of the folders, so it shows the full
            // path instead of only the name of the file
            QStandardItem* item = createFile(
                QString::fromStdString(guiText(p)),
                p,
                icon
            );
            _model->appendRow(item);
            _files.emplace_back(item->index());
        }
    }

    //
    // User entries and hardcoded entries
    addSection(_userHeader, _userPath, iconUser);
    addSection(_hardCodedHeader, _hardCodedPath, QIcon());

    //
    // Find the provided preset and set it as the current one
    const std::optional<std::filesystem::path> presetPath = unrollPath(preset);
    if (presetPath.has_value()) {
        const std::filesystem::path& p = *presetPath;
        const QModelIndex idx = findFile(QString::fromStdString(p.string()));
        if (idx.isValid()) {
            setCurrentModelIndex(idx);
        }
        else {
            // File exists but it is not in the list, so we add it
            QStandardItem* item = createFile(
                QString::fromStdString(guiText(p)),
                p,
                iconExternal
            );
            _model->insertRow(0, item);
            _files.emplace(_files.begin(), item->index());
            setCurrentModelIndex(item->index());
        }
    }
    else {
        // The file did not exist
        const std::string text = std::format("Cannot find '{}'", preset);
        QStandardItem* item = new QStandardItem(QString::fromStdString(text));
        item->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
        item->setData(
            QString::fromStdString(std::format(
                "<p style='white-space: nowrap;'>{}</p>", text
            )),
            Qt::ToolTipRole
        );
        // This entry is deliberately not added to the list of files as it does not refer
        // to one and should thus be skipped when stepping through the entries
        _model->insertRow(0, item);
        setCurrentModelIndex(item->index());
    }

    // Now we are ready to send signals again
    blockSignals(false);
    emitSelectionChanged(Force::Yes);
}

void SplitComboBox::addSection(const std::string& header,
                               const std::filesystem::path& basePath, const QIcon& icon)
{
    QStandardItem* headerItem = new QStandardItem(QString::fromStdString(header));
    headerItem->setFlags(Qt::NoItemFlags);
    // The color has to be set on the item itself rather than through the style sheet
    // since the item delegate always takes the color of a disabled item from the
    // palette, which makes a `::item:disabled` rule have no effect on it
    _model->appendRow(headerItem);

    const std::vector<std::filesystem::path> files =
        openspace::filesystem::walkDirectory(
            basePath,
            openspace::filesystem::Recursive::Yes,
            openspace::filesystem::Sorted::No,
            _fileFilter
        );

    // Both the folders that an entry is sorted into and the text that it shows are
    // derived from the path relative to the base folder, so we sort on that directly
    std::vector<Entry> entries;
    entries.reserve(files.size());
    for (const std::filesystem::path& p : files) {
        std::filesystem::path relative = std::filesystem::relative(p, basePath);
        relative.replace_extension();
        if (relative.empty()) {
            continue;
        }

        entries.push_back({ .path = p, .relative = std::move(relative) });
    }
    std::sort(
        entries.begin(),
        entries.end(),
        [](const Entry& lhs, const Entry& rhs) {
            return isPathLess(lhs.relative, rhs.relative);
        }
    );

    // Maps the relative path of a folder to the item that represents it. This is local
    // to the section as the user content and the built-in content must not share folders
    std::map<std::filesystem::path, QStandardItem*> folders;

    for (const Entry& e : entries) {
        // All of the components but the last one are folders that we might still have to
        // create before the file itself can be added to the innermost one
        QStandardItem* parent = nullptr;
        std::filesystem::path folder;
        for (std::filesystem::path::const_iterator it = e.relative.begin();
             std::next(it) != e.relative.end();
             it++)
        {
            folder /= *it;

            const std::map<std::filesystem::path, QStandardItem*>::iterator jt =
                folders.find(folder);
            if (jt != folders.end()) {
                parent = jt->second;
                continue;
            }

            QStandardItem* folderItem = createFolder(
                QString::fromStdString(it->string()),
                QString::fromStdString(folder.generic_string())
            );
            if (parent) {
                parent->appendRow(folderItem);
            }
            else {
                _model->appendRow(folderItem);
            }
            folders[folder] = folderItem;
            parent = folderItem;
        }

        QStandardItem* item = createFile(
            QString::fromStdString(e.relative.filename().string()),
            e.path,
            icon
        );
        if (parent) {
            parent->appendRow(item);
        }
        else {
            _model->appendRow(item);
        }
        _files.emplace_back(item->index());
    }
}

QStandardItem* SplitComboBox::createFile(const QString& text,
                                         const std::filesystem::path& path,
                                         const QIcon& icon) const
{
    QStandardItem* item = new QStandardItem(text);
    item->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
    if (!icon.isNull()) {
        item->setIcon(icon);
    }

    // Display the name of the file, but store the full path in the user data segment
    item->setData(QString::fromStdString(path.string()), Qt::UserRole);

    const std::string description = _createTooltip(path);
    item->setData(
        QString::fromStdString(std::format(
            "<p>{}</p> <p style='white-space: nowrap;'>{}</p>",
            description.empty() ? "(no description)" : description,
            path.generic_string()
        )),
        Qt::ToolTipRole
    );
    return item;
}

QStandardItem* SplitComboBox::createFolder(const QString& name,
                                           const QString& toolTip) const
{
    QStandardItem* item = new QStandardItem(name);
    // A folder can be expanded and collapsed, but it must not be selectable as there is
    // no file that would correspond to it
    item->setFlags(Qt::ItemIsEnabled);
    item->setData(toolTip, Qt::ToolTipRole);
    return item;
}

QModelIndex SplitComboBox::findFile(const QString& path) const {
    if (path.isEmpty()) {
        return QModelIndex();
    }

    for (const QPersistentModelIndex& index : _files) {
        if (index.data(Qt::UserRole).toString() == path) {
            return index;
        }
    }
    return QModelIndex();
}

int SplitComboBox::currentFileIndex() const {
    const QString path = currentData().toString();
    if (path.isEmpty()) {
        return -1;
    }

    for (size_t i = 0; i < _files.size(); i += 1) {
        if (_files[i].data(Qt::UserRole).toString() == path) {
            return static_cast<int>(i);
        }
    }
    return -1;
}

void SplitComboBox::setCurrentModelIndex(const QModelIndex& index) {
    // A combo box can only be told to select one of the direct children of its root
    // index, but stores the current entry as a model index internally. By temporarily
    // moving the root to the parent of the item that we want to select we can make it
    // select an item that is nested in the tree. Afterwards the root is restored so that
    // the popup shows the full tree again
    setRootModelIndex(index.parent());
    setCurrentIndex(index.row());
    setRootModelIndex(QModelIndex());

    updateDisplayText();
}

void SplitComboBox::updateDisplayText() {
    // The combo box shows the path of the current selection relative to either the user
    // folder or the built-in folder, which is the text by which a profile or a window
    // configuration is identified everywhere else
    const QString path = currentData().toString();
    const QString text = path.isEmpty() ?
        currentText() :
        QString::fromStdString(guiText(std::filesystem::path(path.toStdString())));

    if (text != _displayText) {
        _displayText = text;
        update();
    }
}

void SplitComboBox::emitSelectionChanged(Force force) {
    updateDisplayText();

    std::string path = currentData().toString().toStdString();
    if ((force == Force::No) && _lastSelection.has_value() && (*_lastSelection == path)) {
        // The user picked the entry that was already selected. The combo box reports
        // that as an activation, but there is nothing for us to pass on
        return;
    }
    _lastSelection = path;

    if (!_specialFirst.empty() && path.empty()) {
        // We have a special entry which is at the top of the list and the current entry
        // does not refer to a file
        emit selectionChanged(std::nullopt);
    }
    else {
        emit selectionChanged(std::move(path));
    }
}

std::pair<std::string, std::string> SplitComboBox::currentSelection() const {
    return {
        _displayText.toStdString(),
        currentData().toString().toStdString()
    };
}

void SplitComboBox::showPopup() {
    // Ensure the current selection is visible, which means expanding all of the folders
    // that lead up to it. This has to happen before the base class implementation runs as
    // that determines the size of the popup from the entries that are currently visible
    _isTogglingFolder = false;

    const QModelIndex current = findFile(currentData().toString());
    for (QModelIndex p = current.parent(); p.isValid(); p = p.parent()) {
        _treeView->expand(p);
    }

    QComboBox::showPopup();

    if (current.isValid()) {
        _treeView->scrollTo(current, QAbstractItemView::EnsureVisible);
    }
}

bool SplitComboBox::eventFilter(QObject* object, QEvent* event) {
    // The popup selects whichever entry the mouse is released over or that is active when
    // selecting is pressed. A folder does not correspond to a file, so we intercept these
    // events before the event filter of the combo box gets to see them. Instead we expand
    // or collapse the folder. Mouse presses are intercepted as well so that a click
    // anywhere on the row of a folder toggles it and so that a click on the expansion
    // arrow is not handled twice
    if (object == _treeView->viewport()) {
        switch (event->type()) {
            case QEvent::MouseButtonPress:
            case QEvent::MouseButtonDblClick: {
                const QMouseEvent* e = static_cast<QMouseEvent*>(event);
                const QModelIndex index = _treeView->indexAt(e->position().toPoint());
                if (!index.isValid() || !_model->hasChildren(index)) {
                    break;
                }

                // Expanding a folder scrolls the view, which means that a different entry
                // can be underneath the mouse by the time it is released. So we have to
                // remember that the release belongs to this folder, as the popup would
                // otherwise select whichever entry ended up there
                _isTogglingFolder = true;

                if ((event->type() == QEvent::MouseButtonPress) &&
                    (e->button() == Qt::LeftButton))
                {
                    const bool wasExpanded = _treeView->isExpanded(index);
                    _treeView->setExpanded(index, !wasExpanded);

                    if (!wasExpanded) {
                        // The popup does not grow while it is open, so we move the folder
                        // to the top to show as many of its files as fit. The folder
                        // itself stays visible so that it can be collapsed again
                        _treeView->scrollTo(index, QAbstractItemView::PositionAtTop);
                    }
                }
                return true;
            }
            case QEvent::MouseButtonRelease: {
                if (!_isTogglingFolder) {
                    break;
                }

                _isTogglingFolder = false;
                return true;
            }
            default:
                break;
        }
    }

    if (object == _treeView) {
        switch (event->type()) {
            // Return is handled as a shortcut override by the combo box rather than as a
            // key press, so both have to be intercepted to be certain that a folder can
            // never be selected by accident
            case QEvent::ShortcutOverride:
            case QEvent::KeyPress: {
                const QKeyEvent* e = static_cast<QKeyEvent*>(event);
                const bool isToggle =
                    (e->key() == Qt::Key_Return) ||
                    (e->key() == Qt::Key_Enter) ||
                    (e->key() == Qt::Key_Space);
                if (!isToggle || (e->modifiers() != Qt::NoModifier)) {
                    break;
                }

                const QModelIndex index = _treeView->currentIndex();
                if (!index.isValid() || !_model->hasChildren(index)) {
                    break;
                }

                if (event->type() == QEvent::KeyPress) {
                    _treeView->setExpanded(index, !_treeView->isExpanded(index));
                }
                return true;
            }
            default:
                break;
        }
    }

    return QComboBox::eventFilter(object, event);
}

void SplitComboBox::initStyleOption(QStyleOptionComboBox* option) const {
    QComboBox::initStyleOption(option);

    // The entries in the popup only show the name of the file, but the combo box itself
    // shows the full path relative to the folder that the file was found in
    if (!_displayText.isEmpty()) {
        option->currentText = _displayText;
    }
}

void SplitComboBox::keyPressEvent(QKeyEvent* event) {
    // Opening the popup is handled by the base class
    const bool isPopupKey =
        (event->key() == Qt::Key_F4) ||
        (event->key() == Qt::Key_Space) ||
        (((event->key() == Qt::Key_Up) || (event->key() == Qt::Key_Down)) &&
            (event->modifiers() & Qt::AltModifier));
    if (isPopupKey || _files.empty() || (event->modifiers() != Qt::NoModifier)) {
        QComboBox::keyPressEvent(event);
        return;
    }

    // The base class implementation only walks the top-level entries of the model, which
    // for a tree are the section headers and the folders rather than the files. So we
    // step through the files instead, which are in the same order in which they appear
    const int last = static_cast<int>(_files.size()) - 1;
    const int current = currentFileIndex();
    int target = -1;
    switch (event->key()) {
        case Qt::Key_Up:
        case Qt::Key_PageUp:
            target = (current == -1) ? 0 : std::max(current - 1, 0);
            break;
        case Qt::Key_Down:
        case Qt::Key_PageDown:
            target = (current == -1) ? 0 : std::min(current + 1, last);
            break;
        case Qt::Key_Home:
            target = 0;
            break;
        case Qt::Key_End:
            target = last;
            break;
        default: {
            const QString text = event->text();
            if (text.isEmpty() || !text.at(0).isPrint()) {
                QComboBox::keyPressEvent(event);
                return;
            }

            // Jump to the next file whose name starts with the character that was typed.
            // The base class implementation would search through the entries of the
            // popup instead and could end up selecting a folder
            for (int i = 1; i <= last + 1; i += 1) {
                const int idx = (std::max(current, 0) + i) % (last + 1);
                const QString name = _files[idx].data(Qt::DisplayRole).toString();
                if (name.startsWith(text, Qt::CaseInsensitive)) {
                    target = idx;
                    break;
                }
            }
            break;
        }
    }

    if ((target != -1) && (target != current)) {
        setCurrentModelIndex(_files[target]);
        emitSelectionChanged(Force::No);
    }
    event->accept();
}

void SplitComboBox::wheelEvent(QWheelEvent* event) {
    QStyleOptionComboBox option;
    initStyleOption(&option);
    const bool allowScrolling = style()->styleHint(
        QStyle::SH_ComboBox_AllowWheelScrolling,
        &option,
        this
    ) != 0;
    if (!allowScrolling || _files.empty() || _treeView->isVisible()) {
        QComboBox::wheelEvent(event);
        return;
    }

    // Just as for the key events, the base class implementation would walk the top-level
    // entries of the model rather than the files
    const int last = static_cast<int>(_files.size()) - 1;
    const int current = currentFileIndex();
    int target = current;
    if (event->angleDelta().y() > 0) {
        target = (current == -1) ? 0 : std::max(current - 1, 0);
    }
    else if (event->angleDelta().y() < 0) {
        target = (current == -1) ? 0 : std::min(current + 1, last);
    }

    if ((target != -1) && (target != current)) {
        setCurrentModelIndex(_files[target]);
        emitSelectionChanged(Force::No);
    }
    event->accept();
}

std::optional<std::filesystem::path> SplitComboBox::unrollPath(
                                                            const std::string& pathString)
{
    // Creates path based on system preference (forward slash or backslash)
    const std::filesystem::path inPath =
        std::filesystem::path(pathString).make_preferred();

    // Determine if realtive or absolute path
    if (inPath.is_relative()) {
        // Check type of relative path
        const size_t beginning = pathString.find("${");
        const size_t ending = pathString.find('}');
        if (beginning == 0 && ending != std::string::npos) {
            const std::string sub = pathString.substr(beginning, ending + 1);
            if (FileSys.hasRegisteredToken(sub)) {
                const std::optional<std::filesystem::path> file =
                    validatePath(absPath(pathString));
                if (file.has_value()) {
                    return *file;
                }
            }

            // Variable expansion path cannot be expanded as it does not exist
            return std::nullopt;
        }
        else {
            const std::optional<std::filesystem::path> uFilePath =
                validatePath(absPath(_userPath / inPath));

            if (uFilePath.has_value()) {
                return *uFilePath;
            }

            const std::optional<std::filesystem::path> hcFilePath =
                validatePath(absPath(_hardCodedPath / inPath));
            if (hcFilePath.has_value()) {
                return *hcFilePath;
            }
        }
    }
    else {
        const std::optional<std::filesystem::path> file = validatePath(inPath);
        if (file.has_value()) {
            return *file;
        }
    }

    // We could not confirm that file exists
    return std::nullopt;
}

std::optional<std::filesystem::path> SplitComboBox::validatePath(
                                                           const std::filesystem::path& p)
{
    if (std::filesystem::is_directory(p)) {
        return std::nullopt;
    }

    if (p.has_extension()) {
        return std::filesystem::is_regular_file(p) ?
            std::optional<std::filesystem::path>(p) :
            std::nullopt;
    }

    // Handle file check for paths without file extension
    const std::string name = p.stem().string();
    if (std::filesystem::exists(p.parent_path())) {
        for (const auto& f : std::filesystem::directory_iterator(p.parent_path())) {
            if (f.is_regular_file() && f.path().stem() == name) {
                return f;
            }
        }
    }

    // Didn't find anything
    return std::nullopt;
}

std::string SplitComboBox::guiText(std::filesystem::path absolutePath) const {
    if (absolutePath.string().starts_with(_userPath.string())) {
        std::filesystem::path uPath = std::filesystem::relative(absolutePath, _userPath);
        return uPath.replace_extension().generic_string();
    }

    if (absolutePath.string().starts_with(_hardCodedPath.string())) {
        std::filesystem::path hcPath =
            std::filesystem::relative(absolutePath, _hardCodedPath);
        return hcPath.replace_extension().generic_string();
    }

    return absolutePath.stem().generic_string();
}
