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

#ifndef __OPENSPACE_UI_LAUNCHER___SPLITCOMBOBOX___H__
#define __OPENSPACE_UI_LAUNCHER___SPLITCOMBOBOX___H__

#include <QComboBox>

#include <openspace/misc/boolean.h>
#include <QPersistentModelIndex>
#include <filesystem>
#include <functional>
#include <optional>
#include <string>
#include <utility>
#include <vector>

class QIcon;
class QKeyEvent;
class QStandardItem;
class QStandardItemModel;
class QTreeView;
class QWheelEvent;
class QWidget;

class SplitComboBox final : public QComboBox {
Q_OBJECT
public:
    SplitComboBox(QWidget* parent, std::filesystem::path userPath, std::string userHeader,
        std::filesystem::path hardcodedPath, std::string hardcodedHeader,
        std::string specialFirst,
        std::function<bool(const std::filesystem::path&)> fileFilter,
        std::function<std::string(const std::filesystem::path&)> createTooltip);

    void populateList(const std::string& preset);

    std::pair<std::string, std::string> currentSelection() const;

    void showPopup() override;

signals:
    // Sends the path to the selection or `std::nullopt` iff there was a special non-file
    // entry at the top and that one has been selected
    void selectionChanged(std::optional<std::string> selection);

protected:
    bool eventFilter(QObject* object, QEvent* event) override;
    void initStyleOption(QStyleOptionComboBox* option) const override;
    void keyPressEvent(QKeyEvent* event) override;
    void wheelEvent(QWheelEvent* event) override;

private:
    // Determines whether the current selection is reported even if it did not change
    BooleanType(Force);

    // Adds the header of a section followed by all of the files that are contained in the
    // provided folder, creating an item for each subfolder on the way
    void addSection(const std::string& header, const std::filesystem::path& basePath,
        const QIcon& icon);

    // Creates an item that represents the file at the provided path. The item shows the
    // provided text and stores the full path, a tooltip, and the provided icon
    QStandardItem* createFile(const QString& text, const std::filesystem::path& path,
        const QIcon& icon) const;

    // Creates an item that represents a folder. A folder can be expanded and collapsed,
    // but it can never be selected as there is no file corresponding to it
    QStandardItem* createFolder(const QString& name, const QString& toolTip) const;

    // Returns the index of the item representing the file at the provided path, or an
    // invalid index if no such item exists
    QModelIndex findFile(const QString& path) const;

    // Returns the position of the current selection in the list of files, or -1 if the
    // current selection does not refer to a file
    int currentFileIndex() const;

    // Makes the item at the provided index the current one. In contrast to
    // `setCurrentIndex`, the index may be nested anywhere in the tree
    void setCurrentModelIndex(const QModelIndex& index);

    // Recomputes the text that the combo box shows for the current selection
    void updateDisplayText();

    // Emits `selectionChanged` for the current selection
    void emitSelectionChanged(Force force);

    // Convert string into proper path depending on format: full path, relative path or
    // variable expansion path (i.e ${...})
    std::optional<std::filesystem::path> unrollPath(const std::string& pathString);

    // Determines if a file, with or without file extension, exists for the given path
    std::optional<std::filesystem::path> validatePath(const std::filesystem::path& p);

    // Extracts GUI text from the path
    std::string guiText(std::filesystem::path path) const;

    std::filesystem::path _userPath;
    std::string _userHeader;
    std::filesystem::path _hardCodedPath;
    std::string _hardCodedHeader;

    std::string _specialFirst;

    std::function<bool(const std::filesystem::path&)> _fileFilter;
    std::function<std::string(const std::filesystem::path&)> _createTooltip;

    QStandardItemModel* _model = nullptr;
    QTreeView* _treeView = nullptr;

    // The items that represent the actual files, in the same order in which they are
    // shown. Used to look up a file and to step through the entries with the keyboard
    std::vector<QPersistentModelIndex> _files;

    // The text that the combo box itself shows, which is the path of the current
    // selection relative to its base folder. This is cached since determining it
    // requires accessing the file system
    QString _displayText;

    // The selection that was reported last, used to suppress duplicate reports
    std::optional<std::string> _lastSelection;

    // Whether the mouse button is currently held down on a folder. The release that ends
    // such a click must not select anything, no matter which entry it happens on
    bool _isTogglingFolder = false;
};

#endif // __OPENSPACE_UI_LAUNCHER___SPLITCOMBOBOX___H__
