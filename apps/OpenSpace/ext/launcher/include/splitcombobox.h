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

#ifndef __OPENSPACE_UI_LAUNCHER___SPLITCOMBOBOX___H__
#define __OPENSPACE_UI_LAUNCHER___SPLITCOMBOBOX___H__

#include <QComboBox>

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

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

signals:
    // Sends the path to the selection or `std::nullopt` iff there was a special non-file
    // entry at the top and that one has been selected
    void selectionChanged(std::optional<std::string> selection);

private:
    std::filesystem::path _userPath;
    std::string _userHeader;
    std::filesystem::path _hardCodedPath;
    std::string _hardCodedHeader;

    std::string _specialFirst;

    std::function<bool(const std::filesystem::path&)> _fileFilter;
    std::function<std::string(const std::filesystem::path&)> _createTooltip;
};

#endif // __OPENSPACE_UI_LAUNCHER___SPLITCOMBOBOX___H__
