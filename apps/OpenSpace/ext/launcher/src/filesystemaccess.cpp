/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include "filesystemaccess.h"

FileSystemAccess::FileSystemAccess(std::string fileExtension,
                                   bool hideFileExtensions, bool useCheckboxes)
    : _fileExtension(std::move(fileExtension))
    , _hideFileExtensions(hideFileExtensions)
    , _useCheckboxes(useCheckboxes)
{}

std::string FileSystemAccess::useQtFileSystemModelToTraverseDir(const std::string& dir,
                                                                bool userAssets)
{
    _filesystemModel.setRootPath(QString::fromStdString(dir));
    const QModelIndex index = _filesystemModel.index(_filesystemModel.rootPath());
    const QFileInfo fileInfo = _filesystemModel.fileInfo(index);
    std::vector<std::string> dirsNested;
    std::vector<std::string> out;
    parseChildDirElements(fileInfo, "", 0, dirsNested, out, userAssets);
    std::string combined;
    for (const std::string& o : out) {
        combined += o + "\n";
    }
    return combined;
}

void FileSystemAccess::parseChildDirElements(const QFileInfo& fileInfo,
                                             const std::string& space, int level,
                                             std::vector<std::string>& dirNames,
                                             std::vector<std::string>& output,
                                             bool userAssets)
{
    const QDir dir = QDir(fileInfo.filePath());
    bool hasDirHeaderBeenAdded = false;

    const QFileInfoList fileList = dir.entryInfoList(_fileFilterOptions);
    for (const QFileInfo& fi : fileList) {
        std::string res = space + fi.fileName().toStdString();
        if (level == 0 && userAssets) {
            res = std::format("${{USER_ASSETS}}/{}", res);
        }
        if (fi.isDir()) {
            dirNames.push_back(res);
            parseChildDirElements(
                fi,
                (space + " "),
                level + 1,
                dirNames,
                output,
                userAssets
            );
        }
        else {
            parseChildFile(res, hasDirHeaderBeenAdded, dirNames, output);
        }
    }
    const bool isThisDirAnEmptyDeadEnd = !hasDirHeaderBeenAdded;
    if (isThisDirAnEmptyDeadEnd && !dirNames.empty()) {
        dirNames.pop_back();
    }
}

void FileSystemAccess::parseChildFile(std::string filename, bool& hasDirHeaderBeenAdded,
                                      std::vector<std::string>& dirNames,
                                      std::vector<std::string>& output)
{
    const std::string cbox = _useCheckboxes ? "0" : "";
    if (filename.length() <= _fileExtension.length()) {
        return;
    }
    else {
        const std::string extension = filename.substr(filename.length()
            - _fileExtension.length());
        if (extension != _fileExtension) {
            return;
        }
    }

    if (!hasDirHeaderBeenAdded) {
        for (const std::string& d : dirNames) {
            if (d.length() > 0) {
                output.push_back(cbox + d);
            }
        }
        dirNames.clear();
        hasDirHeaderBeenAdded = true;
    }
    if (_hideFileExtensions) {
        filename = filename.substr(0, filename.length() - _fileExtension.length());
    }
    output.push_back(cbox + filename);
}
