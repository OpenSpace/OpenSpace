/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
                                   std::vector<std::string> approvedPaths,
                                   bool hideFileExtensions, bool useCheckboxes)
    : _fileExtension(std::move(fileExtension))
    , _approvedPaths(std::move(approvedPaths))
    , _hideFileExtensions(hideFileExtensions)
    , _useCheckboxes(useCheckboxes)
{}

std::string FileSystemAccess::useQtFileSystemModelToTraverseDir(QString dir) {
    _filesystemModel.setRootPath(dir);
    QModelIndex index = _filesystemModel.index(_filesystemModel.rootPath());
    QFileInfo fileInfo = _filesystemModel.fileInfo(index);
    std::vector<std::string> dirsNested;
    std::vector<std::string> out;
    parseChildDirElements(fileInfo, "", 0, dirsNested, out);
    std::string combined;
    for (const std::string& o : out) {
        combined += o + "\n";
    }
    return combined;
}

void FileSystemAccess::parseChildDirElements(QFileInfo fileInfo, std::string space,
                                         int level, std::vector<std::string>& dirNames,
                                         std::vector<std::string>& output)
{
    QDir dir(fileInfo.filePath());
    bool hasDirHeaderBeenAdded = false;

    QFileInfoList fileList = dir.entryInfoList(_fileFilterOptions);
    for (int i = 0; i < fileList.size(); i++) {
        QFileInfo fileInfo = fileList[i];
        std::string res = space + fileInfo.fileName().toStdString();

        if (fileInfo.isDir()) {
            if (level != 0 || (level == 0 && isApprovedPath(res))) {
                dirNames.push_back(res);
                parseChildDirElements(fileInfo, (space + " "), level + 1, dirNames,
                    output);
            }
        }
        else {
            parseChildFile(res, hasDirHeaderBeenAdded, dirNames, output);
        }
    }
    bool isThisDirAnEmptyDeadEnd = !hasDirHeaderBeenAdded;
    if (isThisDirAnEmptyDeadEnd && (dirNames.size() != 0)) {
        dirNames.pop_back();
    }
}

bool FileSystemAccess::isApprovedPath(std::string path) {
    bool approvedMatch = false;
    path.erase(0, path.find_first_not_of(" "));

    for (const std::string& p : _approvedPaths) {
        if (path.substr(0, p.length()).compare(p) == 0) {
            approvedMatch = true;
            break;
        }
    }
    return approvedMatch;
}

void FileSystemAccess::parseChildFile(std::string filename, bool& hasDirHeaderBeenAdded,
                                      std::vector<std::string>& dirNames,
                                      std::vector<std::string>& output)
{
    std::string cbox = (_useCheckboxes) ? "0" : "";
    if (filename.length() <= _fileExtension.length()) {
        return;
    }
    else {
        std::string extension = filename.substr(filename.length()
            - _fileExtension.length());
        if (extension.compare(_fileExtension) != 0) {
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
