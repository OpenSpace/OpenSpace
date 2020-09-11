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

#ifndef __OPENSPACE_LAUNCHER___FILESYSTEMACCESS___H__
#define __OPENSPACE_LAUNCHER___FILESYSTEMACCESS___H__

#include <QFileSystemModel>

class filesystemAccess
{
public:
    filesystemAccess(std::string fileExtension,
        std::vector<std::string> approvedPaths, bool hideFileExtensions,
        bool useCheckboxes);
    std::string useQtFileSystemModelToTraverseDir(QString dir);

private:
    void parseChildDirElements(QFileInfo item, std::string space, int level,
        std::vector<std::string>& dirNames, std::vector<std::string>& output);
    void parseChildFile(std::string res, bool& hasDirHeaderBeenAdded,
        std::vector<std::string>& dirNames, std::vector<std::string>& output);
    bool isApprovedPath(std::string path);

    QFileSystemModel _filesystemModel;
    QDir::Filters _fileFilterOptions = QDir::Files | QDir::Dirs | QDir::NoDotAndDotDot;
    std::string _fileExtension;
    std::vector<std::string> _approvedPaths;
    bool _hideFileExtensions = true;
    bool _useCheckboxes = false;
};

#endif // __OPENSPACE_LAUNCHER___FILESYSTEMACCESS___H__
