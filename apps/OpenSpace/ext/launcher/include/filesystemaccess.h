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

#ifndef __OPENSPACE_UI_LAUNCHER___FILESYSTEMACCESS___H__
#define __OPENSPACE_UI_LAUNCHER___FILESYSTEMACCESS___H__

#include <QFileSystemModel>

class FileSystemAccess {
public:
    /**
     * Constructor for filesystemAccess class.
     *
     * \param fileExtension The file extensiopn filter used to find files. Only files with
     *        this extension will be recognized (e.g. '.json')
     * \param hideFileExtensions If `true` then file extensions will be removed from the
     *        listed files in the output
     * \param useCheckboxes If `true` then the text output format will contain a '0' as
     *        the first character in the line (this first character is used to represent
     *        checked ('1'), uncheck ('0') or doesn't exist in filesystem ('x') states
     */
    FileSystemAccess(std::string fileExtension, bool hideFileExtensions,
        bool useCheckboxes);

    /**
     * Function that uses the QtFileSystemModel class to search the given directory.
     *
     * \param dir The directory from which to start the search from
     */
    std::string useQtFileSystemModelToTraverseDir(const std::string& dir,
        bool userAssets = false);

private:
    void parseChildDirElements(const QFileInfo& fileInfo, const std::string& space,
        int level, std::vector<std::string>& dirNames, std::vector<std::string>& output,
        bool userAssets);
    void parseChildFile(std::string filename, bool& hasDirHeaderBeenAdded,
        std::vector<std::string>& dirNames, std::vector<std::string>& output);

    QFileSystemModel _filesystemModel;
    QDir::Filters _fileFilterOptions = QDir::Files | QDir::Dirs | QDir::NoDotAndDotDot;
    std::string _fileExtension;
    std::vector<std::string> _approvedPaths;
    bool _hideFileExtensions = true;
    bool _useCheckboxes = false;
};

#endif // __OPENSPACE_UI_LAUNCHER___FILESYSTEMACCESS___H__
