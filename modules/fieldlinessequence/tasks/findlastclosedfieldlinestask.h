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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FINDLASTCLOSEDFIELDLINESTASK___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FINDLASTCLOSEDFIELDLINESTASK___H__

#include <openspace/util/task.h>

#include <filesystem>

namespace openspace {
class FindLastClosedFieldlinesTask : public Task {
public:
    explicit FindLastClosedFieldlinesTask(const ghoul::Dictionary& dictionary);
    ~FindLastClosedFieldlinesTask() override;

    std::string description() override;
    void perform(const Task::ProgressCallback& progressCallback) override;
    static documentation::Documentation Documentation();

private:
    std::string _tracingVar = "b";
    std::filesystem::path _inputPath;
    std::vector<std::filesystem::path> _sourceFiles;
    int _numberOfPointsOnBoundary = 1;
    float _threshold = 0.5f; // in EarthRadii
    std::filesystem::path _outputFolder;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FINDLASTCLOSEDFIELDLINESTASK___H__
