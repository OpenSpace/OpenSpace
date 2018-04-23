/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_GAIAMISSION___CONSTRUCTOCTREETASK___H__
#define __OPENSPACE_MODULE_GAIAMISSION___CONSTRUCTOCTREETASK___H__

#include <openspace/util/task.h>
#include <modules/gaiamission/rendering/octreemanager.h>

namespace openspace {

namespace documentation { struct Documentation; }

class ConstructOctreeTask : public Task {
public:
    ConstructOctreeTask(const ghoul::Dictionary& dictionary);
    virtual ~ConstructOctreeTask();
    
    std::string description() override;
    void perform(const Task::ProgressCallback& onProgress) override;
    static documentation::Documentation Documentation();

private:
    void constructOctreeFromSingleFile(const Task::ProgressCallback& progressCallback);
    void constructOctreeFromFolder(const Task::ProgressCallback& progressCallback);
    
    std::string _inFileOrFolderPath;
    std::string _outFileOrFolderPath;
    bool _singleFileInput;

    std::shared_ptr<OctreeManager> _octreeManager;
    std::unique_ptr<OctreeManager> _indexOctreeManager;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GAIAMISSION___CONSTRUCTOCTREETASK___H__
