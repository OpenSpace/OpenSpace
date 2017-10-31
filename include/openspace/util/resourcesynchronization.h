/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_CORE___RESOURCESYNCHRONIZATION___H__
#define __OPENSPACE_CORE___RESOURCESYNCHRONIZATION___H__

#include <openspace/util/concurrentjobmanager.h>

#include <ghoul/filesystem/directory.h>
#include <ghoul/misc/dictionary.h>

namespace openspace {

class ResourceSynchronization;

struct SynchronizationProduct {
    std::shared_ptr<ResourceSynchronization> synchronization;
};

class SynchronizationJob : public Job<SynchronizationProduct> {
public:
    SynchronizationJob(std::shared_ptr<ResourceSynchronization> synchronization);
    void execute() = 0;
protected:
    void resolve();
    void updateProgress(float t);
private:
    std::shared_ptr<ResourceSynchronization> _synchronization;
};

class ResourceSynchronization {
public:
    ResourceSynchronization();
    static std::unique_ptr<ResourceSynchronization> createFromDictionary(
        const ghoul::Dictionary& dictionary);

    virtual std::shared_ptr<SynchronizationJob> job();
    void wait();
    bool isResolved();
    void resolve();
    float progress();
    void updateProgress(float t);
private:
    std::atomic<bool> _started;
    std::atomic<bool> _resolved;
    std::atomic<float> _progress;
};






} // namespace openspace

#endif // __OPENSPACE_CORE___RESOURCESYNCHRONIZATION___H__
