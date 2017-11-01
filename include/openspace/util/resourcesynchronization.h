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

#include <openspace/documentation/documentation.h>

namespace openspace {

class ResourceSynchronization;

struct SynchronizationProduct {
    ResourceSynchronization* synchronization;
};

class SynchronizationJob : public Job<SynchronizationProduct> {
public:
    SynchronizationJob(ResourceSynchronization* synchronization);
    void execute() override;
    std::shared_ptr<SynchronizationProduct> product() override;
private:
    ResourceSynchronization* _synchronization;
};

class ResourceSynchronization
    : protected std::enable_shared_from_this<ResourceSynchronization>
{
public:
    static documentation::Documentation Documentation();
    static std::unique_ptr<ResourceSynchronization> createFromDictionary(
        const ghoul::Dictionary& dictionary);

    ResourceSynchronization();
    virtual void synchronize() = 0;
    virtual std::string directory() = 0;

    void setSyncRoot(std::string path);
    void wait();
    bool isResolved();
    void resolve();
    float progress();
    void updateProgress(float t);
    std::shared_ptr<SynchronizationJob> job();
protected:
    std::string _syncRoot;
private:
    std::shared_ptr<SynchronizationJob> _job;
    std::atomic<bool> _started;
    std::atomic<bool> _resolved;
    std::atomic<float> _progress;
};






} // namespace openspace

#endif // __OPENSPACE_CORE___RESOURCESYNCHRONIZATION___H__
