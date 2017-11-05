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

#include <openspace/util/resourcesynchronizer.h>

namespace {
    size_t NumberOfThreads = 8;
}

namespace openspace {

ResourceSynchronizer::ResourceSynchronizer()
    : _jobManager(ThreadPool(NumberOfThreads))
{}

void ResourceSynchronizer::enqueueSynchronization(
    std::shared_ptr<ResourceSynchronization> sync,
    ResourceSyncClient* client)
{
    _managedSynchronizations.emplace(sync.get(), sync);
    _clientMap[sync.get()] = client;

    std::shared_ptr<SynchronizationJob> job = sync->job();
    _jobManager.enqueueJob(job);
}

void ResourceSynchronizer::cancelSynchronization(
    ResourceSynchronization* sync,
    ResourceSyncClient* client)
{
    _managedSynchronizations.erase(sync);
    _clientMap.erase(sync);
}

std::vector<std::shared_ptr<ResourceSynchronization>>
    ResourceSynchronizer::finishedSynchronizations(ResourceSyncClient* client)
{
    // Fetch all finished jobs
    while (_jobManager.numFinishedJobs() > 0) {
        std::shared_ptr<Job<SynchronizationProduct>> j = _jobManager.popFinishedJob();
        ResourceSynchronization* sync = j->product()->synchronization;
        const auto it = _clientMap.find(sync);
        if (it != _clientMap.end()) {
            ResourceSyncClient* c = it->second;
            _finishedSynchronizations[c].push_back(sync);
        }
    }

    // Extract the ones that were queried by the client
    const auto finishedIt = _finishedSynchronizations.find(client);
    if (finishedIt == _finishedSynchronizations.end()) {
        return std::vector<std::shared_ptr<ResourceSynchronization>>();
    }

    std::vector<ResourceSynchronization*>& rawSyncs = finishedIt->second;
    
    std::vector<std::shared_ptr<ResourceSynchronization>> syncs(rawSyncs.size());
    std::transform(rawSyncs.begin(), rawSyncs.end(), syncs.begin(),
        [this](ResourceSynchronization* raw) {
            return _managedSynchronizations[raw];
        }
    );

    _finishedSynchronizations.erase(finishedIt);

    return syncs;
}

} // namespace openspace
