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

#include <openspace/util/resourcesynchronization.h>

#include <openspace/util/factorymanager.h>
#include <openspace/documentation/documentationengine.h>
#include <openspace/documentation/verifier.h>

#include <ghoul/logging/logmanager.h>

#include <memory>

namespace {
    const char* KeyType = "Type";
    const char* _loggerCat = "ResourceSynchronization";
}

namespace openspace {

documentation::Documentation ResourceSynchronization::Documentation() {
    using namespace openspace::documentation;

    return {
        "ResourceSynchronization",
        "resourceSynchronization",
        {
            {
                KeyType,
                new StringAnnotationVerifier(
                    "A valid ResourceSyncrhonization created by a factory"
                ),
                Optional::No,
                "This key specifies the type of ResourceSyncrhonization that gets created. "
                "It has to be one of the valid ResourceSyncrhonizations that are available "
                "for creation (see the FactoryDocumentation for a list of possible "
                "ResourceSyncrhonizations), which depends on the configration of the application"
            }
        }
    };
}

ResourceSynchronization::ResourceSynchronization()
    : _job(std::make_shared<SynchronizationJob>(this))
    , _resolved(false)
{}

std::unique_ptr<ResourceSynchronization> ResourceSynchronization::createFromDictionary(
    const ghoul::Dictionary & dictionary)
{
    documentation::testSpecificationAndThrow(Documentation(), dictionary, "ResourceSynchronization");

    std::string synchronizationType = dictionary.value<std::string>(KeyType);

    auto factory = FactoryManager::ref().factory<ResourceSynchronization>();
    ghoul_assert(factory, "ResourceSynchronization factory did not exist");
    std::unique_ptr<ResourceSynchronization> result = factory->create(synchronizationType, dictionary);
    if (result == nullptr) {
        LERROR("Failed to create a ResourceSynchronization object of type '" << synchronizationType << "'");
        return nullptr;
    }

    return result;
}

std::shared_ptr<SynchronizationJob> ResourceSynchronization::job() {
    return _job;
}

void ResourceSynchronization::setSyncRoot(std::string path) {
    _syncRoot = std::move(path);
}

void ResourceSynchronization::wait() {
}

bool ResourceSynchronization::isResolved() {
    return _resolved;
}

void ResourceSynchronization::resolve() {
    _resolved = true;
}

float ResourceSynchronization::progress() {
    return _progress;
}

void ResourceSynchronization::updateProgress(float t) {
    _progress = std::min(1.0f, std::max(t, 0.0f));
}

// SynchronizationJob methods

SynchronizationJob::SynchronizationJob(ResourceSynchronization* synchronization) {
    _synchronization = synchronization;
}

void SynchronizationJob::execute() {
    _synchronization->synchronize();
}

std::shared_ptr<SynchronizationProduct> SynchronizationJob::product() {
    return std::make_shared<SynchronizationProduct>(
        SynchronizationProduct{ _synchronization }
    );
}

}
