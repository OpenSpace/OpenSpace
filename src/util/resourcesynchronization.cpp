#include "..\..\include\openspace\util\resourcesynchronization.h"
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

namespace openspace {

ResourceSynchronization::ResourceSynchronization() {}

std::unique_ptr<ResourceSynchronization> ResourceSynchronization::createFromDictionary(
    const ghoul::Dictionary & dictionary)
{
    return std::unique_ptr<ResourceSynchronization>();
}

std::shared_ptr<SynchronizationJob> ResourceSynchronization::job()
{
    return std::shared_ptr<SynchronizationJob>();
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

SynchronizationJob::SynchronizationJob(
    std::shared_ptr<ResourceSynchronization> synchronization)
{
    _synchronization = synchronization;
}

void SynchronizationJob::resolve() {
    _synchronization->resolve();
}

void SynchronizationJob::updateProgress(float t) {
    _synchronization->updateProgress(t);
}

}
