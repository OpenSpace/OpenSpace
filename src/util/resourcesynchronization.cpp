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

#include <openspace/util/resourcesynchronization.h>

#include <openspace/documentation/verifier.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/templatefactory.h>
#include <fstream>

namespace {
    struct [[codegen::Dictionary(ResourceSynchronization)]] Parameters {
        // This key specifies the type of ResourceSyncrhonization that gets created. It
        // has to be one of the valid ResourceSyncrhonizations that are available for
        // creation (see the FactoryDocumentation for a list of possible
        // ResourceSyncrhonizations), which depends on the configration of the application
        std::string type
            [[codegen::annotation("A ResourceSynchronization created by a factory")]];

        // A unique identifier that is used to reference this specific
        // ResourceSynchronization object
        std::string identifier [[codegen::identifier()]];

        // A user readable name of this synchronization
        std::string name;
    };
#include "resourcesynchronization_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation ResourceSynchronization::Documentation() {
    return codegen::doc<Parameters>("resourceSynchronization");
}

std::unique_ptr<ResourceSynchronization> ResourceSynchronization::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    ghoul::TemplateFactory<ResourceSynchronization>* factory =
        FactoryManager::ref().factory<ResourceSynchronization>();
    ghoul_assert(factory, "ResourceSynchronization factory did not exist");
    ResourceSynchronization* sync = factory->create(p.type, dictionary);
    sync->_name = p.name;
    return std::unique_ptr<ResourceSynchronization>(sync);
}

ResourceSynchronization::ResourceSynchronization(
                                                std::filesystem::path synchronizationRoot)
    : _synchronizationRoot(std::move(synchronizationRoot))
{}

bool ResourceSynchronization::isResolved() const {
    return _state == State::Resolved;
}

bool ResourceSynchronization::isRejected() const {
    return _state == State::Rejected;
}

bool ResourceSynchronization::isSyncing() const {
    return _state == State::Syncing;
}

size_t ResourceSynchronization::nSynchronizedBytes() const {
    return _nSynchronizedBytes;
}

size_t ResourceSynchronization::nTotalBytes() const {
    return _nTotalBytes;
}

bool ResourceSynchronization::nTotalBytesIsKnown() const {
    return _nTotalBytesKnown;
}

const std::string& ResourceSynchronization::identifier() const {
    return _identifier;
}

const std::string& ResourceSynchronization::name() const {
    return _name;
}

void ResourceSynchronization::createSyncFile(bool) const {
    std::filesystem::path dir = directory();
    std::filesystem::create_directories(dir);

    dir.replace_extension("ossync");
    std::ofstream syncFile(dir, std::ofstream::out);
    // The actual text what is written is not used anywhere, but it might be useful to
    // user that wants to look at it
    syncFile << "Synchronized";
}

bool ResourceSynchronization::hasSyncFile() const {
    std::filesystem::path path = directory();
    path.replace_extension("ossync");
    return std::filesystem::is_regular_file(path);
}

} // namespace openspace
