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

#include <modules/sync/syncmodule.h>

#include <modules/sync/syncs/httpsynchronization.h>
#include <modules/sync/syncs/urlsynchronization.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/resourcesynchronization.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/memorypool.h>
#include <ghoul/misc/templatefactory.h>
#include <optional>

#include "syncmodule_lua.inl"

namespace {
    struct [[codegen::Dictionary(SyncModule)]] Parameters {
        // The list of all repository URLs that are used to fetch data from for
        // HTTPSynchronizations
        std::optional<std::vector<std::string>> httpSynchronizationRepositories;

        // The folder where all of the synchronizations are stored
        std::string synchronizationRoot;
    };
#include "syncmodule_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation SyncModule::Documentation() {
    return codegen::doc<Parameters>("module_sync");
}

SyncModule::SyncModule() : OpenSpaceModule(Name) {}

void SyncModule::internalInitialize(const ghoul::Dictionary& configuration) {
    const Parameters p = codegen::bake<Parameters>(configuration);

    if (p.httpSynchronizationRepositories.has_value()) {
        _synchronizationRepositories = *p.httpSynchronizationRepositories;
    }

    _synchronizationRoot = absPath(p.synchronizationRoot);

    ghoul::TemplateFactory<ResourceSynchronization>* fSynchronization =
        FactoryManager::ref().factory<ResourceSynchronization>();
    ghoul_assert(fSynchronization, "ResourceSynchronization factory was not created");

    fSynchronization->registerClass(
        "HttpSynchronization",
        [this](bool, const ghoul::Dictionary& dictionary, ghoul::MemoryPoolBase* pool) {
            if (pool) {
                void* ptr = pool->allocate(sizeof(HttpSynchronization));
                return new (ptr) HttpSynchronization(
                    dictionary,
                    _synchronizationRoot,
                    _synchronizationRepositories
                );
            }
            else {
                return new HttpSynchronization(
                    dictionary,
                    _synchronizationRoot,
                    _synchronizationRepositories
                );
            }
        }
    );

    fSynchronization->registerClass(
        "UrlSynchronization",
        [this](bool, const ghoul::Dictionary& dictionary, ghoul::MemoryPoolBase* pool) {
            if (pool) {
                void* ptr = pool->allocate(sizeof(UrlSynchronization));
                return new (ptr) UrlSynchronization(dictionary, _synchronizationRoot);
            }
            else {
                return new UrlSynchronization(dictionary, _synchronizationRoot);
            }
        }
    );
}

std::filesystem::path SyncModule::synchronizationRoot() const {
    return _synchronizationRoot;
}

std::vector<documentation::Documentation> SyncModule::documentations() const {
    return {
        HttpSynchronization::Documentation(),
        UrlSynchronization::Documentation()
    };
}

scripting::LuaLibrary SyncModule::luaLibrary() const {
    return {
        "sync",
        {
            codegen::lua::SyncResource,
            codegen::lua::UnsyncResource
        }
    };
}

} // namespace openspace
