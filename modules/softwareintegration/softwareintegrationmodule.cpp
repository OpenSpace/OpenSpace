/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/softwareintegration/softwareintegrationmodule.h>

#include <modules/softwareintegration/network/messagehandler.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/syncengine.h>
#include <modules/softwareintegration/rendering/renderablepointscloud.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/logging/logmanager.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/scripting/lualibrary.h>
#include <ghoul/misc/dictionaryluaformatter.h>

#include "softwareintegrationmodule_lua.inl"

namespace {

constexpr const char* _loggerCat = "SoftwareIntegrationModule";

}  // namespace

namespace openspace {

SoftwareIntegrationModule::SoftwareIntegrationModule()
    : OpenSpaceModule(Name)
{}

SoftwareIntegrationModule::~SoftwareIntegrationModule() {
    internalDeinitialize();
}

void SoftwareIntegrationModule::storeData(
    const SyncableStorage::Identifier& identifier,
    const simp::DataKey key,
    const std::vector<std::byte>& data
) {
    _syncableStorage.store(identifier, key, data);
}

bool SoftwareIntegrationModule::isDataDirty(
    const SyncableStorage::Identifier& identifier,
    const storage::Key key
) {
    return _syncableStorage.isDirty(identifier, key);
}

void SoftwareIntegrationModule::setDataLoaded(
    const SyncableStorage::Identifier& identifier,
    const storage::Key key
) {
    _syncableStorage.setLoaded(identifier, key);
}

bool SoftwareIntegrationModule::dataLoaded(
        const SyncableStorage::Identifier& identifier,
        const storage::Key key
) {
    return _syncableStorage.hasLoaded(identifier, key);
}

void SoftwareIntegrationModule::internalInitialize(const ghoul::Dictionary&) {
    global::syncEngine->addSyncables(getSyncables());
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "No renderable factory existed");

    fRenderable->registerClass<RenderablePointsCloud>("RenderablePointsCloud");

    if (global::windowDelegate->isMaster()) {
        // The Master node will handle all communication with the external software
	    // and forward it to the Client nodes
        _networkState = softwareintegration::network::serve();

        global::callback::postSyncPreDraw->emplace_back([this]() {
            if (_networkState) {
                for (auto& connections : _networkState->softwareConnections) {
                    connections.second->notifyMessageQueueHandler();
                }
            }
            softwareintegration::messagehandler::postSyncCallbacks();
        });
    }
}

void SoftwareIntegrationModule::internalDeinitialize() {
    global::syncEngine->removeSyncables(getSyncables());
    if (_networkState) {
        softwareintegration::network::stopServer(_networkState);
    }
}

std::vector<documentation::Documentation>
SoftwareIntegrationModule::documentations() const
{
    return {
        RenderablePointsCloud::Documentation(),
    };
}

std::vector<Syncable*> SoftwareIntegrationModule::getSyncables() {
    return { &_syncableStorage };
}

scripting::LuaLibrary SoftwareIntegrationModule::luaLibrary() const {
    return {
        "softwareintegration",
        { codegen::lua::LoadSessionData, codegen::lua::SaveSession }
    };
}

}  // namespace openspace
