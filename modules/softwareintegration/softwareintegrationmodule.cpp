/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/engine/globals.h>
#include <openspace/engine/syncengine.h>
#include <modules/softwareintegration/rendering/renderablepointscloud.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/logging/logmanager.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>

namespace {
    constexpr const char* _loggerCat = "SoftwareIntegrationModule";
} // namespace

namespace openspace {

SoftwareIntegrationModule::SoftwareIntegrationModule() : OpenSpaceModule(Name) {
    if (global::windowDelegate->isMaster()) {
        // The Master node will handle all communication with the external software
        // and forward it to the Client nodes
        // 4700, is the defualt port where the tcp socket will be opened to the ext. software
        _networkEngine = std::make_unique<NetworkEngine>();
    }
}

SoftwareIntegrationModule::~SoftwareIntegrationModule() {
    internalDeinitialize();
}

void SoftwareIntegrationModule::storeData(const std::string& key, const std::vector<float>& data) {
    _syncableFloatDataStorage.store(key, data);
}

const std::vector<float>& SoftwareIntegrationModule::fetchData(const std::string& key) {
    return _syncableFloatDataStorage.fetch(key);
}

bool SoftwareIntegrationModule::isDataDirty(const std::string& key) {
    return _syncableFloatDataStorage.isDirty(key);
}

void SoftwareIntegrationModule::internalInitialize(const ghoul::Dictionary&) {
    global::syncEngine->addSyncables(getSyncables());
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "No renderable factory existed");

    fRenderable->registerClass<RenderablePointsCloud>("RenderablePointsCloud");

    if (global::windowDelegate->isMaster()) {
        _networkEngine->start();

        global::callback::postSyncPreDraw->emplace_back([this]() {
            if (!_networkEngine) return;
            _networkEngine->postSync();
        });
    }
}

void SoftwareIntegrationModule::internalDeinitialize() {
    global::syncEngine->removeSyncables(getSyncables());
}

std::vector<documentation::Documentation>
SoftwareIntegrationModule::documentations() const
{
    return {
        RenderablePointsCloud::Documentation(),
    };
}

std::vector<Syncable*> SoftwareIntegrationModule::getSyncables() {
    return { &_syncableFloatDataStorage };
}

} // namespace openspace
