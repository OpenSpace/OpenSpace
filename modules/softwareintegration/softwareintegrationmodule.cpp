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

#include <modules/softwareintegration/rendering/renderablepointscloud.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "SoftwareIntegrationModule";
} // namespace

namespace openspace {

SoftwareIntegrationModule::SoftwareIntegrationModule() : OpenSpaceModule(Name) {}

void SoftwareIntegrationModule::storeData(const std::string& key,
                                          const std::vector<float> data)
{
    _temporaryDataStorage.emplace(key, std::move(data));
}

std::vector<float> SoftwareIntegrationModule::fetchData(const std::string& key) {
    auto it = _temporaryDataStorage.find(key);
    if (it == _temporaryDataStorage.end()) {
        LERROR(fmt::format(
            "Could not find data with key '{}' in the temporary data storage", key
        ));
        return std::vector<float>();
    }

    std::vector<float> data = it->second;
    _temporaryDataStorage.erase(it);

    return std::move(data);
}

void SoftwareIntegrationModule::internalInitialize(const ghoul::Dictionary&) {
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "No renderable factory existed");

    fRenderable->registerClass<RenderablePointsCloud>("RenderablePointsCloud");

    // Open port
    _server.start(4700);

    global::callback::preSync->emplace_back([this]() { _server.update(); });
}

void SoftwareIntegrationModule::internalDeinitialize() {
    _server.stop();
}

std::vector<documentation::Documentation>
SoftwareIntegrationModule::documentations() const
{
    return {
        RenderablePointsCloud::Documentation(),
    };
}

} // namespace openspace
