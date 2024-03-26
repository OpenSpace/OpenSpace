/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/util/factorymanager.h>

#include <openspace/documentation/documentationengine.h>
#include <openspace/documentation/documentation.h>
#include <openspace/rendering/dashboarditem.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/lightsource.h>
#include <openspace/scene/rotation.h>
#include <openspace/scene/scale.h>
#include <openspace/scene/timeframe.h>
#include <openspace/scene/translation.h>
#include <openspace/util/resourcesynchronization.h>
#include <openspace/util/task.h>
#include <sstream>

namespace openspace {

FactoryManager* FactoryManager::_manager = nullptr;

FactoryManager::FactoryNotFoundError::FactoryNotFoundError(std::string t)
    : ghoul::RuntimeError("Could not find TemplateFactory for type '" + t + "'")
    , type(std::move(t))
{
    ghoul_assert(!type.empty(), "Type must not be empty");
}

FactoryManager::FactoryManager() {}

void FactoryManager::initialize() {
    ghoul_assert(!_manager, "Factory Manager must not have been initialized");

    _manager = new FactoryManager;
    _manager->addFactory<Renderable>("Renderable");
    _manager->addFactory<Translation>("Translation");
    _manager->addFactory<Rotation>("Rotation");
    _manager->addFactory<Scale>("Scale");
    _manager->addFactory<TimeFrame>("TimeFrame");
    _manager->addFactory<LightSource>("LightSource");
    _manager->addFactory<Task>("Task");
    _manager->addFactory<ResourceSynchronization>("ResourceSynchronization");
    _manager->addFactory<DashboardItem>("DashboardItem");
}

void FactoryManager::deinitialize() {
    ghoul_assert(_manager, "Factory Manager must have been initialized");

    delete _manager;
    _manager = nullptr;
}

bool FactoryManager::isInitialized() {
    return _manager != nullptr;
}

FactoryManager& FactoryManager::ref() {
    ghoul_assert(_manager, "Factory Manager must have been initialized");

    return *_manager;
}

const std::vector<FactoryManager::FactoryInfo>& FactoryManager::factories() const {
    return _factories;
}

}  // namespace openspace
