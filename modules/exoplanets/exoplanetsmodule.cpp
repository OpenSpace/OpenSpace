/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/exoplanets/exoplanetsmodule.h>
#include <modules/exoplanets/tasks/exoplanetscsvtobintask.h>
#include <modules/exoplanets/rendering/renderableorbitdisc.h>

#include <openspace/util/factorymanager.h>

#include "exoplanetsmodule_lua.inl"

namespace openspace {

const char* _loggerCat = "exoplanets";

using namespace exoplanets;

ExoplanetsModule::ExoplanetsModule() : OpenSpaceModule(Name) {}

void ExoplanetsModule::setClosestExoplanet(Exoplanet closestExo) {
	_exo = closestExo;
}

Exoplanet ExoplanetsModule::getClosestExoplanet() {
	return _exo;
}

void ExoplanetsModule::setStarName(std::string starName) {
	_starName = starName;
}

std::string ExoplanetsModule::getStarName() {
	return _starName;
}
void ExoplanetsModule::setPlsy(std::vector<Exoplanet> plsy) {
    _plsy = plsy;
}
std::vector<Exoplanet> ExoplanetsModule::getPlsy() {
    return _plsy;
}
void ExoplanetsModule::setPlna(std::vector<std::string> plna) {
    _plna = plna;
}
std::vector<std::string> ExoplanetsModule::getPlna() {
    return _plna;
}

scripting::LuaLibrary ExoplanetsModule::luaLibrary() const {

    scripting::LuaLibrary res;
    res.name = "exoplanets";
    res.functions = {
        {
            "addExoplanetSystem",
            &addExoplanetSystem,
            {},
            "string",
            "Adds the nodes to the scene graph of the exoplanet system."
        },
        {
            "removeExoplanetSystem",
            &removeExoplanetSystem,
            {},
            "string",
            "Removes the nodes from the scene graph of the exoplanet system."
        }

    };

    return res;
}

void ExoplanetsModule::internalInitialize(const ghoul::Dictionary&) {

    auto fTask = FactoryManager::ref().factory<Task>();
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fTask, "No task factory existed");
    fTask->registerClass<ExoplanetsCsvToBinTask>("ExoplanetsCsvToBinTask");
    fRenderable->registerClass<RenderableOrbitdisc>("RenderableOrbitdisc");

    OsEng.registerModuleCallback(OpenSpaceEngine::CallbackOption::Initialize, [&] {
        _discoveryMethods = std::make_unique<openspace::exoplanets::DiscoveryMethods>();
        addPropertySubOwner(*_discoveryMethods);
    });
}

std::vector<documentation::Documentation> ExoplanetsModule::documentations() const {
    return {
        ExoplanetsCsvToBinTask::documentation()
    };
}

} // namespace openspace
