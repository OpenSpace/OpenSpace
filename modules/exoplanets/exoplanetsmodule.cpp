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

#include <modules/exoplanets/exoplanetsmodule.h>

#include <modules/exoplanets/rendering/renderableorbitdisc.h>
#include <modules/exoplanets/tasks/exoplanetsdatapreparationtask.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/factorymanager.h>
#include <thread>
#include <chrono>

#include "exoplanetsmodule_lua.inl"

namespace openspace {

using namespace exoplanets;

ExoplanetsModule::ExoplanetsModule() : OpenSpaceModule(Name) {}

scripting::LuaLibrary ExoplanetsModule::luaLibrary() const {
    scripting::LuaLibrary res;
    res.name = "exoplanets";
    res.functions = {
        {
            "addExoplanetSystem",
            &exoplanets::luascriptfunctions::addExoplanetSystem,
            {},
            "string or list of strings",
            "Add one or multiple exoplanet systems to the scene, as specified by the "
            "input. An input string should be the name of the system host star"
        },
        {
            "removeExoplanetSystem",
            &exoplanets::luascriptfunctions::removeExoplanetSystem,
            {},
            "string",
            "Removes the nodes of the specified exoplanet system from the scene graph"
        },
        {
            "listAvailableExoplanetSystems",
            &exoplanets::luascriptfunctions::listAvailableExoplanetSystems,
            {},
            "",
            "Prints a list with the names of all exoplanet systems that can be added to "
            "the scene graph to the OpenSpace Log"
        },
        {
            "getListOfExoplanets",
            &exoplanets::luascriptfunctions::getListOfExoplanets,
            {},
            "",
            "Gets a list with the names of all exoplanet systems"
        }
    };

    return res;
}

void ExoplanetsModule::internalInitialize(const ghoul::Dictionary&) {
    auto fTask = FactoryManager::ref().factory<Task>();
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fTask, "No task factory existed");
    fTask->registerClass<ExoplanetsDataPreparationTask>("ExoplanetsDataPreparationTask");
    fRenderable->registerClass<RenderableOrbitDisc>("RenderableOrbitDisc");
}

std::vector<documentation::Documentation> ExoplanetsModule::documentations() const {
    return {
        ExoplanetsDataPreparationTask::documentation(),
        RenderableOrbitDisc::Documentation()
    };
}

} // namespace openspace
