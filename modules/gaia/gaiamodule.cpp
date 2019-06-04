/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/gaia/gaiamodule.h>

#include <modules/gaia/tasks/constructoctreetask.h>
#include <modules/gaia/rendering/renderablegaiastars.h>
#include <modules/gaia/tasks/readfitstask.h>
#include <modules/gaia/tasks/readspecktask.h>
#include <openspace/documentation/documentation.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>

namespace openspace {

GaiaModule::GaiaModule() : OpenSpaceModule(Name) {}

void GaiaModule::internalInitialize(const ghoul::Dictionary&) {
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "No renderable factory existed");
    fRenderable->registerClass<RenderableGaiaStars>("RenderableGaiaStars");

    auto fTask = FactoryManager::ref().factory<Task>();
    ghoul_assert(fRenderable, "No task factory existed");
    fTask->registerClass<ReadFitsTask>("ReadFitsTask");
    fTask->registerClass<ReadSpeckTask>("ReadSpeckTask");
    fTask->registerClass<ConstructOctreeTask>("ConstructOctreeTask");
}

std::vector<documentation::Documentation> GaiaModule::documentations() const {
    return {
        RenderableGaiaStars::Documentation(),
        ReadFitsTask::Documentation(),
        ReadSpeckTask::Documentation(),
        ConstructOctreeTask::Documentation(),
    };
}

scripting::LuaLibrary GaiaModule::luaLibrary() const {
    scripting::LuaLibrary res;
    res.name = "gaia";
    res.scripts = {
        absPath("${MODULE_GAIA}/scripts/filtering.lua")
    };
    return res;
}

} // namespace openspace
