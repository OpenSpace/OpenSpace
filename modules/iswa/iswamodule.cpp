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

#include <modules/iswa/iswamodule.h>

#include <openspace/rendering/renderable.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/util/factorymanager.h>

#include <ghoul/misc/assert.h>
#include <ghoul/misc/templatefactory.h>

#include <openspace/engine/openspaceengine.h>

#include <openspace/engine/globalscallbacks.h>

#include <modules/iswa/rendering/textureplane.h>
#include <modules/iswa/rendering/dataplane.h>
#include <modules/iswa/rendering/kameleonplane.h>
#include <modules/iswa/rendering/datasphere.h>
#include <modules/iswa/rendering/screenspacecygnet.h>

#include <modules/iswa/util/iswamanager.h>
#include <openspace/scripting/lualibrary.h>

namespace openspace {

IswaModule::IswaModule() : OpenSpaceModule(Name) {
    global::callback::initialize.push_back([]() { IswaManager::initialize(); });
}

void IswaModule::internalInitialize(const ghoul::Dictionary&) {
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "No renderable factory existed");

    fRenderable->registerClass<TexturePlane>("TexturePlane");
    fRenderable->registerClass<DataPlane>("DataPlane");
    fRenderable->registerClass<KameleonPlane>("KameleonPlane");
    fRenderable->registerClass<DataSphere>("DataSphere");

    auto fScreenSpaceRenderable = FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fScreenSpaceRenderable, "No fScreenSpaceRenderable factory existed");

    fScreenSpaceRenderable->registerClass<ScreenSpaceCygnet>("ScreenSpaceCygnet");
}

scripting::LuaLibrary IswaModule::luaLibrary() const {
    return IswaManager::luaLibrary();
}

} // namespace openspace
