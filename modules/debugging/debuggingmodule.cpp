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

#include <modules/debugging/debuggingmodule.h>

#include <modules/debugging/rendering/renderabledebugplane.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/path.h>
#include <openspace/navigation/pathnavigator.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/templatefactory.h>

#include "debuggingmodule_lua.inl"

namespace openspace {

DebuggingModule::DebuggingModule() : OpenSpaceModule(Name) {}

void DebuggingModule::internalInitialize(const ghoul::Dictionary&) {
    ghoul::TemplateFactory<Renderable>* fRenderable =
        FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "No renderable factory existed");

    fRenderable->registerClass<RenderableDebugPlane>("RenderableDebugPlane");
}

std::vector<documentation::Documentation> DebuggingModule::documentations() const {
    return {
        RenderableDebugPlane::Documentation()
    };
}

scripting::LuaLibrary DebuggingModule::luaLibrary() const {
    return {
        "debugging",
        {
            codegen::lua::RenderCameraPath,
            codegen::lua::RemoveRenderedCameraPath,
            codegen::lua::RenderPathControlPoints,
            codegen::lua::RemovePathControlPoints,
            codegen::lua::AddCartesianAxes
        }
    };
}

} // namespace openspace
