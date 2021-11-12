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

#include <modules/debugging/debuggingmodule.h>

#include <modules/debugging/rendering/renderabledebugplane.h>
#include <openspace/documentation/documentation.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/templatefactory.h>

#include "debuggingmodule_lua.inl"

namespace openspace {

DebuggingModule::DebuggingModule() : OpenSpaceModule(Name) {}

void DebuggingModule::internalInitialize(const ghoul::Dictionary&) {
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "No renderable factory existed");

    fRenderable->registerClass<RenderableDebugPlane>("RenderableDebugPlane");
}

std::vector<documentation::Documentation> DebuggingModule::documentations() const {
    return {
        RenderableDebugPlane::Documentation()
    };
}

scripting::LuaLibrary DebuggingModule::luaLibrary() const {
    scripting::LuaLibrary res;
    res.name = "debugging";
    res.functions = {
        {
            "renderCameraPath",
            &luascriptfunctions::renderCameraPath,
            "[number, bool, number]",
            "Render the current camera path from the path navigation system. The "
            "first optional argument is the number of samples to take along the path "
            "(defaults to 100). If a second optional argument is included and set to "
            "true, a line indicating the camera view direction along the path will "
            "also be rendered. This can be useful when debugging camera orientations. "
            "Finally, the third optional argument can be used to set the length "
            "(in meter) of the view direction lines"
        },
        {
            "removeRenderedCameraPath",
            &luascriptfunctions::removeRenderedCameraPath,
            "",
            "Removes the rendered camera path, if there is one"
        },
        {
            "renderPathControlPoints",
            &luascriptfunctions::renderPathControlPoints,
            "[number]",
            "Render the control points for the camera path spline as spheres. The "
            "optional argument can be used to set the radius of the created spheres. "
        },
        {
            "removePathControlPoints",
            &luascriptfunctions::removePathControlPoints,
            "",
            "Removes the rendered control points"
        },
        {
            "addCartesianAxes",
            &luascriptfunctions::addCartesianAxes,
            "string, [number]",
            "Adds a set of Cartesian axes to the scene graph node identified by the "
            "first string, to illustrate its local coordinate system. The second "
            "(optional) argument is a scale value, in meters."
        }
    };

    return res;
}

} // namespace openspace
