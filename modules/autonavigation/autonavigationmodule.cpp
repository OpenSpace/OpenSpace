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

#include <modules/autonavigation/autonavigationmodule.h>

#include <modules/autonavigation/autonavigationmodule_lua.inl>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const openspace::properties::Property::PropertyInfo MinBoundingSphereInfo = {
        "MinimalValidBoundingSphere",
        "Minimal Valid Bounding Sphere",
        "The minimal allowed value for a bounding sphere. Used for computation of target "
        "positions and path generation, to avoid issues when there is no bounding sphere."
    };
} // namespace

namespace openspace {

AutoNavigationModule::AutoNavigationModule()
    : OpenSpaceModule(Name),
    _minValidBoundingSphere(MinBoundingSphereInfo, 10.0, 1.0, 3e10)
{
    addPropertySubOwner(_autoNavigationHandler);
    addProperty(_minValidBoundingSphere);
}

autonavigation::AutoNavigationHandler& AutoNavigationModule::AutoNavigationHandler() {
    return _autoNavigationHandler;
}

double AutoNavigationModule::minValidBoundingSphere() const {
    return _minValidBoundingSphere;
}

std::vector<documentation::Documentation> AutoNavigationModule::documentations() const {
    return {
        autonavigation::Instruction::Documentation(),
        autonavigation::PathSpecification::Documentation()
    };
}

scripting::LuaLibrary AutoNavigationModule::luaLibrary() const {
    scripting::LuaLibrary res;
    res.name = "autonavigation";
    res.scripts = {
        absPath("${MODULE_AUTONAVIGATION}/scripts/rendering.lua")
    };
    res.functions = {
        {
            "isFlying",
            &autonavigation::luascriptfunctions::isFlying,
            {},
            "",
            "Returns true if a camera path is currently running, and false otherwise."
        },
        {
            "continuePath",
            &autonavigation::luascriptfunctions::continuePath,
            {},
            "",
            "Continue playing a paused camera path."
        },
        {
            "stopPath",
            &autonavigation::luascriptfunctions::stopPath,
            {},
            "",
            "Stops a path, if one is being played."
        },
        {
            "goTo",
            &autonavigation::luascriptfunctions::goTo,
            {},
            "string [, bool, double]",
            "Move the camera to the node with the specified name. The optional double "
            "specifies the duration of the motion. If the optional bool is set to true "
            "the target up vector for camera is set based on the target node. Either of "
            "the optional parameters can be left out."
        },
        {
            "goToHeight",
            &autonavigation::luascriptfunctions::goToHeight,
            {},
            "string, double [, bool, double]",
            "Move the camera to the node with the specified name. The second input "
            "parameter is the desired target height. The optional double "
            "specifies the duration of the motion. If the optional bool is set to true "
            "the target up vector for camera is set based on the target node. Either of "
            "the optional parameters can be left out."
        },
        {
            "goToGeo",
            &autonavigation::luascriptfunctions::goToGeo,
            {},
            "string, double, double, double [, bool, double]",
            "Move the camera to the globe with the name given by the input string. "
            "The next three input parameters are latitude, longitude and altitude. "
            "The optional double specifies the duration of the motion. If the optional "
            "bool is set to true the target up vector for camera is set based on the "
            "target node. Either of the optional parameters can be left out."
        },
        {
            "generatePath",
            &autonavigation::luascriptfunctions::generatePath,
            {},
            "table",
            "Generate the path as described by the lua table input argument. "
        },
        {
            "generatePathFromFile",
            &autonavigation::luascriptfunctions::generatePathFromFile,
            {},
            "string",
            "Read an input file with lua instructions and use those to generate a camera "
            "path. "
        },
        {
            "getPathPositions",
            &autonavigation::luascriptfunctions::getPathPositions,
            {},
            "number",
            "FOR DEBUG. Sample positions along the path. The input argument is the "
            "number of samples per path segment. "
        },
        {
            "getPathOrientations",
            &autonavigation::luascriptfunctions::getPathOrientations,
            {},
            "number",
            "FOR DEBUG. Sample orientations along the path. The input argument is the "
            "number of samples per path segment. "
        },
         {
            "getPathViewDirections",
            &autonavigation::luascriptfunctions::getPathViewDirections,
            {},
            "number",
            "FOR DEBUG. Sample view directions along the path. The input argument is "
            "the number of samples per path segment. "
        },
        {
            "getControlPoints",
            &autonavigation::luascriptfunctions::getControlPoints,
            {},
            "",
            "FOR DEBUG. Get control point positions from all pathsegments"
        }
    };
    return res;
}

void AutoNavigationModule::internalInitialize(const ghoul::Dictionary&) {
    global::callback::preSync->emplace_back([this]() {
        _autoNavigationHandler.updateCamera(global::windowDelegate->deltaTime());
    });
}

} // namespace openspace
