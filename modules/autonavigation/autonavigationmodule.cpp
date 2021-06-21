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

#include <modules/autonavigation/autonavigationmodule.h>

#include <modules/autonavigation/autonavigationmodule_lua.inl>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const openspace::properties::Property::PropertyInfo MinBoundingSphereInfo = {
        "MinimalValidBoundingSphere",
        "Minimal Valid Bounding Sphere",
        "The minimal allowed value for a bounding sphere, in meters. Used for "
        "computation of target positions and path generation, to avoid issues when "
        "there is no bounding sphere."
    };

    constexpr openspace::properties::Property::PropertyInfo RelevantNodeTagsInfo = {
        "RelevantNodeTags",
        "Relevant Node Tags",
        "List of tags for the nodes that are relevant for path creation, for example "
        "when avoiding collisions."
    };
} // namespace

namespace openspace {

AutoNavigationModule::AutoNavigationModule()
    : OpenSpaceModule(Name)
    , _minValidBoundingSphere(MinBoundingSphereInfo, 10.0, 1.0, 3e10)
    , _relevantNodeTags(RelevantNodeTagsInfo)
{
    addPropertySubOwner(_pathNavigationHandler);
    addProperty(_minValidBoundingSphere);

    _relevantNodeTags = std::vector<std::string>{
        "planet_solarSystem",
        "moon_solarSystem"
    };;
    _relevantNodeTags.onChange([this]() { findRelevantNodes(); });
    addProperty(_relevantNodeTags);
}

pathnavigation::PathNavigationHandler& AutoNavigationModule::PathNavigationHandler() {
    return _pathNavigationHandler;
}

double AutoNavigationModule::minValidBoundingSphere() const {
    return _minValidBoundingSphere;
}

const std::vector<SceneGraphNode*>& AutoNavigationModule::relevantNodes() {
    if (!_hasInitializedRelevantNodes) {
        findRelevantNodes();
        _hasInitializedRelevantNodes = true;
    }

    return _relevantNodes;
}

void AutoNavigationModule::findRelevantNodes() {
    const std::vector<SceneGraphNode*>& allNodes =
        global::renderEngine->scene()->allSceneGraphNodes();

    const std::vector<std::string> relevantTags = _relevantNodeTags;

    if (allNodes.empty() || relevantTags.empty()) {
        _relevantNodes = std::vector<SceneGraphNode*>();
        return;
    }

    auto isRelevant = [&](const SceneGraphNode* node) {
        const std::vector<std::string> tags = node->tags();
        auto result = std::find_first_of(
            relevantTags.begin(),
            relevantTags.end(),
            tags.begin(),
            tags.end()
        );

        // does not match any tags => not interesting
        if (result == relevantTags.end()) {
            return false;
        }

        return node->renderable() && (node->boundingSphere() > 0.0);
    };

    std::vector<SceneGraphNode*> resultingNodes;
    std::copy_if(
        allNodes.begin(),
        allNodes.end(),
        std::back_inserter(resultingNodes),
        isRelevant
    );

    _relevantNodes = resultingNodes;
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
            &pathnavigation::luascriptfunctions::isFlying,
            {},
            "",
            "Returns true if a camera path is currently running, and false otherwise."
        },
        {
            "continuePath",
            &pathnavigation::luascriptfunctions::continuePath,
            {},
            "",
            "Continue playing a paused camera path."
        },
        {
            "pausePath",
            &pathnavigation::luascriptfunctions::pausePath,
            {},
            "",
            "Pause a playing camera path."
        },
        {
            "stopPath",
            &pathnavigation::luascriptfunctions::stopPath,
            {},
            "",
            "Stops a path, if one is being played."
        },
        {
            "goTo",
            &pathnavigation::luascriptfunctions::goTo,
            {},
            "string [, bool, double]",
            "Move the camera to the node with the specified name. The optional double "
            "specifies the duration of the motion. If the optional bool is set to true "
            "the target up vector for camera is set based on the target node. Either of "
            "the optional parameters can be left out."
        },
        {
            "goToHeight",
            &pathnavigation::luascriptfunctions::goToHeight,
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
            &pathnavigation::luascriptfunctions::goToGeo,
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
            &pathnavigation::luascriptfunctions::generatePath,
            {},
            "table",
            "Generate the path as described by the lua table input argument. "
        },
        {
            "getPathPositions",
            &pathnavigation::luascriptfunctions::getPathPositions,
            {},
            "number",
            "FOR DEBUG. Sample positions along the path. The input argument is the "
            "number of samples per path segment. "
        },
        {
            "getPathOrientations",
            &pathnavigation::luascriptfunctions::getPathOrientations,
            {},
            "number",
            "FOR DEBUG. Sample orientations along the path. The input argument is the "
            "number of samples per path segment. "
        },
         {
            "getPathViewDirections",
            &pathnavigation::luascriptfunctions::getPathViewDirections,
            {},
            "number",
            "FOR DEBUG. Sample view directions along the path. The input argument is "
            "the number of samples per path segment. "
        },
        {
            "getControlPoints",
            &pathnavigation::luascriptfunctions::getControlPoints,
            {},
            "",
            "FOR DEBUG. Get control point positions from all pathsegments"
        }
    };
    return res;
}

void AutoNavigationModule::internalInitialize(const ghoul::Dictionary&) {
    global::callback::preSync->emplace_back([this]() {
        _pathNavigationHandler.updateCamera(global::windowDelegate->deltaTime());
    });
}

} // namespace openspace
