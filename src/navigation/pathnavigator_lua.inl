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

namespace {

/**
 * Returns true if a camera path is currently running, and false otherwise.
 *
 * \return Whether a camera path is currently active, or not
 */
[[codegen::luawrap]] bool isFlying() {
    using namespace openspace;
    return global::openSpaceEngine->currentMode() == OpenSpaceEngine::Mode::CameraPath;
}

/**
 * Continue playing a paused camera path.
 */
[[codegen::luawrap]] void continuePath() {
    openspace::global::navigationHandler->pathNavigator().continuePath();
}

/**
 * Pause a playing camera path.
 */
[[codegen::luawrap]] void pausePath() {
    openspace::global::navigationHandler->pathNavigator().pausePath();
}

/**
 * Stops a path, if one is being played.
 */
[[codegen::luawrap]] void stopPath() {
    openspace::global::navigationHandler->pathNavigator().abortPath();
}

/**
 * Immediately skips to the end of the current camera path, if one is being played.
 */
[[codegen::luawrap]] void skipToEnd() {
    openspace::global::navigationHandler->pathNavigator().skipToEnd();
}

/**
 * Move the camera to the node with the specified identifier. The optional double
 * specifies the duration of the motion, in seconds. If the optional bool is set to true
 * the target up vector for camera is set based on the target node. Either of the optional
 * parameters can be left out.
 */
[[codegen::luawrap]] void flyTo(std::string nodeIdentifier,
                      std::optional<std::variant<bool, double>> useUpFromTargetOrDuration,
                                                           std::optional<double> duration)
{
    using namespace openspace;
    if (useUpFromTargetOrDuration.has_value() &&
        std::holds_alternative<double>(*useUpFromTargetOrDuration) &&
        duration.has_value())
    {
        throw ghoul::lua::LuaError("Duration cannot be specified twice");
    }

    if (!sceneGraphNode(nodeIdentifier)) {
        throw ghoul::lua::LuaError("Unknown node name: " + nodeIdentifier);
    }

    ghoul::Dictionary insDict;
    insDict.setValue("TargetType", std::string("Node"));
    insDict.setValue("Target", nodeIdentifier);
    if (useUpFromTargetOrDuration.has_value()) {
        if (std::holds_alternative<bool>(*useUpFromTargetOrDuration)) {
            insDict.setValue(
                "UseTargetUpDirection",
                std::get<bool>(*useUpFromTargetOrDuration)
            );
        }
        else {
            double d = std::get<double>(*useUpFromTargetOrDuration);
            if (d < 0.0) {
                throw ghoul::lua::LuaError("Duration must be a positive value");
            }
            insDict.setValue("Duration", d);
        }
    }
    if (duration.has_value()) {
        double d = *duration;
        if (d < 0.0) {
            throw ghoul::lua::LuaError("Duration must be a positive value");
        }
        insDict.setValue("Duration", d);
    }

    global::navigationHandler->pathNavigator().createPath(insDict);

    if (global::navigationHandler->pathNavigator().hasCurrentPath()) {
        global::navigationHandler->pathNavigator().startPath();
    }
}

/**
 * Move the camera to the node with the specified identifier. The second argument is the
 * desired target height above the target node's bounding sphere, in meters. The optional
 * double specifies the duration of the motion, in seconds. If the optional bool is set to
 * true, the target up vector for camera is set based on the target node. Either of the
 * optional parameters can be left out.
 */
[[codegen::luawrap]] void flyToHeight(std::string nodeIdentifier, double height,
                      std::optional<std::variant<bool, double>> useUpFromTargetOrDuration,
                                                           std::optional<double> duration)
{
    using namespace openspace;
    if (!sceneGraphNode(nodeIdentifier)) {
        throw ghoul::lua::LuaError("Unknown node name: " + nodeIdentifier);
    }

    ghoul::Dictionary insDict;
    insDict.setValue("TargetType", std::string("Node"));
    insDict.setValue("Target", nodeIdentifier);
    insDict.setValue("Height", height);
    if (useUpFromTargetOrDuration.has_value()) {
        if (std::holds_alternative<bool>(*useUpFromTargetOrDuration)) {
            insDict.setValue(
                "UseTargetUpDirection",
                std::get<bool>(*useUpFromTargetOrDuration)
            );
        }
        else {
            double d = std::get<double>(*useUpFromTargetOrDuration);
            if (d < 0.0) {
                throw ghoul::lua::LuaError("Duration must be a positive value");
            }
            insDict.setValue("Duration", d);
        }
    }
    if (duration.has_value()) {
        double d = *duration;
        if (d < 0.0) {
            throw ghoul::lua::LuaError("Duration must be a positive value");
        }
        insDict.setValue("Duration", d);
    }

    global::navigationHandler->pathNavigator().createPath(insDict);

    if (global::navigationHandler->pathNavigator().hasCurrentPath()) {
        global::navigationHandler->pathNavigator().startPath();
    }
}

/**
 * Create a path to the navigation state described by the input table. Note that roll must
 * be included for the target up direction in the navigation state to be taken into
 * account.
 *
 * \param navigationState A [NavigationState](#core_navigation_state) to fly to
 * \param duration An optional duration for the motion to take, in seconds. For example,
 *                 a value of 5 means "fly to this position over a duration of 5 seconds"
 */
[[codegen::luawrap]] void flyToNavigationState(ghoul::Dictionary navigationState,
                                               std::optional<double> duration)
{
    using namespace openspace;
    try {
        documentation::testSpecificationAndThrow(
            interaction::NavigationState::Documentation(),
            navigationState,
            "NavigationState"
        );
    }
    catch (const documentation::SpecificationError& e) {
        logError(e, "flyToNavigationState");
        throw ghoul::lua::LuaError(std::format("Unable to create a path: {}", e.what()));
    }

    ghoul::Dictionary instruction;
    instruction.setValue("TargetType", std::string("NavigationState"));
    instruction.setValue("NavigationState", navigationState);

    if (duration.has_value()) {
        double d = *duration;
        if (d < 0.0) {
            throw ghoul::lua::LuaError("Duration must be a positive value");
        }
        instruction.setValue("Duration", d);
    }

    global::navigationHandler->pathNavigator().createPath(instruction);

    if (global::navigationHandler->pathNavigator().hasCurrentPath()) {
        global::navigationHandler->pathNavigator().startPath();
    }
}

/**
 * Zoom linearly to the current focus node, using the default distance.
 *
 * \param duration An optional duration for the motion to take, in seconds. For example,
 *                 a value of 5 means "zoom in over 5 seconds"
 */
[[codegen::luawrap]] void zoomToFocus(std::optional<double> duration) {
    using namespace openspace;
    const SceneGraphNode* node = global::navigationHandler->anchorNode();
    if (!node) {
        throw ghoul::lua::LuaError("Could not determine current focus node");
    }

    ghoul::Dictionary insDict;
    insDict.setValue("TargetType", std::string("Node"));
    insDict.setValue("Target", node->identifier());
    insDict.setValue("PathType", std::string("Linear"));

    if (duration.has_value()) {
        double d = *duration;
        if (d < 0.0) {
            throw ghoul::lua::LuaError("Duration must be a positive value");
        }
        insDict.setValue("Duration", d);
    }

    global::navigationHandler->pathNavigator().createPath(insDict);

    if (global::navigationHandler->pathNavigator().hasCurrentPath()) {
        global::navigationHandler->pathNavigator().startPath();
    }
}

/**
 * Fly linearly to a specific distance in relation to the focus node.
 *
 * \param distance The distance to fly to, in meters above the bounding sphere.
 * \param duration An optional duration for the motion to take, in seconds.
 */
[[codegen::luawrap]] void zoomToDistance(double distance, std::optional<double> duration)
{
    using namespace openspace;
    if (distance <= 0.0) {
        throw ghoul::lua::LuaError("The distance must be larger than zero");
    }

    const SceneGraphNode* node = global::navigationHandler->anchorNode();
    if (!node) {
        throw ghoul::lua::LuaError("Could not determine current focus node");
    }

    ghoul::Dictionary insDict;
    insDict.setValue("TargetType", std::string("Node"));
    insDict.setValue("Target", node->identifier());
    insDict.setValue("Height", distance);
    insDict.setValue("PathType", std::string("Linear"));

    if (duration.has_value()) {
        double d = *duration;
        if (d < 0.0) {
            throw ghoul::lua::LuaError("Duration must be a positive value");
        }
        insDict.setValue("Duration", d);
    }

    global::navigationHandler->pathNavigator().createPath(insDict);

    if (global::navigationHandler->pathNavigator().hasCurrentPath()) {
        global::navigationHandler->pathNavigator().startPath();
    }
}

/**
 * Fly linearly to a specific distance in relation to the focus node, given as a relative
 * value based on the size of the object rather than in meters.
 *
 * \param distance The distance to fly to, given as a multiple of the bounding sphere of
 *                 the current focus node bounding sphere. A value of 1 will result in a
 *                 position at a distance of one times the size of the bounding
 *                 sphere away from the object.
 * \param duration An optional duration for the motion, in seconds.
 */
[[codegen::luawrap]] void zoomToDistanceRelative(double distance,
                                                 std::optional<double> duration)
{
    using namespace openspace;
    if (distance <= 0.0) {
        throw ghoul::lua::LuaError("The distance must be larger than zero");
    }

    const SceneGraphNode* node = global::navigationHandler->anchorNode();
    if (!node) {
        throw ghoul::lua::LuaError("Could not determine current focus node");
    }

    distance *= node->boundingSphere();

    ghoul::Dictionary insDict;
    insDict.setValue("TargetType", std::string("Node"));
    insDict.setValue("Target", node->identifier());
    insDict.setValue("Height", distance);
    insDict.setValue("PathType", std::string("Linear"));

  if (duration.has_value()) {
        double d = *duration;
        if (d < 0.0) {
            throw ghoul::lua::LuaError("Duration must be a positive value");
        }
        insDict.setValue("Duration", d);
    }

    global::navigationHandler->pathNavigator().createPath(insDict);

    if (global::navigationHandler->pathNavigator().hasCurrentPath()) {
        global::navigationHandler->pathNavigator().startPath();
    }
}

/**
 * Fade rendering to black, jump to the specified node, and then fade in. This is done by
 * triggering another script that handles the logic.
 *
 * \param navigationState A [NavigationState](#core_navigation_state) to jump to
 * \param fadeDuration An optional duration for the fading. If not included, the
 *                     property in Navigation Handler will be used
 */
[[codegen::luawrap]] void jumpToNavigationState(ghoul::Dictionary navigationState,
                                                std::optional<double> fadeDuration)
{
    using namespace openspace;
    try {
        documentation::testSpecificationAndThrow(
            interaction::NavigationState::Documentation(),
            navigationState,
            "NavigationState"
        );
    }
    catch (const documentation::SpecificationError& e) {
        logError(e, "jumpToNavigationState");
        throw ghoul::lua::LuaError(std::format(
            "Unable to jump to navigation state: {}", e.what()
        ));
    }

    // When copy pasting a printed navigation state from the console, the formatting of
    // the navigation state dictionary won't be completely correct if using the
    // dictionary directly, due to the number keys for arrays. We solve this by first
    // creating an object of the correct datatype
    // (@TODO emmbr 2024-04-03, This formatting problem should probably be fixed)
    interaction::NavigationState ns = interaction::NavigationState(navigationState);

    const std::string script = std::format(
        "openspace.navigation.setNavigationState({})", ghoul::formatLua(ns.dictionary())
    );

    if (fadeDuration.has_value()) {
        global::navigationHandler->triggerFadeToTransition(
            std::move(script),
            static_cast<float>(*fadeDuration)
        );
    }
    else {
        global::navigationHandler->triggerFadeToTransition(script);
    }
}

/**
 * Fade rendering to black, jump to the specified navigation state, and then fade in.
 * This is done by triggering another script that handles the logic.
 *
 * \param nodeIdentifier The identifier of the scene graph node to jump to
 * \param fadeDuration An optional duration for the fading. If not included, the
 *                     property in Navigation Handler will be used
 */
[[codegen::luawrap]] void jumpTo(std::string nodeIdentifier,
                                 std::optional<double> fadeDuration)
{
    using namespace openspace;
    if (SceneGraphNode* n = sceneGraphNode(nodeIdentifier);  !n) {
        throw ghoul::lua::LuaError("Unknown node name: " + nodeIdentifier);
    }

    const std::string script = std::format(
        "openspace.pathnavigation.flyTo('{}', 0)", nodeIdentifier
    );

    if (fadeDuration.has_value()) {
        global::navigationHandler->triggerFadeToTransition(
            std::move(script),
            static_cast<float>(*fadeDuration)
        );
    }
    else {
        global::navigationHandler->triggerFadeToTransition(script);
    }
}

/**
 * Create a camera path as described by the instruction in the input argument.
 *
 * \param pathInstruction A table representing a
 *                        [PathInstruction](#core_path_instruction) that describes a
 *                        camera path to be created
 */
[[codegen::luawrap]] void createPath(ghoul::Dictionary pathInstruction) {
    using namespace openspace;
    global::navigationHandler->pathNavigator().createPath(pathInstruction);
    if (global::navigationHandler->pathNavigator().hasCurrentPath()) {
        global::navigationHandler->pathNavigator().startPath();
    }
}

#include "pathnavigator_lua_codegen.cpp"

} // namespace
