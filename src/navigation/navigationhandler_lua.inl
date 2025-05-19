/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <ghoul/lua/lua_helper.h>

#include <openspace/util/geodetic.h>

namespace {

/**
 * Set the camera position by loading a [NavigationState](#core_navigation_state) from
 * file. The file should be in json format, such as the output files of
 * `saveNavigationState`.
 *
 * \param filePath the path to the file, including the file name (and extension, if it is
 *                 anything other than `.navstate`)
 * \param useTimeStamp if true, and the provided NavigationState includes a timestamp,
 *                     the time will be set as well.
 */
[[codegen::luawrap]] void loadNavigationState(std::string filePath,
                                              bool useTimeStamp = false)
{
    if (filePath.empty()) {
        throw ghoul::lua::LuaError("Filepath string is empty");
    }

    openspace::global::navigationHandler->loadNavigationState(
        filePath,
        useTimeStamp
    );
}

/**
 * Return the current [NavigationState](#core_navigation_state) as a Lua table.
 *
 * By default, the reference frame will be picked based on whether the orbital navigator
 * is currently following the anchor node rotation. If it is, the anchor will be chosen as
 * reference frame. If not, the reference frame will be set to the scene graph root.
 *
 * \param frame the identifier of an optional scene graph node to use as reference frame
 *              for the NavigationState
 *
 * \return a Lua table representing the current NavigationState of the camera
 */
[[codegen::luawrap]] ghoul::Dictionary getNavigationState(
                                                         std::optional<std::string> frame)
{
    using namespace openspace;

    interaction::NavigationState state;
    if (frame.has_value()) {
        const SceneGraphNode* referenceFrame = sceneGraphNode(*frame);
        if (!referenceFrame) {
            throw ghoul::lua::LuaError(
                std::format("Could not find node '{}' as reference frame", *frame)
            );
        }
        state = global::navigationHandler->navigationState(*referenceFrame);
    }
    else {
        state = global::navigationHandler->navigationState();
    }

    return state.dictionary();
}

/**
 * Set the camera position from a provided [NavigationState](#core_navigation_state).
 *
 * \param navigationState a table describing the NavigationState to set
 * \param useTimeStamp if true, and the provided NavigationState includes a timestamp,
 *                     the time will be set as well
 */
[[codegen::luawrap]] void setNavigationState(ghoul::Dictionary navigationState,
                                             bool useTimeStamp = false)
{
    using namespace openspace;

    interaction::NavigationState ns = interaction::NavigationState(navigationState);

    global::navigationHandler->setNavigationStateNextFrame(ns);

    if (useTimeStamp && ns.timestamp.has_value()) {
        global::timeManager->setTimeNextFrame(Time(*ns.timestamp));
    }
}

/**
 * Save the current [NavigationState](#core_navigation_state) to a file with the path
 * given by the first argument.
 *
 * By default, the reference frame will be picked based on whether the orbital navigator
 * is currently following the anchor node rotation. If it is, the anchor will be chosen as
 * reference frame. If not, the reference frame will be set to the scene graph root.
 *
 * \param path the file path for where to save the NavigationState, including the file
 *             name. If no extension is added, the file is saved as a `.navstate` file.
 * \param frame the identifier of the scene graph node which coordinate system should be
 *              used as a reference frame for the NavigationState.
 */
[[codegen::luawrap]] void saveNavigationState(std::string path, std::string frame = "") {
    if (path.empty()) {
        throw ghoul::lua::LuaError("Filepath string is empty");
    }
    openspace::global::navigationHandler->saveNavigationState(path, frame);
}

/**
 * Reset the camera direction to point at the anchor node.
 */
[[codegen::luawrap]] void retargetAnchor() {
    openspace::global::navigationHandler->orbitalNavigator().startRetargetAnchor();
}

/**
 * Reset the camera direction to point at the aim node.
 */
[[codegen::luawrap]] void retargetAim() {
    openspace::global::navigationHandler->orbitalNavigator().startRetargetAim();
}

/**
 * Picks the next node from the interesting nodes out of the profile and selects that.
 * If the current anchor is not an interesting node, the first node in the list will be
 * selected.
 */
[[codegen::luawrap]] void targetNextInterestingAnchor() {
    using namespace openspace;
    if (global::profile->markNodes.empty()) {
        LWARNINGC(
            "targetNextInterestingAnchor",
            "Profile does not define any interesting nodes"
        );
        return;
    }
    const std::vector<std::string>& markNodes = global::profile->markNodes;

    std::string currAnchor =
        global::navigationHandler->orbitalNavigator().anchorNode()->identifier();

    auto it = std::find(markNodes.begin(), markNodes.end(), currAnchor);
    if (it == markNodes.end() || ((it + 1) == markNodes.end())) {
        // We want to use the first node either if
        //  1. The current node is not an interesting node
        //  2. The current node is the last interesting node
        global::navigationHandler->orbitalNavigator().setFocusNode(markNodes.front());
    }
    else {
        // Otherwise we can just select the next one
        global::navigationHandler->orbitalNavigator().setFocusNode(*(it + 1));
    }
    global::navigationHandler->orbitalNavigator().startRetargetAnchor();
}

/**
 * Picks the previous node from the interesting nodes out of the profile and selects that.
 * If the current anchor is not an interesting node, the first node in the list will be
 * selected.
 */
[[codegen::luawrap]] void targetPreviousInterestingAnchor() {
    using namespace openspace;
    if (global::profile->markNodes.empty()) {
        LWARNINGC(
            "targetPreviousInterestingAnchor",
            "Profile does not define any interesting nodes"
        );
        return;
    }
    const std::vector<std::string>& markNodes = global::profile->markNodes;

    std::string currAnchor =
        global::navigationHandler->orbitalNavigator().anchorNode()->identifier();

    auto it = std::find(markNodes.begin(), markNodes.end(), currAnchor);
    if (it == markNodes.end()) {
        // We want to use the first node if the current node is not an interesting node
        global::navigationHandler->orbitalNavigator().setFocusNode(markNodes.front());
    }
    else if (it == markNodes.begin()) {
        // We want to use the last node if the current node is the first in the list
        global::navigationHandler->orbitalNavigator().setFocusNode(markNodes.back());
    }
    else {
        // Otherwise we can just select the previous one
        global::navigationHandler->orbitalNavigator().setFocusNode(*(it - 1));
    }
    global::navigationHandler->orbitalNavigator().startRetargetAnchor();
}

/**
 * Set the current focus node for the navigation, or re-focus on it if it was already the
 * focus node.
 *
 * Per default, the camera will retarget to center the focus node in the view. The
 * velocities will also be reset so that the camera stops moving after any retargeting
 * is done. However, both of these behaviors may be skipped using the optional arguments.
 *
 * \param identifier The identifier of the scene graph node to focus
 * \param shouldRetarget If true, retarget the camera to look at the focus node
 * \param shouldResetVelocities If true, reset the camera velocities so that the camera
 *                              stops after its done retargeting (or immediately if
 *                              retargeting is not done)
 *
 */
[[codegen::luawrap]] void setFocus(std::string identifier, bool shouldRetarget = true,
                                   bool shouldResetVelocities = true)
{
    using namespace openspace;
    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        throw ghoul::lua::LuaError("Unknown node: " + identifier);
    }

    global::navigationHandler->orbitalNavigator().setFocusNode(
        node,
        shouldResetVelocities
    );

    if (shouldRetarget) {
        global::navigationHandler->orbitalNavigator().startRetargetAnchor();
    }
}

/**
 * Bind an axis of a joystick to be used as a certain type, and optionally define
 * detailed settings for the axis.
 *
 * \param joystickName the name for the joystick or game controller that should be bound
 * \param axis the axis of the joystick that should be bound
 * \param axisType the type of movement that the axis should be mapped to
 * \param shouldInvert decides if the joystick axis movement should be inverted or not
 * \param joystickType what type of joystick or axis this is. Decides if the joystick
 *                     behaves more like a joystick or a trigger. Either `"JoystickLike"`
 *                     or `"TriggerLike"`, where `"JoystickLike"` is default
 * \param isSticky if true, the value is calculated relative to the previous value.
 *                 If false, the value is used as is
 * \param shouldFlip reverses the movement of the camera that the joystick produces
 * \param sensitivity sensitivity for this axis, in addition to the global sensitivity
 */
[[codegen::luawrap]] void bindJoystickAxis(std::string joystickName, int axis,
                                           std::string axisType,
                                           bool shouldInvert = false,
                                           std::string joystickType = "JoystickLike",
                                           bool isSticky = false,
                                           bool shouldFlip = false,
                                           double sensitivity = 0.0)
{
    using namespace openspace;
    using JoystickCameraStates = interaction::JoystickCameraStates;
    global::navigationHandler->setJoystickAxisMapping(
        std::move(joystickName),
        axis,
        ghoul::from_string<JoystickCameraStates::AxisType>(axisType),
        JoystickCameraStates::AxisInvert(shouldInvert),
        ghoul::from_string<JoystickCameraStates::JoystickType>(joystickType),
        isSticky,
        JoystickCameraStates::AxisFlip(shouldFlip),
        sensitivity
    );
}

/**
 * Binds an axis of a joystick to a numerical property value in OpenSpace. This means that
 * interacting with the joystick will change the property value, within a given min-max
 * range.
 *
 * The axis value will be rescaled from [-1, 1] to the provided [min, max] range
 * (default is [0, 1]).
 *
 * \param joystickName the name for the joystick or game controller that should be bound
 * \param axis the axis of the joystick that should be bound
 * \param propertyUri the identifier (URI) of the property that this joystick axis should
 *                    modify
 * \param min the minimum value that this axis can set for the property
 * \param max the maximum value that this axis can set for the property
 * \param shouldInvert if the joystick movement should be inverted or not
 * \param isRemote if true, the property change will also be executed on connected nodes.
 *                 If false, the property change will only affect the master node
 */
[[codegen::luawrap]] void bindJoystickAxisProperty(std::string joystickName, int axis,
                                                   std::string propertyUri,
                                                   float min = 0.f, float max = 1.f,
                                                   bool shouldInvert = false,
                                                   bool isRemote = true)
{
    using namespace openspace;
    using JoystickCameraStates = interaction::JoystickCameraStates;
    global::navigationHandler->setJoystickAxisMappingProperty(
        std::move(joystickName),
        axis,
        std::move(propertyUri),
        min,
        max,
        JoystickCameraStates::AxisInvert(shouldInvert),
        isRemote
    );
}

struct [[codegen::Dictionary(JoystickAxis)]] JoystickAxis {
    // The current type of axis binding
    std::string type;

    // Whether the values are inverted
    bool invert;

    // The type of joystick that this axis represents on the controller - either
    // `\"JoystickLike\"` or `\"TriggerLike\"`. A joystick is `\"TriggerLike\"` if it
    // can only be pressed or pushed in one direction. A `\"JoystickLike\"` axis can
    // be pushed in two directions; for example, left and right, or up and down.
    std::string joystickType;

    // Whether or not this axis is “sticky”. An axis is “sticky” if, when you let go of
    // it, the values it represents in the software do not go back to the default.
    bool isSticky;

    // Whether the camera movement for the axis is reversed. In the case of a
    // `\"JoystickLike\"` axis, this is the same as inverting the axis. However, in the
    // case of a `\"TriggerLike\" axis, this can reverse the camera movement for the
    // trigger.
    bool flip;

    // Sensitivity value for this axis
    double sensitivity;

    // The identifier (URI) of the property that is bound to this axes, if one is
    std::optional<std::string> propertyUri;

    // If a property is bound to this axis, this is the min value that can be set using
    // the joystick input
    std::optional<float> minValue;

    // If a property is bound to this axis, this is the max value that can be set using
    // the joystick input
    std::optional<float> maxValue;

    // If a property is bound to this axis, this says whether the property changes should
    // be forwarded to other connected nodes or sessions (similarly to \"isLocal\" for
    // actions)
    std::optional<bool> isRemote;
};

/**
 * Return all the information bound to a certain joystick axis.
 *
 * \param joystickName the name for the joystick or game controller with the axis for
 *                     which to find the information
 * \param axis the joystick axis for which to find the information
 *
 * \return an object with information about the joystick axis
 */
[[codegen::luawrap]] ghoul::Dictionary joystickAxis(std::string joystickName, int axis) {
    using namespace openspace;

    interaction::JoystickCameraStates::AxisInformation info =
        global::navigationHandler->joystickAxisMapping(joystickName, axis);

    ghoul::Dictionary dict;
    dict.setValue("Type", ghoul::to_string(info.type));
    dict.setValue("Inverted", static_cast<bool>(info.invert));
    dict.setValue("JoystickType", ghoul::to_string(info.joystickType));
    dict.setValue("Sticky", info.isSticky);
    dict.setValue("Flip", static_cast<bool>(info.flip));
    dict.setValue("Sensitivity", info.sensitivity);

    bool isPropertyBound = !info.propertyUri.empty();
    if (isPropertyBound) {
        dict.setValue("PropertyUri", info.propertyUri);
        dict.setValue("MinValue", static_cast<double>(info.minValue));
        dict.setValue("MaxValue", static_cast<double>(info.maxValue));
        dict.setValue("IsRemote", info.isRemote);
    }

    return dict;
}

/**
 * Set the deadzone value for a particular joystick axis, which means that any input less
 * than this value is completely ignored.
 *
 * \param joystickName the name for the joystick or game controller
 * \param axis the joystick axis for which to set the deadzone
 * \param deadzone the new deadzone value
 */
[[codegen::luawrap("setAxisDeadZone")]] void setJoystickAxisDeadZone(
                                                                 std::string joystickName,
                                                                                 int axis,
                                                                           float deadzone)
{
    using namespace openspace;
    global::navigationHandler->setJoystickAxisDeadzone(joystickName, axis, deadzone);
}

/**
 * Returns the deadzone for the desired axis of the provided joystick.
 *
 * \param joystickName the name for the joystick or game controller which information
 *                     should be returned
 * \param axis the joystick axis for which to get the deadzone value
 *
 * \return the deadzone value
 */
[[codegen::luawrap("axisDeadzone")]] float joystickAxisDeadzone(std::string joystickName,
                                                                int axis)
{
    float deadzone = openspace::global::navigationHandler->joystickAxisDeadzone(
        joystickName,
        axis
    );
    return deadzone;
}

/**
 * Bind a Lua script to one of the buttons for a joystick.
 *
 * \param joystickName the name for the joystick or game controller
 * \param button the button to which to bind the script
 * \param command the script that should be executed on button trigger
 * \param documentation the documentation for the provided script/command
 * \param action the action for when the script should be executed. This defaults to
 *               `"Press"`, which means that the script is run when the user presses the
 *               button. Alternatives are `"Idle"` (if the button is unpressed and has
 *               been unpressed since the last frame), `"Repeat"` (if the button has been
 *               pressed since longer than the last frame), and `"Release"` (if the button
 *               was released since the last frame)
 * \param isRemote a value saying whether the command is going to be executable
 *                 locally or remotely, where the latter is the default
 */
[[codegen::luawrap]] void bindJoystickButton(std::string joystickName, int button,
                                             std::string command,
                                             std::string documentation,
                                             std::string action = "Press",
                                             bool isRemote = true)
{
    using namespace openspace;
    interaction::JoystickAction act =
        ghoul::from_string<interaction::JoystickAction>(action);

    global::navigationHandler->bindJoystickButtonCommand(
        joystickName,
        button,
        command,
        act,
        interaction::JoystickCameraStates::ButtonCommandRemote(isRemote),
        documentation
    );
}

/**
 * Remove all commands that are currently bound to a button of a joystick or game
 * controller
 *
 * \param joystickName the name for the joystick or game controller
 * \param button the button for which to clear the commands
 */
[[codegen::luawrap]] void clearJoystickButton(std::string joystickName, int button) {
    openspace::global::navigationHandler->clearJoystickButtonCommand(
        joystickName,
        button
    );
}

/**
 * Get the Lua script that is currently bound to be executed when the provided button is
 * pressed/triggered.
 *
 * \param joystickName the name for the joystick or game controller
 * \param button the button for which to get the command
 *
 * \return the currently bound Lua script
 */
[[codegen::luawrap]] std::string joystickButton(std::string joystickName, int button) {
    using namespace openspace;
    const std::vector<std::string>& cmds =
        global::navigationHandler->joystickButtonCommand(joystickName, button);

    std::string cmd = std::accumulate(
        cmds.cbegin(),
        cmds.cend(),
        std::string(),
        [](const std::string& lhs, const std::string& rhs) {
            return lhs + ";" + rhs;
        }
    );
    return cmd;
}

/**
 * Directly add to the global rotation of the camera (around the focus node).
 *
 * \param xValue the value to add in the x-direction (a positive value rotates to the
 *               right and a negative value to the left)
 * \param yValue the value to add in the y-direction (a positive value rotates the focus
 *               upwards and a negative value downwards)
 */
[[codegen::luawrap]] void addGlobalRotation(double xValue, double yValue) {
    using namespace openspace;
    global::navigationHandler->orbitalNavigator().scriptStates().addGlobalRotation(
        glm::dvec2(xValue, yValue)
    );
}

/**
 * Directly adds to the local rotation of the camera (around the camera's current
 * position).
 *
 * \param xValue the value to add in the x-direction (a positive value rotates to the
 *               left and a negative value to the right)
 * \param yValue the value to add in the y-direction (a positive value rotates the camera
 *               upwards and a negative value downwards)
 */
[[codegen::luawrap]] void addLocalRotation(double xValue, double yValue) {
    using namespace openspace;
    global::navigationHandler->orbitalNavigator().scriptStates().addLocalRotation(
        glm::dvec2(xValue, yValue)
    );
}


/**
 * Directly adds to the truck movement of the camera. This is the movement along the line
 * from the camera to the focus node.
 *
 * A positive value moves the camera closer to the focus, and a negative value moves the
 * camera further away.
 *
 * \param value the value to add
 */
[[codegen::luawrap]] void addTruckMovement(double value) {
    using namespace openspace;
    // @TODO: Note that the x value isn't actually used and the code in the navigation
    // handlers for these should be cleaned up. The same goes for the roll funcitons below
    global::navigationHandler->orbitalNavigator().scriptStates().addTruckMovement(
        glm::dvec2(0.0, value)
    );
}

/**
 * Directly adds to the local roll of the camera. This is the rotation around the camera's
 * forward/view direction.
 *
 * \param value the value to add
 */
[[codegen::luawrap]] void addLocalRoll(double value) {
    using namespace openspace;
    global::navigationHandler->orbitalNavigator().scriptStates().addLocalRoll(
        glm::dvec2(value, 0.0)
    );
}

/**
 * Directly adds to the global roll of the camera. This is a rotation around the line
 * between the focus node and the camera (not always the same as the camera view
 * direction)
 *
 * \param value the value to add
 */
[[codegen::luawrap]] void addGlobalRoll(double value) {
    using namespace openspace;
    global::navigationHandler->orbitalNavigator().scriptStates().addGlobalRoll(
        glm::dvec2(value, 0.0)
    );
}

/**
 * Immediately start applying the chosen IdleBehavior. If none is specified, use the one
 * set to default in the OrbitalNavigator.
 */
[[codegen::luawrap]] void triggerIdleBehavior(std::string choice = "") {
    using namespace openspace;
    try {
        global::navigationHandler->orbitalNavigator().triggerIdleBehavior(choice);
    }
    catch (ghoul::RuntimeError& e) {
        throw ghoul::lua::LuaError(e.message);
    }
}

/**
 * Return the complete list of connected joysticks.
 *
 * \return a list of joystick names
 */
[[codegen::luawrap]] std::vector<std::string> listAllJoysticks() {
    using namespace openspace;
    return global::navigationHandler->listAllJoysticks();
}

/**
 * Return the distance to the current focus node.
 *
 * \return the distance, in meters
 */
[[codegen::luawrap]] double distanceToFocus() {
    using namespace openspace;

    const SceneGraphNode * focus = global::navigationHandler->anchorNode();
    Camera * camera = global::navigationHandler->camera();

    return glm::distance(camera->positionVec3(), focus->worldPosition());
}

/**
 * Return the distance to the current focus node's bounding sphere.
 *
 * \return the distance, in meters
 */
[[codegen::luawrap]] double distanceToFocusBoundingSphere() {
    using namespace openspace;

    const SceneGraphNode* focus = global::navigationHandler->anchorNode();
    Camera* camera = global::navigationHandler->camera();

    double distance = glm::distance(camera->positionVec3(), focus->worldPosition());

    return distance - focus->boundingSphere();
}

/**
 * Return the distance to the current focus node's interaction sphere.
 *
 * \return the distance, in meters
 */
[[codegen::luawrap]] double distanceToFocusInteractionSphere() {
    using namespace openspace;

    const SceneGraphNode* focus = global::navigationHandler->anchorNode();
    Camera* camera = global::navigationHandler->camera();

    double distance = glm::distance(camera->positionVec3(), focus->worldPosition());

    return distance - focus->interactionSphere();
}

/**
 * Immediately move the camera to a geographic coordinate on a node by first fading the
 * rendering to black, jump to the specified coordinate, and then fade in. If the node is
 * a globe, the longitude and latitude values are expressed in the body's native
 * coordinate system. If it is not, the position on the surface of the interaction sphere
 * is used instead.
 *
 * This is done by triggering another script that handles the logic.
 *
 * \param node The identifier of a scene graph node. If an empty string is provided, the
 *        current anchor node is used
 * \param latitude The latitude of the target coordinate, in degrees
 * \param longitude The longitude of the target coordinate, in degrees
 * \param altitude An optional altitude, given in meters over the reference surface of
 *                 the globe. If no altitude is provided, the altitude will be kept as
 *                 the current distance to the reference surface of the specified node
 * \param fadeDuration An optional duration for the fading. If not included, the
 *                     property in Navigation Handler will be used
 */
[[codegen::luawrap]] void jumpToGeo(std::string node, double latitude, double longitude,
                                    std::optional<double> altitude,
                                    std::optional<double> fadeDuration)
{
    using namespace openspace;

    std::string script;

    if (altitude.has_value()) {
        script = std::format(
            "openspace.navigation.flyToGeo('{}', {}, {}, {}, 0)",
            node, latitude, longitude, *altitude
        );
    }
    else {
        script = std::format(
            "openspace.navigation.flyToGeo2('{}', {}, {}, true, 0)",
            node, latitude, longitude
        );
    }

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
 * Immediately move the camera to a geographic coordinate on a globe. If the node is a
 * globe, the longitude and latitude is expressed in the body's native coordinate system.
 * If it is not, the position on the surface of the interaction sphere is used instead.
 *
 * \param node The identifier of a scene graph node. If an empty string is provided, the
 *        current anchor node is used
 * \param latitude The latitude of the target coordinate, in degrees
 * \param longitude The longitude of the target coordinate, in degrees
 * \param altitude An optional altitude, given in meters over the reference surface of
 *                 the globe. If no altitude is provided, the altitude will be kept as
 *                 the current distance to the reference surface of the specified globe.
 */
[[codegen::luawrap("goToGeo")]] void goToGeoDeprecated(std::string node, double latitude,
                                                       double longitude,
                                                       std::optional<double> altitude)
{
    LWARNINGC(
        "Deprecation",
        "'goToGeo' function is deprecated and should be replaced with 'jumpToGeo'"
    );

    return jumpToGeo(std::move(node), latitude, longitude, altitude, 0);
}

void flyToGeoInternal(std::string node, double latitude, double longitude,
                      std::optional<double> altitude, std::optional<double> duration,
                      std::optional<bool> shouldUseUpVector)
{
    using namespace openspace;

    const SceneGraphNode* n;
    if (!node.empty()) {
        n = sceneGraphNode(node);
        if (!n) {
            throw ghoul::lua::LuaError("Unknown scene graph node: " + node);
        }
    }
    else {
        n = global::navigationHandler->orbitalNavigator().anchorNode();
        if (!n) {
            throw ghoul::lua::LuaError("No anchor node is set");
        }
    }

    const glm::dvec3 positionModelCoords = cartesianCoordinatesFromGeo(
        *n,
        latitude,
        longitude,
        altitude
    );

    const glm::dvec3 currentPosW = global::navigationHandler->camera()->positionVec3();
    const glm::dvec3 currentPosModelCoords =
        glm::inverse(n->modelTransform()) * glm::dvec4(currentPosW, 1.0);

    constexpr double LengthEpsilon = 10.0; // meters
    if (glm::distance(currentPosModelCoords, positionModelCoords) < LengthEpsilon) {
        LINFOC("GlobeBrowsing", "flyToGeo: Already at the requested position");
        return;
    }

    ghoul::Dictionary instruction;
    instruction.setValue("TargetType", std::string("Node"));
    instruction.setValue("Target", n->identifier());
    instruction.setValue("Position", positionModelCoords);
    instruction.setValue("PathType", std::string("ZoomOutOverview"));

    if (duration.has_value()) {
        if (*duration < 0) {
            throw ghoul::lua::LuaError("Duration must be a positive value");
        }
        instruction.setValue("Duration", *duration);
    }

    if (shouldUseUpVector.has_value()) {
        instruction.setValue("UseTargetUpDirection", *shouldUseUpVector);

    }

    global::navigationHandler->pathNavigator().createPath(instruction);
    global::navigationHandler->pathNavigator().startPath();
}

/**
 * Fly the camera to a geographic coordinate (latitude and longitude) on a globe, using
 * the path navigation system. If the node is a globe, the longitude and latitude is
 * expressed in the body's native coordinate system. If it is not, the position on the
 * surface of the interaction sphere is used instead.
 *
 * The distance to fly to can either be set to be the current distance of the camera to
 * the target object, or the default distance from the path navigation system.
 *
 * \param node The identifier of a scene graph node. If an empty string is provided, the
 *             current anchor node is used
 * \param latitude The latitude of the target coordinate, in degrees
 * \param longitude The longitude of the target coordinate, in degrees
 * \param useCurrentDistance If true, use the current distance of the camera to the
 *                           target globe when going to the specified position. If false,
 *                           or not specified, set the distance based on the bounding
 *                           sphere and the distance factor setting in Path Navigator
 * \param duration An optional duration for the motion to take, in seconds. For example,
 *                 a value of 5 means "fly to this position over a duration of 5 seconds"
 * \param shouldUseUpVector If true, try to use the up-direction when computing the
 *                          target position for the camera. For globes, this means that
 *                          North should be up, in relation to the camera's view
 *                          direction. Note that for this to take effect, rolling motions
 *                          must be enabled in the Path Navigator settings.
 */
[[codegen::luawrap]] void flyToGeo2(std::string node, double latitude, double longitude,
                                    std::optional<bool> useCurrentDistance,
                                    std::optional<double> duration,
                                    std::optional<bool> shouldUseUpVector)
{
    using namespace openspace;

    std::optional<double> altitude;
    if (useCurrentDistance.has_value() && *useCurrentDistance) {
        altitude = std::nullopt;
    }
    else {
        altitude = global::navigationHandler->pathNavigator().defaultArrivalHeight(node);
    }

    flyToGeoInternal(
        node,
        latitude,
        longitude,
        std::nullopt,
        duration,
        shouldUseUpVector
    );
}

 /**
  * Fly the camera to a geographic coordinate (latitude, longitude and altitude) on a
  * globe, using the path navigation system. If the node is a globe, the longitude and
  * latitude is expressed in the body's native coordinate system. If it is not, the
  * position on the surface of the interaction sphere is used instead.
  *
  * \param node The identifier of a scene graph node. If an empty string is provided, the
  *        current anchor node is used
  * \param latitude The latitude of the target coordinate, in degrees
  * \param longitude The longitude of the target coordinate, in degrees
  * \param altitude The altitude of the target coordinate, in meters
  * \param duration An optional duration for the motion to take, in seconds. For example,
  *                 a value of 5 means "fly to this position over a duration of 5 seconds"
  * \param shouldUseUpVector If true, try to use the up-direction when computing the
  *                          target position for the camera. For globes, this means that
  *                          North should be up, in relation to the camera's view
  *                          direction. Note that for this to take effect, rolling motions
  *                          must be enabled in the Path Navigator settings.
  */
[[codegen::luawrap]] void flyToGeo(std::string node, double latitude,
                                   double longitude, double altitude,
                                   std::optional<double> duration,
                                   std::optional<bool> shouldUseUpVector)
{
    flyToGeoInternal(node, latitude, longitude, altitude, duration, shouldUseUpVector);
}

/**
 * Returns the position in the local Cartesian coordinate system of the specified node
 * that corresponds to the given geographic coordinates. In the local coordinate system,
 * the position (0,0,0) corresponds to the globe's center. If the node is a globe, the
 * longitude and latitude is expressed in the body's native coordinate system. If it is
 * not, the position on the surface of the interaction sphere is used instead.
 *
 * \param nodeIdentifier The identifier of the scene graph node
 * \param latitude The latitude of the geograpic position, in degrees
 * \param longitude The longitude of the geographic position, in degrees
 * \param altitude The altitude, in meters
 */
[[codegen::luawrap]]
std::tuple<double, double, double>
localPositionFromGeo(std::string nodeIdentifier, double latitude, double longitude,
                     double altitude)
{
    using namespace openspace;

    SceneGraphNode* n = sceneGraphNode(nodeIdentifier);
    if (!n) {
        throw ghoul::lua::LuaError("Unknown globe identifier: " + nodeIdentifier);
    }

    glm::vec3 p = cartesianCoordinatesFromGeo(*n, latitude, longitude, altitude);
    return { p.x, p.y, p.z };
}

/**
* Returns the position in the local Cartesian coordinate system of the specified globe
* that corresponds to the given geographic coordinates. In the local coordinate system,
* the position (0,0,0) corresponds to the globe's center. If the node is a globe, the
* longitude and latitude is expressed in the body's native coordinate system. If it is
* not, the position on the surface of the interaction sphere is used instead.
*
* Deprecated in favor of `localPositionFromGeo`.
*
* \param globeIdentifier The identifier of the scene graph node
* \param latitude The latitude of the geograpic position, in degrees
* \param longitude The longitude of the geographic position, in degrees
* \param altitude The altitude, in meters
*/
[[codegen::luawrap("getLocalPositionFromGeo")]]
std::tuple<double, double, double>
localPositionFromGeoDeprecated(std::string nodeIdentifier, double latitude,
                               double longitude, double altitude)
{
    LWARNINGC(
        "Deprecation",
        "'getLocalPositionFromGeo' function is deprecated and should be replaced with "
        "'localPositionFromGeo'"
    );

    return localPositionFromGeo(std::move(nodeIdentifier), latitude, longitude, altitude);
}

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
 * Move the camera to the node with the specified identifier. The optional double
 * specifies the duration of the motion, in seconds. If the optional bool is set to true
 * the target up vector for camera is set based on the target node. Either of the optional
 * parameters can be left out.
 *
 * \param nodeIdentifier The identifier of the node to which we want to fly
 * \param useUpFromTargetOrDuration If this value is a boolean value (`true` or `false`),
 *        this value determines whether we want to end up with the camera facing along the
 *        selected node's up direction. If this value is a numerical value, refer to the
 *        documnentation of the `duration` parameter
 * \param duration The duration (in seconds) how long the flying to the selected node
 *        should take. If this value is left out, a sensible default value is uses, which
 *        can be configured in the engine
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
 *
 * \param nodeIdentifier The identifier of the node to which we want to fly
 * \param height The height (in meters) to which we want to fly. The way the height is
 *        defined specifically determines on the type of node to which the fly-to command
 *        is pointed.
 * \param useUpFromTargetOrDuration If this value is a boolean value (`true` or `false`),
 *        this value determines whether we want to end up with the camera facing along the
 *        selected node's up direction. If this value is a numerical value, refer to the
 *        documnentation of the `duration` parameter
 * \param duration The duration (in seconds) how long the flying to the selected node
 *        should take. If this value is left out, a sensible default value is uses, which
 *        can be configured in the engine
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
 * \param navigationState A [NavigationState](#core_navigation_state) to jump to.
 * \param useTimeStamp if true, and the provided NavigationState includes a timestamp,
 *                     the time will be set as well.
 * \param fadeDuration An optional duration for the fading. If not included, the
 *                     property in Navigation Handler will be used.
 */
[[codegen::luawrap]] void jumpToNavigationState(ghoul::Dictionary navigationState,
                                                std::optional<bool> useTimeStamp,
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

    bool setTime = (ns.timestamp.has_value() && useTimeStamp.value_or(false));

    std::string script = std::format(
        "openspace.navigation.setNavigationState({}, {})",
        ghoul::formatLua(ns.dictionary()), setTime
    );

    if (fadeDuration.has_value()) {
        global::navigationHandler->triggerFadeToTransition(
            std::move(script),
            static_cast<float>(*fadeDuration)
        );
    }
    else {
        global::navigationHandler->triggerFadeToTransition(std::move(script));
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

    std::string script = std::format(
        "openspace.navigation.flyTo('{}', 0)", nodeIdentifier
    );

    if (fadeDuration.has_value()) {
        global::navigationHandler->triggerFadeToTransition(
            std::move(script),
            static_cast<float>(*fadeDuration)
        );
    }
    else {
        global::navigationHandler->triggerFadeToTransition(std::move(script));
    }
}

#include "navigationhandler_lua_codegen.cpp"

} // namespace
