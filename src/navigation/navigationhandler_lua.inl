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
 * By default, the reference frame will be picked based on whether the orbital navigator is
 * currently following the anchor node rotation. If it is, the anchor will be chosen as
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
 *               pressed since longer than the last frame), and `"Release"` (if the button was
 *               released since the last frame)
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

#include "navigationhandler_lua_codegen.cpp"

} // namespace
