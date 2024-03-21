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
 * Load a navigation state from file. The file should be a lua file returning the
 * navigation state as a table formatted as a Navigation State, such as the output files
 * of saveNavigationState. If usetimeStamp is set to true and the provided navigation
 * state has a timestamp, time will be set as well.
 */
[[codegen::luawrap]] void loadNavigationState(std::string cameraStateFilePath,
                                              bool useTimeStamp = false)
{
    if (cameraStateFilePath.empty()) {
        throw ghoul::lua::LuaError("Filepath string is empty");
    }

    openspace::global::navigationHandler->loadNavigationState(
        cameraStateFilePath,
        useTimeStamp
    );
}

/**
 * Return the current navigation state as a Lua table. The optional argument is the scene
 * graph node to use as reference frame. By default, the reference frame will picked based
 * on whether the orbital navigator is currently following the anchor node rotation. If it
 * is, the anchor will be chosen as reference frame. If not, the reference frame will be
 * set to the scene graph root.
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
                fmt::format("Could not find node '{}' as reference frame", *frame)
            );
        }
        state = global::navigationHandler->navigationState(*referenceFrame);
    }
    else {
        state = global::navigationHandler->navigationState();
    }

    return state.dictionary();
}

// Set the navigation state. The first argument must be a valid Navigation State. If
// useTimeStamp is set to true and the provided navigation state has a timestamp, time
// will be set as well.
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
 * Save the current navigation state to a file with the path given by the first argument.
 * The optional second argument is the scene graph node to use as reference frame. By
 * default, the reference frame will picked based on whether the orbital navigator is
 * currently following the anchor node rotation. If it is, the anchor will be chosen as
 * reference frame. If not, the reference frame will be set to the scene graph root.
 */
[[codegen::luawrap]] void saveNavigationState(std::string path, std::string frame = "") {
    if (path.empty()) {
        throw ghoul::lua::LuaError("Filepath string is empty");
    }
    openspace::global::navigationHandler->saveNavigationState(path, frame);
}

// Reset the camera direction to point at the anchor node.
[[codegen::luawrap]] void retargetAnchor() {
    openspace::global::navigationHandler->orbitalNavigator().startRetargetAnchor();
}

// Reset the camera direction to point at the aim node.
[[codegen::luawrap]] void retargetAim() {
    openspace::global::navigationHandler->orbitalNavigator().startRetargetAim();
}

// Picks the next node from the interesting nodes out of the profile and selects that. If
// the current anchor is not an interesting node, the first will be selected
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

// Picks the previous node from the interesting nodes out of the profile and selects that.
// If the current anchor is not an interesting node, the first will be selected
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
 * Finds the input joystick with the given 'name' and binds the axis identified by the
 * second argument to be used as the type identified by the third argument. If
 * 'isInverted' is 'true', the axis value is inverted. 'joystickType' is if the joystick
 * behaves more like a joystick or a trigger, where the first is the default. If
 * 'isSticky' is 'true', the value is calculated relative to the previous value. If
 * 'shouldFlip' is true, then the camera movement for the axis is reversed. If
 * 'sensitivity' is given then that value will affect the sensitivity of the axis together
 * with the global sensitivity.
 *
 * \param joystickName the name for the joystick or game controller that should be bound
 * \param axis the axis of the joystick that should be bound
 * \param axisType the type of movement that the axis should be mapped to
 * \param shouldInvert if the joystick movement should be inverted or not
 * \param joystickType what type of joystick or axis this is. Either
 *                     <code>"JoystickLike"</code> or <code>"TriggerLike"</code>.
 * \param isSticky if true, the value is calculated relative to the previous value,
 *                 if false the the value is used as is.
 * \param shouldFlip reverses the movement of the camera that the joystick produces
 * \param sensitivity sensitivity for this axis
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
 * Finds the input joystick with the given 'name' and binds the axis identified by the
 * second argument to be bound to the property identified by the third argument. 'min' and
 * 'max' is the minimum and the maximum allowed value for the given property and the axis
 * value is rescaled from [-1, 1] to [min, max], default is [0, 1]. If 'isInverted' is
 * 'true', the axis value is inverted. The last argument determines whether the property
 * change is going to be executed locally or remotely, where the latter is the default.
 *
 * \param joystickName the name for the joystick or game controller that should be bound
 * \param axis the axis of the joystick that should be bound
 * \param propertyUri the property that this joystick axis should modify
 * \param min the minimum value that this axis can set for the property
 * \param max the maximum value that this axis can set for the property
 * \param shouldInvert if the joystick movement should be inverted or not
 * \param isRemote if true, the property change will also be executed on connected nodes
 *                 if false, the property change will only affect the master node
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

/**
 * Finds the input joystick with the given 'name' and returns the joystick axis
 * information for the passed axis. The information that is returned is the current axis
 * binding as a string, whether the values are inverted as bool, the joystick type as a
 * string, whether the axis is sticky as bool, the sensitivity as number, the property uri
 * bound to the axis as string (empty is type is not Property), the min and max values for
 * the property as numbers and whether the property change will be executed remotly as
 * bool.
 */
[[codegen::luawrap]]
std::tuple<
    std::string, bool, std::string, bool, bool, double, std::string, float, float, bool
>
joystickAxis(std::string joystickName, int axis)
{
    using namespace openspace;

    interaction::JoystickCameraStates::AxisInformation info =
        global::navigationHandler->joystickAxisMapping(joystickName, axis);

    return {
        ghoul::to_string(info.type),
        info.invert,
        ghoul::to_string(info.joystickType),
        info.isSticky,
        info.flip,
        info.sensitivity,
        info.propertyUri,
        info.minValue,
        info.maxValue,
        info.isRemote
    };
}

/**
 * Finds the input joystick with the given 'name' and sets the deadzone for a particular
 * joystick axis, which means that any input less than this value is completely ignored.
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
 * Finds the input joystick with the given 'name' and binds a Lua script given by the
 * third argument to be executed when the joystick button identified by the second
 * argument is triggered. The fifth argument determines when the script should be
 * executed, this defaults to 'Press', which means that the script is run when the user
 * presses the button. The fourth arguemnt is the documentation of the script in the third
 * argument. The sixth argument determines whether the command is going to be executable
 * locally or remotely, where the latter is the default.
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
 * Finds the input joystick with the given 'name' and removes all commands that are
 * currently bound to the button identified by the second argument.
 */
[[codegen::luawrap]] void clearJoystickButton(std::string joystickName, int button) {
    openspace::global::navigationHandler->clearJoystickButtonCommand(
        joystickName,
        button
    );
}

/**
 * Finds the input joystick with the given 'name' and returns the script that is currently
 * bound to be executed when the provided button is pressed.
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

// Directly adds to the global rotation of the camera.
[[codegen::luawrap]] void addGlobalRotation(double v1, double v2) {
    using namespace openspace;
    global::navigationHandler->orbitalNavigator().scriptStates().addGlobalRotation(
        glm::dvec2(v1, v2)
    );
}

// Directly adds to the local rotation of the camera.
[[codegen::luawrap]] void addLocalRotation(double v1, double v2) {
    using namespace openspace;
    global::navigationHandler->orbitalNavigator().scriptStates().addLocalRotation(
        glm::dvec2(v1, v2)
    );
}

// Directly adds to the truck movement of the camera.
[[codegen::luawrap]] void addTruckMovement(double v1, double v2) {
    using namespace openspace;
    global::navigationHandler->orbitalNavigator().scriptStates().addTruckMovement(
        glm::dvec2(v1, v2)
    );
}

// Directly adds to the local roll of the camera.
[[codegen::luawrap]] void addLocalRoll(double v1, double v2) {
    using namespace openspace;
    global::navigationHandler->orbitalNavigator().scriptStates().addLocalRoll(
        glm::dvec2(v1, v2)
    );
}

// Directly adds to the global roll of the camera.
[[codegen::luawrap]] void addGlobalRoll(double v1, double v2) {
    using namespace openspace;
    global::navigationHandler->orbitalNavigator().scriptStates().addGlobalRoll(
        glm::dvec2(v1, v2)
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
 * Return the complete list of connected joysticks
 */
[[codegen::luawrap]] std::vector<std::string> listAllJoysticks() {
    using namespace openspace;
    return global::navigationHandler->listAllJoysticks();
}

/**
 * Returns the distance in meters to the current focus node
 */
[[codegen::luawrap]] double distanceToFocus() {
    using namespace openspace;

    const SceneGraphNode * focus = global::navigationHandler->anchorNode();
    Camera * camera = global::navigationHandler->camera();

    return glm::distance(camera->positionVec3(), focus->worldPosition());
}

/**
 * Returns the distance in meters to the current focus node's bounding sphere
 */
[[codegen::luawrap]] double distanceToFocusBoundingSphere() {
    using namespace openspace;

    const SceneGraphNode* focus = global::navigationHandler->anchorNode();
    Camera* camera = global::navigationHandler->camera();

    double distance = glm::distance(camera->positionVec3(), focus->worldPosition());

    return distance - focus->boundingSphere();
}

/**
 * Returns the distance in meters to the current focus node's interaction sphere
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
