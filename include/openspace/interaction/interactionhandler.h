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

#ifndef __OPENSPACE_CORE___INTERACTIONHANDLER___H__
#define __OPENSPACE_CORE___INTERACTIONHANDLER___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/interaction/interactionmonitor.h>
#include <openspace/interaction/joystickinputstate.h>
#include <openspace/interaction/keyboardinputstate.h>
#include <openspace/interaction/mouseinputstate.h>
#include <openspace/interaction/touchinputstate.h>
#include <openspace/interaction/websocketinputstate.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec4property.h>

namespace openspace::interaction {

/**
 * This handler is responsible for keeping track of the different input states provided
 * through the software's interaction system.
 */
class InteractionHandler: public properties::PropertyOwner {
public:
    InteractionHandler();
    virtual ~InteractionHandler() override;

    void initialize();
    void deinitialize();

    void preSynchronization();

    const MouseInputState& mouseInputState() const;
    const KeyboardInputState& keyboardInputState() const;

    WebsocketInputStates& websocketInputStates();
    const WebsocketInputStates& websocketInputStates() const;

    JoystickInputStates& joystickInputStates();
    const JoystickInputStates& joystickInputStates() const;

    TouchInputState& touchInputState();
    const TouchInputState& touchInputState() const;

    bool disabledKeybindings() const;
    bool disabledMouse() const;
    bool disabledJoystick() const;

    void keyboardCallback(Key key, KeyModifier modifier, KeyAction action);

    void mouseButtonCallback(MouseButton button, MouseAction action);
    void mousePositionCallback(double x, double y);
    void mouseScrollWheelCallback(double pos);

    void renderOverlay() const;

    /**
     * Signal to the InteractionMonitor that an interaction happened. Should be called
     * in any function representing an interaction that is not handled by this handler.
     */
    void markInteraction();

    void clearJoystickStates();

private:
    MouseInputState _mouseInputState;
    KeyboardInputState _keyboardInputState;
    WebsocketInputStates _websocketInputStates;
    JoystickInputStates _joystickInputStates;
    TouchInputState _touchInputState;

    // Keeps track of when interaction has happened
    InteractionMonitor _interactionMonitor;

    properties::BoolProperty _disableKeybindings;
    properties::BoolProperty _disableMouseInputs;
    properties::BoolProperty _disableJoystickInputs;
    //properties::BoolProperty _disableTouchInputs; // TODO

    struct {
        properties::PropertyOwner owner;
        properties::BoolProperty enable;
        properties::Vec4Property color;

        bool isMouseFirstPress = false;
        bool isMousePressed = false;
        glm::vec2 clickPosition;
        glm::vec2 currentPosition;
    } _mouseVisualizer;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___INTERACTIONHANDLER___H__
