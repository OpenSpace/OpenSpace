/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/interaction/inputstate.h>

#include <ghoul/logging/logmanager.h>

#include <algorithm>

namespace openspace::interaction {

void InputState::keyboardCallback(Key key, KeyModifier modifier, KeyAction action) {
    if (action == KeyAction::Press) {
        _keysDown.emplace_back(key, modifier);
    }
    else if (action == KeyAction::Release) {
        // Remove all key pressings for 'key'
        _keysDown.remove_if([key](std::pair<Key, KeyModifier> keyModPair)
        { return keyModPair.first == key; });
    }
}

void InputState::mouseButtonCallback(MouseButton button, MouseAction action) {
    if (action == MouseAction::Press) {
        _mouseButtonsDown.push_back(button);
    }
    else if (action == MouseAction::Release) {
        // Remove all key pressings for 'button'
        _mouseButtonsDown.remove_if([button](MouseButton buttonInList)
        { return button == buttonInList; });
    }
}

void InputState::mousePositionCallback(double mouseX, double mouseY) {
    _mousePosition = glm::dvec2(mouseX, mouseY);
}

void InputState::mouseScrollWheelCallback(double mouseScrollDelta) {
    _mouseScrollDelta = mouseScrollDelta;
}

const std::list<std::pair<Key, KeyModifier> >& InputState::getPressedKeys() const {
    return _keysDown;
}

const std::list<MouseButton>& InputState::getPressedMouseButtons() const {
    return _mouseButtonsDown;
}

glm::dvec2 InputState::getMousePosition() const {
    return _mousePosition;
}

double InputState::getMouseScrollDelta() const {
    return _mouseScrollDelta;
}

bool InputState::isKeyPressed(std::pair<Key, KeyModifier> keyModPair) const {
    return std::find(_keysDown.begin(), _keysDown.end(), keyModPair) != _keysDown.end();
}

bool InputState::isKeyPressed(Key key) const {
    return std::find_if(_keysDown.begin(), _keysDown.end(),
        [key](const std::pair<Key, KeyModifier>& keyModPair) {
            return key == keyModPair.first;
        }) != _keysDown.end();
}

bool InputState::isMouseButtonPressed(MouseButton mouseButton) const {
    return std::find(_mouseButtonsDown.begin(), _mouseButtonsDown.end(),
        mouseButton) != _mouseButtonsDown.end();
}

} // namespace openspace::interaction
