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

#include <openspace/interaction/mouseinputstate.h>

#include <algorithm>

namespace openspace::interaction {

void MouseInputState::mouseButtonCallback(MouseButton button, MouseAction action) {
    if (action == MouseAction::Press) {
        _mouseButtonsDown.push_back(button);
    }
    else if (action == MouseAction::Release) {
        _mouseButtonsDown.erase(
            std::remove(_mouseButtonsDown.begin(), _mouseButtonsDown.end(), button),
            _mouseButtonsDown.end()
        );
    }
}

void MouseInputState::mousePositionCallback(double mouseX, double mouseY) {
    _mousePosition = glm::dvec2(mouseX, mouseY);
}

void MouseInputState::mouseScrollWheelCallback(double mouseScrollDelta) {
    _mouseScrollDelta = mouseScrollDelta;
}

const std::vector<MouseButton>& MouseInputState::pressedMouseButtons() const {
    return _mouseButtonsDown;
}

glm::dvec2 MouseInputState::mousePosition() const {
    return _mousePosition;
}

double MouseInputState::mouseScrollDelta() const {
    return _mouseScrollDelta;
}

bool MouseInputState::isMouseButtonPressed(MouseButton mouseButton) const {
    auto it = std::find(_mouseButtonsDown.begin(), _mouseButtonsDown.end(), mouseButton);
    return it != _mouseButtonsDown.end();
}

} // namespace openspace::interaction
