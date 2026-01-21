/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_CORE___MOUSECAMERASTATES___H__
#define __OPENSPACE_CORE___MOUSECAMERASTATES___H__

#include <openspace/navigation/orbitalnavigator/orbitalcamerastates.h>

namespace openspace::interaction {

class KeyboardInputState;
class MouseInputState;

class MouseCameraStates : public OrbitalCameraStates {
public:
    MouseCameraStates(double sensitivity, double velocityScaleFactor);

    void updateVelocitiesFromInput(const MouseInputState& mouseState,
        const KeyboardInputState& keyboardState, double deltaTime);

    void setInvertMouseButton(bool value);

private:
    bool _isMouseButtonInverted = false;
    double _currentSensitivityRamp = 1.0;

    // Mouse positions before a certain type of interaction is started
    struct {
        glm::dvec2 primary = glm::dvec2(0.0);
        glm::dvec2 secondary = glm::dvec2(0.0);
        glm::dvec2 button3 = glm::dvec2(0.0);
    } _prevMousePos;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___MOUSECAMERASTATES___H__
