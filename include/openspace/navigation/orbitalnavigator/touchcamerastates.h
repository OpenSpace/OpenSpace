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

#ifndef __OPENSPACE_CORE___TOUCHCAMERASTATES___H__
#define __OPENSPACE_CORE___TOUCHCAMERASTATES___H__

#include <openspace/navigation/orbitalnavigator/orbitalcamerastates.h>

#include <openspace/util/touch.h>
#include <ghoul/glm.h>
#include <array>

#define TOUCH_DEBUG_MODE

namespace openspace::interaction {

class TouchInputState;

class TouchCameraStates : public OrbitalCameraStates {
public:
    enum class InteractionType {
        ROTATION = 0,
        PINCH,
        PAN,
        ROLL,
        NONE
    };

    TouchCameraStates(double sensitivity, double velocityScaleFactor);
    virtual ~TouchCameraStates() override;

    void updateVelocitiesFromInput(const TouchInputState& mouseState,
        double deltaTime);

private:
    /**
     * Compute new velocity according to the interpreted action.
     */
    UpdateStates computeVelocities(const std::vector<TouchInputHolder>& touchPoints,
        const std::vector<TouchInput>& lastProcessed);

    /**
     * Returns an enum for what interaction to be used, depending on what input was
     * received.
     */
    InteractionType interpretInteraction(const std::vector<TouchInputHolder>& list,
        const std::vector<TouchInput>& lastProcessed);

    void resetAfterInput();

    std::array<TouchInputHolder, 2> _pinchInputs;
    glm::vec2 _centroid = glm::vec2(0.f);
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___TOUCHCAMERASTATES___H__
