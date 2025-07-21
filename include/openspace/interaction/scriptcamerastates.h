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

#ifndef __OPENSPACE_CORE___SCRIPTCAMERASTATES___H__
#define __OPENSPACE_CORE___SCRIPTCAMERASTATES___H__

#include <openspace/interaction/camerainteractionstates.h>

namespace openspace::interaction {

class ScriptCameraStates : public CameraInteractionStates {
public:
    ScriptCameraStates();

    void updateStateFromInput(double deltaTime);

    void addLocalRotation(const glm::dvec2& delta);
    void addGlobalRotation(const glm::dvec2& delta);
    void addTruckMovement(const glm::dvec2& delta);
    void addLocalRoll(const glm::dvec2& delta);
    void addGlobalRoll(const glm::dvec2& delta);

private:
    glm::dvec2 _localRotation = glm::dvec2(0.0);
    glm::dvec2 _globalRotation = glm::dvec2(0.0);
    glm::dvec2 _truckMovement = glm::dvec2(0.0);
    glm::dvec2 _localRoll = glm::dvec2(0.0);
    glm::dvec2 _globalRoll = glm::dvec2(0.0);
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___SCRIPTCAMERASTATES___H__
