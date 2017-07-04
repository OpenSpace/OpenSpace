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

#ifndef __OPENSPACE_CORE___INTERACTIONMODE___H__
#define __OPENSPACE_CORE___INTERACTIONMODE___H__

#include <openspace/interaction/interpolator.h>
#include <openspace/interaction/inputstate.h>
#include <openspace/interaction/delayedvariable.h>

#include <openspace/network/parallelconnection.h>
#include <openspace/util/mouse.h>
#include <openspace/util/keys.h>
#include <openspace/util/timeline.h>

#include <list>

namespace openspace {

class Camera;
class SceneGraphNode;

namespace globebrowsing {
    class RenderableGlobe;
}

namespace interaction {

class InteractionMode {
public:
    InteractionMode();
    virtual ~InteractionMode();

    virtual void setFocusNode(SceneGraphNode* focusNode);

    SceneGraphNode* focusNode();
    Interpolator<double>& rotateToFocusNodeInterpolator();
    virtual bool followingNodeRotation() const = 0;
    
    virtual void updateMouseStatesFromInput(const InputState& inputState, double deltaTime) = 0;
    virtual void updateCameraStateFromMouseStates(Camera& camera, double deltaTime) = 0;

protected:
    struct MouseState {
        MouseState(double scaleFactor)
            : velocity(scaleFactor, 1)
            , previousPosition(0.0, 0.0) {}
        void setFriction(double friction) {
            velocity.setFriction(friction);
        }
        void setVelocityScaleFactor(double scaleFactor) {
            velocity.setScaleFactor(scaleFactor);
        }
        glm::dvec2 previousPosition;
        DelayedVariable<glm::dvec2, double> velocity;
    };

    SceneGraphNode* _focusNode = nullptr;
    glm::dvec3 _previousFocusNodePosition;
    glm::dquat _previousFocusNodeRotation;
    
    Interpolator<double> _rotateToFocusNodeInterpolator;
};

} // namespace interaction
} // namespace openspace

#endif // __OPENSPACE_CORE___INTERACTIONMODE___H__
