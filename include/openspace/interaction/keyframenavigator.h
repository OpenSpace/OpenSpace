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

#ifndef __OPENSPACE_CORE___KEYFRAMENAVIGATOR___H__
#define __OPENSPACE_CORE___KEYFRAMENAVIGATOR___H__

#include <openspace/util/timeline.h>
#include <openspace/network/parallelconnection.h>

#include <glm/glm.hpp>
#include <glm/gtx/quaternion.hpp>

namespace openspace { class Camera; }

namespace openspace::interaction {

class KeyframeNavigator {
public:
    struct CameraPose {
        glm::dvec3 position;
        glm::quat rotation;
        std::string focusNode;
        bool followFocusNodeRotation;
    };

    KeyframeNavigator() = default;
    ~KeyframeNavigator() = default;

    void updateCamera(Camera& camera);
    Timeline<CameraPose>& timeline();

    void addKeyframe(double timestamp, KeyframeNavigator::CameraPose pose);
    void removeKeyframesAfter(double timestamp);
    void clearKeyframes();
    size_t nKeyframes() const;
    const std::vector<datamessagestructures::CameraKeyframe>& keyframes() const;

private:
    Timeline<CameraPose> _cameraPoseTimeline;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___KEYFRAMENAVIGATOR___H__
