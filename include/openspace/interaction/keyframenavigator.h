/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
#include <openspace/util/timemanager.h>
#include <openspace/network/parallelpeer.h>

#include <ghoul/glm.h>
#include <glm/gtx/quaternion.hpp>

namespace openspace { class Camera; }
namespace openspace { class TimeManager; }

namespace openspace::interaction {

enum class KeyframeTimeRef {
    relative_applicationStart,
    relative_recordedStart,
    absolute_simTimeJ2000
};

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

    /**
    * Update camera position using the next camera pose keyframe from the timeline.
    * Returns true if camera was set to a pose from the next keyframe.
    * Returns false if no keyframes are available after the current time.
    * \param camera A reference to the camera object to have its pose updated.
    * \returns true only if a new future keyframe is available to set camera pose.
    */
    bool updateCamera(Camera& camera);
    Timeline<CameraPose>& timeline();

    void addKeyframe(double timestamp, KeyframeNavigator::CameraPose pose);
    void removeKeyframesAfter(double timestamp);
    void clearKeyframes();
    size_t nKeyframes() const;
    const std::vector<datamessagestructures::CameraKeyframe>& keyframes() const;
    double currentTime() const;
    void setTimeReferenceMode(KeyframeTimeRef refType, double referenceTimestamp);

private:
    Timeline<CameraPose> _cameraPoseTimeline;
    KeyframeTimeRef _timeframeMode = KeyframeTimeRef::relative_applicationStart;
    double _referenceTimestamp = 0.0;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___KEYFRAMENAVIGATOR___H__
