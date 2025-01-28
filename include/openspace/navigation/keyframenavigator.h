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

#ifndef __OPENSPACE_CORE___KEYFRAMENAVIGATOR___H__
#define __OPENSPACE_CORE___KEYFRAMENAVIGATOR___H__

#include <openspace/network/messagestructures.h>
#include <openspace/util/timeline.h>
#include <ghoul/glm.h>
#include <ghoul/misc/boolean.h>
#include <glm/gtx/quaternion.hpp>

namespace openspace {
    class Camera;
    class TimeManager;
} // namespace openspace

namespace openspace::interaction {

enum class KeyframeTimeRef {
    Relative_applicationStart,
    Relative_recordedStart,
    Absolute_simTimeJ2000
};

class KeyframeNavigator {
public:
    BooleanType(Inclusive);

    struct CameraPose {
        glm::dvec3 position = glm::dvec3(0.0);
        glm::quat rotation = glm::quat(0.f, 0.f, 0.f, 0.f);
        std::string focusNode;
        float scale = 1.f;
        bool followFocusNodeRotation = false;

        CameraPose() = default;
        CameraPose(datamessagestructures::CameraKeyframe&& kf);
        auto operator<=>(const CameraPose&) const = default;
    };

    /**
     * Update camera position using the next camera pose keyframe from the timeline.
     * Returns true if camera was set to a pose from the next keyframe. Returns false if
     * no keyframes are available after the current time.
     *
     * \param camera A reference to the camera object to have its pose updated
     * \param ignoreFutureKeyframes `true` if only past keyframes are to be used
     * \return true only if a new future keyframe is available to set camera pose
     */
    void updateCamera(Camera& camera, bool ignoreFutureKeyframes);
    static void updateCamera(Camera* camera, const CameraPose& prevPose,
        const CameraPose& nextPose, double t, bool ignoreFutureKeyframes);

    Timeline<CameraPose>& timeline();
    void addKeyframe(double timestamp, KeyframeNavigator::CameraPose pose);
    void removeKeyframesAfter(double timestamp, Inclusive inclusive = Inclusive::No);
    void clearKeyframes();
    size_t nKeyframes() const;
    double currentTime() const;
    void setTimeReferenceMode(KeyframeTimeRef refType, double referenceTimestamp);

private:
    Timeline<CameraPose> _cameraPoseTimeline;
    KeyframeTimeRef _timeframeMode = KeyframeTimeRef::Relative_applicationStart;
    double _referenceTimestamp = 0.0;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___KEYFRAMENAVIGATOR___H__
