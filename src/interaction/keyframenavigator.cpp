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

#include <openspace/interaction/keyframenavigator.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>
#include <openspace/util/time.h>

#include <ghoul/logging/logmanager.h>

#include <glm/gtx/quaternion.hpp>

namespace {
    const char* _loggerCat = "keyframenavigator";
}

//#define TIMING_DEBUGGING

namespace openspace::interaction {

bool KeyframeNavigator::updateCamera(Camera& camera, bool ignoreFutureKeyframes) {
#ifdef TIMING_DEBUGGING
    static int pulledCamKeyframes = 0;
#endif
    double now = currentTime();
    bool foundPrevKeyframe = false;

    if (_cameraPoseTimeline.nKeyframes() == 0) {
#ifdef TIMING_DEBUGGING
        LINFO(fmt::format("quit w/ 0 frames left"));
#endif
        return false;
    }

    const Keyframe<CameraPose>* nextKeyframe =
                                              _cameraPoseTimeline.firstKeyframeAfter(now);
    const Keyframe<CameraPose>* prevKeyframe =
                                              _cameraPoseTimeline.lastKeyframeBefore(now);
    double nextTime = 0;
    double prevTime = 0;
    double t = 0;

    if (nextKeyframe) {
        nextTime = nextKeyframe->timestamp;
    } else {
#ifdef TIMING_DEBUGGING
        LINFO(fmt::format("quit w/ no nextKeyframe"));
#endif
        if (ignoreFutureKeyframes) {
#ifdef TIMING_DEBUGGING
            LINFO(fmt::format("removing previous-time keyframes"));
#endif
            _cameraPoseTimeline.removeKeyframesBefore(now);
        }
        return false;
    }

    if (prevKeyframe) {
        prevTime = prevKeyframe->timestamp;
        t = (now - prevTime) / (nextTime - prevTime);
        foundPrevKeyframe = true;
    } else {
        // If there is no keyframe before: Only use the next keyframe.
        prevTime = nextTime;
        prevKeyframe = nextKeyframe;
        t = 1;
    }

    _cameraPoseTimeline.removeKeyframesBefore(prevTime);

    if (!foundPrevKeyframe && ignoreFutureKeyframes) {
//#ifdef TIMING_DEBUGGING
//        LINFO(fmt::format("quit w/out prevKeyframe ({} left)", _cameraPoseTimeline.nKeyframes()));
//#endif
        return false;
    }

    const CameraPose& prevPose = prevKeyframe->data;
    const CameraPose& nextPose = nextKeyframe->data;

    Scene* scene = camera.parent()->scene();
    SceneGraphNode* prevFocusNode = scene->sceneGraphNode(prevPose.focusNode);
    SceneGraphNode* nextFocusNode = scene->sceneGraphNode(nextPose.focusNode);

    if (!prevFocusNode || !nextFocusNode) {
        return false;
    }

    glm::dvec3 prevKeyframeCameraPosition = prevPose.position;
    glm::dvec3 nextKeyframeCameraPosition = nextPose.position;
    glm::dquat prevKeyframeCameraRotation = prevPose.rotation;
    glm::dquat nextKeyframeCameraRotation = nextPose.rotation;

    // Transform position and rotation based on focus node rotation
    // (if following rotation)
    if (prevPose.followFocusNodeRotation) {
        prevKeyframeCameraRotation = glm::dquat(
            prevFocusNode->worldRotationMatrix() *
            glm::dmat3(glm::dquat(prevPose.rotation))
        );
        prevKeyframeCameraPosition = prevFocusNode->worldRotationMatrix() *
                                     prevPose.position;
    }
    if (nextPose.followFocusNodeRotation) {
        nextKeyframeCameraRotation = glm::dquat(
            nextFocusNode->worldRotationMatrix() *
            glm::dmat3(glm::dquat(nextPose.rotation))
        );
        nextKeyframeCameraPosition = nextFocusNode->worldRotationMatrix() *
                                     nextPose.position;
    }

#ifdef TIMING_DEBUGGING
    pulledCamKeyframes++;
    if ((pulledCamKeyframes == int((pulledCamKeyframes / 20) * 20)) || _cameraPoseTimeline.nKeyframes() < 10)
        LINFO(fmt::format("cameraPose (timed {:8.3f} with t={:5.3f}) @ {:8.3f} ({} left)", nextTime, t, now, _cameraPoseTimeline.nKeyframes()));
#endif

    // Transform position based on focus node position
    prevKeyframeCameraPosition += prevFocusNode->worldPosition();
    nextKeyframeCameraPosition += nextFocusNode->worldPosition();

    // Linear interpolation
    camera.setPositionVec3(
        prevKeyframeCameraPosition * (1 - t) + nextKeyframeCameraPosition * t
    );
    camera.setRotation(
        glm::slerp(prevKeyframeCameraRotation, nextKeyframeCameraRotation, t)
    );
    return true;
}

double KeyframeNavigator::currentTime() const {
    if( _timeframeMode == KeyframeTimeRef::relative_recordedStart )
        return (OsEng.windowWrapper().applicationTime() - _referenceTimestamp);
    else if( _timeframeMode == KeyframeTimeRef::absolute_simTimeJ2000 )
        return OsEng.timeManager().time().j2000Seconds();
    else
        return OsEng.windowWrapper().applicationTime();
}

void KeyframeNavigator::setTimeReferenceMode(KeyframeTimeRef refType, double referenceTimestamp) {
    _timeframeMode = refType;
    _referenceTimestamp = referenceTimestamp;
}

Timeline<KeyframeNavigator::CameraPose>& KeyframeNavigator::timeline() {
    return _cameraPoseTimeline;
}

void KeyframeNavigator::addKeyframe(double timestamp, KeyframeNavigator::CameraPose pose)
{
/*#ifdef TIMING_DEBUGGING
static int addedCamKeyframes = 0;
#endif*/
    timeline().addKeyframe(timestamp, pose);
/*#ifdef TIMING_DEBUGGING
    addedCamKeyframes++;
    if(addedCamKeyframes == int((addedCamKeyframes / 40) * 40))
        LINFO(fmt::format("+ camKeyFrame @ {}", timestamp));
#endif*/
}

void KeyframeNavigator::removeKeyframesAfter(double timestamp) {
    timeline().removeKeyframesAfter(timestamp);
}

void KeyframeNavigator::clearKeyframes() {
    timeline().clearKeyframes();
}

size_t KeyframeNavigator::nKeyframes() const {
    return _cameraPoseTimeline.nKeyframes();
}

} // namespace openspace::interaction
