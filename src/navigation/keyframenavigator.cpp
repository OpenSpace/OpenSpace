/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/navigation/keyframenavigator.h>

#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <ghoul/logging/logmanager.h>

#include <glm/gtx/quaternion.hpp>

namespace openspace::interaction {

KeyframeNavigator::CameraPose::CameraPose(datamessagestructures::CameraKeyframe&& kf)
    : position(std::move(kf._position))
    , rotation(std::move(kf._rotation))
    , focusNode(std::move(kf._focusNode))
    , scale(kf._scale)
    , followFocusNodeRotation(kf._followNodeRotation)
{}

bool KeyframeNavigator::updateCamera(Camera& camera, bool ignoreFutureKeyframes) {
    const double now = currentTime();
    bool foundPrevKeyframe = false;

    if (_cameraPoseTimeline.nKeyframes() == 0) {
        return false;
    }

    const Keyframe<CameraPose>* nextKeyframe =
        _cameraPoseTimeline.firstKeyframeAfter(now);
    const Keyframe<CameraPose>* prevKeyframe =
        _cameraPoseTimeline.lastKeyframeBefore(now);

    double nextTime = 0.0;
    if (nextKeyframe) {
        nextTime = nextKeyframe->timestamp;
    }
    else {
        if (ignoreFutureKeyframes) {
            _cameraPoseTimeline.removeKeyframesBefore(now);
        }
        return false;
    }

    double prevTime = 0.0;
    double t = 0.0;
    if (prevKeyframe) {
        prevTime = prevKeyframe->timestamp;
        t = (now - prevTime) / (nextTime - prevTime);
        foundPrevKeyframe = true;
    }
    else {
        // If there is no keyframe before: Only use the next keyframe.
        prevTime = nextTime;
        prevKeyframe = nextKeyframe;
        t = 1;
    }

    const CameraPose prevPose = prevKeyframe->data;
    const CameraPose nextPose = nextKeyframe->data;
    _cameraPoseTimeline.removeKeyframesBefore(prevTime);

    if (!foundPrevKeyframe && ignoreFutureKeyframes) {
        return false;
    }

    return updateCamera(&camera, prevPose, nextPose, t, ignoreFutureKeyframes);
}

bool KeyframeNavigator::updateCamera(Camera* camera, const CameraPose& prevPose,
                                     const CameraPose& nextPose, double t,
                                     bool ignoreFutureKeyframes)
{
    Scene* scene = camera->parent()->scene();
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

    // Transform position based on focus node position
    prevKeyframeCameraPosition += prevFocusNode->worldPosition();
    nextKeyframeCameraPosition += nextFocusNode->worldPosition();

    // Linear interpolation
    t = std::max(0.0, std::min(1.0, t));
    const glm::dvec3 nowCameraPosition =
        prevKeyframeCameraPosition * (1.0 - t) + nextKeyframeCameraPosition * t;
    glm::dquat nowCameraRotation = glm::slerp(
        prevKeyframeCameraRotation,
        nextKeyframeCameraRotation,
        t
    );

    camera->setPositionVec3(std::move(nowCameraPosition));
    camera->setRotation(std::move(nowCameraRotation));

    // We want to affect view scaling, such that we achieve
    // logarithmic interpolation of distance to an imagined focus node.
    // To do this, we interpolate the scale reciprocal logarithmically.
    if (!ignoreFutureKeyframes) {
        const float prevInvScaleExp = glm::log(1.f / prevPose.scale);
        const float nextInvScaleExp = glm::log(1.f / nextPose.scale);
        const float interpolatedInvScaleExp = static_cast<float>(
            prevInvScaleExp * (1.0 - t) + nextInvScaleExp * t
        );
        camera->setScaling(1.f / glm::exp(interpolatedInvScaleExp));
    }

    return true;
}

double KeyframeNavigator::currentTime() const {
    if (_timeframeMode == KeyframeTimeRef::Relative_recordedStart) {
        return (global::windowDelegate->applicationTime() - _referenceTimestamp);
    }
    else if (_timeframeMode == KeyframeTimeRef::Absolute_simTimeJ2000) {
        return global::timeManager->time().j2000Seconds();
    }
    else {
        return global::windowDelegate->applicationTime();
    }
}

void KeyframeNavigator::setTimeReferenceMode(KeyframeTimeRef refType,
                                             double referenceTimestamp)
{
    _timeframeMode = refType;
    _referenceTimestamp = referenceTimestamp;
}

Timeline<KeyframeNavigator::CameraPose>& KeyframeNavigator::timeline() {
    return _cameraPoseTimeline;
}

void KeyframeNavigator::addKeyframe(double timestamp, KeyframeNavigator::CameraPose pose)
{
    timeline().addKeyframe(timestamp, std::move(pose));
}

void KeyframeNavigator::removeKeyframesAfter(double timestamp, Inclusive inclusive) {
    timeline().removeKeyframesAfter(timestamp, inclusive);
}

void KeyframeNavigator::clearKeyframes() {
    timeline().clearKeyframes();
}

size_t KeyframeNavigator::nKeyframes() const {
    return _cameraPoseTimeline.nKeyframes();
}

} // namespace openspace::interaction
