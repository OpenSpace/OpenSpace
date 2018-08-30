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

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>
#include <openspace/util/time.h>
#include <ghoul/logging/logmanager.h>

namespace openspace::interaction {

void KeyframeNavigator::updateCamera(Camera& camera) {
    double now = global::windowDelegate.applicationTime();

    if (_cameraPoseTimeline.nKeyframes() == 0) {
        return;
    }

    const Keyframe<CameraPose>* nextKeyframe =
                                              _cameraPoseTimeline.firstKeyframeAfter(now);
    const Keyframe<CameraPose>* prevKeyframe =
                                              _cameraPoseTimeline.lastKeyframeBefore(now);

    double nextTime = 0.0;
    if (nextKeyframe) {
        nextTime = nextKeyframe->timestamp;
    } else {
        return;
    }

    double prevTime = 0.0;
    double t = 0.0;
    if (prevKeyframe) {
        prevTime = prevKeyframe->timestamp;
        t = (now - prevTime) / (nextTime - prevTime);
    } else {
        // If there is no keyframe before: Only use the next keyframe.
        prevTime = nextTime;
        prevKeyframe = nextKeyframe;
        t = 1;
    }

    _cameraPoseTimeline.removeKeyframesBefore(prevTime);

    const CameraPose& prevPose = prevKeyframe->data;
    const CameraPose& nextPose = nextKeyframe->data;

    Scene* scene = camera.parent()->scene();
    SceneGraphNode* prevFocusNode = scene->sceneGraphNode(prevPose.focusNode);
    SceneGraphNode* nextFocusNode = scene->sceneGraphNode(nextPose.focusNode);

    if (!prevFocusNode || !nextFocusNode) {
        return;
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
    camera.setPositionVec3(
        prevKeyframeCameraPosition * (1 - t) + nextKeyframeCameraPosition * t
    );
    camera.setRotation(
        glm::slerp(prevKeyframeCameraRotation, nextKeyframeCameraRotation, t)
    );

    // We want to affect view scaling, such that we achieve
    // logarithmic interpolation of distance to an imagined focus node.
    // To do this, we interpolate the scale reciprocal logarithmically.
    const float prevInvScaleExp = glm::log(1.0 / prevPose.scale);
    const float nextInvScaleExp = glm::log(1.0 / nextPose.scale);
    const float interpolatedInvScaleExp = prevInvScaleExp * (1 - t) + nextInvScaleExp * t;
    camera.setScaling(1.0 / glm::exp(interpolatedInvScaleExp));
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
