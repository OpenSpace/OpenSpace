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

#include <openspace/interaction/externinteraction.h>

#include <openspace/openspace.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/keyframenavigator.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/camera.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>

#include <ghoul/logging/logmanager.h>

namespace {
const uint32_t ProtocolVersion = 3;
const size_t MaxLatencyDiffs = 64;
const char* _loggerCat = "ExternInteraction";

static const openspace::properties::Property::PropertyInfo BufferTimeInfo = {
    "BufferTime",
    "Buffer Time",
    "" // @TODO Missing documentation
};

static const openspace::properties::Property::PropertyInfo TimeKeyFrameInfo = {
    "TimeKeyframeInterval",
    "Time keyframe interval",
    "" // @TODO Missing documentation
};

static const openspace::properties::Property::PropertyInfo CameraKeyFrameInfo = {
    "CameraKeyframeInterval",
    "Camera Keyframe interval",
    "" // @TODO Missing documentation
};

static const openspace::properties::Property::PropertyInfo TimeToleranceInfo = {
    "TimeTolerance",
    "Time tolerance",
    "" // @TODO Missing documentation
};

} // namespace

namespace openspace {

ExternInteraction::ExternInteraction()
    : properties::PropertyOwner({ "ExternInteration", "ExternInteraction" })
{
}

void ExternInteraction::cameraInteraction(datamessagestructures::CameraKeyframe kf) {
    interaction::KeyframeNavigator::CameraPose pose;
    pose.focusNode = kf._focusNode;
    pose.position = kf._position;
    pose.rotation = kf._rotation;
    pose.scale = kf._scale;
    pose.followFocusNodeRotation = kf._followNodeRotation;

    global::navigationHandler.keyframeNavigator().addKeyframe(kf._timestamp, pose);
}

void ExternInteraction::timeInteraction(datamessagestructures::TimeKeyframe kf) {
    TimeKeyframeData timeKfData;
    timeKfData.delta = kf._dt;
    timeKfData.pause = kf._paused;
    timeKfData.jump = kf._requiresTimeJump;

    global::timeManager.addKeyframe(kf._timestamp, timeKfData);
}

void ExternInteraction::scriptInteraction(datamessagestructures::ScriptMessage sm) {
    global::scriptEngine.queueScript(
        sm._script,
        scripting::ScriptEngine::RemoteScripting::No
    );
}
    
#pragma optimize ("", off)

void ExternInteraction::generateCameraKeyframe(datamessagestructures::CameraKeyframe& kf) {
    SceneGraphNode* focusNode = global::navigationHandler.focusNode();
    if (!focusNode) {
        return;
    }
    
    //kf._position = global::navigationHandler.camera()->positionVec3();
    kf._position = global::navigationHandler.focusNodeToCameraVector();

    kf._followNodeRotation =
        global::navigationHandler.orbitalNavigator().followingNodeRotation();
    if (kf._followNodeRotation) {
        kf._position = glm::inverse(focusNode->worldRotationMatrix()) * kf._position;
        kf._rotation = global::navigationHandler.focusNodeToCameraRotation();
    }
    else {
        kf._rotation = global::navigationHandler.camera()->rotationQuaternion();
    }
    
    kf._focusNode = focusNode->identifier();
    kf._scale = global::navigationHandler.camera()->scaling();
    
    // Timestamp as current runtime of OpenSpace instance
    kf._timestamp = global::windowDelegate.applicationTime();
}
    
void ExternInteraction::generateTimeKeyframe(datamessagestructures::TimeKeyframe& kf) {
    const Time& time = global::timeManager.time();
    
    kf._dt = global::timeManager.deltaTime();
    kf._paused = global::timeManager.isPaused();
    kf._time = time.j2000Seconds();
    
    // Timestamp as current runtime of OpenSpace instance
    kf._timestamp = global::windowDelegate.applicationTime();
}
    
void ExternInteraction::generateScriptMessage(datamessagestructures::ScriptMessage& sm,
                                              std::string script) {
    sm._script = std::move(script);
    // Timestamp as current runtime of OpenSpace instance
    sm._timestamp = global::windowDelegate.applicationTime();
}


} // namespace openspace
