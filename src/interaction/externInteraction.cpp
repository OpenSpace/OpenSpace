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

#include <openspace/interaction/externInteraction.h>

#include <openspace/openspace.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>

#include <ghoul/logging/logmanager.h>

#include "../network/parallelpeer_lua.inl"

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
    pose.followFocusNodeRotation = kf._followNodeRotation;

    OsEng.navigationHandler().keyframeNavigator().addKeyframe(kf._timestamp, pose);
}

void ExternInteraction::timeInteraction(datamessagestructures::TimeKeyframe kf) {
    Time time(kf._time);
    time.setDeltaTime(kf._dt);
    time.setPause(kf._paused);
    time.setTimeJumped(kf._requiresTimeJump);

    OsEng.timeManager().addKeyframe(kf._timestamp, time);
}

void ExternInteraction::scriptInteraction(datamessagestructures::ScriptMessage sm) {
    OsEng.scriptEngine().queueScript(
        sm._script,
        scripting::ScriptEngine::RemoteScripting::No
    );
}
    
void ExternInteraction::generateCameraKeyframe(datamessagestructures::CameraKeyframe& kf) {
    SceneGraphNode* focusNode = OsEng.navigationHandler().focusNode();
    if (!focusNode) {
        return;
    }
    
    kf._position = OsEng.navigationHandler().focusNodeToCameraVector();
    
    kf._followNodeRotation =
    OsEng.navigationHandler().orbitalNavigator().followingNodeRotation();
    if (kf._followNodeRotation) {
        kf._position = glm::inverse(focusNode->worldRotationMatrix()) * kf._position;
        kf._rotation = OsEng.navigationHandler().focusNodeToCameraRotation();
    }
    else {
        kf._rotation = OsEng.navigationHandler().camera()->rotationQuaternion();
    }
    
    kf._focusNode = focusNode->identifier();
    
    // Timestamp as current runtime of OpenSpace instance
    kf._timestamp = OsEng.windowWrapper().applicationTime();
}
    
void ExternInteraction::generateTimeKeyframe(datamessagestructures::TimeKeyframe& kf) {
    Time& time = OsEng.timeManager().time();
    
    kf._dt = time.deltaTime();
    kf._paused = time.paused();
    kf._time = time.j2000Seconds();
    
    // Timestamp as current runtime of OpenSpace instance
    kf._timestamp = OsEng.windowWrapper().applicationTime();
}
    
void ExternInteraction::generateScriptMessage(datamessagestructures::ScriptMessage& sm,
                                              std::string script) {
    sm._script = std::move(script);
}


} // namespace openspace
