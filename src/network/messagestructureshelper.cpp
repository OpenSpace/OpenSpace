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

#include <openspace/network/messagestructureshelper.h>

#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>

namespace openspace::datamessagestructures {

CameraKeyframe generateCameraKeyframe() {
    interaction::NavigationHandler& navHandler = *global::navigationHandler;
    CameraKeyframe kf;
    const SceneGraphNode* focusNode = navHandler.orbitalNavigator().anchorNode();

    if (!focusNode) {
        return kf;
    }

    //kf._position = global::navigationHandler.camera()->positionVec3();
    kf._position = navHandler.orbitalNavigator().anchorNodeToCameraVector();

    kf._followNodeRotation = navHandler.orbitalNavigator().followingAnchorRotation();
    if (kf._followNodeRotation) {
        kf._position = glm::inverse(focusNode->worldRotationMatrix()) * kf._position;
        kf._rotation = navHandler.orbitalNavigator().anchorNodeToCameraRotation();
    }
    else {
        kf._rotation = navHandler.camera()->rotationQuaternion();
    }

    kf._focusNode = focusNode->identifier();
    kf._scale = navHandler.camera()->scaling();

    // Timestamp as current runtime of OpenSpace instance
    kf._timestamp = global::windowDelegate->applicationTime();

    return kf;
}

TimeKeyframe generateTimeKeyframe() {
    TimeKeyframe kf;
    const Time& time = global::timeManager->time();

    kf._dt = global::timeManager->deltaTime();
    kf._paused = global::timeManager->isPaused();
    kf._time = time.j2000Seconds();

    // Timestamp as current runtime of OpenSpace instance
    kf._timestamp = global::windowDelegate->applicationTime();
    return kf;
}

ScriptMessage generateScriptMessage(std::string script) {
    ScriptMessage sm;
    sm._script = std::move(script);
    // Timestamp as current runtime of OpenSpace instance
    sm._timestamp = global::windowDelegate->applicationTime();
    return sm;
}

} // namespace openspace::datamessagestructures

