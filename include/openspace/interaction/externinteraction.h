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

#ifndef __OPENSPACE_CORE___EXTERNINTERACTION___H__
#define __OPENSPACE_CORE___EXTERNINTERACTION___H__

#include <openspace/network/messagestructures.h>
#include <openspace/properties/propertyowner.h>
#include <ghoul/io/socket/tcpsocket.h>

#include <vector>

namespace openspace {

class ExternInteraction : public properties::PropertyOwner {
public:
    ExternInteraction();
    /**
    * Method that generates a keyframeNavigator CameraPose from a CameraKeyframe
    * object, and then adds this to the navigationHandler's keyframe navigator.
    * \param kf The camera keyframe to add.
    */
    void cameraInteraction(datamessagestructures::CameraKeyframe kf);
    /**
    * Method that generates a TimeKeyframeData from a TimeKeyframe object, and
    * then adds this to the timeManager.
    * \param kf The time keyframe to add.
    */
    void timeInteraction(datamessagestructures::TimeKeyframe kf);
    /**
    * Method that passes a ScriptMessage object to the script engine, calling its
    * queueScript method to add it for execution.
    * \param sm The ScriptMessage object to queue in the script engine.
    */
    void scriptInteraction(datamessagestructures::ScriptMessage sm);
    /**
    * Method that accepts a reference to a CameraKeyframe object, and populates
    * it with the current properties of the camera from the navigation handler.
    * \returns CameraKeyframe with current state from NavigationHandler.
    */
    datamessagestructures::CameraKeyframe generateCameraKeyframe();
    /**
    * Method that accepts a reference to a TimeKeyframe object, and populates
    * it with the current time values from the application time manager.
    * \returns TimeKeyframe The time keyframe.
    */
    datamessagestructures::TimeKeyframe generateTimeKeyframe();
    /**
    * Method that accepts a reference to a ScriptMessage object and a script
    * string, and populates the ScriptMessage with the script and timestamp
    * of the current application time.
    * \param script The script to execute in std::string form.
    * \returns ScriptMessage The ScriptMessage data structure with script.
    */
    datamessagestructures::ScriptMessage generateScriptMessage(std::string script);
private:
};

} // namespace openspace

#endif // __OPENSPACE_CORE___EXTERNINTERACTION___H__
