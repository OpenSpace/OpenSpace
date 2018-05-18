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

#include <openspace/interaction/sessionRecording.h>
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
    const char* _loggerCat = "ExternInteraction";
}

namespace openspace {

bool SessionRecording::startRecording(std::string filename) {
    if( _state == sessionState::playback ) {
        _playbackFile.close();
    }
    _state = sessionState::recording;
    _recordFile.open(filename);
    if( ! _recordFile.is_open() || ! _recordFile.good() )
    {
        LERROR(fmt::format("Unable to open file {} for keyframe recording", filename.c_str()));
        return false;
    }
    _timestampRecordStarted = OsEng.windowWrapper().applicationTime();
}

void SessionRecording::stopRecording() {
    if( _state == sessionState::recording )
        _state = sessionState::idle;
    _recordFile.close();
}
    
bool SessionRecording::startPlayback(std::string filename) {
    _playbackLineNum = 1;
    _playbackFilename = filename;
    _playbackFile.open(_playbackFilename, std::ifstream::in);
    if( ! _playbackFile.is_open() || ! _playbackFile.good() )
    {
        LERROR(fmt::format("Unable to open file {} for keyframe playback", filename.c_str()));
        return false;
    }
    playbackAddEntriesToTimeline();
}

void SessionRecording::saveCameraKeyframe() {
    SceneGraphNode* focusNode = OsEng.navigationHandler().focusNode();
    if (!focusNode) {
        return;
    }

    //Create a camera keyframe, then call to populate it with current position
    // & orientation of camera
    datamessagestructures::CameraKeyframe kf;
    _externInteract.generateCameraKeyframe(kf);
    
    std::stringstream keyframeLine = std::stringstream("camera ");
    //Add simulation timestamp and timestamp relative to recording start
    keyframeLine << kf._timestamp << " ";
    keyframeLine << (kf._timestamp - _timestampRecordStarted) << " ";
    //Add camera position
    keyframeLine << kf._position.x << " "
                 << kf._position.y << " "
                 << kf._position.z << " ";
    //Add camera rotation
    keyframeLine << kf._rotation.x << " "
                 << kf._rotation.y << " "
                 << kf._rotation.z << " "
                 << kf._rotation.w << " ";
    if( kf._followNodeRotation )
        keyframeLine << "F ";
    else
        keyframeLine << "- ";
    keyframeLine << kf._focusNode;
    
    saveKeyframeToFile(keyframeLine.str());
}

void SessionRecording::saveTimeKeyframe() {
    //Create a time keyframe, then call to populate it with current time props
    datamessagestructures::TimeKeyframe kf;
    _externInteract.generateTimeKeyframe(kf);

    std::stringstream keyframeLine = std::stringstream("time ");
    //Add simulation timestamp and timestamp relative to recording start
    keyframeLine << kf._timestamp << " ";
    keyframeLine << (kf._timestamp - _timestampRecordStarted) << " ";
    keyframeLine << kf._dt << " ";
    if( kf._paused )
        keyframeLine << "P ";
    else
        keyframeLine << "R ";
    
    if( kf._requiresTimeJump )
        keyframeLine << "J ";
    else
        keyframeLine << "- ";

    keyframeLine << kf._time;
    
    saveKeyframeToFile(keyframeLine.str());
}

void SessionRecording::saveScript(std::string scriptToSave) {
    datamessagestructures::ScriptMessage sm;
    _externInteract.generateScriptMessage(sm, scriptToSave);

    std::stringstream keyframeLine = std::stringstream("script ");
    //Add simulation timestamp and timestamp relative to recording start
    keyframeLine << sm._timestamp << " ";
    keyframeLine << (sm._timestamp - _timestampRecordStarted) << " ";
    
    keyframeLine << scriptToSave;
    
    saveKeyframeToFile(keyframeLine.str());
}

void SessionRecording::preSynchronization() {
    if( _state == sessionState::recording ) {
        saveCameraKeyframe();
        saveTimeKeyframe();
    }
}

bool SessionRecording::isRecording() {
    return (_state == sessionState::recording);
}

void SessionRecording::playbackAddEntriesToTimeline() {
    std::string line;
    while( std::getline(_playbackFile, line) ) {
        _playbackLineNum++;
        std::istringstream iss(line);
        std::string entryType;
        if( ! (iss >> entryType) ) {
            LERROR(fmt::format("Error reading entry type @ line {} of playback file {}",
                _playbackLineNum, _playbackFilename.c_str()));
            break;
        }

        if( entryType.compare("camera") == 0 )
            playbackCamera(line);
        else if( entryType.compare("time") == 0 )
            playbackTimeChange(line);
        else if( entryType.compare("script") == 0 )
            playbackScript(line);
    }
    LINFO(fmt::format("Finished parsing {} entries from playback file {}",
        _playbackLineNum - 1, _playbackFilename.c_str()));
}
    
void SessionRecording::playbackCamera(std::string& entry) {
    bool parseResult = true;
    std::istringstream iss(entry);
    std::string entryType;
    double timeOs, timeRel;
    std::string rotationFollowing;
    
    KeyframeNavigator::CameraPose pbFrame;
    //datamessagestructures::CameraKeyframe pbFrame;
    
    parseResult = (iss >> entryType
                       >> timeOs
                       >> timeRel
                       >> pbFrame.position.x
                       >> pbFrame.position.y
                       >> pbFrame.position.z
                       >> pbFrame.rotation.x
                       >> pbFrame.rotation.y
                       >> pbFrame.rotation.z
                       >> pbFrame.rotation.w
                       >> rotationFollowing
                       >> pbFrame.focusNode);
    if( ! parseResult ) {
        LERROR(fmt::format("Error parsing line {} of playback file", _playbackLineNum));
        break;
    }
    if( rotationFollowing.compare("F") == 0 )
        pbFrame.followFocusNodeRotation = true;
    else
        pbFrame.followFocusNodeRotation = false;
    
    OsEng.navigationHandler().keyframeNavigator().addKeyframe(timeOs, pbFrame);
    //_externInteraction.cameraInteraction(pbFrame);
}

void SessionRecording::playbackTimeChange(std::string& entry) {
    std::istringstream iss(entry);
    std::string entryType;
    double timeOs, timeRel;
    std::string paused, jump;

    datamessagestructures::TimeKeyframe pbFrame;
    parseResult = (iss >> entryType
                       >> timeOs
                       >> timeRel
                       >> pbFrame._dt
                       >> paused
                       >> jump
                       >> pbFrame._time);
    if( ! parseResult ) {
        LERROR(fmt::format("Error parsing line {} of playback file", _playbackLineNum));
        break;
    }

    if( paused.compare("P") == 0 )
        pbFrame._paused = true;
    else
        pbFrame._paused = false;
    if( jump.compare("J") == 0 )
        pbFrame._requiresTimeJump = true;
    else
        pbFrame._requiresTimeJump = false;

    //TODO: keyframeNavigator doesn't handle time info yet
    OsEng.navigationHandler().keyframeNavigator().addKeyframe(timeOs, pbFrame);
    //_externInteraction.timeInteraction(pbFrame);
}

void SessionRecording::playbackScript(std::string& entry) {
    std::istringstream iss(entry);
    std::string entryType;
    double timeOs, timeRel;
    
    datamessagestructures::ScriptMessage pbFrame;
    parseResult = (iss >> entryType
                       >> timeOs
                       >> timeRel
                       >> pbFrame._script);
    if( ! parseResult ) {
        LERROR(fmt::format("Error parsing line {} of playback file", _playbackLineNum));
        break;
    }
    //TODO: keyframeNavigator doesn't handle scripts yet
    OsEng.navigationHandler().keyframeNavigator().addKeyframe(timeOs, pbFrame);
    //_externInteraction.scriptInteraction(pbFrame);
}

void SessionRecording::saveKeyframeToFile(std::string entry) {
    _recordFile << entry << std::endl;
}

} // namespace openspace
