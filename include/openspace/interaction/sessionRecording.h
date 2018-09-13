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

#ifndef __OPENSPACE_CORE___SESSIONRECORDING___H__
#define __OPENSPACE_CORE___SESSIONRECORDING___H__

#include <openspace/interaction/externInteraction.h>
#include <openspace/interaction/keyframenavigator.h>
#include <openspace/network/messagestructures.h>
#include <openspace/scripting/lualibrary.h>
#include <ghoul/io/socket/tcpsocket.h>

#include <vector>
#include <fstream>
#include <iomanip>

namespace openspace::interaction {

class KeyframeNavigator;

class SessionRecording : public properties::PropertyOwner {
public:
    enum class sessionState {
        idle = 0,
        recording,
        playback
    };
    enum class recordedType {
        camera = 0,
        time,
        script
    };

    SessionRecording();
    ~SessionRecording();

    bool startRecording(std::string filename);
    void stopRecording();
    void saveScript(std::string scriptToSave);
    bool isRecording();
    bool startPlayback(std::string filename, KeyframeTimeRef timeMode);
    void saveCameraKeyframe();
#ifdef SESSION_RECORDING_TIME
    void saveTimeKeyframe();
#endif
    void preSynchronization();
    void playbackAddEntriesToTimeline();
    void playbackCamera(std::string& entry);
#ifdef SESSION_RECORDING_TIME
    void playbackTimeChange(std::string& entry);
#endif
    void playbackScript(std::string& entry);
    void stopPlayback();
    /**
    * \return The Lua library that contains all Lua functions available to affect the
    * interaction
    */
    static openspace::scripting::LuaLibrary luaLibrary();
    
private:
    ExternInteraction _externInteract;
    bool _isRecording = false;
    double _timestampRecordStarted;
    double _timestampPlaybackStarted_application;
    double _timestampPlaybackStarted_simulation;
    double _timestampApplicationStarted_simulation;
    void saveKeyframeToFile(std::string entry);
    double getAppropriateTimestamp(std::istringstream& inputLine);
    double getEquivalentSimulationTime(std::istringstream& inputLine);
    double getEquivalentApplicationTime(std::istringstream& inputLine);
    void signalPlaybackFinishedForComponent(recordedType type);
    sessionState _state = sessionState::idle;
    std::string _playbackFilename;
    std::ifstream _playbackFile;
    std::ofstream _recordFile;
    int _playbackLineNum = 1;
    KeyframeTimeRef _playbackTimeReferenceMode;
    bool _playbackActive_camera = false;
#ifdef SESSION_RECORDING_TIME
    bool _playbackActive_time = false;
#endif
    bool _playbackActive_script = false;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___SESSIONRECORDING___H__
