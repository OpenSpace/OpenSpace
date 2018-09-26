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
#define RECORD_BINARY
//#define SESSION_RECORDING_TIME
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

    void deinitialize();

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
    void playbackCamera();
#ifdef SESSION_RECORDING_TIME
    void playbackTimeChange();
#endif
    void playbackScript();
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
    bool hasCameraChangedFromPrev(datamessagestructures::CameraKeyframe kfNew);
    double getAppropriateTimestamp(double timeOs, double timeRec, double timeSim);
    double getEquivalentSimulationTime(double timeOs, double timeRec, double timeSim);
    double getEquivalentApplicationTime(double timeOs, double timeRec, double timeSim);
    void signalPlaybackFinishedForComponent(recordedType type);
#ifdef RECORD_BINARY
    void writeToFileBuffer(const double& src);
    void writeToFileBuffer(const unsigned char c);
    void writeToFileBuffer(bool b);
    void writeToFileBuffer(const std::string s);
    void saveKeyframeToFileBinary();
    void readFromPlayback(unsigned char& result);
    void readFromPlayback(double& result);
    void readFromPlayback(float& result);
    void readFromPlayback(size_t& result);
    void readFromPlayback(bool& result);
    void readFromPlayback(std::string& result);
#else
    void saveKeyframeToFile(std::string entry);
#endif //#ifdef RECORD_BINARY

    sessionState _state = sessionState::idle;
    std::string _playbackFilename;
    std::ifstream _playbackFile;
    std::string _playbackLineParsing;
    std::ofstream _recordFile;
    int _playbackLineNum = 1;
    KeyframeTimeRef _playbackTimeReferenceMode;
    datamessagestructures::CameraKeyframe _prevRecordedCameraKeyframe;
    bool _playbackActive_camera = false;
#ifdef SESSION_RECORDING_TIME
    bool _playbackActive_time = false;
#endif
    bool _playbackActive_script = false;
#ifdef RECORD_BINARY
    static const size_t saveBufferCameraSize_min = 82;
    static const size_t saveBufferStringSize_max = 500;
    unsigned char _keyframeBuffer[(saveBufferCameraSize_min + saveBufferStringSize_max)];
    unsigned int _bufferIndex = 0;
    unsigned int _playbackIndex = 0;
#endif
};

} // namespace openspace

#endif // __OPENSPACE_CORE___SESSIONRECORDING___H__
