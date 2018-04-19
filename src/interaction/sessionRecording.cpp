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
#include <openspace/network/externInteraction.h>

#include <openspace/openspace.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>

#include <ghoul/logging/logmanager.h>

#include "parallelpeer_lua.inl"



namespace openspace {

SessionRecording::SessionRecording()
   /* : properties::PropertyOwner({ "ParallelPeer", "Parallel Peer" })
    , _bufferTime(BufferTimeInfo, 0.2f, 0.01f, 5.0f)
    , _timeKeyframeInterval(TimeKeyFrameInfo, 0.1f, 0.f, 1.f)
    , _cameraKeyframeInterval(CameraKeyFrameInfo, 0.1f, 0.f, 1.f)
    , _timeTolerance(TimeToleranceInfo, 1.f, 0.5f, 5.f)
    , _lastTimeKeyframeTimestamp(0)
    , _lastCameraKeyframeTimestamp(0)*/
{
   /* addProperty(_bufferTime);

    addProperty(_timeKeyframeInterval);
    addProperty(_cameraKeyframeInterval);
    addProperty(_timeTolerance);

    _connectionEvent = std::make_shared<ghoul::Event<>>(); */
}


void SessionRecording::recordCamera() {
}

void SessionRecording::recordTimeChange() {
}

void SessionRecording::recordScript() {
}



} // namespace openspace
