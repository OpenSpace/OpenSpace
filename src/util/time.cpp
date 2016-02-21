/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <openspace/util/time.h>

#include <openspace/util/spicemanager.h>
#include <openspace/util/syncbuffer.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>

#include "time_lua.inl"

namespace openspace {

Time* Time::_instance = nullptr;

void Time::initialize() {
    ghoul_assert(_instance == nullptr, "Static time must not have been ininitialized");
    _instance = new Time();
}

void Time::deinitialize() {
    ghoul_assert(_instance, "Static time must have been ininitialized");
    delete _instance;
    _instance = nullptr;
}

Time& Time::ref() {
    ghoul_assert(_instance, "Static time must have been ininitialized");
    return *_instance;
}

bool Time::isInitialized() {
	return (_instance != nullptr);
}

void Time::setTime(double value, bool requireJump) {
	_time = value;
	_timeJumped = requireJump;
}

double Time::currentTime() const {
	return _syncedTime;
}

double Time::advanceTime(double tickTime) {
    if (_timePaused)
        return _time;
    else
	    return _time += _dt * tickTime;
}

void Time::setDeltaTime(double deltaT) {
	_dt = deltaT;
}

double Time::deltaTime() const {
	return _syncedDt;
}

void Time::setPause(bool pause) {
    _timePaused = pause;
}

bool Time::togglePause() {
    _timePaused = !_timePaused;
    return _timePaused;
}

void Time::setTime(std::string time, bool requireJump) {
    _time = SpiceManager::ref().ephemerisTimeFromDate(std::move(time));
	_timeJumped = requireJump;
}

std::string Time::currentTimeUTC() const {
    return SpiceManager::ref().dateFromEphemerisTime(_syncedTime);
}

void Time::serialize(SyncBuffer* syncBuffer) {
	_syncMutex.lock();

	syncBuffer->encode(_sharedTime);
	syncBuffer->encode(_sharedDt);
	syncBuffer->encode(_sharedTimeJumped);

	_syncMutex.unlock();
}

void Time::deserialize(SyncBuffer* syncBuffer) {
	_syncMutex.lock();

	syncBuffer->decode(_sharedTime);
	syncBuffer->decode(_sharedDt);
	syncBuffer->decode(_sharedTimeJumped);

    if (_sharedTimeJumped)
        _jockeHasToFixThisLater = true;

	_syncMutex.unlock();
}

void Time::postSynchronizationPreDraw() {
	_syncMutex.lock();

	_syncedTime = _sharedTime;
	_syncedDt = _sharedDt;
    _syncedTimeJumped = _sharedTimeJumped;

    if (_jockeHasToFixThisLater) {
        _syncedTimeJumped = true;
        _jockeHasToFixThisLater = false;
    }

	_syncMutex.unlock();	
}

void Time::preSynchronization() {
	_syncMutex.lock();

	_sharedTime = _time;
	_sharedDt = _dt;
	_sharedTimeJumped = _timeJumped;

	_syncMutex.unlock();
}

bool Time::timeJumped() const {
	return _syncedTimeJumped;
}

void Time::setTimeJumped(bool jumped) {
	_timeJumped = jumped;
}
    
bool Time::paused() const {
    return _timePaused;
}

scripting::ScriptEngine::LuaLibrary Time::luaLibrary() {
	scripting::ScriptEngine::LuaLibrary timeLibrary = {
		"time",
		{
			{
				"setDeltaTime",
				&luascriptfunctions::time_setDeltaTime,
				"number",
				"Sets the amount of simulation time that happens "
				"in one second of real time",
                true
			},
			{
				"deltaTime",
				&luascriptfunctions::time_deltaTime,
				"",
				"Returns the amount of simulated time that passes in one "
				"second of real time"
			},
            {
                "setPause",
                &luascriptfunctions::time_setPause,
                "bool",
                "Pauses the simulation time or restores the delta time",
                true
            },
            {
                "togglePause",
                &luascriptfunctions::time_togglePause,
                "",
                "Toggles the pause function, i.e. temporarily setting the delta time to 0"
                " and restoring it afterwards",
                true
            },
			{
				"setTime",
				&luascriptfunctions::time_setTime,
				"{number, string}",
				"Sets the current simulation time to the "
				"specified value. If the parameter is a number, the value is the number "
				"of seconds past the J2000 epoch. If it is a string, it has to be a "
				"valid ISO 8601 date string (YYYY-MM-DDTHH:MN:SS)",
                true
			},
			{
				"currentTime",
				&luascriptfunctions::time_currentTime,
				"",
				"Returns the current time as the number of seconds since "
				"the J2000 epoch"
			},
			{
				"currentTimeUTC",
				&luascriptfunctions::time_currentTimeUTC,
				"",
				"Returns the current time as an ISO 8601 date string "
				"(YYYY-MM-DDTHH:MN:SS)"
			}
		}
	};
	return timeLibrary;
}

} // namespace openspace
