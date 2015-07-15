/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/util/constants.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/syncbuffer.h>

#include <ghoul/filesystem/filesystem.h>

#include <cassert>
#include <string>

#include "time_lua.inl"

namespace {
	const std::string _loggerCat = "Time";
}

namespace openspace {

Time* Time::_instance = nullptr;

Time::Time()
    : _time(-1.0)
	, _dt(1.0)
	, _timeJumped(false)
    , _timePaused(false)
    , _sharedTime(-1.0)
    , _sharedDt(1.0)
    , _sharedTimeJumped(false)
	, _syncedTime(-1.0)
	, _syncedDt(1.0)
	, _syncedTimeJumped(false)
{
}

bool Time::initialize() {
	assert( _instance == nullptr);
	 _instance = new Time();
	 return true;
}

void Time::deinitialize() {
	assert(_instance);
	delete _instance;
	_instance = nullptr;
}

Time& Time::ref() {
	assert(_instance);
    return *_instance;
}

bool Time::isInitialized() {
	return (_instance != nullptr);
}

void Time::setTime(double value, bool requireJump) {
	_time = std::move(value);
	_timeJumped = requireJump;
}

double Time::currentTime() const {
	assert(_instance);
	//return _time;
	return _syncedTime;
}

double Time::advanceTime(double tickTime) {
    if (_timePaused)
        return _time;
    else
	    return _time += _dt * tickTime;
}

void Time::setDeltaTime(double deltaT) {
	_dt = std::move(deltaT);
}

double Time::deltaTime() const {
	//return _dt;
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
	SpiceManager::ref().getETfromDate(std::move(time), _time);
	_timeJumped = requireJump;
}

std::string Time::currentTimeUTC() const {
	std::string date;
	//SpiceManager::ref().getDateFromET(_time, date);
	SpiceManager::ref().getDateFromET(_syncedTime, date);
	return date;
}

void Time::serialize(SyncBuffer* syncBuffer){
	_syncMutex.lock();

	syncBuffer->encode(_sharedTime);
	syncBuffer->encode(_sharedDt);
	syncBuffer->encode(_sharedTimeJumped);

	_syncMutex.unlock();
}

void Time::deserialize(SyncBuffer* syncBuffer){
	_syncMutex.lock();

	syncBuffer->decode(_sharedTime);
	syncBuffer->decode(_sharedDt);
	syncBuffer->decode(_sharedTimeJumped);

    if (_sharedTimeJumped)
        _jockeHasToFixThisLater = true;

	_syncMutex.unlock();
}

void Time::postSynchronizationPreDraw(){
	_syncMutex.lock();

	_syncedTime = _sharedTime;
	_syncedDt = _sharedDt;
    //if (_sharedTimeJumped)
	    _syncedTimeJumped = _sharedTimeJumped;

    if (_jockeHasToFixThisLater) {
        _syncedTimeJumped = true;
        _jockeHasToFixThisLater = false;
    }

	_syncMutex.unlock();	
}

void Time::preSynchronization(){
	_syncMutex.lock();

	_sharedTime = _time;
	_sharedDt = _dt;
	_sharedTimeJumped = _timeJumped;

	_syncMutex.unlock();
}

bool Time::timeJumped() const {
	//return _timeJumped;
	return _syncedTimeJumped;
}

void Time::setTimeJumped(bool jumped){
	_timeJumped = jumped;
}
    
bool Time::paused() const{
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
