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

namespace {
	const std::string _loggerCat = "Time";
}

namespace openspace {

namespace luascriptfunctions {

/**
 * \ingroup LuaScripts
 * setDeltaTime(number):
 * Sets the delta time by calling the Time::setDeltaTime method
 */
int time_setDeltaTime(lua_State* L) {
	const bool isFunction = (lua_isfunction(L, -1) != 0);
	if (isFunction) {
		// If the top of the stack is a function, it is ourself
		const char* msg = lua_pushfstring(L, "method called without argument");
		return luaL_error(L, "bad argument (%s)", msg);
	}

	const bool isNumber = (lua_isnumber(L, -1) != 0);
	if (isNumber) {
		double value = lua_tonumber(L, -1);
		openspace::Time::ref().setDeltaTime(value);
		return 0;
	}
	else {
		const char* msg = lua_pushfstring(L, "%s expected, got %s",
								lua_typename(L, LUA_TNUMBER), luaL_typename(L, -1));
		return luaL_error(L, "bad argument #%d (%s)", 1, msg);
	}

}

/**
 * \ingroup LuaScripts
 * deltaTime():
 * Returns the delta time by calling the Time::deltaTime method
 */
int time_deltaTime(lua_State* L) {
	lua_pushnumber(L, openspace::Time::ref().deltaTime());
	return 1;
}

/**
 * \ingroup LuaScripts
 * togglePause():
 * Toggles a pause functionm i.e. setting the delta time to 0 and restoring it afterwards
 */
int time_togglePause(lua_State* L) {
    openspace::Time::ref().togglePause();
    return 0;
}

/**
 * \ingroup LuaScripts
 * togglePause():
 * Toggles a pause functionm i.e. setting the delta time to 0 and restoring it afterwards
 */
int time_setPause(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 1)
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

    bool pause = lua_toboolean(L, -1) == 1;
    openspace::Time::ref().setPause(pause);
    return 0;
}


/**
 * \ingroup LuaScripts
 * setTime({number, string}):
 * Sets the simulation time to the passed value. If the parameter is a number, it is
 * interpreted as the number of seconds past the J2000 epoch and the
 * Time::setTime(double) method is called. If the parameter is a string, it is
 * interpreted as a structured date string and the Time::setTime(std::string) method
 * is called
 */
int time_setTime(lua_State* L) {
	const bool isFunction = (lua_isfunction(L, -1) != 0);
	if (isFunction) {
		// If the top of the stack is a function, it is ourself
		const char* msg = lua_pushfstring(L, "method called without argument");
		return luaL_error(L, "bad argument (%s)", 1, msg);
	}

	const bool isNumber = (lua_isnumber(L, -1) != 0);
	const bool isString = (lua_isstring(L, -1) != 0);
	if (!isNumber && !isString) {
		const char* msg = lua_pushfstring(L, "%s or %s expected, got %s",
								lua_typename(L, LUA_TNUMBER),
								lua_typename(L, LUA_TSTRING), luaL_typename(L, -1));
		return luaL_error(L, "bad argument #%d (%s)", 1, msg);
	}
	if (isNumber) {
		double value = lua_tonumber(L, -1);
		openspace::Time::ref().setTime(value);
		return 0;
	}
	if (isString) {
		const char* time = lua_tostring(L, -1);
		openspace::Time::ref().setTime(time);
		return 0;
	}
	return 0;
}

/**
 * \ingroup LuaScripts
 * currentTime():
 * Returns the current simulation time as the number of seconds past the J2000 epoch.
 * It is returned by calling the Time::currentTime method.
 */
int time_currentTime(lua_State* L) {
	lua_pushnumber(L, openspace::Time::ref().currentTime());
	return 1;
}

/**
 * \ingroup LuaScripts
 * currentTimeUTC():
 * Returns the current simulation time as a structured ISO 8601 string using the UTC
 * timezone by calling the Time::currentTimeUTC method
 */
int time_currentTimeUTC(lua_State* L) {
	lua_pushstring(L, openspace::Time::ref().currentTimeUTC().c_str());
	return 1;
}

} // namespace luascriptfunctions


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

void Time::setTime(double value) {
	_time = std::move(value);
	_timeJumped = true;
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

void Time::setTime(std::string time) {
	SpiceManager::ref().getETfromDate(std::move(time), _time);
	_timeJumped = true;
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

scripting::ScriptEngine::LuaLibrary Time::luaLibrary() {
	scripting::ScriptEngine::LuaLibrary timeLibrary = {
		"time",
		{
			{
				"setDeltaTime",
				&luascriptfunctions::time_setDeltaTime,
				"number",
				"Sets the amount of simulation time that happens "
				"in one second of real time"
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
                "Pauses the simulation time or restores the delta time"
            },
            {
                "togglePause",
                &luascriptfunctions::time_togglePause,
                "",
                "Toggles the pause function, i.e. temporarily setting the delta time to 0"
                " and restoring it afterwards"
            },
			{
				"setTime",
				&luascriptfunctions::time_setTime,
				"{number, string}",
				"Sets the current simulation time to the "
				"specified value. If the parameter is a number, the value is the number "
				"of seconds past the J2000 epoch. If it is a string, it has to be a "
				"valid ISO 8601 date string (YYYY-MM-DDTHH:MN:SS)"
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
