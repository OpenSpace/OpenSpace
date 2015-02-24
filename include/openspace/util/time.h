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

#ifndef __TIME_H__
#define __TIME_H__

#include <openspace/scripting/scriptengine.h>
#include <string>
#include <mutex>

namespace openspace {

/**
 * This singleton class represents the current simulation time in OpenSpace. It
 * internally stores the time and provides methods to set the time directly
 * (setTime(double), setTime(std::string)) using a <code>double</code> value using the 
 * number of seconds passed since the J2000 epoch or a <code>string</code> that denotes
 * a valid date string in accordance to the Spice library
 * (http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/str2et_c.html). The time can
 * be retrieved as the number of seconds since the J2000 epoch with currentTime() or as a
 * UTC string following ISO 8601 with the method currentTimeUTC().
 *
 * In addition to the time itself, it also stores a delta time value. This value denotes
 * the number of seconds that pass for each real-time second. This value is set with
 * setDeltaTime(double), retrieved with deltaTime() and solely used in the
 * advanceTime(double), which takes a <code>tickTime</code> parameter. The value of the
 * parameter is dependent on the usage of the class and must be equal to the real-world 
 * time that has passed since the last call to the method. For example, if the
 * advanceTime(double) method is called each frame, the <code>tickTime</code> has to be
 * equal to the frame time.
 */

	class SyncBuffer;

class Time {
public:
	/**
	 * Initializes the Time singleton.
	 * \return <code>true</code> if the initialization succeeded, <code>false</code>
	 * otherwise
	 */
	static bool initialize();

	/**
	 * Deinitializes the Time singleton. This method will not unload the kernel that was
	 * possibly loaded during the initialize method.
	 */
	static void deinitialize();

	/**
	 * Returns the reference to the Time singleton object.
	 * \return The reference to the Time singleton object
	 */
    static Time& ref();

	/**
	 * Returns <code>true</code> if the singleton has been successfully initialized,
	 * <code>false</code> otherwise
	 * \return <code>true</code> if the singleton has been successfully initialized,
	 * <code>false</code> otherwise
	 */
	static bool isInitialized();
	
	/**
	 * Sets the current time to the specified value in seconds past the J2000 epoch. This
	 * value can be negative to represent dates before the epoch.
	 * \param value The number of seconds after the J2000 epoch
	 */
	void setTime(double value);

	/**
	 * Sets the current time to the specified value given as a Spice compliant string as
	 * described in the Spice documentation
	 * (http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/str2et_c.html)
	 * \param time The time to be set as a date string
	 */
	void setTime(std::string time);

	/**
	 * Returns the current time as the number of seconds past the J2000 epoch. If the
	 * current time is a date before that epoch, the returned value will be negative.
	 * \return The current time as the number of seconds past the J2000 epoch
	 */
	double currentTime() const;

	/**
	 * Returns the current time as a formatted date string compliant with ISO 8601 and
	 * thus also compliant with the Spice library.
	 * \return The current time as a formatted date string
	 */
	std::string currentTimeUTC() const;

	/**
	 * Sets the delta time value that is the number of seconds that should pass for each
	 * real-time second. This value is used in the advanceTime(double) method to easily
	 * advance the simulation time.
	 * \param deltaT The number of seconds that should pass for each real-time second
	 */
	void setDeltaTime(double deltaT);

	/**
	 * Returns the delta time, that is the number of seconds that pass in the simulation
	 * for each real-time second
	 * \return The number of seconds that pass for each real-time second
	 */
	double deltaTime() const;

	/**
	 * Advances the simulation time using the deltaTime() and the <code>tickTime</code>.
	 * The deltaTime() is the number of simulation seconds that pass for each real-time
	 * second. <code>tickTime</code> is the number of real-time seconds that passed since
	 * the last call to this method. If this method is called in the render loop, the
	 * <code>tickTime</code> should be equivalent to the frame time.
	 * \param tickTime The number of real-time seconds that passed since the last call
	 * to this method
	 * \return The new time value after advancing the time
	 */
	double advanceTime(double tickTime);

	/**
	* Retreats the simulation time using the deltaTime() and the <code>tickTime</code>.
	* The deltaTime() is the number of simulation seconds that pass for each real-time
	* second. <code>tickTime</code> is the number of real-time seconds that passed since
	* the last call to this method. If this method is called in the render loop, the
	* <code>tickTime</code> should be equivalent to the frame time.
	* \param tickTime The number of real-time seconds that passed since the last call
	* to this method
	* \return The new time value after retreating the time
	*/
	double retreatTime(double tickTime);

	void serialize(SyncBuffer* syncBuffer);

	void deserialize(SyncBuffer* syncBuffer);

	void postSynchronizationPreDraw();

	void preSynchronization();

	bool timeJumped() const;

	void setTimeJumped(bool jumped);

	/**
	 * Returns the Lua library that contains all Lua functions available to change the
	 * current time, retrieve the current time etc. The functions contained are
	 * - openspace::luascriptfunctions::time_setDeltaTime
	 * - openspace::luascriptfunctions::time_deltaTime
	 * - openspace::luascriptfunctions::time_setTime
	 * - openspace::luascriptfunctions::time_currentTime
	 * - openspace::luascriptfunctions::time_currentTimeUTC
	 * \return The Lua library that contains all Lua functions available to change the
	 * Time singleton
	 */
	static scripting::ScriptEngine::LuaLibrary luaLibrary();

private:
	/// Creates the time object. Only used in the initialize() method
	Time();
	Time(const Time& src) = delete;
    Time& operator=(const Time& rhs) = delete;

	static Time* _instance; ///< The singleton instance
	

	double _deltaTimePerSecond; ///< The delta time that is used to advance the time
	
	//sync variables
	
	//local copies 
	double _time; ///< The time stored as the number of seconds past the J2000 epoch
	double _dt;
	bool _timeJumped;
    bool _jockeHasToFixThisLater;

	//shared copies
	double _sharedTime;
	double _sharedDt;
	bool _sharedTimeJumped;

	//synced copies
	double _syncedTime;
	double _syncedDt;
	bool _syncedTimeJumped;
	
	
	std::mutex _syncMutex;
};

} // namespace openspace

#endif // __TIME_H__
