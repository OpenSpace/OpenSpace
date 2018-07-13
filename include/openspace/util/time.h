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

#ifndef __OPENSPACE_CORE___TIME___H__
#define __OPENSPACE_CORE___TIME___H__

#include <string>

namespace openspace {

namespace scripting { struct LuaLibrary; }

/**
 * This singleton class represents the current simulation time in OpenSpace. It
 * internally stores the time and provides methods to set the time directly
 * (setTime(double), setTime(std::string)) using a <code>double</code> value using the
 * number of seconds passed since the J2000 epoch or a <code>string</code> that denotes
 * a valid date string in accordance to the Spice library
 * (http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/str2et_c.html). The time can
 * be retrieved as the number of seconds since the J2000 epoch with currentTime() or as a
 * UTC string following ISO 8601 with the method UTC().
 *
 * In addition to the time itself, it also stores a delta time value. This value denotes
 * the number of seconds that pass for each real-time second. This value is set with
 * setDeltaTime(double), retrieved with deltaTime() and solely used in the
 * advanceTime(double), which takes a <code>tickTime</code> parameter. The value of the
 * parameter is dependent on the usage of the class and must be equal to the real-world
 * time that has passed since the last call to the method. For example, if the
 * advanceTime(double) method is called each frame, the <code>tickTime</code> has to be
 * equal to the frame time.
 *
 * The synchronization of the simulation time requires
 */
class Time {
public:
    /**
     * Converts the \p timeString representing a date to a double precision
     * value representing the ephemeris time; that is the number of TDB
     * seconds past the J2000 epoch.
     * \param timeString A string representing the time to be converted
     * \return The converted time; the number of TDB seconds past the J2000 epoch,
     * representing the passed \p timeString
     * \pre \p timeString must not be empty
     */
    static double convertTime(const std::string& time);

    Time(double secondsJ2000 = -1);
    Time(const Time& other) = default;

    /**
     * Initializes the Time singleton.
     * \return <code>true</code> if the initialization succeeded, <code>false</code>
     * otherwise
     * \pre The Time singleton must not have been initialized
     */
    static void initialize();

    /**
     * Deinitializes the Time singleton. This method will not unload the kernel that was
     * possibly loaded during the initialize method.
     * \pre The Time singleton must have been initialized
     */
    static void deinitialize();

    static Time now();

    /**
     * Sets the current time to the specified value in seconds past the J2000 epoch. This
     * value can be negative to represent dates before the epoch.
     * \param value The number of seconds after the J2000 epoch
     */
    void setTime(double j2000Seconds);

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
    double j2000Seconds() const;

    /**
     * Returns the current time as a formatted date string compliant with ISO 8601 and
     * thus also compliant with the Spice library.
     * \return The current time as a formatted date string
     */
    std::string UTC() const;

    /**
    * Returns the current time as a ISO 8601 formatted, i.e YYYY-MM-DDThh:mm:ssZ
    * \return The current time as a ISO 8601 formatted string
    */
    std::string ISO8601() const;

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
     * Returns the Lua library that contains all Lua functions available to change the
     * current time, retrieve the current time etc.
     * \return The Lua library that contains all Lua functions available to change the
     * time
     */
    static scripting::LuaLibrary luaLibrary();

private:
    double _time;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___TIME___H__
