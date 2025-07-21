/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/util/timeconstants.h>
#include <openspace/util/timeconversion.h>
#include <ghoul/lua/lua_helper.h>

namespace {

/**
 * Set the amount of simulation time that happens in one second of real time.
 *
 * \param deltaTime The value to set the speed to, in seconds per real time second
 */
[[codegen::luawrap]] void setDeltaTime(double deltaTime) {
    openspace::global::timeManager->setDeltaTime(deltaTime);
}

/**
 * Set the list of discrete delta time steps for the simulation speed that can be quickly
 * jumped between. The list will be sorted to be in increasing order. A negative verison
 * of each specified time step will be added per default as well.
 *
 * \param deltaTime The list of delta times, given in seconds per real time second.
 *                  Should only include positive values.
 */
[[codegen::luawrap]] void setDeltaTimeSteps(std::vector<double> deltaTime) {
    openspace::global::timeManager->setDeltaTimeSteps(deltaTime);
}

/**
 * Immediately set the simulation speed to the first delta time step in the list that is
 * larger than the current choice of simulation speed, if any.
 */
[[codegen::luawrap]] void setNextDeltaTimeStep() {
    openspace::global::timeManager->setNextDeltaTimeStep();
}

/**
 * Immediately set the simulation speed to the first delta time step in the list that is
 * smaller than the current choice of simulation speed, if any.
 */
[[codegen::luawrap]] void setPreviousDeltaTimeStep() {
    openspace::global::timeManager->setPreviousDeltaTimeStep();
}

/**
 * Interpolate the simulation speed to the first delta time step in the list that is
 * larger than the current simulation speed, if any.
 *
 * \param interpolationDuration The number of seconds that the interpolation should
 *                              be done over. If excluded, the time is decided based
 *                              on the default value specified in the TimeManager.
 */
[[codegen::luawrap]] void interpolateNextDeltaTimeStep(
                                              std::optional<double> interpolationDuration)
{
    using namespace openspace;

    double interp = interpolationDuration.value_or(
        global::timeManager->defaultDeltaTimeInterpolationDuration()
    );
    global::timeManager->interpolateNextDeltaTimeStep(interp);
}

/**
 * Interpolate the simulation speed to the first delta time step in the list that is
 * smaller than the current simulation speed, if any.
 *
 * \param interpolationDuration The number of seconds that the interpolation should
 *                              be done over. If excluded, the time is decided based
 *                              on the default value specified in the TimeManager.
 */
[[codegen::luawrap]] void interpolatePreviousDeltaTimeStep(
                                              std::optional<double> interpolationDuration)
{
    using namespace openspace;

    double interp = interpolationDuration.value_or(
        global::timeManager->defaultDeltaTimeInterpolationDuration()
    );
    global::timeManager->interpolatePreviousDeltaTimeStep(interp);
}

/**
 * Set the amount of simulation time that happens in one second of real time, by smoothly
 * interpolating to that value.
 *
 * \param deltaTime The value to set the speed to, in seconds per real time second
 * \param interpolationDuration The number of seconds that the interpolation should
 *                              be done over. If excluded, the time is decided based
 *                              on the default value for delta time interpolation
 *                              specified in the TimeManager.
 */
[[codegen::luawrap]] void interpolateDeltaTime(double deltaTime,
                                              std::optional<double> interpolationDuration)
{
    using namespace openspace;

    double interp = interpolationDuration.value_or(
        global::timeManager->defaultDeltaTimeInterpolationDuration()
    );

    global::timeManager->interpolateDeltaTime(deltaTime, interp);
}

/**
 * Returns the amount of simulated time that passes in one second of real time.
 *
 * \return The simulated delta time, in seconds per real time second
 */
[[codegen::luawrap]] double deltaTime() {
    return openspace::global::timeManager->deltaTime();
}

/**
 * Toggle the pause function, i.e. if the simulation is paused it will resume, and
 * otherwise it will be paused. Note that to pause means temporarily setting the delta
 * time to 0, and unpausing means restoring it to whatever delta time value is set.
 */
[[codegen::luawrap]] void togglePause() {
    using namespace openspace;
    global::timeManager->setPause(!global::timeManager->isPaused());
}

/**
 * Toggle the pause function, i.e. if the simulation is paused it will resume, and
 * otherwise it will be paused. This is done by smoothly interpolating from the
 * current delta time value to 0 (pause), or from 0 to the current delta time value
 * (unpause).
 *
 * \param interpolationDuration The number of seconds that the interpolation should be
 *                              done over. If excluded, the time is decided based on the
 *                              default value for pause/unpause specified in the
 *                              TimeManager.
 */
[[codegen::luawrap]] void interpolateTogglePause(
                                              std::optional<double> interpolationDuration)
{
    using namespace openspace;

    double interp = interpolationDuration.value_or(
        global::timeManager->isPaused() ?
        global::timeManager->defaultUnpauseInterpolationDuration() :
        global::timeManager->defaultPauseInterpolationDuration()
    );

    global::timeManager->interpolatePause(!global::timeManager->isPaused(), interp);
}

/**
 * This allows for a keypress (via keybinding) to have dual functionality. In normal
 * operational mode it will behave just like time_interpolateTogglePause, but during
 * playback of a session recording it will pause the playback without manipulating the
 * delta time.
 */
[[codegen::luawrap]] void pauseToggleViaKeyboard() {
    using namespace openspace;

    OpenSpaceEngine::Mode m = global::openSpaceEngine->currentMode();
    if (m == OpenSpaceEngine::Mode::SessionRecordingPlayback) {
        bool isPlaybackPaused = global::sessionRecordingHandler->isPlaybackPaused();
        global::sessionRecordingHandler->setPlaybackPause(!isPlaybackPaused);
    }
    else {
        const bool isPaused = !global::timeManager->isPaused();
        global::timeManager->interpolatePause(
            isPaused,
            isPaused ?
                global::timeManager->defaultPauseInterpolationDuration() :
                global::timeManager->defaultUnpauseInterpolationDuration()
        );
    }
}

/**
 * Set whether the simulation should be paused or not. Note that to pause means
 * temporarily setting the delta time to 0, and unpausing means restoring it to whatever
 * delta time value is set.
 *
 * \param isPaused True if the simulation should be paused, and false otherwise
 */
[[codegen::luawrap]] void setPause(bool isPaused) {
    openspace::global::timeManager->setPause(isPaused);
}

/**
 * Same behaviour as `setPause`, but with interpolation. That is, if it should be paused,
 * the delta time will be interpolated to 0, and if unpausing, the delta time will be
 * interpolated to whatever delta time value is set.
 *
 * \param isPaused True if the simulation should be paused, and false otherwise
 * \param interpolationDuration The number of seconds that the interpolation should be
 *                              done over. If excluded, the time is decided based on the
 *                              default value for pause/unpause specified in the
 *                              TimeManager.
 */
[[codegen::luawrap]] void interpolatePause(bool isPaused,
                                           std::optional<double> interpolationDuration)
{
    using namespace openspace;

    double interp = interpolationDuration.value_or(
        isPaused ?
            global::timeManager->defaultPauseInterpolationDuration() :
            global::timeManager->defaultUnpauseInterpolationDuration()
    );

    global::timeManager->interpolatePause(isPaused, interp);
}

/**
 * Returns whether the simulation time is currently paused or is progressing.
 *
 * \return True if the simulation is paused, and false otherwise
 */
[[codegen::luawrap]] bool isPaused() {
    return openspace::global::timeManager->isPaused();
}

/**
 * Set the current simulation time to the specified value. The time can be specified
 * either a number of seconds past the J2000 epoch, or as a ISO 8601 string.
 *
 * Note that providing time zone using the Z format is not supported. UTC is assumed.
 *
 * \param time The time to set. If the parameter is a number, the value is the number of
 *             seconds past the J2000 epoch. If it is a string, it has to be a valid ISO
 *             8601-like date string of the format YYYY-MM-DDTHH:MN:SS.
 */
[[codegen::luawrap]] void setTime(std::variant<double, std::string> time) {
    using namespace openspace;

    double t;
    if (std::holds_alternative<std::string>(time)) {
        t = Time::convertTime(std::get<std::string>(time));
    }
    else {
        t = std::get<double>(time);
    }

    global::timeManager->setTimeNextFrame(Time(t));
}

 /**
  * Set the current simulation time to the specified value, using interpolation. The time
  * can be specified either a number of seconds past the J2000 epoch, or as a ISO 8601
  * string.
  *
  * Note that providing time zone using the Z format is not supported. UTC is assumed.
  *
  * \param time The time to set. If the parameter is a number, the value is the number of
  *             seconds past the J2000 epoch. If it is a string, it has to be a valid ISO
  *             8601-like date string of the format YYYY-MM-DDTHH:MN:SS.
  * \param interpolationDuration The number of seconds that the interpolation should
  *                              be done over. If excluded, the time is decided based
  *                              on the default value for time interpolation specified in
  *                              the TimeManager.
  */
[[codegen::luawrap]] void interpolateTime(std::variant<std::string, double> time,
                                          std::optional<double> interpolationDutation)
{
    using namespace openspace;

    double targetTime =
        std::holds_alternative<std::string>(time) ?
        Time::convertTime(std::get<std::string>(time)) :
        std::get<double>(time);


    double interp = interpolationDutation.value_or(
        global::timeManager->defaultTimeInterpolationDuration()
    );
    if (interp > 0) {
        global::timeManager->interpolateTime(targetTime, interp);
    }
    else {
        global::timeManager->setTimeNextFrame(Time(targetTime));
    }
}

 /**
  * Increment the current simulation time by the specified number of seconds, using
  * interpolation.
  *
  * \param delta The number of seconds to increase the current simulation time by
  * \param interpolationDuration The number of seconds that the interpolation should
  *                              be done over. If excluded, the time is decided based
  *                              on the default value for time interpolation specified in
  *                              the TimeManager.
  */
[[codegen::luawrap]] void interpolateTimeRelative(double delta,
                                              std::optional<double> interpolationDuration)
{
    using namespace openspace;

    double interp = interpolationDuration.value_or(
        global::timeManager->defaultTimeInterpolationDuration()
    );

    if (interp > 0) {
        global::timeManager->interpolateTimeRelative(delta, interp);
    }
    else {
        global::timeManager->setTimeNextFrame(
            Time(global::timeManager->time().j2000Seconds() + delta)
        );
    }
}

/**
 * Returns the current time as the number of seconds since the J2000 epoch.
 *
 * \return The current time, as the number of seconds since the J2000 epoch
 */
[[codegen::luawrap]] double currentTime() {
    return openspace::global::timeManager->time().j2000Seconds();
}

/**
 * Returns the current time as an ISO 8601 date string (YYYY-MM-DDTHH:MN:SS).
 *
 * \return The current time, as an ISO 8601 date string
 */
[[codegen::luawrap("UTC")]] std::string currentTimeUTC() {
    return std::string(openspace::global::timeManager->time().ISO8601());
}


/**
 * Returns the current time as an date string of the form
 * (YYYY MON DDTHR:MN:SC.### ::RND) as returned by SPICE.
 *
 * \return The current time, in the format used by SPICE (YYYY MON DDTHR:MN:SC.### ::RND)
 */
[[codegen::luawrap("SPICE")]] std::string currentTimeSpice() {
    return std::string(openspace::global::timeManager->time().UTC());
}

/**
 * Returns the current wall time as an ISO 8601 date string (YYYY-MM-DDTHH-MN-SS) in the
 * UTC timezone.
 *
 * \return The current wall time, in the UTC time zone, as an ISO 8601 date string
 */
[[codegen::luawrap]] std::string currentWallTime() {
    return openspace::Time::currentWallTime();
}

/**
 * Returns the current application time as the number of seconds since the OpenSpace
 * application started.
 *
 * \return The number of seconds since OpenSpace started
 */
[[codegen::luawrap]] double currentApplicationTime() {
    return openspace::global::windowDelegate->applicationTime();
}

/**
 * Modify a specified timestamp by a given delta time. That is, advance the value either
 * forwards or backwards in time.
 *
 * The returned value will be of the same type as the first argument. That is, either a
 * number of seconds past the J2000 epoch, or an ISO 8601 date string.
 *
 * \param base The timestamp to alter, either given as an ISO 8601 date string or a
 *             number of seconds past the J2000 epoch.
 * \param change The amount of time to add to the specified timestamp. Can be given
 *               either in a number of seconds (including negative), or as a string of
 *               the form [-]XX(s,m,h,d,M,y] with (s)econds, (m)inutes, (h)ours, (d)ays,
 *               (M)onths, and (y)ears as units and an optional - sign to move backwards
 *               in time.
 *
 * \return The updated timestamp
 */
[[codegen::luawrap]] std::variant<std::string, double> advancedTime(
                                                   std::variant<std::string, double> base,
                                                 std::variant<std::string, double> change)
{
    std::string b;
    if (std::holds_alternative<std::string>(base)) {
        b = std::get<std::string>(base);
    }
    else {
        b = openspace::Time(std::get<double>(base)).ISO8601();
    }

    std::string c;
    if (std::holds_alternative<std::string>(change)) {
        c = std::get<std::string>(change);
    }
    else {
        double v = std::get<double>(change);
        c = std::format("{}s", v);
    }

    std::string res = openspace::Time::advancedTime(std::move(b), std::move(c));

    if (std::holds_alternative<std::string>(base)) {
        return res;
    }
    else {
        return openspace::Time::convertTime(res);
    }
}

/**
 * Convert the given time from either a J2000 seconds number to an ISO 8601 timestamp,
 * or vice versa.
 *
 * If the given time is a timestamp, the function returns a double precision value
 * representing the ephemeris version of that time; that is, the number of TDB seconds
 * past the J2000 epoch.
 *
 * If the given time is a J2000 seconds value, the function returns a ISO 8601 timestamp.
 *
 * \param time The timestamp to convert, either given as an ISO 8601 date string or a
 *             number of seconds past the J2000 epoch.
 *
 * \return The converted timestamp
 */
[[codegen::luawrap]] std::variant<std::string, double> convertTime(
                                                   std::variant<std::string, double> time)
{
    using namespace openspace;

    // Convert from timestamp to J2000 seconds
    if (std::holds_alternative<std::string>(time)) {
        return Time::convertTime(std::get<std::string>(time));
    }
    // Convert from J2000 seconds to ISO 8601 timestamp
    else {
        return std::string(Time(std::get<double>(time)).ISO8601());
    }
}

/**
 * Returns the number of seconds between the provided start time and end time.
 *
 * If the end time is before the start time, the return value is negative. If the start
 * time is equal to the end time, the return value is 0.
 *
 * \param start The start time for the computation, given as an ISO 8601 date string
 * \param end The end time for the computation, given as an ISO 8601 date string
 *
 * \return The time between the start time and end time
 */
[[codegen::luawrap]] double duration(std::string start, std::string end) {
    using namespace openspace;

    const double tStart = Time::convertTime(start);
    const double tEnd = Time::convertTime(end);
    return tEnd - tStart;
}

/**
 * Returns the number of seconds per day, where a day in this case is exactly 24 hours.
 * The total number of seconds is equal to 86400.
 *
 * \return The number of seconds in a day
 */
[[codegen::luawrap]] double secondsPerDay() {
    return openspace::timeconstants::SecondsPerDay;
}

/**
 * Returns the number of seconds in a Gregorian year, which is equal to 31556952.
 *
 * \return The number of seconds in a Gregorian year
 */
[[codegen::luawrap]] double secondsPerYear() {
    // We could use a call to SPICE here, but the value is a constant anyway
    return openspace::timeconstants::SecondsPerYear;
}

#include "time_lua_codegen.cpp"

} // namespace
