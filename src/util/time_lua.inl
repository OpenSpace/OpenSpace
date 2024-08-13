/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/util/timeconversion.h>

namespace {

/**
 * Sets the amount of simulation time that happens in one second of real time.
 */
[[codegen::luawrap]] void setDeltaTime(double deltaTime) {
    openspace::global::timeManager->setDeltaTime(deltaTime);
}

/**
 * Sets the list of discrete delta time steps for the simulation speed that can be quickly
 * jumped between. The list will be sorted to be in increasing order. A negative verison
 * of each specified time step will be added per default as well.
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
 * smaller than the current choice of simulation speed if any.
 */
[[codegen::luawrap]] void setPreviousDeltaTimeStep() {
    openspace::global::timeManager->setPreviousDeltaTimeStep();
}

/**
 * Interpolate the simulation speed to the first delta time step in the list that is
 * larger than the current simulation speed, if any. If an input value is given, the
 * interpolation is done over the specified number of seconds.
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
 * smaller than the current simulation speed, if any. If an input value is given, the
 * interpolation is done over the specified number of seconds.
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
 * Sets the amount of simulation time that happens in one second of real time. If a second
 * input value is given, the interpolation is done over the specified number of seconds.
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
 */
[[codegen::luawrap]] double deltaTime() {
    return openspace::global::timeManager->deltaTime();
}

/**
 * Toggles the pause function, i.e. temporarily setting the delta time to 0 and restoring
 * it afterwards.
 */
[[codegen::luawrap]] void togglePause() {
    using namespace openspace;
    global::timeManager->setPause(!global::timeManager->isPaused());
}

/**
 * Toggles the pause function, i.e. temporarily setting the delta time to 0 and restoring
 * it afterwards. If an input value is given, the interpolation is done over the specified
 * number of seconds.
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
        bool isPlaybackPaused = global::sessionRecording->isPlaybackPaused();
        global::sessionRecording->setPlaybackPause(!isPlaybackPaused);
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
 * Toggles a pause function i.e. setting the delta time to 0 and restoring it afterwards.
 */
[[codegen::luawrap]] void setPause(bool isPaused) {
    openspace::global::timeManager->setPause(isPaused);
}

/**
 * Same behaviour as setPause, but with interpolation. If no interpolation duration is
 * provided, the interpolation time will be based on the
 * `defaultPauseInterpolationDuration` and `defaultUnpauseInterpolationDuration`
 * properties of the TimeManager.
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
 */
[[codegen::luawrap]] bool isPaused() {
    return openspace::global::timeManager->isPaused();
}

/**
 * Sets the current simulation time to the specified value. If the parameter is a number,
 * the value is the number of seconds past the J2000 epoch. If it is a string, it has to
 * be a valid ISO 8601-like date string of the format YYYY-MM-DDTHH:MN:SS. Note: providing
 * time zone using the Z format is not supported. UTC is assumed.
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
 * Sets the current simulation time to the specified value. If the first parameter is a
 * number, the target is the number of seconds past the J2000 epoch. If it is a string, it
 * has to be a valid ISO 8601-like date string of the format YYYY-MM-DDTHH:MN:SS (Note:
 * providing time zone using the Z format is not supported. UTC is assumed). If a second
 * input value is given, the interpolation is done over the specified number of seconds.
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
 * Increments the current simulation time by the specified number of seconds. If a second
 * input value is given, the interpolation is done over the specified number of seconds.
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
 */
[[codegen::luawrap]] double currentTime() {
    return openspace::global::timeManager->time().j2000Seconds();
}

/**
 * Returns the current time as an ISO 8601 date string (YYYY-MM-DDTHH:MN:SS).
 */
[[codegen::luawrap("UTC")]] std::string currentTimeUTC() {
    return std::string(openspace::global::timeManager->time().ISO8601());
}


/**
 * Returns the current time as an date string of the form
 * (YYYY MON DDTHR:MN:SC.### ::RND) as returned by SPICE.
 */
[[codegen::luawrap("SPICE")]] std::string currentTimeSpice() {
    return std::string(openspace::global::timeManager->time().UTC());
}

/**
 * Returns the current wall time as an ISO 8601 date string (YYYY-MM-DDTHH-MN-SS) in the
 * UTC timezone.
 */
[[codegen::luawrap]] std::string currentWallTime() {
    return openspace::Time::currentWallTime();
}

/**
 * Returns the current application time as the number of seconds since the OpenSpace
 * application started.
 */
[[codegen::luawrap]] double currentApplicationTime() {
    return openspace::global::windowDelegate->applicationTime();
}

/**
 * Modifies the passed time (first argument) by the delta time (second argument). The
 * first argument can either be an ISO 8601 date string or the number of seconds past the
 * J2000 epoch. The second argument can either be a string of the form [-]XX(s,m,h,d,M,y]
 * with (s)econds, (m)inutes, (h)ours, (d)ays, (M)onths, and (y)ears as units and an
 * optional - sign to move backwards in time. If the second argument is a number, it is
 * interpreted as a number of seconds. The return value is of the same type as the first
 * argument.
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
 * Converts the given time to either a J2000 seconds number or a ISO 8601 timestamp,
 * depending on the type of the given time.
 *
 * If the given time is a timestamp: the function returns a double precision value
 * representing the ephemeris version of that time; that is the number of TDB seconds past
 * the J2000 epoch.
 *
 * If the given time is a J2000 seconds value: the function returns a ISO 8601 timestamp.
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
 * Returns the number of seconds between the provided start time and end time. If the end
 * time is before the start time, the return value is negative. If the start time is equal
 * to the end time, the return value is 0.
 * Both start and end times must be valid ISO 8601 date strings.
 */
[[codegen::luawrap]] double duration(std::string start, std::string end) {
    using namespace openspace;

    const double tStart = Time::convertTime(start);
    const double tEnd = Time::convertTime(end);
    return tEnd - tStart;
}

/**
 * Returns the number of seconds per day where a day in this case is exactly 24 hours.
 * The total number of seconds is equal to 86400.
 */
[[codegen::luawrap]] double secondsPerDay() {
    return openspace::SecondsPerDay;
}

/**
 * Returns the number of seconds in a Julian year, which is equal to 31557600.
 */
[[codegen::luawrap]] double secondsPerYear() {
    // We could use a call to SPICE here, but the value is a constant anyway
    return openspace::SecondsPerYear;
}

#include "time_lua_codegen.cpp"

} // namespace
