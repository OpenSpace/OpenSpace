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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TIMEQUANTIZER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TIMEQUANTIZER___H__

#include <openspace/util/timerange.h>
#include <string>
#include <vector>

namespace openspace { class Time; }

namespace openspace::globebrowsing {

/**
 * RangedTime class is used to define an acceptable time range. Functionality includes
 * checking if a given date/time is within that range, or clamping a date to enforce this
 * range.
 */
class RangedTime {
public:
    RangedTime() = default;

    /**
     * Constructor that accepts an ISO8601 date/time string (`YYYY-MM-DDTHH:mm:ss`) for an
     * allowable time range defined by start and end values.
     *
     * \param start The date/time start of the time range
     * \param end The date/time ending of the time range
     */
    RangedTime(std::string start, std::string end);

    /**
     * Checks if a date/time value falls within the start/end range defined in this
     * instance of the class.
     *
     * \param checkTime The time to test if it falls within the range
     * \return `true` if the input date/time falls between the start and end date/times
     */
    bool includes(const Time& checkTime) const;

    /**
     * Enforces the start/end range on a given date/time string by clamping the value.
     *
     * \param checkTime An ISO8601 date/time string to clamp if falls outside of range
     * \return Clamped value of input parameter, will be equal to the start value if less
     *         than start, equal to end if greater than end, or equal to input parameter
     *         if falls in-between
     */
    const char* clamp(const char* checkTime);

    /**
     * Get the start date/time of the time range
     *
     * \return The ISO8601 date/time string that defines the start of the range
     */
    std::string_view start() const;

    /**
     * Get the end date/time of the time range.
     *
     * \return The ISO8601 date/time string that defines the end of the range
     */
    std::string_view end() const;

    /**
     * Set the start date/time of the time range.
     *
     * \param start The ISO8601 date/time string that defines the start of the range
     */
    void setStart(const std::string start);

    /**
     * Set the end date/time of the time range.
     *
     * \param end The ISO8601 date/time string that defines the end of the range
     */
    void setEnd(const std::string end);

private:
    std::string _start;
    std::string _end;
    double _startJ2000;
    double _endJ2000;
};

/**
 * DateTime class is used to manage a date/time value and provide methods for increment/
 * decrementing the value, which gets complicated because of the varying days of the
 * different months, leap years, etc.
 *
 * This class exists to handle date/time values within a "people-friendly" calendar
 * schedule. For example, a temporal data set that's updated on the 10th of every month
 * will sometimes be updated in 28 days, other times in 31 days. The intent of the class
 * is to handle all of the "special cases" where simply using J2000 seconds won't work.
 */
class DateTime {
public:
    DateTime() = default;

    /**
     * Constructor that initializes with date/time string.
     *
     * \param initDateTime The ISO8601 date/time string (`YYYY-MM-DDTHH:mm:ss`)
     */
    explicit DateTime(std::string_view initDateTime);

    /**
     * Set the date/time value.
     *
     * \param input The ISO8601 date/time string (`YYYY-MM-DDTHH:mm:ss`) to set
     */
    void setTime(std::string_view input);

    /**
     * Get the date/time value in ISO8601 format.
     *
     * \return The date/time value string
     */
    std::string ISO8601() const;

    /**
     * Get the J2000 seconds equivalent of the object's date/time, using the loaded
     * SPICE kernel.
     *
     * \return J2000 seconds of date/time
     */
    double J2000() const;

    /**
     * Get the year of the object's date/time (YYYY format).
     *
     * \return Integer value of the year
     */
    int year() const;

    /**
     * Get the month of the object's date/time (1 - 12).
     *
     * \return Integer value of the month
     */
    int month() const;

    /**
     * Get the day-of-month of the object's date/time (1 - 31).
     *
     * \return Integer value of the day
     */
    int day() const;

    /**
     * Get the hour of the object's date/time.
     *
     * \return Integer value of the hour (0 - 23)
     */
    int hour() const;

    /**
     * Get the minute of the object's date/time.
     *
     * \return Integer value of the minutes
     */
    int minute() const;

    /**
     * Get the seconds of the object's date/time.
     *
     * \return Integer value of the seconds
     */
    int second() const;

    /**
     * Set the year of the object's date/time.
     *
     * \param y Integer value of the year
     */
    void setYear(int y);

    /**
     * Set the month of the object's date/time (1 - 12).
     *
     * \param m Integer value of the year
     */
    void setMonth(int m);

    /**
     * Set the day-of-month of the object's date/time (1 - 31).
     *
     * \param d Integer value of the day
     */
    void setDay(int d);

    /**
     * Set the hour of the object's date/time (0 - 23).
     *
     * \param h Integer value of the hour
     */
    void setHour(int h);

    /**
     * Set the minute of the object's date/time.
     *
     * \param m Integer value of the minute
     */
    void setMinute(int m);

    /**
     * Set the seconds of the object's date/time.
     *
     * \param s Integer value of the seconds
     */
    void setSecond(int s);

    /**
     * Increment operation for the date/time.
     *
     * \param value integer value for number of units in an operation
     * \param unit single char that specifies the unit of increment. Allowable units are:
     *        (y)ear, (M)onth, (d)ay, (h)our, (m)inute, (s)econd
     * \param error The difference in J2000 seconds from current date/time to target (a
     *        positive value means target is in the future)
     * \param resolution The J2000 seconds of the interval defined by the value & unit
     * \return The number of increments that were performed in order to get as close as
     *         possible to the target, where each increment is defined by the value & unit
     *         (and approximated but not fixed by the resolution param)
     */
    int increment(int value, char unit, double error, double resolution);

    /**
     * Decrement operation for the date/time.
     *
     * \param value integer value for number of units in an operation
     * \param unit single char that specifies the unit of decrement. Allowable units are:
     *        (y)ear, (M)onth, (d)ay, (h)our, (m)inute, (s)econd
     * \param error The difference in J2000 seconds from current date/time to target (a
     *        positive value means target is in the future)
     * \param resolution The J2000 seconds of the interval defined by the value & unit
     * \return The number of decrements that were performed in order to get as close as
     *         possible to the target, where each decrement is defined by the value &
     *         unit (and approximated but not fixed by the resolution param)
     */
    int decrement(int value, char unit, double error, double resolution);

    /**
     * Single increment operation for the date/time.
     *
     * \param value integer value for number of units in an operation
     * \param unit single char that specifies the unit of increment. Allowable units are:
     *        (y)ear, (M)onth, (d)ay, (h)our, (m)inute, (s)econd
     */
    void incrementOnce(int value, char unit);

    /**
     * Single decrement operation for the date/time.
     *
     * \param value integer value for number of units in an operation
     * \param unit single char that specifies the unit of decrement. Allowable units are:
     *        (y)ear, (M)onth, (d)ay, (h)our, (m)inute, (s)econd
     */
    void decrementOnce(int value, char unit);

private:
    int _year = 2000;
    int _month = 1;
    int _day = 1;
    int _hour = 0;
    int _minute = 0;
    int _second = 0;
};

/**
 * Used to quantize time to discrete values.
 */
class TimeQuantizer {
public:
    TimeQuantizer() = default;

    /**
     * Constructor that initializes with formatted strings for start & ends date/times,
     * and a time resolution within that range.
     *
     * \param start the ISO8601 date/time string (YYYY-MM-DDTHH:mm:ss) for start
     * \param end the ISO8601 date/time string (YYYY-MM-DDTHH:mm:ss) for end
     * \param resolution the formatted resolution, which consists of an integer & unit
     *        character. The acceptable unit characters are:
     *        (y)ear   Example: '1y' = 1 year. No range limitations
     *        (M)onth  Example: '4M' = 4 months.    Allowable values: 1, 2, 3, 4, 6
     *        (d)ay    Example: '10d' = 10 days.    Allowable values: 1 - 28
     *        (h)our   Example: '12h' = 12 hours.   Allowable values: 1, 2, 3, 4, 6, 12
     *        (m)inute Example: '15m' = 15 minutes. Allowable values: 0, 15, 30
     */
    TimeQuantizer(std::string start, std::string end, const std::string& resolution);

    /**
     * Set the time range start & end date/time range.
     *
     * \param start The ISO8601 date/time string for start of the time range
     * \param end The ISO8601 date/time string for end of the time range
     */
    void setStartEndRange(const std::string& start, const std::string& end);

    /**
     * Set the time resolution.
     *
     * \param resolutionString String that defines the resolution within the time range
     *        see comment header for constructor for the allowable values and ranges
     */
    void setResolution(const std::string& resolutionString);

    /**
     * Takes a time resulition string and parses it into a double value representing the
     * time resolution as seconds.
     *
     * Example: parseTimeResolutionStr("1d");
     *
     * \param resolutionStr String with the format {number}{unit} where supported units
     *        are: (s)econds, (m)inutes, (h)ours, (d)ays, (y)ears
     * \return The time resolution in seconds
     */
    double parseTimeResolutionStr(const std::string& resolutionStr);

    /**
     * Quantizes a OpenSpace Time into descrete values. If the provided Time \p t is
     * outside the time range, it will be clamped to the the time range.
     *
     * \param t Time instance, which will be quantized
     * \param clamp Whether or not time should be clamped if not t is in the time range
     * \return Whether or not time was quantized
     */
    bool quantize(Time& t, bool clamp);

    /**
     * Returns a list of quantized Time strings that represent all the valid quantized
     * time%s between \p start and \p end.
     *
     * \param start The start time for the time range quantization
     * \param end The end time for the time range quantization
     * \return A list of quantized times between \p start and \p end
     */
    std::vector<std::string> quantized(Time& start, Time& end);

private:
    void verifyStartTimeRestrictions();
    void verifyResolutionRestrictions(const int value, const char unit);
    void doFirstApproximation(DateTime& q, const DateTime& unQ, double value, char unit);
    double computeSecondsFromResolution(const int valueIn, const char unit);
    double _resolution = 0.0;
    double _resolutionValue = 0.0;
    char _resolutionUnit = 'd';
    DateTime _dt;
    DateTime _start;
    RangedTime _timerange;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TIMEQUANTIZER___H__
