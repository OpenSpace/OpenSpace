/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

/* RangedTime class is used to define an acceptable time range. Functionality includes
 * checking if a given date/time is within that range, or clamping a date to enforce
 * this range.
 */
class RangedTime {
public:
    RangedTime() {};

    /*
     * Constructor that accepts an ISO8601 date/time string (YYYY-MM-DDTHH:mm:ss) for an
     * allowable time range defined by start and end values.
     *
     * \param start The date/time start of the time range.
     * \param end   The date/time ending of the time range.
     */
    RangedTime(std::string start, std::string end);

    /*
     * Checks if a date/time value falls within the start/end range defined in this
     * instance of the class.
     *
     * \param checkTime An ISO8601 date/time string to test if it falls within the range
     *
     * \returns true if the input date/time falls between the start and end date/times
    */
    bool includes(const std::string& checkTime);

    /*
     * Enforces the start/end range on a given date/time string by clamping the value
     *
     * \param checkTime An ISO8601 date/time string to clamp if falls outside of range
     *
     * \returns clamped value of input parameter, will be equal to the start value if
     *          less than start, equal to end if greater than end, or equal to input
     *          parameter if falls in-between
    */
    std::string clamp(const std::string& checkTime);

    /*
     * Get the start date/time of the time range
     *
     * \returns The ISO8601 date/time string that defines the start of the range
     */
    std::string start() const;

    /*
     * Get the end date/time of the time range
     *
     * \returns The ISO8601 date/time string that defines the end of the range
     */
    std::string end() const;

    /*
     * Set the start date/time of the time range
     *
     * \param The ISO8601 date/time string that defines the start of the range
     */
    void setStart(const std::string start);

    /*
     * Set the end date/time of the time range
     *
     * \param The ISO8601 date/time string that defines the end of the range
     */
    void setEnd(const std::string start);

private:
    std::string _start;
    std::string _end;
    double _startJ2000;
    double _endJ2000;
};

/* DateTime class is used to manage a date/time value and provide methods for increment/
 * decrementing the value, which gets complicated because of the varying days of the
 * different months, leap years, etc.
 * This class exists to handle date/time values within a "people-friendly" calendar
 * schedule. For example, a temporal data set that's updated on the 10th of every month
 * will sometimes be updated in 28 days, other times in 31 days. The intent of the class
 * is to handle all of the "special cases" where simply using J2000 seconds won't work.
 */
class DateTime {
public:
    DateTime() = default;
    /*
     * Constructor that initializes with date/time string
     *
     * \params initDateTime the ISO8601 date/time string (YYYY-MM-DDTHH:mm:ss)
     */
    DateTime(const std::string& initDateTime);

    /*
     * Set the date/time value
     *
     * \params input the ISO8601 date/time string (YYYY-MM-DDTHH:mm:ss) to set
     */
    void setTime(const std::string& input);

    /*
     * Used to deep-copy from another DateTime instance
     *
     * \params src the DateTime object to copy from
     */
    void operator=(DateTime& src);

    /*
     * Get the date/time value in ISO8601 format
     *
     * \returns the date/time value string
     */
    std::string ISO8601() const;

    /*
     * Get the J2000 seconds equivalent of the object's date/time, using
     * the loaded SPICE kernel
     *
     * \returns J2000 seconds of date/time
     */
    double J2000() const;

    /*
     * Get the year of the object's date/time (YYYY format)
     * \returns integer value of the year
     */
    int year() const;

    /*
     * Get the month of the object's date/time (1 - 12)
     * \returns integer value of the month
     */
    int month() const;

    /*
     * Get the day-of-month of the object's date/time (1 - 31)
     * \returns integer value of the day
     */
    int day() const;

    /*
     * Get the hour of the object's date/time
     * \returns integer value of the hour (0 - 23)
     */
    int hour() const;

    /*
     * Get the minute of the object's date/time
     * \returns integer value of the minutes
     */
    int minute() const;

    /*
     * Get the seconds of the object's date/time
     * \returns integer value of the seconds
     */
    int second() const;

    /*
     * Set the year of the object's date/time
     * \param y integer value of the year
     */
    void setYear(int y);

    /*
     * Set the month of the object's date/time (1 - 12)
     * \param m integer value of the year
     */
    void setMonth(int m);

    /*
     * Set the day-of-month of the object's date/time (1 - 31)
     * \param d integer value of the day
     */
    void setDay(int d);

    /*
     * Set the hour of the object's date/time (0 - 23)
     * \param h integer value of the hour
     */
    void setHour(int h);

    /*
     * Set the minute of the object's date/time
     * \param m integer value of the minute
     */
    void setMinute(int m);

    /*
     * Set the seconds of the object's date/time
     * \param s integer value of the seconds
     */
    void setSecond(int s);

    /*
     * Increment operation for the date/time
     *
     * \param value integer value for number of units in an operation
     * \param unit single char that specifies the unit of increment. Allowable units are:
     *             (y)ear, (M)onth, (d)ay, (h)our, (m)inute, (s)econd
     * \param error The difference in J2000 seconds from current date/time to target
     *              (a positive value means target is in the future)
     * \param resolution The J2000 seconds of the interval defined by the value & unit
     *
     * \returns The number of increments that were performed in order to get as close as
     *          possible to the target, where each increment is defined by the value &
     *          unit (and approximated but not fixed by the resolution param)
     */
    int increment(int value, char unit, double error, double resolution);

    /*
     * Decrement operation for the date/time
     *
     * \param value integer value for number of units in an operation
     * \param unit single char that specifies the unit of decrement. Allowable units are:
     *             (y)ear, (M)onth, (d)ay, (h)our, (m)inute, (s)econd
     * \param error The difference in J2000 seconds from current date/time to target
     *              (a positive value means target is in the future)
     * \param resolution The J2000 seconds of the interval defined by the value & unit
     *
     * \returns The number of decrements that were performed in order to get as close as
     *          possible to the target, where each decrement is defined by the value &
     *          unit (and approximated but not fixed by the resolution param)
     */
    int decrement(int value, char unit, double error, double resolution);

    /*
     * Single increment operation for the date/time
     *
     * \param value integer value for number of units in an operation
     * \param unit single char that specifies the unit of increment. Allowable units are:
     *             (y)ear, (M)onth, (d)ay, (h)our, (m)inute, (s)econd
     */
    void incrementOnce(int value, char unit);

    /*
     * Single decrement operation for the date/time
     *
     * \param value integer value for number of units in an operation
     * \param unit single char that specifies the unit of decrement. Allowable units are:
     *             (y)ear, (M)onth, (d)ay, (h)our, (m)inute, (s)econd
     */
    void decrementOnce(int value, char unit);

private:
    // index_ values are indices into an ISO8601 YYYY-MM-ddTHH:mm:ss string
    const int index_year = 0;
    const int index_month = 5;
    const int index_day = 8;
    const int index_hour = 11;
    const int index_minute = 14;
    const int index_second = 17;

    const int len_year = 4;
    const int len_nonYear = 2;

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
    /*
     * Constructor that initializes with formatted strings for start & ends date/times,
     * and a time resolution within that range
     *
     * \params start the ISO8601 date/time string (YYYY-MM-DDTHH:mm:ss) for start
     * \params end the ISO8601 date/time string (YYYY-MM-DDTHH:mm:ss) for end
     * \params resolution the formatted resolution, which consists of an integer & unit
     *         character. The acceptable unit characters are:
     *         (y)ear   Example: '1y' = 1 year. No range limitations
     *         (M)onth  Example: '4M' = 4 months.    Allowable values: 1, 2, 3, 4, 6
     *         (d)ay    Example: '10d' = 10 days.    Allowable values: 1 - 28
     *         (h)our   Example: '12h' = 12 hours.   Allowable values: 1, 2, 3, 4, 6, 12
     *         (m)inute Example: '15m' = 15 minutes. Allowable values: 0, 15, 30
     */
    TimeQuantizer(std::string start, std::string end, const std::string& resolution);

    /*
     * Set the time range start & end date/time range.
     *
     * \param start The ISO8601 date/time string for start of the time range
     * \param end   The ISO8601 date/time string for end of the time range.
     */
    void setStartEndRange(const std::string& start, const std::string& end);

    /*
     * Set the time resolution
     *
     * \param resolutionString String that defines the resolution within the time range.
     *                         see comment header for constructor for the allowable
     *                         values and ranges.
     */
    void setResolution(const std::string& resolutionString);

    /**
    * Takes a time resulition string and parses it into a double
    * value representing the time resolution as seconds.
    *
    * Example: parseTimeResolutionStr("1d");
    *
    * \param resolutionStr with the format {number}{unit} where supported units are:
    *        (s)econds, (m)inutes, (h)ours, (d)ays, (y)ears
    * \return the time resolution in seconds
    */
    double parseTimeResolutionStr(const std::string& resolutionStr);

    /**
    * Quantizes a OpenSpace Time into descrete values. If the provided Time \p t is
    * outside the time range, it will be clamped to the the time range.
    *
    * \param t Time instance, which will be quantized
    * \param clamp Whether or not time should be clamped if not t is in the time range
    * \return wether or not time was quantized
    */
    bool quantize(Time& t, bool clamp);

    /**
    * Returns a list of quantized Time strings that represent all the valid quantized
    * time%s between \p start and \p end.
    *
    * \param start The start time for the time range quantization
    * \param end The end time for the time range quantization
    * \return A list of quantized times between \p start and \end
    */
    std::vector<std::string> quantized(Time& start, Time& end);

private:
    void verifyStartTimeRestrictions();
    void verifyResolutionRestrictions(const int value, const char unit);
    double diff(DateTime& from, DateTime& to);
    void doFirstApproximation(DateTime& q, DateTime& unQ, double value, char unit);
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
