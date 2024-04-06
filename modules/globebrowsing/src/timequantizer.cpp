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

#include <modules/globebrowsing/src/timequantizer.h>

#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/format.h>
#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/profiling.h>
#include <date/date.h>
#include <algorithm>
#include <charconv>
#include <iomanip>
#include <sstream>

// @TODO (abock, 2020-08-07) All of the time handling in this class should be cleaned up
//       a bit. There are lots of conversions between ISO strings for time and Time
//       objects and back which eat up performance.  For example, the TimeRange should
//       operate on Time objects rather than date strings and the DateTime likewise (if
//       this class needs to exist at all)

namespace openspace::globebrowsing {

namespace {
    // returns the number of days in a given month and year (takes leap year into account)
    constexpr int monthSize(int month_, int year_) {
        using namespace date;
        const year_month_day_last d = year(year_) / month(month_) / last;
        return static_cast<int>(static_cast<unsigned>(d.day()));
    }

    /**
     * singleIncrement is used for any of the date/time types, and handles overflow
     * values using the min/max parameters
     *
     * \param oper the date/time variable to operate on (will be changed)
     * \param val the value of the increment, which may be changed in this function
     *        if an overflow occurs
     * \param min the minimum allowable value
     * \param max the maximum allowable value (determines where overflow occurs)
     */
    bool singleIncrement(int& oper, int& val, int min, int max) {
        oper += val;
        if (oper <= max) {
            return true;
        }
        oper = oper - max - (1 - min);
        // Only single increments for the less-significant units on rollover
        val = 1;
        return false;
    }

    /**
     * singleDecrement is used for any of the date/time types, and handles underflow
     * values using the min/max parameters
     *
     * \param oper the date/time variable to operate on (will be changed)
     * \param val the value of the decrement, which may be changed in this function
     *        if an underflow occurs
     * \param min the minimum allowable value
     * \param max the maximum allowable value (determines where underflow occurs)
     */
    bool singleDecrement(int& oper, int& val, int min, int max) {
        oper -= val;
        if (oper >= min) {
            return true;
        }
        oper = oper + max + (1 - min);
        // Only single increments for the less-significant units on rollover
        val = 1;
        return false;
    }

} // namespace

RangedTime::RangedTime(std::string start, std::string end)
    : _start(std::move(start))
    , _end(std::move(end))
{
    setStart(_start);
    setEnd(_end);
}

void RangedTime::setStart(std::string start) {
    Time t1;
    t1.setTime(start);
    _startJ2000 = t1.j2000Seconds();
    _start = std::move(start);
}

void RangedTime::setEnd(std::string end) {
    Time t2;
    t2.setTime(end);
    _endJ2000 = t2.j2000Seconds();
    _end = std::move(end);
}

bool RangedTime::includes(const Time& checkTime) const {
    const double tj = checkTime.j2000Seconds();
    return (_startJ2000 <= tj && tj <= _endJ2000);
}

const char* RangedTime::clamp(const char* checkTime) {
    Time t;
    t.setTime(checkTime);
    const double tj = t.j2000Seconds();
    if (tj < _startJ2000) {
        return _start.c_str();
    }
    else if (tj > _endJ2000) {
        return _end.c_str();
    }
    else {
        return checkTime;
    }
}

std::string_view RangedTime::start() const {
    return _start;
}

std::string_view RangedTime::end() const {
    return _end;
}

DateTime::DateTime(std::string_view initDateTime) {
    setTime(initDateTime);
}

void DateTime::setTime(std::string_view input) {
    // Indices into an ISO8601 YYYY-MM-ddTHH:mm:ss string
    constexpr size_t IndexYear = 0;
    constexpr size_t IndexMonth = 5;
    constexpr size_t IndexDay = 8;
    constexpr size_t IndexHour = 11;
    constexpr size_t IndexMinute = 14;
    constexpr size_t IndexSecond = 17;

    std::from_chars(input.data() + IndexYear, input.data() + IndexYear + 4, _year);
    std::from_chars(input.data() + IndexMonth, input.data() + IndexMonth + 2, _month);
    std::from_chars(input.data() + IndexDay, input.data() + IndexDay + 2, _day);
    std::from_chars(input.data() + IndexHour, input.data() + IndexHour + 2, _hour);
    std::from_chars(input.data() + IndexMinute, input.data() + IndexMinute + 2, _minute);
    std::from_chars(input.data() + IndexSecond, input.data() + IndexSecond + 2, _second);
}

std::string DateTime::ISO8601() const {
    return std::format(
        "{:0>4}-{:0>2}-{:0>2}T{:0>2}:{:0>2}:{:0>2}",
        _year, _month, _day, _hour, _minute, _second
    );
}

double DateTime::J2000() const {
    char Buffer[20];
    std::memset(Buffer, 0, 20);
    std::format_to(
        Buffer,
        "{:0>4}-{:0>2}-{:0>2}T{:0>2}:{:0>2}:{:0>2}",
        _year, _month, _day, _hour, _minute, _second
    );
    return Time::convertTime(Buffer);
}

int DateTime::increment(int value, char unit, double error, double resolution) {
    unsigned int nIncrements = std::abs(static_cast<int>(error / resolution));
    if (nIncrements == 0) {
        nIncrements = 1;
    }
    for (unsigned int i = 0; i < nIncrements; i++) {
        incrementOnce(value, unit);
    }
    return nIncrements;
}

void DateTime::incrementOnce(int value, char unit) {
    bool inBounds = true;
    switch (unit) {
        case 'm':
            if (singleIncrement(_minute, value, 0, 59)) {
                break;
            }
            [[ fallthrough ]];
        case 'h':
            if (singleIncrement(_hour, value, 0, 23)) {
                break;
            }
            [[ fallthrough ]];
        case 'd':
            if (singleIncrement(_day, value, 1, monthSize(_month, _year))) {
                break;
            }
            [[ fallthrough ]];
        case 'M':
            inBounds = singleIncrement(_month, value, 1, 12);
            _day = std::clamp(_day, 1, monthSize(_month, _year));
            if (inBounds) {
                break;
            }
            [[ fallthrough ]];
        case 'y':
            _year += value;
            break;
        default:
            throw ghoul::RuntimeError(std::format(
                "Invalid unit in incrementOnce '{}'. Expected 'y', 'M', 'd', 'h', or 'm'",
                unit
            ));
    }
}

int DateTime::decrement(int value, char unit, double error, double resolution) {
    unsigned int nDecrements = std::abs(static_cast<int>(error / resolution));
    if (nDecrements == 0) {
        nDecrements = 1;
    }
    for (unsigned int i = 0; i < nDecrements; i++) {
        decrementOnce(value, unit);
    }
    return nDecrements;
}

void DateTime::decrementOnce(int value, char unit) {
    bool inBounds = true;
    switch (unit) {
        case 'm':
            if (singleDecrement(_minute, value, 0, 59)) {
                break;
            }
            [[ fallthrough ]];
        case 'h':
            if (singleDecrement(_hour, value, 0, 23)) {
                break;
            }
            [[ fallthrough ]];
        case 'd':
            if (singleDecrement(_day, value, 1,
                monthSize(_month == 1 ? 12 : _month - 1, _year)))
            {
                break;
            }
            [[ fallthrough ]];
        case 'M':
            inBounds = singleDecrement(_month, value, 1, 12);
            _day = std::clamp(_day, 1, monthSize(_month, _year));
            if (inBounds) {
                break;
            }
            [[ fallthrough ]];
        case 'y':
            _year -= value;
            break;

        default:
            throw ghoul::RuntimeError(
                "Invalid unit format in TQ decrementOnce '" + std::to_string(unit) +
                "'. Expected 'y', 'M', 'd', 'h', or 'm'"
            );
    }
}

int DateTime::year() const {
    return _year;
}

int DateTime::month() const {
    return _month;
}

int DateTime::day() const {
    return _day;
}

int DateTime::hour() const {
    return _hour;
}

int DateTime::minute() const {
    return _minute;
}

int DateTime::second() const {
    return _second;
}

void DateTime::setYear(int y) {
    _year = y;
}

void DateTime::setMonth(int m) {
    _month = m;
}

void DateTime::setDay(int d) {
    _day = d;
}

void DateTime::setHour(int h) {
    _hour = h;
}

void DateTime::setMinute(int m) {
    _minute = m;
}

void DateTime::setSecond(int s) {
    _second = (s > 0) ? ((s <= 59) ? s : 59) : 0;
}

TimeQuantizer::TimeQuantizer(std::string start, std::string end,
                             const std::string& resolution)
    : _resolution(parseTimeResolutionStr(resolution))
    , _start(start)
    , _timerange(std::move(start), std::move(end))
{
    verifyStartTimeRestrictions();
}

double TimeQuantizer::parseTimeResolutionStr(const std::string& resolutionStr) {
    const char unit = resolutionStr.back();
    const std::string numberString = resolutionStr.substr(0, resolutionStr.length() - 1);

    char* p = nullptr;
    const double value = strtol(numberString.c_str(), &p, 10);
    _resolutionValue = value;
    _resolutionUnit = unit;
    if (*p) { // not a number
        throw ghoul::RuntimeError("Cannot convert " + numberString + " to number");
    }
    else {
        verifyResolutionRestrictions(static_cast<int>(value), unit);
        return computeSecondsFromResolution(static_cast<int>(value), unit);
    }
}

void TimeQuantizer::setStartEndRange(const std::string& start, const std::string& end) {
    _start.setTime(start);
    verifyStartTimeRestrictions();

    _timerange.setStart(start);
    _timerange.setEnd(end);
}

void TimeQuantizer::setResolution(const std::string& resolutionString) {
    _resolution = parseTimeResolutionStr(resolutionString);
    verifyStartTimeRestrictions();
}

void TimeQuantizer::verifyStartTimeRestrictions() {
    // If monthly time resolution then restrict to 28 days so every month is consistent
    int dayUpperLimit = 0;
    std::string helpfulDescription = "the selected month";
    if (_resolutionUnit == 'M') {
        dayUpperLimit = 28;
        helpfulDescription = "monthly increment";
    }
    else if (_resolutionUnit == 'y') {
        //Get month sizes using a fixed non-leap year
        dayUpperLimit = monthSize(_start.month(), 2001);
        helpfulDescription += " on a yearly increment";
    }
    else {
        dayUpperLimit = 31;
    }
    if (_start.day() < 1 || _start.day() > dayUpperLimit) {
        throw ghoul::RuntimeError(std::format(
            "Invalid start day value of {} for {}, valid days are 1 - {}",
            _start.day(), helpfulDescription, dayUpperLimit
        ));
    }
    if (_start.hour() != 0 || _start.minute() != 0 || _start.second() != 0) {
        throw ghoul::RuntimeError(std::format(
            "Invalid start time value of {}:{}:{}, time must be 00:00:00",
            _start.hour(), _start.minute(), _start.second()
        ));
    }
}

void TimeQuantizer::verifyResolutionRestrictions(const int value, const char unit) {
    switch (unit) {
        case 'y':
            break;
        case 'M':
            if (value < 1 || value > 6) {
                throw ghoul::RuntimeError(std::format(
                    "Invalid resolution count of {} for (M)onth option. Valid counts are "
                    "1, 2, 3, 4, or 6", value
                ));
            }
            break;
        case 'd':
            if (value < 1 || value > 28) {
                throw ghoul::RuntimeError(std::format(
                    "Invalid resolution count of {} for (d)ay option. Valid counts are "
                    "1 - 28", value
                ));
            }
            break;
        case 'h':
            if ((value < 1 || value > 4) && value != 6 && value != 12) {
                throw ghoul::RuntimeError(std::format(
                    "Invalid resolution count of {} for (h)our option. Valid counts are "
                    "1, 2, 3, 4, 6, or 12", value
                ));
            }
            break;
        case 'm':
            if (value != 15 && value != 30) {
                throw ghoul::RuntimeError(std::format(
                    "Invalid resolution count of {} for (m)inute option. Valid counts "
                    "are 15 or 30", value
                ));
            }
            break;
        default:
            throw ghoul::RuntimeError(std::format(
                "Invalid unit format '{}'. Expected 'y', 'M', 'd', 'h', or 'm'", unit
            ));
    }
}

double TimeQuantizer::computeSecondsFromResolution(const int valueIn, const char unit) {
    double value = static_cast<double>(valueIn);
    // convert value to seconds, based on unit.
    // The switch statment has intentional fall throughs
    switch (unit) {
        case 'y':
            value *= 365;
            [[fallthrough]];
        case 'd':
            value *= 24.0;
            [[fallthrough]];
        case 'h':
            value *= 60.0;
            [[fallthrough]];
        case 'm':
            value *= 60.0;
            [[fallthrough]];
        case 's':
            value *= 1.0;
            break;

        case 'M':
            value *= (30.4 * 24.0 * 60.0 * 60.0);
            break;

        default:
            throw ghoul::RuntimeError(std::format(
                "Invalid resolution unit format '{}'. Expected 'y', 'M', 'd', 'h', 'm', "
                "or 's'", unit
            ));
    }
    return value;
}

bool TimeQuantizer::quantize(Time& t, bool clamp) {
    ZoneScoped;

    constexpr const char Format[] = "YYYY-MM-DDTHR:MN:SC.###";
    constexpr int BufferSize = sizeof(Format);
    char unquantizedString[BufferSize];
    std::memset(unquantizedString, 0, BufferSize);
    SpiceManager::ref().dateFromEphemerisTime(
        t.j2000Seconds(),
        unquantizedString,
        BufferSize,
        Format
    );

    const DateTime unquantized = DateTime(
        std::string_view(unquantizedString, BufferSize)
    );
    // resolutionFraction helps to improve iteration performance
    constexpr double ResolutionFraction = 0.7;
    constexpr int IterationLimit = 50;

    if (_timerange.includes(t)) {
        int iterations = 0;
        int lastIncr = 0;
        int lastDecr = 0;
        DateTime quantized = DateTime(_timerange.start());
        doFirstApproximation(quantized, unquantized, _resolutionValue, _resolutionUnit);
        double error = unquantized.J2000() - quantized.J2000();
        while (error > (_resolution * ResolutionFraction) || error < 0) {
            if (error > 0) {
                lastIncr = quantized.increment(
                    static_cast<int>(_resolutionValue),
                    _resolutionUnit,
                    error,
                    _resolution
                );
            }
            else if (error < 0) {
                lastDecr = quantized.decrement(
                    static_cast<int>(_resolutionValue),
                    _resolutionUnit,
                    error,
                    _resolution
                );
            }
            error = unquantized.J2000() - quantized.J2000();
            const bool hasSettled = (lastIncr == 1 && lastDecr == 1 && error >= 0.0);
            iterations++;
            if (hasSettled || iterations > IterationLimit) {
                break;
            }
        }
        char Buffer[20];
        std::memset(Buffer, 0, 20);
        std::format_to(
            Buffer,
            "{:0>4}-{:0>2}-{:0>2}T{:0>2}:{:0>2}:{:0>2}",
            quantized.year(), quantized.month(), quantized.day(),
            quantized.hour(), quantized.minute(), quantized.second()
        );

        t.setTime(_timerange.clamp(Buffer));
        return true;
    }
    else if (clamp) {
        t.setTime(_timerange.clamp(unquantizedString));
        return true;
    }
    else {
        return false;
    }
}

void TimeQuantizer::doFirstApproximation(DateTime& quantized, const DateTime& unQ,
                                         double value, char unit)
{
    ZoneScoped;

    switch (unit) {
        case 'y':
        {
            const double minYearsToAdjust = static_cast<double>(
                unQ.year()) - static_cast<double>(_start.year()
            );
            const double minIncrementsAdjust = minYearsToAdjust / value;
            quantized.setYear(
                _start.year() + static_cast<int>(minIncrementsAdjust * value)
            );
            break;
        }
        case 'M':
        {
            const bool isMonthPastQuantizedMonth = unQ.month() > static_cast<int>(value);
            quantized.setYear(isMonthPastQuantizedMonth ? unQ.year() : unQ.year() - 1);
            break;
        }
        case 'd':
        {
            const double error = (unQ.J2000() - quantized.J2000()) / (60.0 * 60.0 * 24.0);
            const int originalHour = quantized.hour();
            const int originalMinute = quantized.minute();
            const int originalSecond = quantized.second();
            const double addToTime = std::round(error) * 86400;
            const Time testDay = Time(quantized.J2000() + addToTime);

            char Buffer[25];
            testDay.ISO8601(Buffer);
            quantized.setTime(std::string_view(Buffer, 25));
            quantized.setHour(originalHour);
            quantized.setMinute(originalMinute);
            quantized.setSecond(originalSecond);
            break;
        }
        case 'h':
        {
            quantized = unQ;
            quantized.setMinute(0);
            quantized.setSecond(0);
            if (unQ.hour() >= 12) {
                quantized.setHour(0);
            }
            else {
                quantized.decrementOnce(1, 'd');
            }
            break;
        }
        case 'm':
            quantized = unQ;
            quantized.setMinute(0);
            quantized.setSecond(0);
            if (quantized.hour() > 0) {
                quantized.decrementOnce(1, 'h');
            }
            else {
                quantized.decrementOnce(1, 'd');
            }
            break;
        default:
            throw ghoul::RuntimeError(std::format(
                "Invalid unit '{}'. Expected 'y', 'M', 'd', 'h', or 'm'", unit
            ));
    }
}

std::vector<std::string> TimeQuantizer::quantized(Time& start, Time& end) {
    const DateTime s = DateTime(start.ISO8601());
    quantize(start, true);

    const DateTime e = DateTime(end.ISO8601());
    quantize(end, true);

    const double startSeconds = s.J2000();
    const double endSeconds = e.J2000();
    ghoul_assert(
        static_cast<int>(endSeconds - startSeconds) % static_cast<int>(_resolution) == 0,
        "Quantization error"
    );

    std::vector<std::string> result;
    DateTime itr = s;
    const RangedTime range = RangedTime(
        std::string(start.ISO8601()),
        std::string(end.ISO8601())
    );
    while (range.includes(Time(itr.ISO8601()))) {
        itr.incrementOnce(static_cast<int>(_resolutionValue), _resolutionUnit);
        result.push_back(itr.ISO8601());
    }

    return result;
}

} // namespace openspace::globebrowsing
