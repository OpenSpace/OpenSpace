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

#include <modules/globebrowsing/src/timequantizer.h>

#include <openspace/util/time.h>
#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <algorithm>
#include <sstream>
#include <iomanip>

namespace openspace::globebrowsing {


TimeQuantizer::TimeQuantizer(const std::string& start, const std::string& end,
                             const std::string& resolution)
    : _start(start)
    , _timerange(start, end)
{
    verifyStartTimeRestrictions();
    _resolution = parseTimeResolutionStr(resolution);
}

double TimeQuantizer::parseTimeResolutionStr(const std::string& resolutionStr) {
    const char unit = resolutionStr.back();
    std::string numberString = resolutionStr.substr(0, resolutionStr.length() - 1);

    char* p;
    double value = strtol(numberString.c_str(), &p, 10);
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
}

void TimeQuantizer::verifyStartTimeRestrictions() {
    if (_start.day() < 1 || _start.day() > 28) {
        throw ghoul::RuntimeError(
            "Invalid start day value of " + std::to_string(_start.day()) +
            " for day of month. Valid days are 1 - 28."
        );
    }
    if (_start.hour() != 0 || _start.minute() != 0 || _start.second() != 0) {
        throw ghoul::RuntimeError(
            "Invalid start time value of " + std::to_string(_start.hour()) + ":" +
            std::to_string(_start.minute()) + ":" + std::to_string(_start.second()) +
            ". Time must be 00:00:00."
        );
    }
}

void TimeQuantizer::verifyResolutionRestrictions(const int value, const char unit) {
    switch (unit) {
    case 'y':
        break;

    case 'M':
        switch (value) {
        case 1:
        case 2:
        case 3:
        case 4:
        case 6:
            break;

        default:
            throw ghoul::RuntimeError(
                "Invalid resolution count of " + std::to_string(value) + " for (M)onth"
                + " option. Valid counts are 1, 2, 3, 4, or 6."
            );
        }
        break;

    case 'd':
        if (value < 1 || value > 28) {
            throw ghoul::RuntimeError(
                "Invalid resolution count of " + std::to_string(value) + " for (d)ay"
                + " option. Valid counts are 1 - 28."
            );
        }
        break;

    case 'h':
        switch (value) {
        case 1:
        case 2:
        case 3:
        case 4:
        case 6:
        case 12:
            break;

        default:
            throw ghoul::RuntimeError(
                "Invalid resolution count of " + std::to_string(value) + " for (h)our"
                + " option. Valid counts are 1, 2, 3, 4, 6, or 12."
            );
        }
        break;

    case 'm':
        switch (value) {
        case 15:
        case 30:
            break;

        default:
            throw ghoul::RuntimeError(
                "Invalid resolution count of " + std::to_string(value) + " for (m)inute"
                + " option. Valid counts are 1, 2, 3, 4, 6, or 12."
            );
        }
        break;

    default:
        throw ghoul::RuntimeError(
            "Invalid resolution unit format '" + std::to_string(unit) +
            "'. Expected 'y', 'M', 'd', 'h', or 'm'."
        );
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
        throw ghoul::RuntimeError(
            "Invalid resolution unit format '" + std::to_string(unit) +
            "'. Expected 'y', 'M', 'd', 'h', 'm' or 's'."
        );
    }
    return value;
}

bool TimeQuantizer::quantize(Time& t, bool clamp) {
    const std::string unquantizedStr = t.ISO8601();
    DateTime unquantized(unquantizedStr);
    //resolutionFraction helps to improve iteration performance
    const double resolutionFraction = 0.7;
    double error = 0.0;
    const int iterationLimit = 50;
    int iterations = 0;
    int lastIncr = 0;
    int lastDecr = 0;

    if (_timerange.includes(unquantizedStr)) {
        DateTime quantized = DateTime(_timerange.start());
        doFirstApproximation(quantized, unquantized, _resolutionValue, _resolutionUnit);
        error = diff(quantized, unquantized);
        while (error > (_resolution * resolutionFraction) || error < 0) {
            if (error > 0) {
                lastIncr = quantized.increment(static_cast<int>(_resolutionValue),
                    _resolutionUnit, error, _resolution);
            }
            else if (error < 0) {
                lastDecr = quantized.decrement(static_cast<int>(_resolutionValue),
                    _resolutionUnit, error, _resolution);
            }
            error = diff(quantized, unquantized);
            bool hasSettled = (lastIncr == 1 && lastDecr == 1 && error >= 0.0);
            iterations++;
            if (hasSettled || iterations > iterationLimit) {
                break;
            }
        }
        quantized.setTime(_timerange.clamp(quantized.ISO8601()));
        t.setTime(quantized.J2000());
        return true;
    }
    else if (clamp) {
        const std::string clampedTime = _timerange.clamp(unquantizedStr);
        t.setTime(clampedTime);
        return true;
    }
    else {
        return false;
    }
}

double TimeQuantizer::diff(DateTime& from, DateTime& to) {
    return to.J2000() - from.J2000();
}

void TimeQuantizer::doFirstApproximation(DateTime& quantized, DateTime& unQ,
                                         double value, char unit)
{
    double minYearsToAdjust;
    double minIncrementsAdjust;
    bool isSimMonthPastQuantizedMonth;
    double error = 0.0;
    int originalHour, originalMinute, originalSecond;
    Time testDay;
    double addToTime;

    switch (unit) {
    case 'y':
        minYearsToAdjust = static_cast<double>(unQ.year()) -
            static_cast<double>(_start.year());
        minIncrementsAdjust = minYearsToAdjust / value;
        quantized.setYear(_start.year() + static_cast<int>(minIncrementsAdjust) * value);
        break;

    case 'M':
        isSimMonthPastQuantizedMonth = unQ.month() > static_cast<int>(value);
        quantized.setYear((isSimMonthPastQuantizedMonth) ? unQ.year() : unQ.year() - 1);
        break;

    case 'd':
        error = diff(quantized, unQ);
        error /= 86400;
        originalHour = quantized.hour();
        originalMinute = quantized.minute();
        originalSecond = quantized.second();
        addToTime = std::round(error) * 86400;
        testDay.setTime(quantized.J2000() + addToTime);
        quantized.setTime(testDay.ISO8601());
        quantized.setHour(originalHour);
        quantized.setMinute(originalMinute);
        quantized.setSecond(originalSecond);
        break;

    case 'h':
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
        throw ghoul::RuntimeError(
            "Invalid unit format in doFirstApproximation '" + std::to_string(unit) +
            "'. Expected 'y', 'M', 'd', 'h', or 'm'."
        );
    }
}

std::vector<std::string> TimeQuantizer::quantized(Time& start, Time& end) {
    DateTime s(start.ISO8601());
    quantize(start, true);

    DateTime e(end.ISO8601());
    quantize(end, true);

    const double startSeconds = s.J2000();
    const double endSeconds = e.J2000();
    const double delta = endSeconds - startSeconds;

    ghoul_assert(int(delta) % int(_resolution) == 0, "Quantization error");
    const int nSteps = static_cast<int>(delta / _resolution);

    std::vector<std::string> result;
    DateTime itr = s;
    RangedTime range(start.ISO8601(), end.ISO8601());
    while (range.includes(itr.ISO8601())) {
        itr.incrementOnce(static_cast<int>(_resolutionValue), _resolutionUnit);
        result.push_back(itr.ISO8601());
    }

    return result;
}

DateTime::DateTime(std::string initDateTime) {
    setTime(initDateTime);
};

void DateTime::setTime(const std::string& input) {
    _year = std::stoi(input.substr(index_year, len_year));
    _month = std::stoi(input.substr(index_month, len_nonYear));
    _day = std::stoi(input.substr(index_day, len_nonYear));
    _hour = std::stoi(input.substr(index_hour, len_nonYear));
    _minute = std::stoi(input.substr(index_minute, len_nonYear));
    _second = std::stoi(input.substr(index_second, len_nonYear));
}

std::string DateTime::ISO8601() {
    std::stringstream sstr = std::stringstream();
    sstr << std::setfill('0') << std::setw(4) << _year << "-";
    sstr << std::setfill('0') << std::setw(2) << _month << "-";
    sstr << std::setfill('0') << std::setw(2) << _day << "T";
    sstr << std::setfill('0') << std::setw(2) << _hour << ":";
    sstr << std::setfill('0') << std::setw(2) << _minute << ":";
    sstr << std::setfill('0') << std::setw(2) << _second;
    return sstr.str();
};

double DateTime::J2000() {
    Time t;
    std::string timeString = ISO8601();
    t.setTime(timeString);
    return t.j2000Seconds();
}

void DateTime::operator= (DateTime& src) {
    _year = src.year();
    _month = src.month();
    _day = src.day();
    _hour = src.hour();
    _minute = src.minute();
    _second = src.second();
}

int DateTime::increment(int value, char unit, double error, double resolution) {
    unsigned int nIncrements = std::abs(static_cast<int>(error / resolution));
    if (nIncrements == 0) {
        nIncrements = 1;
    }
    for (unsigned int i = 0; i < nIncrements; ++i) {
        incrementOnce(value, unit);
    }
    return nIncrements;
}

void DateTime::incrementOnce(int value, char unit) {
    bool inBounds = true;
    switch (unit) {
    case 'm':
        if (singleIncrement(_minute, value, 0, 59))
            break;
        //else fall-through if overflow...

    case 'h':
        if (singleIncrement(_hour, value, 0, 23))
            break;
        //else fall-through if overflow...

    case 'd':
        if (singleIncrement(_day, value, 1, monthSize(_month, _year)))
            break;
        //else fall-through if overflow...

    case 'M':
        inBounds = singleIncrement(_month, value, 1, 12);
        _day = std::clamp(_day, 1, monthSize(_month, _year));
        if (inBounds)
            break;
        //else fall-through if overflow...

    case 'y':
        _year += value;
        break;

    default:
        throw ghoul::RuntimeError(
            "Invalid unit format in TQ incrementOnce '" + std::to_string(unit) +
            "'. Expected 'y', 'M', 'd', 'h', or 'm'."
        );
    }
}

bool DateTime::singleIncrement(int& oper, int& val, int min, int max) {
    oper += val;
    if (oper <= max) {
        return true;
    }
    oper = oper - max - (1 - min);
    //Only single increments for the less-significant units on rollover
    val = 1;
    return false;
}

int DateTime::decrement(int value, char unit, double error, double resolution) {
    unsigned int nDecrements = std::abs(static_cast<int>(error / resolution));
    if (nDecrements == 0) {
        nDecrements = 1;
    }
    for (unsigned int i = 0; i < nDecrements; ++i) {
        decrementOnce(value, unit);
    }
    return nDecrements;
}

void DateTime::decrementOnce(int value, char unit) {
    bool inBounds = true;
    switch (unit) {
    case 'm':
        if (singleDecrement(_minute, value, 0, 59))
            break;
        //else fall-through if underflow...

    case 'h':
        if (singleDecrement(_hour, value, 0, 23))
            break;
        //else fall-through if underflow...

    case 'd':
        if (singleDecrement(_day, value, 1,
            monthSize(_month == 1 ? 12 : _month - 1, _year)))
        {
            break;
        }
        //else fall-through if underflow...

    case 'M':
        inBounds = singleDecrement(_month, value, 1, 12);
        _day = std::clamp(_day, 1, monthSize(_month, _year));
        if (inBounds)
            break;
        //else fall-through if underflow...

    case 'y':
        _year -= value;
        break;

    default:
        throw ghoul::RuntimeError(
            "Invalid unit format in TQ decrementOnce '" + std::to_string(unit) +
            "'. Expected 'y', 'M', 'd', 'h', or 'm'."
        );
    }
}

bool DateTime::singleDecrement(int& oper, int& val, int min, int max) {
    oper -= val;
    if (oper >= min) {
        return true;
    }
    oper = oper + max + (1 - min);
    //Only single increments for the less-significant units on rollover
    val = 1;
    return false;
}

int DateTime::monthSize(int month, int year) {
    bool leap = ((year - 2000) % 4) == 0;

    switch (month) {
    case 2:
        return (leap) ? 29 : 28;
        break;

    case 4:
    case 6:
    case 9:
    case 11:
        return 30;

    case 1:
    case 3:
    case 5:
    case 7:
    case 8:
    case 10:
    case 12:
    default:
        return 31;
    }
}

int DateTime::year() {
    return _year;
}
int DateTime::month() {
    return _month;
}
int DateTime::day() {
    return _day;
}
int DateTime::hour() {
    return _hour;
}
int DateTime::minute() {
    return _minute;
}
int DateTime::second() {
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

RangedTime::RangedTime(const std::string start, const std::string end)
    : _start(start),
      _end(end)
{
    setStart(start);
    setEnd(end);
}

void RangedTime::setStart(const std::string start) {
    Time t1;
    t1.setTime(start);
    _startJ2000 = t1.j2000Seconds();
    _start = start;
}

void RangedTime::setEnd(const std::string end) {
    Time t2;
    t2.setTime(end);
    _endJ2000 = t2.j2000Seconds();
    _end = end;
}

bool RangedTime::includes(const std::string& checkTime) {
    Time t;
    t.setTime(checkTime);
    double tj = t.j2000Seconds();
    return (_startJ2000 <= tj && tj <= _endJ2000);
}

std::string RangedTime::clamp(const std::string& checkTime) {
    Time t;
    t.setTime(checkTime);
    double tj = t.j2000Seconds();
    if (tj < _startJ2000) {
        return _start;
    }
    else if (tj > _endJ2000) {
        return _end;
    }
    else {
        return checkTime;
    }
}

std::string RangedTime::start() {
    return _start;
}

std::string RangedTime::end() {
    return _end;
}

} // namespace openspace::globebrowsing
