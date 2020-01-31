/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

namespace openspace::globebrowsing {

TimeQuantizer::TimeQuantizer(const std::string& start, const std::string& end,
    double resolution)
    : _start(start)
    , _resolution(resolution)
{
    setStartEndRange(start, end);
}

TimeQuantizer::TimeQuantizer(const std::string& start, const std::string& end,
                             const std::string& resolution)
    : TimeQuantizer(start, end, parseTimeResolutionStr(resolution))
{}

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
        return computeSecondsFromResolution(value, unit);
    }
}

void TimeQuantizer::setStartEndRange(const std::string& start, const std::string& end) {
    _start.setTime(start);
    _timerange.setStart(start);
    _timerange.setEnd(end);
}

void TimeQuantizer::setResolution(const std::string& resolutionString) {
    _resolution = parseTimeResolutionStr(resolutionString);
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
    case 'h': value *= 60.0;
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
            "Invalid unit format '" + std::string(1, unit) +
            "'. Expected 'y', 'd', 'h', 'm' or 's'."
        );
    }
    return value;
}

bool TimeQuantizer::quantize(Time& t, bool clamp) {
    const std::string unquantized = t.ISO8601();
    DateTime unquantizedDt(unquantized);
    double error = 0.0;

    if (_timerange.includes(unquantized)) {
        DateTime quantized = DateTime(_timerange.start());
        doFastForwardApproximation(quantized, _resolutionValue, _resolutionUnit);
        while (error = diff(quantized, unquantizedDt) > 0 && error > _resolution) {
            if (error > _resolution) {
                quantized.increment(_resolutionValue, _resolutionUnit);
            }
            else {
                quantized.decrement(_resolutionValue, _resolutionUnit);
            }
        }
        quantized.setTime(_timerange.clamp(quantized.ISO8601()));
        t.setTime(quantized.J2000());
        return true;
    }
    else if (clamp) {
        const std::string clampedTime = _timerange.clamp(unquantized);
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

void TimeQuantizer::doFastForwardApproximation(DateTime& dt, double value, char unit) {
    switch (unit) {
    case 'y':
        _start.setYear(_start.year() + static_cast<int>((dt.year() - _start.year()) / unit));
        break;

    case 'M':
        _start.setYear((dt.month() > unit) ? dt.year() : dt.year() - 1);
        break;

    case 'd':
        if (dt.month() > 1) {
            _start.setYear(dt.year());
            _start.setMonth(dt.month() - 1);
        }
        else {
            _start.setYear(dt.year() - 1);
            _start.setMonth(12);
        }
        break;

    case 'h':
        _start = dt;
        if (dt.hour() >= 12) {
            _start.setHour(0);
        }
        else {
            dt.decrement(1, 'd');
        }
        break;

    case 'm':
        _start = dt;
        if (dt.hour() > 0) {
            _start.setHour(dt.hour() - 1);
        }
        else {
            dt.decrement(1, 'd');
        }
        break;
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
        itr.increment(_resolutionValue, _resolutionUnit);
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
    char str[19];
    snprintf(str + index_year, len_year + 1, "%04d-", _year);
    snprintf(str + index_month, len_nonYear + 1, "%02d-", _month);
    snprintf(str + index_day, len_nonYear + 1, "%02dT", _day);
    snprintf(str + index_hour, len_nonYear + 1, "%02d:", _hour);
    snprintf(str + index_minute, len_nonYear + 1, "%02d:", _minute);
    snprintf(str + index_second, len_nonYear, "%02d", _second);
    return std::string(str);
};

double DateTime::J2000() {
    Time t;
    t.setTime(ISO8601());
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

void DateTime::increment(int value, char unit) {
    int min, max = 0;

    switch (unit) {
    case 'm':
        if (singleIncrement(_minute, value, 0, 59))
            break;
        //fall-through...

    case 'h':
        if (singleIncrement(_hour, value, 0, 23))
            break;
        //fall-through...

    case 'd':
        if (singleIncrement(_day, value, 1, monthSize(_month, _year)))
            break;
        //fall-through...

    case 'M':
        if (singleIncrement(_minute, value, 1, 12))
            break;
        //fall-through...

    case 'Y':
        _year += value;
        break;
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

void DateTime::decrement(int value, char unit) {
    int min, max = 0;

    switch (unit) {
    case 'm':
        if (singleDecrement(_minute, value, 0, 59))
            break;
        //fall-through...

    case 'h':
        if (singleDecrement(_hour, value, 0, 23))
            break;
        //fall-through...

    case 'd':
        if (singleDecrement(_day, value, 1, monthSize(_month, _year)))
            break;
        //fall-through...

    case 'M':
        if (singleDecrement(_minute, value, 1, 12))
            break;
        //fall-through...

    case 'Y':
        _year += value;
        break;
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
    Time t1;
    t1.setTime(start);
    _startJ2000 = t1.j2000Seconds();
    Time t2;
    t2.setTime(start);
    _endJ2000 = t2.j2000Seconds();
}

void RangedTime::setStart(const std::string start) {
    _start = start;
}

void RangedTime::setEnd(const std::string end) {
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
