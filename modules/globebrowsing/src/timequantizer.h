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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TIMEQUANTIZER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TIMEQUANTIZER___H__

#include <openspace/util/timerange.h>
#include <string>
#include <vector>

namespace openspace { class Time; }

namespace openspace::globebrowsing {

    class RangedTime {
    public:
        RangedTime() {};
        RangedTime(const std::string start, const std::string end);
        bool includes(const std::string& checkTime);
        std::string clamp(const std::string& checkTime);
        std::string start();
        std::string end();
        void setStart(const std::string start);
        void setEnd(const std::string start);

    private:
        std::string _start;
        std::string _end;
        double _startJ2000;
        double _endJ2000;
    };

    class DateTime {
    public:
        DateTime() {};
        DateTime(std::string initDateTime);
        void operator= (const DateTime& src);

        std::string ISO8601();
        double J2000();
        void increment(int value, char unit);
        void decrement(int value, char unit);
        bool singleIncrement(int& oper, int& val, int min, int max);
        bool singleDecrement(int& oper, int& val, int min, int max);
        int monthSize(int month, int year);

        int year();
        int month();
        int day();
        int hour();
        int minute();
        int second();
        void setYear(int);
        void setMonth(int);
        void setDay(int);
        void setHour(int);
        void setMinute(int);
        void setSecond(int);

        const int index_year = 0;
        const int index_month = 5;
        const int index_day = 8;
        const int index_hour = 11;
        const int index_minute = 14;
        const int index_second = 17;

        const int len_year = 4;
        const int len_nonYear = 2;

    private:
        int _year;
        int _month;
        int _day;
        int _hour;
        int _minute;
        int _second;
    };

/**
* Used to quantize time to descrete values.
*/
class TimeQuantizer {
    TimeQuantizer() = default;
    TimeQuantizer(const std::string& start, const std::string& end, double resolution);
    TimeQuantizer(const std::string& start, const std::string& end,
        const std::string& resolution);

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
    bool quantize(Time& t, bool clamp) const;

    /**
    * Returns a list of quantized Time objects that represent all the valid quantized
    * Time%s between \p start and \p end.
    *
    * \param start The start time for the time range quantization
    * \param end The end time for the time range quantization
    * \return A list of quantized times between \p start and \end
    */
    std::vector<Time> quantized(const Time& start, const Time& end) const;

    double diff(DateTime& from, DateTime& to);

    void doFastForwardApproximation(DateTime& dt, double value, char unit);

private:
    double computeSecondsFromResolution(const int valueIn, const char unit);
    RangedTime _timerange;
    double _resolution = 0.0;
    double _resolutionValue = 0.0;
    char _resolutionUnit = 'd';
    DateTime _dt;
    DateTime _start;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TIMEQUANTIZER___H__
