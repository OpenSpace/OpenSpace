/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/sonification/include/timesonification.h>

#include <openspace/util/timemanager.h>

namespace {
    constexpr int NumSecPerDay = 86400;
    constexpr double TimePrecision = 0.0001;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        TimeSonificationInfo =
    {
       "TimeSonification",
       "Time Sonification",
       "Sonification that alters all other sonificatoins based on the current delta time"
    };

} // namespace

namespace openspace {

TimeSonification::TimeSonification(const std::string& ip, int port)
    : SonificationBase(TimeSonificationInfo, ip, port)
{
    _timeSpeed = 0.0;
}

void TimeSonification::update(const Camera*) {
    double timeSpeed = global::timeManager->deltaTime() / NumSecPerDay;

    if (abs(_timeSpeed - timeSpeed) > TimePrecision) {
        _timeSpeed = timeSpeed;

        std::string label = "/Time";
        std::vector<OscDataType> data(1);
        data[0] = _timeSpeed;

        _connection->send(label, data);
    }
}

} // namespace openspace
