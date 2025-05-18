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

#include <modules/server/include/logging/notificationlog.h>

namespace openspace {

NotificationLog::NotificationLog(CallbackFunction callbackFunction,
                                 ghoul::logging::LogLevel minimumLogLevel)
    : ghoul::logging::Log(
        TimeStamping::Yes,
        DateStamping::Yes,
        CategoryStamping::Yes,
        LogLevelStamping::Yes,
        minimumLogLevel
    )
    , _callbackFunction(std::move(callbackFunction))
{}

void NotificationLog::log(ghoul::logging::LogLevel level, std::string_view category,
                          std::string_view message)
{
    ZoneScoped;

    const std::lock_guard lock(_mutex);
    const std::string timeStamp = timeString();
    const std::string dateStamp = dateString();
    _callbackFunction(timeStamp, dateStamp, category, level, message);
}

} // namespace openspace
