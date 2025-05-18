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

#ifndef __OPENSPACE_MODULE_SERVER___NOTIFICATIONLOG___H__
#define __OPENSPACE_MODULE_SERVER___NOTIFICATIONLOG___H__

#include <ghoul/logging/log.h>

#include <ghoul/misc/profiling.h>
#include <functional>

namespace openspace {

/**
 * A concrete subclass of Log that passes logs to the provided callback function. The
 * callback is specified using `std::function`. Trying to log messages when the callback
 * object has been deleted results in undefined behavior.
 */
class NotificationLog : public ghoul::logging::Log {
public:
    /// The type of function that is used as a callback in this log
    using CallbackFunction = std::function<void(
        std::string_view timeString,
        std::string_view dateString,
        std::string_view category,
        ghoul::logging::LogLevel logLevel,
        std::string_view message)>;

    /**
     * Constructor that calls Log constructor.
     *
     * \param callbackFunction The callback function that is called for each log message
     * \param minimumLogLevel The minimum log level that this logger will accept
     */
    NotificationLog(CallbackFunction callbackFunction,
        ghoul::logging::LogLevel minimumLogLevel = ghoul::logging::LogLevel::Warning);

    /**
     * Method that logs a message with a given level and category to the console.
     *
     * \param level The log level with which the message shall be logged
     * \param category The category of this message.
     * \param message The message body of the log message
     */
    void log(ghoul::logging::LogLevel level, std::string_view category,
        std::string_view message) override;

private:
    CallbackFunction _callbackFunction;
    TracyLockable(std::mutex, _mutex);
};

} // namespace ghoul::logging

#endif // __OPENSPACE_MODULE_SERVER___NOTIFICATIONLOG___H__
