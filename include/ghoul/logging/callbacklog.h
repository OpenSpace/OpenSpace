/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#ifndef __GHOUL___CALLBACKLOG___H__
#define __GHOUL___CALLBACKLOG___H__

#include <ghoul/logging/log.h>

#include <ghoul/misc/profiling.h>
#include <functional>
#include <mutex>

namespace ghoul::logging {

/**
 * A concrete subclass of Log that passes logs to the provided callback function. The
 * callback is specified using `std::function`. Trying to log messages when the callback
 * object has been deleted results in undefined behavior. The formatting of the log
 * messages depends on the stamping settings. The different possibilities are:
 * ```
 * [DATE | TIME] CATEGORY (LEVEL) MESSAGE
 * [DATE] CATEGORY (LEVEL) MESSAGE
 * [TIME] CATEGORY (LEVEL) MESSAGE
 * ```
 * And the remaining possibilities with `CATEGORY` and `LEVEL` missing.
 */
class CallbackLog : public Log {
public:
    /**
     * The type of function that is used as a callback in this log.
     */
    using CallbackFunction = std::function<void(std::string)>;

    /**
     * Constructor that calls the Log constructor and initializes this CallbackLog.
     *
     * \param callbackFunction The callback function that is called for each log message
     * \param timeStamping Determines if the log should print the time when a message is
     *        logged
     * \param dateStamping Determines if the log should print the time when a message is
     *        logged
     * \param categoryStamping Determines if the log should print the categories
     * \param logLevelStamping Determines if the log should print the log level
     * \param minimumLogLevel The minimum log level that this logger will accept
     */
    CallbackLog(CallbackFunction callbackFunction,
        TimeStamping timeStamping = TimeStamping::Yes,
        DateStamping dateStamping = DateStamping::Yes,
        CategoryStamping categoryStamping = CategoryStamping::Yes,
        LogLevelStamping logLevelStamping = LogLevelStamping::Yes,
        LogLevel minimumLogLevel = LogLevel::AllLogging);

    /**
     * Method that logs a message with a given level and category to the console.
     *
     * \param level The log level with which the message shall be logged
     * \param category The category of this message. Can be used by each subclass
     *        individually
     * \param message The message body of the log message
     */
    void log(LogLevel level, std::string_view category,
        std::string_view message) override;

    /**
     * Replaces the old callback with this `callbackFunction`. This function is not
     * checked and it is the caller's responsiblity to assure that the function object is
     * callable.
     *
     * \param callbackFunction The new callback function that will be called henceforth
     */
    void setCallback(CallbackFunction callbackFunction);

    /**
     * Returns the callback function that is used in this CallbackLog.
     *
     * \return The callback function that is used in this CallbackLog
     */
    const CallbackFunction& callback() const;

protected:
    CallbackFunction _callbackFunction;
    TracyLockable(std::mutex, _mutex);
};

} // namespace ghoul::logging

#endif // __GHOUL___CALLBACKLOG___H__
