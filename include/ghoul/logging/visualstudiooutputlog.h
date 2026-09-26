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

#ifndef __GHOUL___VISUALSTUDIOOUTPUTLOG___H__
#define __GHOUL___VISUALSTUDIOOUTPUTLOG___H__

#include <ghoul/logging/log.h>

namespace ghoul::logging {

/**
 * A concrete subclass of Log that logs the messages to the Visual Studio output window or
 * the system debugger using the `OutputDebugString` method. If neither Visual Studio nor
 * a system debugger is attached, this log will silently ignore all incoming log messages.
 *
 * If this logger is included in any non `WIN32` system, it will also silently ignore all
 * logging messages
 */
class VisualStudioOutputLog : public Log {
public:
    /**
     * Constructor that calls Log constructor.
     *
     * \param timeStamping Determines if the log should print the time when a message is
     *        logged in the log messages
     * \param dateStamping Determines if the log should print the time when a message is
     *        logged in the log messages
     * \param categoryStamping Determines if the log should print the categories in the
     *        log messages
     * \param logLevelStamping Determines if the log should print the log level in the
     *        log messages
     * \param minimumLogLevel The minimum log level that this logger will accept
     */
    VisualStudioOutputLog(TimeStamping timeStamping = TimeStamping::No,
        DateStamping dateStamping = DateStamping::No,
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
};

} // namespace ghoul::logging

#endif // __GHOUL___VISUALSTUDIOOUTPUTLOG___H__
