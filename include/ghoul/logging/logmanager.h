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

#ifndef __GHOUL___LOGMANAGER___H__
#define __GHOUL___LOGMANAGER___H__

#include <ghoul/logging/loglevel.h>
#include <ghoul/misc/boolean.h>
#include <array>
#include <memory>
#include <string_view>
#include <vector>

namespace ghoul::logging {

class Log;

/**
 * The central singleton class that is responsible for handling Log%s and logging methods.
 * This singleton class provides methods to add new Log%s, remove Log%s, and relay
 * messages to all Log%s added to the LogManager. A log message consists of a LogLevel, a
 * category and a message. The category is mainly used as a prefix and/or grouping within
 * the log files and may have other meanings depending on the specific Log%s. The
 * LogManager instance has to be initialized with the #initialize method and can be
 * accessed using the #ref method afterwards. Initializing an instance twice or accessing
 * an uninitialized LogManager will result in an assertion. The logging is performed
 * thread-safe.
 *
 * The different LogLevel available are: LogLevel::Debug, LogLevel::Info,
 * LogLevel::Warning, LogLevel::Error, LogLevel::Fatal.
 *
 * If a LogManager was created with a LogLevel x, all messages with LogLevel `y <= x` will
 * be passed to Log handler.
 *
 * Macros are defined to make logging messages easier. These macros are: #LDEBUG,
 * #LDEBUGC, #LINFO, #LINFOC, #LWARNING, #LWARNINGC, #LERROR, #LERRORC, #LFATAL, #LFATALC.
 * The *C versions of the macros requires the category and the message as a parameter. The
 * versions without the C require an `std::string` variable named `_loggerCat` to be
 * defined in the scope of the macro "call".
 */
class LogManager {
public:
    BooleanType(ImmediateFlush);

    static void initialize(LogLevel level = LogLevel::Info,
        ImmediateFlush immediateFlush = ImmediateFlush::No);
    static void deinitialize();
    static bool isInitialized();
    static LogManager& ref();

    /**
     * Creates and initializes an empty LogManager with the passed LogLevel.
     *
     * \param level The lowest LogLevel that will be passed to the containing Log%s
     * \param immediateFlush Determines if all Log%s will be flushed out immediately
     *        after a message was received. In the case of file-backed logs, the files
     *        will be written out to disk and in case of a console log, the console will
     *        be updated. Passing `true` will slow down the execution but guarantees that
     *        a crash immediately after a log message won't lead to data loss
     */
    LogManager(LogLevel level = LogLevel::Info,
        ImmediateFlush immediateFlush = ImmediateFlush::No);

    /**
     * The main method to log messages. If the `level` is >= the level this LogManager was
     * created with, the `message` will be passed to the stored Log%s. The `category` will
     * be used in different ways depending on the Log in question, but examples are
     * grouping or prepending to the message.
     *
     * \param level The level of the message that should be passed to the Log%s
     * \param category The category of the message, which will be used depending on the
     *        Log%s
     * \param message The message that will be passed to the Log%s. May contain control
     *        sequences
     */
    void logMessage(LogLevel level, std::string_view category, std::string_view message);

    /**
     * The main method to log messages. If the `level` is >= the level this LogManager was
     * created with, the `message` will be passed to the stored Log%s. The `category` of
     * the message will be an empty string, which causes it to be ignored by most Log%s.
     *
     * \param level The level of the message that should be passed to the Log%s
     * \param message The message that will be passed to the Log%s. May contain control
     *        sequences
     */
    void logMessage(LogLevel level, std::string_view message);

    /**
     * Returns the LogLevel that this LogManager has been initialized with. This method is
     * inlined as it is used in the LOGC macro and it might lead the compiler to do some
     * optimization for loglevels that are unwanted.
     *
     * \return The LogLevel that this LogManager has been initialized with
     */
    LogLevel logLevel() const;

    /**
     * Returns the message counter status for the passed LogLevel \p level.
     *
     * \param level The LogLevel for which the counter is returned
     * \return The number of messages that have been logged for the passed \p level since
     *         creation of the LogManager or the last call to resetCounter
     */
    int messageCounter(LogLevel level);

    /**
     * Resets the internal log message counting back to 0 for all log levels.
     */
    void resetMessageCounters();

    /**
     * Adds the passed log to the list of managed Log%s.
     *
     * \param log The Log that should be added to this LogManager
     */
    void addLog(std::unique_ptr<Log> log);

    /**
     * Removes the passed log from the list of managed Log%s. This transfers the ownership
     * of the Log back to the caller and he is responsible for deleting the Log. Trying to
     * remove a Log that is not part of this LogManager has no effect and is permitted.
     *
     * \param log The Log that should be removed from this LogManager
     */
    void removeLog(Log* log);

    /**
     * Flushes all of the registered Log%s. This can be useful in cases when an
     * unscheduled shutdown is imminent, but all messages must be written first. Will call
     * the Log::flush method on all logs.
     */
    void flushLogs();

private:
    static LogManager* _instance;

    /// The LogLevel
    LogLevel _level;

    /// Whether all logs should be flushed immediately
    ImmediateFlush _immediateFlush;

    /// Stores the Logs which are managed by this LogManager
    std::vector<std::unique_ptr<Log>> _logs;

    /// Stores the number of messages for each log level (7)
    std::array<int, 7> _logCounters = { 0, 0, 0, 0, 0, 0, 0 };
};

} // namespace ghoul::logging

#define LogMgr (ghoul::logging::LogManager::ref())

inline void log(ghoul::logging::LogLevel level, std::string_view category,
    std::string_view message);

#define LTRACE(__msg__) LTRACEC(_loggerCat, __msg__)
inline void LTRACEC(std::string_view category, std::string_view message);

#define LDEBUG(__msg__) LDEBUGC(_loggerCat, __msg__)
inline void LDEBUGC(std::string_view category, std::string_view message);

#define LINFO(__msg__) LINFOC(_loggerCat, __msg__)
inline void LINFOC(std::string_view category, std::string_view message);

#define LWARNING(__msg__) LWARNINGC(_loggerCat, __msg__)
inline void LWARNINGC(std::string_view category, std::string_view message);

#define LERROR(__msg__) LERRORC(_loggerCat, __msg__)
inline void LERRORC(std::string_view category, std::string_view message);

#define LFATAL(__msg__) LFATALC(_loggerCat, __msg__)
inline void LFATALC(std::string_view category, std::string_view message);

#include "logmanager.inl"

#endif // __GHOUL___LOGMANAGER___H__
