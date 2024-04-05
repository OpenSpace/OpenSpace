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

#ifndef __OPENSPACE_CORE___SCREENLOG___H__
#define __OPENSPACE_CORE___SCREENLOG___H__

#include <ghoul/logging/log.h>

#include <ghoul/misc/profiling.h>
#include <chrono>
#include <mutex>
#include <string_view>
#include <vector>

namespace openspace {

/**
 * The ScreenLog is an implementation of the ghoul::logging::Log abstract interface that
 * can be used to present log messages in an on-screen GUI. For this, every incoming log
 * message (#log) is tagged with the current time and all stored log messages can expire
 * based on the time-to-live as specified in the constructor (#removeExpiredEntries).
 */
class ScreenLog : public ghoul::logging::Log {
public:
    /// Just a shortcut for the LogLevel access
    using LogLevel = ghoul::logging::LogLevel;

    /**
     * This struct stores the incoming log entries with their #level, #timeString,
     * #category, #message, and the generated #timeStamp used for the expiry calculation.
     */
    struct LogEntry {
        /// The ghoul::logging::LogLevel of the log message
        LogLevel level;

        /// The timepoint when the log message arrived at the ScreenLog
        std::chrono::time_point<std::chrono::steady_clock> timeStamp;

        /// The time string as retrieved from the log message
        std::string timeString;

        /// The category as retrieved from the log message
        std::string category;

        /// The actual message of the log entry
        std::string message;
    };

    /**
     * Constructor that creates a ScreenLog with the provided \p timeToLive, and the
     * minimum \p logLevel that is stored. Log message with a lower
     * ghoul::logging::LogLevel are automatically discarded.
     *
     * \param timeToLive The time-to-live for the messages in this ScreenLog. Expired
     *        messages are removed whenever the #removeExpiredEntries method is called
     * \param logLevel The minimum ghoul::logging::LogLevel that messages must
     *        have in order to be stored in the ScreenLog
     */
    ScreenLog(std::chrono::seconds timeToLive, LogLevel logLevel = LogLevel::Info);

    /**
     * Destructor
     */
    ~ScreenLog() override = default;

    /**
     * Overwritten ghoul::loggling::Log method that is called whenever a new log message
     * shall be stored.
     *
     * \param level The ghoul::logging::LogLevel of the incoming log message
     * \param category The category of the log message
     * \param message The actual log message that was transmitted
     */
    void log(ghoul::logging::LogLevel level, std::string_view category,
        std::string_view message) override;

    /**
     * This method removes all the stored LogEntry%s that have expired, calculated by
     * their `timeStamp` and the #_timeToLive value.
     *
     * \post All entries retrieved by the #entries function have a `timeStamp` that is
     *       lower than the current time + #_timeToLive. The current time used is the time
     *       when this method was last called
     */
    void removeExpiredEntries();

    /**
     * Returns the list of all stored LogEntry%s.
     *
     * \return The list of all stored LogEntry%s
     */
    const std::vector<LogEntry>& entries() const;

private:
    /// The list of all LogEntry%s stored by this ScreenLog
    std::vector<LogEntry> _entries;

    /// The time-to-live for the LogEntry%s in this ScreenLog. Is used by the
    /// #removeExpiredEntries method to remove expired entries
    std::chrono::seconds _timeToLive;

    /// The minimum LogLevel of messages
    LogLevel _logLevel;

    /// A mutex to ensure thread-safety since the logging and the removal of expired
    /// entires can occur on different threads
    mutable TracyLockable(std::mutex, _mutex);
};

} // namespace openspace

#endif // __OPENSPACE_CORE___SCREENLOG___H__
