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

#ifndef __GHOUL___HTMLLOG___H__
#define __GHOUL___HTMLLOG___H__

#include <ghoul/logging/textlog.h>

#include <vector>

namespace ghoul::logging {

/**
 * A subclass of TextLog that logs the messages to a structured HTML file on hard disk.
 * The log containing all components will contain a table with the following format:
 * ```
 * --------------------------------------------
 * | DATE | TIME | CATEGORY | LEVEL | MESSAGE |
 * |      |      |          |       |         |
 * |      |      |          |       |         |
 * ```
 * If a specific value should not be stamped, the according table entry will be missing
 * from the HTML file. The file will be opened in the constructor and closed in the
 * destructor of this class. A HTMLLog is always created anew and cannot be appended to.
 * That means that the user of this class has to perform log file rotation.
 */
class HTMLLog : public TextLog {
public:
    /**
     * Constructor that calls TextLog constructor and opens the file that will log the
     * messages. If the file does not exist, it will be created.
     *
     * \param filename The path and filename of the file that will receive the log
     *        messages
     * \param nLogRotation The number of log files that should be kept. If this is 0, only
     *        a single file will be used. If this is 3, there will be files
     *        `filename.ext`, `filename-1.ext`, and `filename-2.ext` with the numbered
     *        files being the previous versions of the log file
     * \param timeStamping Determines if the log should print the time when a message is
     *        logged in the log messages
     * \param dateStamping Determines if the log should print the time when a message is
     *        logged in the log messages
     * \param categoryStamping Determines if the log should print the categories in the
     *        log messages
     * \param logLevelStamping Determines if the log should print the log level in the log
     *        messages
     * \param cssIncludes The list of CSS files that are included in the header of the
     *        HTML file
     * \param jsIncludes The list of JavaScript files that are included in the header of
     *        the HTML file
     * \param minimumLogLevel The minimum log level that this logger will accept
     *
     * \pre \p filename must not be empty
     */
    HTMLLog(const std::filesystem::path& filename, int nLogRotation,
        TimeStamping timeStamping = TimeStamping::Yes,
        DateStamping dateStamping = DateStamping::Yes,
        CategoryStamping categoryStamping = CategoryStamping::Yes,
        LogLevelStamping logLevelStamping = LogLevelStamping::Yes,
        const std::vector<std::filesystem::path>& cssIncludes
            = std::vector<std::filesystem::path>(),
        const std::vector<std::filesystem::path>& jsIncludes
            = std::vector<std::filesystem::path>(),
        LogLevel minimumLogLevel = LogLevel::AllLogging);

    /**
     * Destructor that closes and finalizes the HTML file.
     */
    ~HTMLLog() override;

    /**
     * Method that logs a message with a given \p level and \p category to the text file.
     *
     * \param level The log level with which the message shall be logged
     * \param category The category of this message. Can be used by each subclass
     *        individually
     * \param message The message body of the log message
     */
    void log(LogLevel level, std::string_view category,
        std::string_view message) override;

protected:
    /**
     * Returns a css class string for the passed level:
     *   - LogLevel::Trace -> `log-level-trace`
     *   - LogLevel::Debug -> `log-level-debug`
     *   - LogLevel::Info -> `log-level-info`
     *   - LogLevel::Warning -> `log-level-warning`
     *   - LogLevel::Error -> `log-level-error`
     *   - LogLevel::Fatal -> `log-level-fatal`
     */
    static std::string classForLevel(LogLevel level);

    /**
     * Returns a HTML color string for the passed color:
     *   - LogLevel::Trace -> Grey
     *   - LogLevel::Debug -> Green
     *   - LogLevel::Info -> Black
     *   - LogLevel::Warning -> Yellow
     *   - LogLevel::Error -> Red
     *   - LogLevel::Fatal -> Cyan
     */
    static std::string colorForLevel(LogLevel level);

private:
    const bool _useCustomStyling;
};

} // namespace ghoul::logging

#endif // __GHOUL___HTMLLOG___H__
