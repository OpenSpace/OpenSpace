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

#include <ghoul/logging/htmllog.h>

#include <ghoul/format.h>
#include <ghoul/misc/assert.h>
#include <iterator>

namespace ghoul::logging {

HTMLLog::HTMLLog(const std::filesystem::path& filename, int nLogRotation,
                 TimeStamping timeStamping, DateStamping dateStamping,
                 CategoryStamping categoryStamping, LogLevelStamping logLevelStamping,
                 const std::vector<std::filesystem::path>& cssIncludes,
                 const std::vector<std::filesystem::path>& jsIncludes,
                 LogLevel minimumLogLevel)
    : TextLog(
        filename,
        nLogRotation,
        Append::No,
        timeStamping,
        dateStamping,
        categoryStamping,
        logLevelStamping,
        minimumLogLevel
    )
    , _useCustomStyling(cssIncludes.size() > 1 || jsIncludes.size() > 1)
{
    std::string output = "<html>\n\t<head>\n\t\t<title>Log File</title>\n\t\t<style>\n";
    const std::back_insert_iterator<std::string> backInserter(output);

    for (const std::filesystem::path& c : cssIncludes) {
        std::ifstream cssInput = std::ifstream(c);
        std::copy(
            std::istreambuf_iterator<char>{cssInput},
            std::istreambuf_iterator<char>(),
            backInserter
        );
    }

    output += "\t\t</style>\n\t\t<script>\n";

    for (const std::filesystem::path& j : jsIncludes) {
        std::ifstream jsInput = std::ifstream(j);
        std::copy(
            std::istreambuf_iterator<char>{jsInput},
            std::istreambuf_iterator<char>(),
            backInserter
        );
    }

    output += "\t\t</script>\n\t</head>\n\t<body>\n\t<table>\n\t\t<thead>\n\t\t\t<tr>\n";
    if (isDateStamping()) {
        output += "\t\t\t\t<th class=\"log-date\">Date</th>\n";
    }
    if (isTimeStamping()) {
        output += "\t\t\t\t<th class=\"log-time\">Time</th>\n";
    }
    if (isCategoryStamping()) {
        output += "\t\t\t\t<th class=\"log-category\">Category</th>\n";
    }
    if (isLogLevelStamping()) {
        output += "\t\t\t\t<th class=\"log-level\">Level</th>\n";
    }
    output +=
        "\t\t\t\t<th class=\"log-message\">Message</th>\n\t\t\t</tr>\n\t\t<tbody>\n";
    writeLine(output);
}

HTMLLog::~HTMLLog() {
    writeLine("\t\t</tbody>\n\t</table>\n\t</body>\n</html>");
}

void HTMLLog::log(LogLevel level, std::string_view category, std::string_view message) {
    std::string output;
    if (_useCustomStyling) {
        output = "\t\t\t<tr class=\"" + classForLevel(level) + "\">\n";
    }
    else {
        output = "\t\t\t<tr bgcolor=\"" + colorForLevel(level) + "\">\n";
    }
    if (isDateStamping()) {
        output += "\t\t\t\t<td class=\"log-date\">" + dateString() + "</td>\n";
    }
    if (isTimeStamping()) {
        output += "\t\t\t\t<td class=\"log-time\">" + timeString() + "</td>\n";
    }
    if (isCategoryStamping()) {
        output += "\t\t\t\t<td class=\"log-category\">";
        output += category;
        output += "</td>\n";
    }
    if (isLogLevelStamping()) {
        output += std::format(
            "\t\t\t\t<td class=\"log-level\">{}</td>\n", to_string(level)
        );
    }

    output += "\t\t\t\t<td class=\"log-message\">";
    for (const char c : message) {
        switch (c) {
            case '<':
                output += "&lt;";
                break;
            case '>':
                output += "&gt;";
                break;
            case '&':
                output += "&amp;";
                break;
            case '\n':
                output += "<br>";
                break;
            default:
                output += c;
                break;
        }
    }

    output += "</td>\n\t\t\t</tr>\n";
    writeLine(output);
}

std::string HTMLLog::classForLevel(LogLevel level) {
    switch (level) {
        case LogLevel::Trace:     return "log-level-trace";
        case LogLevel::Debug:     return "log-level-debug";
        case LogLevel::Info:      return "log-level-info";
        case LogLevel::Warning:   return "log-level-warning";
        case LogLevel::Error:     return "log-level-error";
        case LogLevel::Fatal:     return "log-level-fatal";
        case LogLevel::NoLogging: return "log-level-no-logging";
        default:                  throw MissingCaseException();
    }
}

std::string HTMLLog::colorForLevel(LogLevel level) {
    switch (level) {
        case LogLevel::Trace:     return "#999999";
        case LogLevel::Debug:     return "#00CC00";
        case LogLevel::Info:      return "#FFFFFF";
        case LogLevel::Warning:   return "#FFFF00";
        case LogLevel::Error:     return "#FF0000";
        case LogLevel::Fatal:     return "#00FFFF";
        case LogLevel::NoLogging: return "#FFFFFF";
        default:                  throw MissingCaseException();
    }
}

} // namespace ghoul::logging
