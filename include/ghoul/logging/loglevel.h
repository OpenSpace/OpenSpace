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

#ifndef __GHOUL___LOGLEVEL___H__
#define __GHOUL___LOGLEVEL___H__

#include <ghoul/format.h>
#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/stringconversion.h>
#include <string_view>

namespace ghoul::logging {

/**
 * Enumerates all available LogLevel for the LogManager. The LogLevels are guaranteed to
 * be strictly ordered from least important to important.
 */
enum class LogLevel {
    /// All log messages are accepted
    AllLogging = 0,
    /// Used for high-throughput messages, for example to trace function calls
    Trace = 1,
    /// Used for Debug output
    Debug = 2,
    /// Used for informational messages which can be ignored, but might be informative
    Info = 3,
    /// Warnings which do not represent a problem in itself, but may hint to a wrong
    /// configuration
    Warning = 4,
    /// Errors which will pose problems, but do not necessarily require the immediate
    /// end of the application
    Error = 5,
    /// Error which will be so severe that the application cannot recover from them
    Fatal = 6,
    /// Used as a placeholder to inhibit all LogMessages
    NoLogging = 7
};

} // namespace ghoul::logging

namespace ghoul {

/**
 * Returns the string representation of the passed LogLevel. The name of each level is
 * equal to its enum value.
 *
 * \return The string representation of the passed LogLevel
 */
constexpr std::string_view to_string(logging::LogLevel string) {
    switch (string) {
        case logging::LogLevel::AllLogging: return "All";
        case logging::LogLevel::Trace:      return "Trace";
        case logging::LogLevel::Debug:      return "Debug";
        case logging::LogLevel::Info:       return "Info";
        case logging::LogLevel::Warning:    return "Warning";
        case logging::LogLevel::Error:      return "Error";
        case logging::LogLevel::Fatal:      return "Fatal";
        case logging::LogLevel::NoLogging:  return "None";
        default:                   throw MissingCaseException();
    }
}

/**
 * Returns the LogLevel for the passed string representation. The name of each level is
 * equal to its enum value.
 *
 * \return The LogLevel for the passed string representation
 */
template <>
constexpr logging::LogLevel from_string(std::string_view string) {
    if (string == "All") { return logging::LogLevel::AllLogging; }
    if (string == "Trace") { return logging::LogLevel::Trace; }
    if (string == "Debug") { return logging::LogLevel::Debug; }
    if (string == "Info") { return logging::LogLevel::Info; }
    if (string == "Warning") { return logging::LogLevel::Warning; }
    if (string == "Error") { return logging::LogLevel::Error; }
    if (string == "Fatal") { return logging::LogLevel::Fatal; }
    if (string == "None") { return logging::LogLevel::NoLogging; }

    throw RuntimeError(std::format("Unknown log level '{}'", string));
}

/**
 * Returns the color representation of the passed LogLevel.
 */
constexpr glm::vec4 toColor(logging::LogLevel level) {
    constexpr glm::vec4 White(0.9f, 0.9f, 0.9f, 1.f);

    switch (level) {
        case logging::LogLevel::Debug:   return glm::vec4(0.f, 1.f, 0.f, 1.f);
        case logging::LogLevel::Warning: return glm::vec4(1.f, 1.f, 0.f, 1.f);
        case logging::LogLevel::Error:   return glm::vec4(1.f, 0.f, 0.f, 1.f);
        case logging::LogLevel::Fatal:   return glm::vec4(0.3f, 0.3f, 0.85f, 1.f);
        default:                         return White;
    }
}

} // namespace

#endif // __GHOUL___LOGLEVEL___H__
