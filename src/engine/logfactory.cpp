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

#include <openspace/engine/logfactory.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/loglevel.h>
#include <ghoul/logging/htmllog.h>
#include <ghoul/logging/textlog.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>
#include <optional>

namespace {
    constexpr std::string_view BootstrapPath = "${WEB}/common/bootstrap.min.css";
    constexpr std::string_view CssPath = "${WEB}/log/style.css";
    constexpr std::string_view JsPath = "${WEB}/log/script.js";

    struct [[codegen::Dictionary(LogFactory)]] Parameters {
        enum class Type {
            Html [[codegen::key("html")]],
            Text
        };
        // The type of the new log to be generated
        Type type;

        // The filename to which the log will be written
        std::string file;

        // Determines whether the file will be cleared at startup or if the contents will
        // be appended to previous runs
        std::optional<bool> append;

        // The number of files that should be kept around for this Log
        std::optional<int> logRotation [[codegen::greater(0)]];

        // Determines whether the log entires should be stamped with the time at which the
        // message was logged
        std::optional<bool> timeStamping;

        // Determines whether the log entries should be stamped with the date at which the
        // message was logged
        std::optional<bool> dateStamping;

        // Determines whether the log entries should be stamped with the category that
        // creates the log message
        std::optional<bool> categoryStamping;

        // Determines whether the log entries should be stamped with the log level that
        // was used to create the log message
        std::optional<bool> logLevelStamping;

        enum class [[codegen::map(ghoul::logging::LogLevel)]] LogLevel {
            AllLogging,
            Trace,
            Debug,
            Info,
            Warning,
            Error,
            Fatal,
            NoLogging
        };
        // The log level for this specific text-based log
        std::optional<LogLevel> logLevel;
    };
#include "logfactory_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation LogFactoryDocumentation() {
    return codegen::doc<Parameters>("core_logfactory");
}

std::unique_ptr<ghoul::logging::Log> createLog(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    const std::filesystem::path filename = absPath(p.file);
    const bool append = p.append.value_or(true);
    const int nLogRotation = p.logRotation.value_or(0);
    const bool timeStamp = p.timeStamping.value_or(true);
    const bool dateStamp = p.dateStamping.value_or(true);
    const bool categoryStamp = p.categoryStamping.value_or(true);
    const bool logLevelStamp = p.logLevelStamping.value_or(true);
    const ghoul::logging::LogLevel level = codegen::map<ghoul::logging::LogLevel>(
        p.logLevel.value_or(Parameters::LogLevel::AllLogging)
    );

    switch (p.type) {
        case Parameters::Type::Html:
        {
            const std::vector<std::filesystem::path> cssFiles = {
                absPath(BootstrapPath),
                absPath(CssPath)
            };
            const std::vector<std::filesystem::path> jsFiles = { absPath(JsPath) };

            return std::make_unique<ghoul::logging::HTMLLog>(
                filename,
                nLogRotation,
                ghoul::logging::Log::TimeStamping(timeStamp),
                ghoul::logging::Log::DateStamping(dateStamp),
                ghoul::logging::Log::CategoryStamping(categoryStamp),
                ghoul::logging::Log::LogLevelStamping(logLevelStamp),
                cssFiles,
                jsFiles,
                level
            );
        }
        case Parameters::Type::Text:
            return std::make_unique<ghoul::logging::TextLog>(
                filename,
                nLogRotation,
                ghoul::logging::TextLog::Append(append),
                ghoul::logging::Log::TimeStamping(timeStamp),
                ghoul::logging::Log::DateStamping(dateStamp),
                ghoul::logging::Log::CategoryStamping(categoryStamp),
                ghoul::logging::Log::LogLevelStamping(logLevelStamp),
                level
            );
        default:
            throw ghoul::MissingCaseException();
    }
}

} // namespace openspace
