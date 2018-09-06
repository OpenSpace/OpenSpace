/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

namespace {
    constexpr const char* KeyType = "Type";
    constexpr const char* KeyFilename = "File";
    constexpr const char* KeyAppend = "Append";
    constexpr const char* KeyTimeStamping = "TimeStamping";
    constexpr const char* KeyDateStamping = "DateStamping";
    constexpr const char* KeyCategoryStamping = "CategoryStamping";
    constexpr const char* KeyLogLevelStamping = "LogLevelStamping";
    constexpr const char* KeyLogLevel = "LogLevel";

    constexpr const char* ValueHtmlLog = "html";
    constexpr const char* ValueTextLog = "Text";

    constexpr const char* BootstrapPath = "${WEB}/common/bootstrap.min.css";
    constexpr const char* CssPath = "${WEB}/log/style.css";
    constexpr const char* JsPath = "${WEB}/log/script.js";
} // namespace

namespace openspace {

documentation::Documentation LogFactoryDocumentation() {
    using namespace documentation;

    return {
        "LogFactory",
        "core_logfactory",
        {
            {
                KeyType,
                new StringInListVerifier({
                    // List from createLog
                    ValueTextLog, ValueHtmlLog
                }),
                Optional::No,
                "The type of the new log to be generated."
            },
            {
                KeyFilename,
                new StringVerifier,
                Optional::No,
                "The filename to which the log will be written."
            },
            {
                KeyAppend,
                new BoolVerifier,
                Optional::Yes,
                "Determines whether the file will be cleared at startup or if the "
                "contents will be appended to previous runs."
            },
            {
                KeyTimeStamping,
                new BoolVerifier,
                Optional::Yes,
                "Determines whether the log entires should be stamped with the time at "
                "which the message was logged."
            },
            {
                KeyDateStamping,
                new BoolVerifier,
                Optional::Yes,
                "Determines whether the log entries should be stamped with the date at "
                "which the message was logged."
            },
            {
                KeyCategoryStamping,
                new BoolVerifier,
                Optional::Yes,
                "Determines whether the log entries should be stamped with the "
                "category that creates the log message."
            },
            {
                KeyLogLevelStamping,
                new BoolVerifier,
                Optional::Yes,
                "Determines whether the log entries should be stamped with the log level "
                "that was used to create the log message."
            }
        }
    };
}

std::unique_ptr<ghoul::logging::Log> createLog(const ghoul::Dictionary& dictionary) {
    documentation::testSpecificationAndThrow(
        LogFactoryDocumentation(),
        dictionary,
        "LogFactory"
    );

    // 'type' and 'filename' are required keys
    const std::string& type = dictionary.value<std::string>(KeyType);
    const std::string& filename = absPath(dictionary.value<std::string>(KeyFilename));

    // the rest are optional
    bool append = true;
    if (dictionary.hasKeyAndValue<bool>(KeyAppend)) {
        append = dictionary.value<bool>(KeyAppend);
    }
    bool timeStamp = true;
    if (dictionary.hasKeyAndValue<bool>(KeyTimeStamping)) {
        timeStamp = dictionary.value<bool>(KeyTimeStamping);
    }
    bool dateStamp = true;
    if (dictionary.hasKeyAndValue<bool>(KeyDateStamping)) {
        dateStamp = dictionary.value<bool>(KeyDateStamping);
    }
    bool categoryStamp = true;
    if (dictionary.hasKeyAndValue<bool>(KeyCategoryStamping)) {
        categoryStamp = dictionary.value<bool>(KeyCategoryStamping);
    }
    bool logLevelStamp = true;
    if (dictionary.hasKeyAndValue<bool>(KeyLogLevelStamping)) {
        logLevelStamp = dictionary.value<bool>(KeyLogLevelStamping);
    }
    std::string logLevel;
    if (dictionary.hasKeyAndValue<std::string>(KeyLogLevel)) {
        logLevel = dictionary.value<std::string>(KeyLogLevel);
    }

    using Append = ghoul::logging::TextLog::Append;
    using TimeStamping = ghoul::logging::Log::TimeStamping;
    using DateStamping = ghoul::logging::Log::DateStamping;
    using CategoryStamping = ghoul::logging::Log::CategoryStamping;
    using LogLevelStamping = ghoul::logging::Log::LogLevelStamping;

    if (type == ValueHtmlLog) {
        std::vector<std::string> cssFiles{absPath(BootstrapPath), absPath(CssPath)};
        std::vector<std::string> jsFiles{absPath(JsPath)};

        if (logLevel.empty()) {
            return std::make_unique<ghoul::logging::HTMLLog>(
                filename,
                Append(append),
                TimeStamping(timeStamp),
                DateStamping(dateStamp),
                CategoryStamping(categoryStamp),
                LogLevelStamping(logLevelStamp),
                cssFiles,
                jsFiles
            );
        }
        else {
            return std::make_unique<ghoul::logging::HTMLLog>(
                filename,
                Append(append),
                TimeStamping(timeStamp),
                DateStamping(dateStamp),
                CategoryStamping(categoryStamp),
                LogLevelStamping(logLevelStamp),
                cssFiles,
                jsFiles,
                ghoul::logging::levelFromString(logLevel)
            );
        }
    }
    else if (type == ValueTextLog) {
        if (logLevel.empty()) {
            return std::make_unique<ghoul::logging::TextLog>(
                filename,
                Append(append),
                TimeStamping(timeStamp),
                DateStamping(dateStamp),
                CategoryStamping(categoryStamp),
                LogLevelStamping(logLevelStamp)
            );
        }
        else {
            return std::make_unique<ghoul::logging::TextLog>(
                filename,
                Append(append),
                TimeStamping(timeStamp),
                DateStamping(dateStamp),
                CategoryStamping(categoryStamp),
                LogLevelStamping(logLevelStamp),
                ghoul::logging::levelFromString(logLevel)
            );
        }
    }
    else {
        throw ghoul::MissingCaseException();
    }
}

} // namespace openspace
