/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/loglevel.h>
#include <ghoul/logging/htmllog.h>
#include <ghoul/logging/textlog.h>

namespace {
    const char* keyType = "Type";
    const char* keyFilename = "File";
    const char* keyAppend = "Append";
    const char* keyTimeStamping = "TimeStamping";
    const char* keyDateStamping = "DateStamping";
    const char* keyCategoryStamping = "CategoryStamping";
    const char* keyLogLevelStamping = "LogLevelStamping";
    const char* keyLogLevel = "LogLevel";

    const char* valueHtmlLog = "html";
    const char* valueTextLog = "Text";

    const char* BootstrapPath = "${OPENSPACE_DATA}/web/common/bootstrap.min.css";
    const char* CssPath = "${OPENSPACE_DATA}/web/log/style.css";
    const char* JsPath = "${OPENSPACE_DATA}/web/log/script.js";
}

namespace openspace {

documentation::Documentation LogFactoryDocumentation() {
    using namespace documentation;

    return {
        "LogFactory",
        "core_logfactory",
        {
            {
                keyType,
                new StringInListVerifier({
                    // List from createLog
                    valueTextLog, valueHtmlLog
                }),
                "The type of the new log to be generated."
            },
            {
                keyFilename,
                new StringVerifier,
                "The filename to which the log will be written."
            },
            {
                keyAppend,
                new BoolVerifier,
                "Determines whether the file will be cleared at startup or if the "
                "contents will be appended to previous runs.",
                Optional::Yes
            },
            {
                keyTimeStamping,
                new BoolVerifier,
                "Determines whether the log entires should be stamped with the time at "
                "which the message was logged.",
                Optional::Yes
            },
            {
                keyDateStamping,
                new BoolVerifier,
                "Determines whether the log entries should be stamped with the date at "
                "which the message was logged.",
                Optional::Yes
            },
            {
                keyCategoryStamping,
                new BoolVerifier,
                "Determines whether the log entries should be stamped with the "
                "category that creates the log message.",
                Optional::Yes
            },
            {
                keyLogLevelStamping,
                new BoolVerifier,
                "Determines whether the log entries should be stamped with the log level "
                "that was used to create the log message.",
                Optional::Yes
            }
        },
        Exhaustive::Yes
    };
}

std::unique_ptr<ghoul::logging::Log> createLog(const ghoul::Dictionary& dictionary) {
    using namespace std::string_literals;

    documentation::testSpecificationAndThrow(
        LogFactoryDocumentation(),
        dictionary,
        "LogFactory"
    );

    // 'type' and 'filename' are required keys
    std::string type = dictionary.value<std::string>(keyType);
    std::string filename = absPath(dictionary.value<std::string>(keyFilename));

    // the rest are optional
    bool append = true;
    if (dictionary.hasKeyAndValue<bool>(keyAppend)) {
        append = dictionary.value<bool>(keyAppend);
    }
    bool timeStamp = true;
    if (dictionary.hasKeyAndValue<bool>(keyTimeStamping)) {
        timeStamp = dictionary.value<bool>(keyTimeStamping);
    }
    bool dateStamp = true;
    if (dictionary.hasKeyAndValue<bool>(keyDateStamping)) {
        dateStamp = dictionary.value<bool>(keyDateStamping);
    }
    bool categoryStamp = true;
    if (dictionary.hasKeyAndValue<bool>(keyCategoryStamping)) {
        categoryStamp = dictionary.value<bool>(keyCategoryStamping);
    }
    bool logLevelStamp = true;
    if (dictionary.hasKeyAndValue<bool>(keyLogLevelStamping)) {
        logLevelStamp = dictionary.value<bool>(keyLogLevelStamping);
    }
    std::string logLevel;
    if (dictionary.hasKeyAndValue<std::string>(keyLogLevel)) {
        logLevel = dictionary.value<std::string>(keyLogLevel);
    }


    using Append = ghoul::logging::TextLog::Append;
    using TimeStamping = ghoul::logging::Log::TimeStamping;
    using DateStamping = ghoul::logging::Log::DateStamping;
    using CategoryStamping = ghoul::logging::Log::CategoryStamping;
    using LogLevelStamping = ghoul::logging::Log::LogLevelStamping;

    if (type == valueHtmlLog) {
        std::vector<std::string> cssFiles{absPath(BootstrapPath), absPath(CssPath)};
        std::vector<std::string> jsFiles{absPath(JsPath)};

        if (logLevel.empty()) {
            return std::make_unique<ghoul::logging::HTMLLog>(
                filename,
                append ? Append::Yes : Append::No,
                timeStamp ? TimeStamping::Yes : TimeStamping::No,
                dateStamp ? DateStamping::Yes : DateStamping::No,
                categoryStamp ? CategoryStamping::Yes : CategoryStamping::No,
                logLevelStamp ? LogLevelStamping::Yes : LogLevelStamping::No,
                cssFiles,
                jsFiles
            );
        }
        else {
            return std::make_unique<ghoul::logging::HTMLLog>(
                filename,
                append ? Append::Yes : Append::No,
                timeStamp ? TimeStamping::Yes : TimeStamping::No,
                dateStamp ? DateStamping::Yes : DateStamping::No,
                categoryStamp ? CategoryStamping::Yes : CategoryStamping::No,
                logLevelStamp ? LogLevelStamping::Yes : LogLevelStamping::No,
                cssFiles, jsFiles,
                ghoul::logging::levelFromString(logLevel)
                );
        }
    }
    else if (type == valueTextLog) {
        if (logLevel.empty()) {
            return std::make_unique<ghoul::logging::TextLog>(
                filename,
                append ? Append::Yes : Append::No,
                timeStamp ? TimeStamping::Yes : TimeStamping::No,
                dateStamp ? DateStamping::Yes : DateStamping::No,
                categoryStamp ? CategoryStamping::Yes : CategoryStamping::No,
                logLevelStamp ? LogLevelStamping::Yes : LogLevelStamping::No
            );
        }
        else {
            return std::make_unique<ghoul::logging::TextLog>(
                filename,
                append ? Append::Yes : Append::No,
                timeStamp ? TimeStamping::Yes : TimeStamping::No,
                dateStamp ? DateStamping::Yes : DateStamping::No,
                categoryStamp ? CategoryStamping::Yes : CategoryStamping::No,
                logLevelStamp ? LogLevelStamping::Yes : LogLevelStamping::No,
                ghoul::logging::levelFromString(logLevel)
            );
        }
    }
    ghoul_assert(false, "Missing case in the documentation for LogFactory");
}
    
} // namespace openspace
