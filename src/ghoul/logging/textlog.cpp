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

#include <ghoul/logging/textlog.h>

#include <ghoul/format.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>

namespace ghoul::logging {

TextLog::TextLog(const std::filesystem::path& filename, int nLogRotation,
                 Append writeToAppend, TimeStamping timeStamping,
                 DateStamping dateStamping, CategoryStamping categoryStamping,
                 LogLevelStamping logLevelStamping, LogLevel minimumLogLevel)
    : Log(timeStamping, dateStamping, categoryStamping, logLevelStamping, minimumLogLevel)
    , _printFooter(writeToAppend)
{
    ghoul_assert(!filename.empty(), "Filename must not be empty");
    ghoul_assert(nLogRotation > 0, "Log rotation must be positive");
    if (nLogRotation > 0) {
        ghoul_assert(writeToAppend == Append::No, "We can't log rotate when appending");
    }

    _file.exceptions(std::ofstream::failbit | std::ofstream::badbit);

    while (nLogRotation > 0) {
        // Move all of the existing logs one position up

        const std::filesystem::path fname = filename.stem();
        const std::filesystem::path ext = filename.extension();

        std::filesystem::path newCandidate = filename;
        newCandidate.replace_filename(std::format("{}-{}{}", fname, nLogRotation, ext));

        std::filesystem::path oldCandidate = filename;
        if (nLogRotation > 1) {
            // We don't actually have a -0 version, it is just the base name
            oldCandidate.replace_filename(
                std::format("{}-{}{}", fname, nLogRotation - 1, ext)
            );
        }

        if (std::filesystem::exists(newCandidate)) {
            std::filesystem::remove(newCandidate);
        }
        if (std::filesystem::exists(oldCandidate)) {
            std::filesystem::rename(oldCandidate, newCandidate);
        }

        nLogRotation--;
    }

    if (writeToAppend) {
        _file.open(filename, std::ofstream::out | std::ofstream::app);
    }
    else {
        _file.open(filename, std::ofstream::out | std::ostream::trunc);
    }
}

TextLog::~TextLog() {
    if (_printFooter) {
        _file << "--------\n";
    }
}

void TextLog::log(LogLevel level, std::string_view category, std::string_view message) {
    ZoneScoped;

    if (category.empty() && message.empty()) {
        writeLine("\n");
    }
    else {
        std::string msg = createFullMessageString(level, category, message) + '\n';
        writeLine(msg);
    }
}

void TextLog::flush() {
    const std::unique_lock lock(_fileMutex);
    _file.flush();
}

void TextLog::writeLine(const std::string& line) {
    const std::unique_lock lock(_fileMutex);
    _file << line;
}

} // namespace ghoul::logging
