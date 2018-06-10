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

#include <openspace/util/screenlog.h>

#include <algorithm>

using std::string;

namespace openspace {

ScreenLog::ScreenLog(std::chrono::seconds timeToLive, LogLevel logLevel)
    : _timeToLive(std::move(timeToLive))
    , _logLevel(logLevel)
{}

void ScreenLog::removeExpiredEntries() {
    std::lock_guard<std::mutex> guard(_mutex);
    const auto t = std::chrono::steady_clock::now();

    const auto rit = std::remove_if(
        _entries.begin(),
        _entries.end(),
        [&t, ttl = _timeToLive](const LogEntry& e) { return (t - e.timeStamp) > ttl; }
    );

    _entries.erase(rit, _entries.end() );
}

void ScreenLog::log(LogLevel level, const string& category, const string& message) {
    std::lock_guard<std::mutex> guard(_mutex);
    if (level >= _logLevel) {
        _entries.push_back({
            level,
            std::chrono::steady_clock::now(),
            Log::timeString(),
            category,
            message
        });
    }
}

const std::vector<ScreenLog::LogEntry>& ScreenLog::entries() const {
    return _entries;
}

} // namespace openspace
