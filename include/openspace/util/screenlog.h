/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#include <ghoul/logging/log.h>
#include <vector>
#include <tuple>
#include <utility> // pair

namespace openspace {

class ScreenLog : public ghoul::logging::Log {
public:
	//typedef std::tuple<ghoul::logging::LogManager::LogLevel, std::string, std::string> LogEntry;

	struct LogEntry {
		LogEntry(ghoul::logging::LogManager::LogLevel l, double t, std::string ts, std::string c, std::string m) : level(l), timeStamp(t), timeString(ts), category(c), message(m) {};
		ghoul::logging::LogManager::LogLevel level;
		double timeStamp;
		std::string timeString;
		std::string category;
		std::string message;
	};

	typedef std::vector<LogEntry>::iterator iterator;
	typedef std::vector<LogEntry>::const_iterator const_iterator;
	typedef std::vector<LogEntry>::reverse_iterator reverse_iterator;
	typedef std::vector<LogEntry>::const_reverse_iterator const_reverse_iterator;

	typedef std::pair<reverse_iterator, reverse_iterator> range;
	typedef std::pair<const_reverse_iterator, const_reverse_iterator> const_range;

	const size_t MaximumSize = 1000;

	ScreenLog();

	virtual void log(ghoul::logging::LogManager::LogLevel level, const std::string& category,
		const std::string& message);

	const_range last(size_t n = 10);

private:

	std::vector<LogEntry> _entries;

};
}