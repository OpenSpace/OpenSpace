/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <ghoul/opengl/ghoul_gl.h>
#include <sgct.h> // sgct::Engine::instance()->getTime()

namespace openspace {

ScreenLog::ScreenLog() {}

void ScreenLog::log(ghoul::logging::LogManager::LogLevel level, const std::string& category, const std::string& message) {
	if (level >= ghoul::logging::LogManager::LogLevel::Info)
		_entries.emplace_back(level, sgct::Engine::instance()->getTime(), Log::getTimeString(), category, message);

	// Once reaching maximum size, reduce to half
	if (_entries.size() > MaximumSize) {
		_entries.erase(_entries.begin(), _entries.begin() + MaximumSize / 2);
	}
}

ScreenLog::const_range ScreenLog::last(size_t n) {
	if (_entries.size() > n) {
		return std::make_pair(_entries.rbegin(), _entries.rbegin() + n);
	} else {
		return std::make_pair(_entries.rbegin(), _entries.rend());
	}
}

}