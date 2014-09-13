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

#include <openspace/util/time.h>

#include <openspace/interaction/interactionhandler.h>
#include <openspace/util/spicemanager.h>

#include <ghoul/filesystem/filesystem.h>

#include <cassert>

namespace {
	const std::string _loggerCat = "Time";
}

namespace openspace {

Time* Time::_instance = nullptr;

Time::Time() 
	: _time(-1.0)
	, _deltaTimePerSecond(1.0)
{
	SpiceManager::ref().loadKernel(absPath("${OPENSPACE_DATA}/spice/naif0010.tls"), "leap");
	// load spice time kernel
	//furnsh_c (absPath("${OPENSPACE_DATA}/spice/naif0010.tls").c_str());

	// convert UTC to ET 
	//str2et_c ( "2006 JAN 31 01:00", &_time );
}

bool Time::initialize() {
	assert( _instance == nullptr);
	 _instance = new Time();
	 return true;
}

void Time::deinitialize() {
	assert(_instance);
	delete _instance;
	_instance = nullptr;
}

Time& Time::ref() {
	assert(_instance);
    return *_instance;
}

bool Time::isInitialized() {
	return _instance != nullptr;
}

//void Time::setTime(const char* stringTime) {
//	assert(_instance);
//	// convert UTC to ET 
//	//str2et_c ( stringTime, &_time );
//}

void Time::setTime(double value) {
	_time = std::move(value);
}

double Time::currentTime() const {
	assert(_instance);
	return _time;
}

double Time::advanceTime(double tickTime) {
	return _time += _deltaTimePerSecond * tickTime;
}

void Time::setDeltaTime(double deltaT) {
	_deltaTimePerSecond = std::move(deltaT);
}

double Time::deltaTime() const {
	return _deltaTimePerSecond;
}

void Time::setTimeUTC(std::string time) {
	_time = SpiceManager::ref().convertStringToTdbSeconds(std::move(time));
}

std::string Time::currentTimeUTC() const {
	return SpiceManager::ref().convertTdbSecondsToString(_time, "MON DD,YYYY  HR:MN:SC.#### (TDB) ::TDB");
}

} // namespace openspace
