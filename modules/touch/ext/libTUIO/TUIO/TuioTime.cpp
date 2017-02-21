/*
 TUIO C++ Library
 Copyright (c) 2005-2016 Martin Kaltenbrunner <martin@tuio.org>
 
 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 3.0 of the License, or (at your option) any later version.
 
 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 Lesser General Public License for more details.
 
 You should have received a copy of the GNU Lesser General Public
 License along with this library.
*/

#include "TuioTime.h"
using namespace TUIO;
	
long TuioTime::start_seconds = 0;
long TuioTime::start_micro_seconds = 0;

TuioTime::TuioTime (long msec) {
	seconds = msec/MSEC_SECOND;
	micro_seconds = USEC_MILLISECOND*(msec%MSEC_SECOND);
}
	
TuioTime::TuioTime (long sec, long usec) {
	seconds = sec;
	micro_seconds = usec;
}

TuioTime TuioTime::operator+(long us) {
	long sec = seconds + us/USEC_SECOND;
	long usec = micro_seconds + us%USEC_SECOND;
	return TuioTime(sec,usec);
}

TuioTime TuioTime::operator+(TuioTime ttime) {
	long sec = seconds + ttime.getSeconds();
	long usec = micro_seconds + ttime.getMicroseconds();
	sec += usec/USEC_SECOND;
	usec = usec%USEC_SECOND;
	return TuioTime(sec,usec);
}

TuioTime TuioTime::operator-(long us) {
	long sec = seconds - us/USEC_SECOND;
	long usec = micro_seconds - us%USEC_SECOND;
	
	if (usec<0) {
		usec += USEC_SECOND;
		sec--;
	}			
	
	return TuioTime(sec,usec);
}

TuioTime TuioTime::operator-(TuioTime ttime) {
	long sec = seconds - ttime.getSeconds();
	long usec = micro_seconds - ttime.getMicroseconds();
	
	if (usec<0) {
		usec += USEC_SECOND;
		sec--;
	}
	
	return TuioTime(sec,usec);
}

void TuioTime::operator=(TuioTime ttime) {
	seconds = ttime.getSeconds();
	micro_seconds = ttime.getMicroseconds();
}

bool TuioTime::operator==(TuioTime ttime) {
	if ((seconds==(long)ttime.getSeconds()) && (micro_seconds==(long)ttime.getMicroseconds())) return true;
	else return false;
}

bool TuioTime::operator!=(TuioTime ttime) {
	if ((seconds!=(long)ttime.getSeconds()) || (micro_seconds!=(long)ttime.getMicroseconds())) return true;
	else return false;
}

void TuioTime::reset() {
	seconds = 0;
	micro_seconds = 0;
}

long TuioTime::getSeconds() const{
	return seconds;
}

long TuioTime::getMicroseconds() const{
	return micro_seconds;
}

long TuioTime::getTotalMilliseconds() const{
	return seconds*MSEC_SECOND+micro_seconds/MSEC_SECOND;
}

void TuioTime::initSession() {
	TuioTime startTime = TuioTime::getSystemTime();
	start_seconds = startTime.getSeconds();
	start_micro_seconds = startTime.getMicroseconds();
}

TuioTime TuioTime::getSessionTime() {
	return  (getSystemTime() - getStartTime());
}

TuioTime TuioTime::getStartTime() {
	return TuioTime(start_seconds,start_micro_seconds);
}

TuioTime TuioTime::getSystemTime() {
#ifdef WIN32
	TuioTime systemTime(GetTickCount());
#else
	struct timeval tv;
	struct timezone tz;
	gettimeofday(&tv,&tz);
	TuioTime systemTime(tv.tv_sec,tv.tv_usec);
#endif	
	return systemTime;
}
