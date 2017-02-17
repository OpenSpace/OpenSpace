/*
 TUIO2 C++ Library
 Copyright (c) 2009-2014 Martin Kaltenbrunner <martin@tuio.org>
 
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
using namespace TUIO2;
	
long TuioTime::start_seconds = 0;
long TuioTime::start_micro_seconds = 0;

TuioTime::TuioTime (long msec) {
	seconds = msec/MSEC_SECOND;
	micro_seconds = USEC_MILLISECOND*(msec%MSEC_SECOND);
}

TuioTime::TuioTime (osc::TimeTag timetag) {
    osc::uint32 secs = timetag >> 32;
    osc::uint32 frac = timetag & 0x00000000FFFFFFFF;
    
    seconds = secs - JAN_1970;
    micro_seconds =  frac / NTP_UNITS;
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
    
    TuioTime system_time = getSystemTime();
    seconds = system_time.getSeconds();
    micro_seconds = system_time.getMicroseconds();
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

unsigned int TuioTime::getFrameID() const{
    return frame_id;
}

void TuioTime::setFrameID(unsigned int f_id) {
    frame_id = f_id;
}

/*void TuioTime::initSession() {
    TuioTime startTime = TuioTime::getSystemTime();
    start_seconds = startTime.getSeconds();
    start_micro_seconds = startTime.getMicroseconds();
}

TuioTime TuioTime::getSessionTime() {
    return  (getSystemTime() - getStartTime());
}*/

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

osc::TimeTag TuioTime::getSystemTimeTag() {

    TuioTime systemTime = getSystemTime();
    
    osc::uint32 secs = systemTime.getSeconds() + JAN_1970;
    osc::uint32 frac = systemTime.getMicroseconds() * NTP_UNITS;
    
    osc::uint64 timetag = (osc::uint64) secs << 32 | frac;
    return osc::TimeTag(timetag);
}
