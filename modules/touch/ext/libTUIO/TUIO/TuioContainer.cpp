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

#include "TuioContainer.h"
using namespace TUIO;

TuioContainer::TuioContainer (TuioTime ttime, long si, float xp, float yp):TuioPoint(ttime, xp,yp)
,state(TUIO_ADDED)
,source_id(0)
,source_name("undefined")
,source_addr("localhost")
{
	session_id = si;
	x_speed = 0.0f;
	y_speed = 0.0f;
	motion_speed = 0.0f;
	motion_accel = 0.0f;
	x_accel = 0.0f;
	y_accel = 0.0f;
	TuioPoint p(currentTime,xpos,ypos);
	path.push_back(p);
	lastPoint = &path.back();
}

TuioContainer::TuioContainer (long si, float xp, float yp):TuioPoint(xp,yp)
,state(TUIO_ADDED)
,source_id(0)
,source_name("undefined")
,source_addr("localhost")
{
	session_id = si;
	x_speed = 0.0f;
	y_speed = 0.0f;
	motion_speed = 0.0f;
	motion_accel = 0.0f;
	x_accel = 0.0f;
	y_accel = 0.0f;
	TuioPoint p(currentTime,xpos,ypos);
	path.push_back(p);
	lastPoint = &path.back();
}

TuioContainer::TuioContainer (TuioContainer *tcon):TuioPoint(tcon)
,state(TUIO_ADDED)
,source_id(0)
,source_name("undefined")
,source_addr("localhost")
{
	session_id = tcon->getSessionID();
	x_speed = 0.0f;
	y_speed = 0.0f;
	motion_speed = 0.0f;
	motion_accel = 0.0f;
	x_accel = 0.0f;
	y_accel = 0.0f;
	
	TuioPoint p(currentTime,xpos,ypos);
	path.push_back(p);
	lastPoint = &path.back();
}

void TuioContainer::setTuioSource(int src_id, const char *src_name, const char *src_addr) {
	source_id = src_id;
	source_name = std::string(src_name);
	source_addr = std::string(src_addr);
}

const char* TuioContainer::getTuioSourceName() const{
	return source_name.c_str();
}

const char* TuioContainer::getTuioSourceAddress() const{
	return source_addr.c_str();
}

int TuioContainer::getTuioSourceID() const{
	return source_id;
}

void TuioContainer::update (TuioTime ttime, float xp, float yp) {
	lastPoint = &path.back();
	TuioPoint::update(ttime,xp, yp);

	TuioTime diffTime = currentTime - lastPoint->getTuioTime();
	float dt = diffTime.getTotalMilliseconds()/1000.0f;
	float dx = xpos - lastPoint->getX();
	float dy = ypos - lastPoint->getY();
	float dist = sqrt(dx*dx+dy*dy);
	float last_motion_speed = motion_speed;
	float last_x_speed = x_speed;
	float last_y_speed = y_speed;

	x_speed = dx/dt;
	y_speed = dy/dt;
	motion_speed = dist/dt;
	motion_accel = (motion_speed - last_motion_speed)/dt;
	x_accel = (x_speed - last_x_speed)/dt;
	y_accel = (y_speed - last_y_speed)/dt;

	TuioPoint p(currentTime,xpos,ypos);
	path.push_back(p);
    if (path.size()>MAX_PATH_SIZE) path.pop_front();

	if (motion_accel>0) state = TUIO_ACCELERATING;
	else if (motion_accel<0) state = TUIO_DECELERATING;
	else state = TUIO_STOPPED;
}

void TuioContainer::stop(TuioTime ttime) {
	if ( state==TUIO_IDLE )update(ttime,xpos,ypos);
	else state=TUIO_IDLE;
}

void TuioContainer::update (TuioTime ttime, float xp, float yp, float xs, float ys, float ma) {
	TuioPoint::update(ttime,xp, yp);
	x_speed = xs;
	y_speed = ys;
	motion_speed = (float)sqrt(x_speed*x_speed+y_speed*y_speed);
	motion_accel = ma;
	x_accel = ma;
	y_accel = ma;

	lastPoint = &path.back();
	TuioPoint p(currentTime,xpos,ypos);
	path.push_back(p);
    if (path.size()>MAX_PATH_SIZE) path.pop_front();

	if (motion_accel>0) state = TUIO_ACCELERATING;
	else if (motion_accel<0) state = TUIO_DECELERATING;
	else state = TUIO_STOPPED;
}

void TuioContainer::update (float xp, float yp, float xs, float ys, float ma) {
	TuioPoint::update(xp,yp);
	x_speed = xs;
	y_speed = ys;
	motion_speed = (float)sqrt(x_speed*x_speed+y_speed*y_speed);
	motion_accel = ma;
	x_accel = ma;
	y_accel = ma;

	lastPoint = &path.back();
	TuioPoint p(currentTime,xpos,ypos);
	path.push_back(p);
    if (path.size()>MAX_PATH_SIZE) path.pop_front();

	if (motion_accel>0) state = TUIO_ACCELERATING;
	else if (motion_accel<0) state = TUIO_DECELERATING;
	else state = TUIO_STOPPED;
}

void TuioContainer::update (TuioContainer *tcon) {
	TuioPoint::update(tcon);
	x_speed = tcon->getXSpeed();
	y_speed =  tcon->getYSpeed();
	motion_speed =  tcon->getMotionSpeed();
	motion_accel = tcon->getMotionAccel();
	x_accel = motion_accel;
	y_accel = motion_accel;

	lastPoint = &path.back();
	TuioPoint p(tcon->getTuioTime(),xpos,ypos);
	path.push_back(p);
    if (path.size()>MAX_PATH_SIZE) path.pop_front();

	if (motion_accel>0) state = TUIO_ACCELERATING;
	else if (motion_accel<0) state = TUIO_DECELERATING;
	else state = TUIO_STOPPED;
}

void TuioContainer::remove(TuioTime ttime) {
	currentTime = ttime;
	state = TUIO_REMOVED;
}

long TuioContainer::getSessionID() const{
	return session_id;
}

void TuioContainer::setSessionID(long s_id) {
	session_id = s_id;
}

float TuioContainer::getXSpeed() const{
	return x_speed;
}

float TuioContainer::getYSpeed() const{
	return y_speed;
}

TuioPoint TuioContainer::getPosition() const{
	TuioPoint p(xpos,ypos);
	return p;
}

std::list<TuioPoint> TuioContainer::getPath() const{
	return path;
}

float TuioContainer::getMotionSpeed() const{
	return motion_speed;
}

float TuioContainer::getMotionAccel() const{
	return motion_accel;
}

int TuioContainer::getTuioState() const{
	return state;
}

bool TuioContainer::isMoving() const{
	if ((state==TUIO_ACCELERATING) || (state==TUIO_DECELERATING)) return true;
	else return false;
}

TuioPoint TuioContainer::predictPosition() {
	/*if (path.size()>1) {
		std::list<TuioPoint>::iterator iter = path.end();
		std::advance(iter, -2);
		
		TuioTime diffTime = currentTime - (*iter).getTuioTime();
		float dt = diffTime.getTotalMilliseconds()/1000.0f;
		
		float tx = x_speed * dt;
		float ty = y_speed * dt;
		
		float nx = xpos+tx-tx*x_accel*dt;
		float ny = ypos+ty-ty*y_accel*dt;
		
		//if (xposFilter && yposFilter) {
		//	nx = xposFilter->filter(nx,dt);
		//	ny = yposFilter->filter(ny,dt);
		//	//std::cout << dt << " " << xp << " " << xpos << " " << yp << " " << ypos << std::endl;
		//}
		
		//std::cout << nx << " " << ny << std::endl;
		return TuioPoint(nx,ny);
	} else return TuioPoint(xpos,ypos);*/
	
	TuioTime diffTime = currentTime - lastPoint->getTuioTime();
	float dt = diffTime.getTotalMilliseconds()/1000.0f;
	
	float tx = x_speed * dt;
	float ty = y_speed * dt;
	
	float nx = xpos+tx-tx*x_accel*dt;
	float ny = ypos+ty-ty*y_accel*dt;
	
	//if (xposFilter && yposFilter) {
	//	nx = xposFilter->filter(nx,dt);
	//	ny = yposFilter->filter(ny,dt);
	//	//std::cout << dt << " " << xp << " " << xpos << " " << yp << " " << ypos << std::endl;
	//}
	
	//std::cout << nx << " " << ny << std::endl;
	return TuioPoint(nx,ny);

	
}

