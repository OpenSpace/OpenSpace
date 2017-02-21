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

#include "TuioBlob.h"
using namespace TUIO;

TuioBlob::TuioBlob (TuioTime ttime, long si, int bi, float xp, float yp, float a, float w, float h, float f):TuioContainer(ttime, si, xp, yp) {
	blob_id = bi;
	angle = a;
	angle_sum = a;
	width = w;
	height = h;
	area = f;
	rotation_speed = 0.0f;
	rotation_accel = 0.0f;
	
	angleFilter = NULL;
	angleThreshold = 0.0f;
	widthFilter = NULL;
	heightFilter = NULL;
	sizeThreshold = 0.0f;
}

TuioBlob::TuioBlob (long si, int bi, float xp, float yp, float a, float  w, float h, float f):TuioContainer(si, xp, yp) {
	blob_id = bi;
	angle = a;
	angle_sum = a;
	width = w;
	height = h; 
	area = f;
	rotation_speed = 0.0f;
	rotation_accel = 0.0f;
	
	angleFilter = NULL;
	angleThreshold = 0.0f;
	widthFilter = NULL;
	heightFilter = NULL;
	sizeThreshold = 0.0f;
}

TuioBlob::TuioBlob (TuioBlob *tblb):TuioContainer(tblb) {
	blob_id = tblb->getBlobID();
	angle = tblb->getAngle();
	angle_sum = tblb->getAngleSum();
	width = tblb->getWidth();
	height = tblb->getHeight();
	area = tblb->getArea();
	rotation_speed = 0.0f;
	rotation_accel = 0.0f;
	
	angleFilter = NULL;
	angleThreshold = 0.0f;
	widthFilter = NULL;
	heightFilter = NULL;
	sizeThreshold = 0.0f;
}

int TuioBlob::getBlobID() const{
	return blob_id;
}

void TuioBlob::setBlobID(long b_id) {
	blob_id = b_id;
}

void TuioBlob::update (TuioTime ttime, float xp, float yp, float a, float w, float h, float f, float xs, float ys, float rs, float ma, float ra) {
	TuioContainer::update(ttime,xp,yp,xs,ys,ma);
	angle = a;
	angle_sum = a;
	width = w;
	height = h;
	area = f;
	rotation_speed = rs;
	rotation_accel = ra;
	if ((rotation_accel!=0) && (state==TUIO_STOPPED)) state = TUIO_ROTATING;
}

void TuioBlob::update (float xp, float yp, float a, float w, float h, float f, float xs, float ys, float rs, float ma, float ra) {
	TuioContainer::update(xp,yp,xs,ys,ma);
	angle = a;
	angle_sum = a;
	width = w;
	height = h;
	area = f;
	rotation_speed = rs;
	rotation_accel = ra;
	if ((rotation_accel!=0) && (state==TUIO_STOPPED)) state = TUIO_ROTATING;
}

void TuioBlob::update (TuioTime ttime, float xp, float yp, float a, float w, float h, float f) {
	TuioPoint lastPoint = path.back();
	TuioContainer::update(ttime,xp,yp);
	
	TuioTime diffTime = currentTime - lastPoint.getTuioTime();
	float dt = diffTime.getTotalMilliseconds()/1000.0f;
	float last_rotation_speed = rotation_speed;
	
	float prev_angle = angle_sum;
	float da = a-angle;
	if (da > M_PI/2.0f) angle_sum += (da-2*M_PI);
	else if (da < M_PI/-2.0f) angle_sum += (da+2*M_PI);
	else angle_sum += da;
	
	if (angleFilter) angle_sum = angleFilter->filter(angle_sum,dt);
	if (fabs(angle_sum-prev_angle)<angleThreshold) angle_sum = prev_angle;
	
	int m = floor(angle_sum/(2*M_PI));
	angle = angle_sum-(m*(2*M_PI));
	
	da = (angle-a)/(2*M_PI);
	if (da > 0.75f) da-=1.0f;
	else if (da < -0.75f) da+=1.0f;
	
	if (widthFilter && heightFilter) {
		w = widthFilter->filter(w,dt);
		h = heightFilter->filter(h,dt);
	}
	
	float dw = fabs(width - w);
	float dh = fabs(height - h);
	if ((dw>sizeThreshold) || (dh>sizeThreshold)) {
		width = w;
		height = h;
	}
	
	area = f;
	
	rotation_speed = (float)da/dt;
	rotation_accel =  (rotation_speed - last_rotation_speed)/dt;
	
	if ((rotation_accel!=0) && (state==TUIO_STOPPED)) state = TUIO_ROTATING;
}

void TuioBlob::stop (TuioTime ttime) {
	update(ttime,xpos,ypos,angle,width,height,area);
}

void TuioBlob::update (TuioBlob *tblb) {
	TuioContainer::update(tblb);
	angle = tblb->getAngle();
	angle = tblb->getAngleSum();
	width = tblb->getWidth();
	height = tblb->getHeight();
	area = tblb->getArea();
	rotation_speed = tblb->getRotationSpeed();
	rotation_accel = tblb->getRotationAccel();
	if ((rotation_accel!=0) && (state==TUIO_STOPPED)) state = TUIO_ROTATING;
}

float TuioBlob::getWidth() const{ 
	return width;
}

float TuioBlob::getHeight() const{ 
	return height;
}

int TuioBlob::getScreenWidth(int w) const{ 
	return (int)(w*width);
}

int TuioBlob::getScreenHeight(int h) const{ 
	return (int)(h*height);
}

float TuioBlob::getArea() const{ 
	return area;
}

float TuioBlob::getAngle() const{
	return angle;
}

float TuioBlob::getAngleSum() const{
	return angle_sum;
}

float TuioBlob::getAngleDegrees() const{ 
	return (float)(angle/M_PI*180);
}

float TuioBlob::getRotationSpeed() const{ 
	return rotation_speed;
}

float TuioBlob::getRotationAccel() const{
	return rotation_accel;
}

bool TuioBlob::isMoving() const{ 
	if ((state==TUIO_ACCELERATING) || (state==TUIO_DECELERATING) || (state==TUIO_ROTATING)) return true;
	else return false;
}

void TuioBlob::addAngleThreshold(float thresh) {
	angleThreshold = thresh;
}

void TuioBlob::removeAngleThreshold() {
	angleThreshold = 0.0f;
}

void TuioBlob::addAngleFilter(float mcut, float beta) {
	
	if (angleFilter) delete angleFilter;
	angleFilter = new OneEuroFilter(60.0f, mcut, beta, 1.0f);
}

void TuioBlob::removeAngleFilter() {
	
	if (angleFilter) delete angleFilter;
	angleFilter = NULL;
}

void TuioBlob::addSizeThreshold(float thresh) {
	sizeThreshold = thresh;
}

void TuioBlob::removeSizeThreshold() {
	sizeThreshold = 0.0f;
}

void TuioBlob::addSizeFilter(float mcut, float beta) {
	
	if (widthFilter) delete widthFilter;
	widthFilter = new OneEuroFilter(60.0f, mcut, beta, 1.0f);
	if (heightFilter) delete heightFilter;
	heightFilter = new OneEuroFilter(60.0f, mcut, beta, 1.0f);
}

void TuioBlob::removeSizeFilter() {
	
	if (widthFilter) delete widthFilter;
	widthFilter = NULL;
	if (heightFilter) delete heightFilter;
	heightFilter = NULL;
}

