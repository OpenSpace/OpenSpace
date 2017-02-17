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

#include "TuioBounds.h"
using namespace TUIO2;

TuioBounds::TuioBounds (TuioTime ttime, TuioObject *tobj, float xp, float yp, float a, float w, float h, float f):TuioComponent(ttime, tobj, xp, yp,a) {
	width = w;
	height = h;
    area = f;
}

TuioBounds::TuioBounds (TuioObject *tobj, float xp, float yp, float a, float  w, float h, float f):TuioComponent(tobj, xp, yp, a) {
	width = w;
	height = h; 
	area = f;
}

TuioBounds::TuioBounds (TuioBounds *tbnd):TuioComponent(tbnd) {
	angle = tbnd->getAngle();
	width = tbnd->getWidth();
	height = tbnd->getHeight();
	area = tbnd->getArea();
	rotation_speed = 0.0f;
	rotation_accel = 0.0f;
}

void TuioBounds::update (TuioTime ttime, float xp, float yp, float a, float w, float h, float f, float xs, float ys, float rs, float ma, float ra) {
	TuioComponent::update(ttime,xp,yp,a,xs,ys,rs,ma,ra);
	width = w;
	height = h;
	area = f;
}

void TuioBounds::update (float xp, float yp, float a, float w, float h, float f, float xs, float ys, float rs, float ma, float ra) {
	TuioComponent::update(xp,yp,a,xs,ys,rs,ma,ra);
	width = w;
	height = h;
	area = f;
}

void TuioBounds::update (TuioTime ttime, float xp, float yp, float a, float w, float h, float f) {
    TuioComponent::update(ttime,xp,yp,a);
	
	width = w;
	height = h;
	area = f;
}

void TuioBounds::stop (TuioTime ttime) {
	update(ttime,xpos,ypos,angle,width,height,area);
}

void TuioBounds::update (TuioBounds *tbnd) {
	TuioComponent::update(tbnd);
	width = tbnd->getWidth();
	height = tbnd->getHeight();
	area = tbnd->getArea();
}

float TuioBounds::getWidth() const{ 
	return width;
}

float TuioBounds::getHeight() const{ 
	return height;
}

int TuioBounds::getScreenWidth(int w) const{ 
	return (int)(w*width);
}

int TuioBounds::getScreenHeight(int h) const{ 
	return (int)(h*height);
}

float TuioBounds::getArea() const{ 
	return area;
}
