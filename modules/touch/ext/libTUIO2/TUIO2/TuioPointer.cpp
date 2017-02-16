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

#include "TuioPointer.h"

using namespace TUIO2;

TuioPointer::TuioPointer (TuioTime ttime, TuioObject *tobj, unsigned short ti, unsigned short ui, unsigned int pi, float xp, float yp, float a, float sa, float r, float p):TuioComponent(ttime,tobj,xp,yp,a) {
    type_id = ti;
    user_id = ui;
    pointer_id = pi;
    shear = sa;
    radius = r;
    pressure = p;
}

TuioPointer::TuioPointer (TuioObject *tobj, unsigned short ti, unsigned short ui, unsigned int pi, float xp, float yp, float a, float sa, float r, float p):TuioComponent(tobj,xp,yp,a) {
    type_id = ti;
    user_id = ui;
    shear = sa;
    pointer_id = pi;
    radius = r;
    pressure = p;
}

TuioPointer::TuioPointer (TuioObject *tobj, unsigned int pi, float xp, float yp, float a, float sa, float r, float p):TuioComponent(tobj,xp,yp,a) {
    type_id = 0;
    user_id = 0;
    pointer_id = pi;
    shear = sa;
    radius = r;
    pressure = p;
}

TuioPointer::TuioPointer (TuioPointer *tptr):TuioComponent(tptr) {
	pointer_id = tptr->getPointerID();
    type_id = tptr->getTypeID();
    user_id = tptr->getUserID();
    shear = tptr->getShear();
    radius = tptr->getRadius();
    pressure = tptr->getPressure();
}

void TuioPointer::update (TuioTime ttime, float xp, float yp, float a, float sa, float r, float p, float xs, float ys, float ps, float ma, float pa) {
    TuioComponent::update(ttime,xp,yp,a,xs,ys,0,ma,0);
    shear = sa;
    radius = r;
    pressure = p;
    pressure_speed = ps;
    pressure_accel = pa;
}

void TuioPointer::update (float xp, float yp, float a, float sa, float r, float p, float xs, float ys, float ps, float ma, float pa) {
    TuioComponent::update(xp,yp,a,xs,ys,0,ma,0);
    shear = sa;
    radius = r;
    pressure = p;
    pressure_speed = ps;
    pressure_accel = pa;
}

void TuioPointer::update (TuioTime ttime, float xp, float yp, float a, float sa, float r, float p) {
    TuioComponent::update(ttime,xp,yp,a);
    shear = sa;
    radius = r;
    pressure = p;
}

void TuioPointer::update (TuioPointer *tptr) {
    TuioComponent::update(tptr);
    shear = tptr->getShear();
    radius = tptr->getRadius();
    pressure = tptr->getPressure();
}

unsigned int TuioPointer::getPointerID() const{
	return pointer_id;
};

unsigned short TuioPointer::getTypeID() const{
    return type_id;
};

unsigned short TuioPointer::getUserID() const{
    return user_id;
};

unsigned int TuioPointer::getTypeUserID() const {
    int tu_id = user_id << 16 | type_id;
    return tu_id;
}

void TuioPointer::setTypeUserID(unsigned int tu_id) {
    user_id = tu_id >> 16;
    type_id = tu_id & 0x0000FFFF;
}

float TuioPointer::getShear() const{
    return shear;
};

float TuioPointer::getRadius() const{
    return radius;
};

float TuioPointer::getPressure() const{
    return pressure;
};

float TuioPointer::getPressureSpeed() const{
    return pressure_speed;
}

float TuioPointer::getPressureAccel() const{
    return pressure_accel;
}

