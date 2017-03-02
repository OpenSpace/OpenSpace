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

#include "TuioCursor.h"

using namespace TUIO;

TuioCursor::TuioCursor (TuioTime ttime, long si, int ci, float xp, float yp):TuioContainer(ttime,si,xp,yp) {
	cursor_id = ci;
}

TuioCursor::TuioCursor (long si, int ci, float xp, float yp):TuioContainer(si,xp,yp) {
	cursor_id = ci;
}

TuioCursor::TuioCursor (TuioCursor *tcur):TuioContainer(tcur) {
	cursor_id = tcur->getCursorID();
}

int TuioCursor::getCursorID() const{
	return cursor_id;
};

