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

#include "TuioSymbol.h"

using namespace TUIO2;

TuioSymbol::TuioSymbol (TuioTime ttime, TuioObject *tobj, unsigned short ti, unsigned short ui, unsigned int sym, const char *type, const char *data):TuioComponent(ttime,tobj,0,0,0) {
    currentTime = ttime;
    startTime = currentTime;
    type_id = ti;
    user_id = ui;
    symbol_id = sym;
    symbol_type = std::string(type);
    symbol_data = std::string(data);
    state = TUIO_ADDED;
}
	
TuioSymbol::TuioSymbol (TuioObject *tobj, unsigned short ti, unsigned short ui, unsigned int sym, const char *type, const char *data):TuioComponent(tobj,0,0,0) {
    currentTime = TuioTime::getSystemTime();
    startTime = currentTime;
    type_id = ti;
    user_id = ui;
    symbol_id = sym;
    symbol_type = std::string(type);
    symbol_data = std::string(data);
    state = TUIO_ADDED;
}

TuioSymbol::TuioSymbol (TuioSymbol *tsym):TuioComponent(tsym->getTuioTime(),tsym->getContainingTuioObject(),0,0,0) {
    currentTime = TuioTime::getSystemTime();
    startTime = currentTime;
    session_id = tsym->getSessionID();
    type_id = tsym->getTypeID();
    user_id = tsym->getUserID();
    symbol_id = tsym->getSymbolID();
    symbol_type = tsym->getSymbolType();
    symbol_data = tsym->getSymbolData();
    state = TUIO_ADDED;
}

unsigned int TuioSymbol::getSymbolID() const {
    return symbol_id;
}
        
unsigned short TuioSymbol::getTypeID() const {
    return type_id;
}
        
unsigned short TuioSymbol::getUserID() const {
    return user_id;
}

unsigned int TuioSymbol::getTypeUserID() const {
    int tu_id = user_id << 16 | type_id;
    return tu_id;
}

void TuioSymbol::setTypeUserID(unsigned int tu_id) {
    user_id = tu_id >> 16;
    type_id = tu_id & 0x0000FFFF;
}

const char* TuioSymbol::getSymbolType() const {
    return symbol_type.c_str();
}
        
const char* TuioSymbol::getSymbolData() const {
    return symbol_data.c_str();
}

void TuioSymbol::update(TuioTime ttime) {
    TuioComponent::update(ttime,0,0);

}

