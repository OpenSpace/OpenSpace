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

#include "TuioObject.h"

using namespace TUIO2;

TuioObject::TuioObject (unsigned int s_id) {
    currentTime = TuioTime::getSystemTime();
    session_id = s_id;
    startTime = currentTime;
    
    token = NULL;
    pointer = NULL;
    bounds = NULL;
    symbol = NULL;
    state = TUIO_ADDED;
}

TuioObject::TuioObject (TuioTime ttime, unsigned int s_id) {
    currentTime = ttime;
    session_id = s_id;
    startTime = currentTime;
    
    token = NULL;
    pointer = NULL;
    bounds = NULL;
    symbol = NULL;
    state = TUIO_ADDED;
}

TuioObject::TuioObject (TuioTime ttime, TuioSource *src, unsigned int s_id) {
    currentTime = ttime;
    session_id = s_id;
    startTime = currentTime;
    
    setTuioSource(src);
    token = NULL;
    pointer = NULL;
    bounds = NULL;
    symbol = NULL;
    state = TUIO_ADDED;
}

TuioObject::~TuioObject() {
    deleteAllTuioComponents();
}

unsigned int TuioObject::getSessionID() {
    return session_id;
}

void TuioObject::setTuioSource(TuioSource *src) {
    source.setSourceString(src->getSourceID(),src->getSourceString());
}

TuioSource* TuioObject::getTuioSource() {
    return &source;
}

void TuioObject::setTuioToken (TuioToken *ttok) {
    token = ttok;
    token->setContainingTuioObject(this);
    currentTime = TuioTime::getSystemTime();
    state = TUIO_ADDED;
}

void TuioObject::setTuioPointer (TuioPointer *tptr) {
    pointer = tptr;
    pointer->setContainingTuioObject(this);
    currentTime = TuioTime::getSystemTime();
    state = TUIO_ADDED;
}

void TuioObject::setTuioBounds (TuioBounds *tbnd) {
	bounds = tbnd;
    bounds->setContainingTuioObject(this);
    currentTime = TuioTime::getSystemTime();
    state = TUIO_ADDED;
}

void TuioObject::setTuioSymbol (TuioSymbol *tsym) {
    symbol = tsym;
    symbol->setContainingTuioObject(this);
    currentTime = TuioTime::getSystemTime();
    state = TUIO_ADDED;
}

void TuioObject::removeAllTuioComponents(TuioTime ttime) {
    removeTuioToken(ttime);
    removeTuioPointer(ttime);
    removeTuioBounds(ttime);
    removeTuioSymbol(ttime);
}

void TuioObject::removeTuioToken (TuioTime ttime) {
    if (token != NULL) token->remove(ttime);
    currentTime = ttime;
 }

void TuioObject::removeTuioPointer (TuioTime ttime) {
    if (pointer != NULL) pointer->remove(ttime);
    currentTime = ttime;
}

void TuioObject::removeTuioBounds (TuioTime ttime) {
    if (bounds != NULL) bounds->remove(ttime);
    currentTime = ttime;
}

void TuioObject::removeTuioSymbol (TuioTime ttime) {
    if (symbol != NULL) symbol->remove(ttime);
    currentTime = ttime;
}

void TuioObject::deleteAllTuioComponents() {
    deleteTuioToken();
    deleteTuioPointer();
    deleteTuioBounds();
    deleteTuioSymbol();
}

void TuioObject::deleteTuioToken () {
    if (token != NULL) {
        delete token;
        token = NULL;
        //currentTime = TuioTime::getSessionTime();
    }
}

void TuioObject::deleteTuioPointer () {
    if (pointer != NULL) {
        delete pointer;
        pointer = NULL;
        //currentTime = TuioTime::getSessionTime();
    }
}

void TuioObject::deleteTuioBounds () {
    if (bounds != NULL) {
        delete bounds;
        bounds = NULL;
        //currentTime = TuioTime::getSessionTime();
    }
}

void TuioObject::deleteTuioSymbol () {
    if (symbol != NULL) {
        delete symbol;
        symbol = NULL;
        //currentTime = TuioTime::getSessionTime();
    }
}

void TuioObject::clearAllTuioComponents() {
    clearTuioToken();
    clearTuioPointer();
    clearTuioBounds();
    clearTuioSymbol();
}

void TuioObject::clearTuioToken () {
    if (token != NULL) {
        token = NULL;
        //currentTime = TuioTime::getSessionTime();
    }
}

void TuioObject::clearTuioPointer () {
    if (pointer != NULL) {
        pointer = NULL;
        //currentTime = TuioTime::getSessionTime();
    }
}

void TuioObject::clearTuioBounds () {
    if (bounds != NULL) {
        bounds = NULL;
        //currentTime = TuioTime::getSessionTime();
    }
}

void TuioObject::clearTuioSymbol () {
    if (symbol != NULL) {
        symbol = NULL;
        //currentTime = TuioTime::getSessionTime();
    }
}

bool TuioObject::containsAnyTuioComponent () {
    if (token != NULL) return true;
    else if (pointer != NULL) return true;
    else if (bounds != NULL) return true;
    else if (symbol != NULL) return true;
    else return false;
}

bool TuioObject::containsTuioToken () {
    if (token != NULL) return true;
    else return false;
}

bool TuioObject::containsTuioPointer () {
    if (pointer != NULL) return true;
    else return false;
}

bool TuioObject::containsTuioBounds () {
    if (bounds != NULL) return true;
    else return false;
}

bool TuioObject::containsTuioSymbol () {
    if (symbol != NULL) return true;
    else return false;
}

bool TuioObject::containsNewTuioToken () {
    if (token == NULL) return false;
    else if (token->getTuioState()==TUIO_ADDED) return true;
    else return false;
}

bool TuioObject::containsNewTuioPointer () {
    if (pointer == NULL) return false;
    else if (pointer->getTuioState()==TUIO_ADDED) return true;
    else return false;
}

bool TuioObject::containsNewTuioBounds () {
    if (bounds == NULL) return false;
    else if (bounds->getTuioState()==TUIO_ADDED) return true;
    else return false;
}

bool TuioObject::containsNewTuioSymbol () {
    if (symbol == NULL) return false;
    else if (symbol->getTuioState()==TUIO_ADDED) return true;
    else return false;
}

TuioToken* TuioObject::getTuioToken () {
    return token;
}

TuioPointer* TuioObject::getTuioPointer () {
    return pointer;
}

TuioBounds* TuioObject::getTuioBounds () {
    return bounds;
}

TuioSymbol* TuioObject::getTuioSymbol () {
    return symbol;
}

void TuioObject::stop(TuioTime ttime){
    if (token!=NULL) token->stop(ttime);
    if (pointer!=NULL) pointer->stop(ttime);
    if (bounds!=NULL) bounds->stop(ttime);
    currentTime = ttime;
}

void TuioObject::remove(TuioTime ttime){
    if (token!=NULL) token->remove(ttime);
    if (pointer!=NULL) pointer->remove(ttime);
    if (bounds!=NULL) bounds->remove(ttime);
    currentTime = ttime;
    state = TUIO_REMOVED;
}

void TuioObject::update(TuioTime ttime){
    currentTime = ttime;
    state = TUIO_IDLE;
}

bool TuioObject::isMoving(){
    if ((token!=NULL) && token->isMoving()) return true;
    if ((pointer!=NULL) && pointer->isMoving()) return true;
    if ((bounds!=NULL) && bounds->isMoving()) return true;
    return false;
}

TuioTime TuioObject::getTuioTime() const{
    return currentTime;
}

TuioTime TuioObject::getStartTime() const{
	return startTime;
}

unsigned char TuioObject::getTuioState() const{
    return state;
}

