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

#include "spicepositioninformation.h"

#include <util/spice.h>

namespace openspace {
    
SpicePositionInformation::SpicePositionInformation(): _target(0), _origin(0), _position() {}
SpicePositionInformation::~SpicePositionInformation() {}

bool SpicePositionInformation::initializeWithDictionary(ghoul::Dictionary* dictionary) {
    
    std::string body, observer;
    if (dictionary->getValue("Body", body) && dictionary->getValue("Observer", observer)) {
        int bsuccess = 0;
        int osuccess = 0;
        Spice::ref().bod_NameToInt(body, &_target, &bsuccess);
        Spice::ref().bod_NameToInt(observer, &_origin, &osuccess);
        
        if (bsuccess && osuccess) {
            return true;
        }
    }
    
    return true;
}

const psc& SpicePositionInformation::position() const {
    return _position;
}

void SpicePositionInformation::update() {
    double state[3];
    
    Spice::ref().spk_getPosition(_target, _origin, state);
    _position = psc::CreatePSC(state[0], state[1], state[2]);
}

} // namespace openspace