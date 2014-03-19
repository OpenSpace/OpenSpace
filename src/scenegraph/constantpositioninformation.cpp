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

#include <openspace/scenegraph/constantpositioninformation.h>

namespace openspace {

ConstantPositionInformation::ConstantPositionInformation(const ghoul::Dictionary& dictionary) {
    double x = 0.0, y = 0.0, z = 0.0, e = 0.0;
    if (dictionary.hasKey("Position.1")) {
        dictionary.getValue("Position.1", x);
        dictionary.getValue("Position.2", y);
        dictionary.getValue("Position.3", z);
        dictionary.getValue("Position.4", e);
    }
    _position = psc(x, y, z, e);
}

ConstantPositionInformation::~ConstantPositionInformation() {}

bool ConstantPositionInformation::initialize() {
    return true;
}

const psc& ConstantPositionInformation::position() const {    
    return _position;
}

void ConstantPositionInformation::update() {

}

} // namespace openspace