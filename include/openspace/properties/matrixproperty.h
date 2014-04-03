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

#ifndef __MATRIXPROPERTY_H__
#define __MATRIXPROPERTY_H__

#include "openspace/properties/numericalproperty.h"

#include <ghoul/glm.h>

namespace openspace {
namespace properties {

REGISTER_NUMERICALPROPERTY_HEADER(Mat2Property, glm::mat2x2);
REGISTER_NUMERICALPROPERTY_HEADER(Mat2x3Property, glm::mat2x3);
REGISTER_NUMERICALPROPERTY_HEADER(Mat2x4Property, glm::mat2x4);
REGISTER_NUMERICALPROPERTY_HEADER(Mat3x2Property, glm::mat3x2);
REGISTER_NUMERICALPROPERTY_HEADER(Mat3Property, glm::mat3x3);
REGISTER_NUMERICALPROPERTY_HEADER(Mat3x4Property, glm::mat3x4);
REGISTER_NUMERICALPROPERTY_HEADER(Mat4x2Property, glm::mat4x2);
REGISTER_NUMERICALPROPERTY_HEADER(Mat4x3Property, glm::mat4x3);
REGISTER_NUMERICALPROPERTY_HEADER(Mat4Property, glm::mat4x4);
REGISTER_NUMERICALPROPERTY_HEADER(DMat2Property, glm::dmat2x2);
REGISTER_NUMERICALPROPERTY_HEADER(DMat2x3Property, glm::dmat2x3);
REGISTER_NUMERICALPROPERTY_HEADER(DMat2x4Property, glm::dmat2x4);
REGISTER_NUMERICALPROPERTY_HEADER(DMat3x2Property, glm::dmat3x2);
REGISTER_NUMERICALPROPERTY_HEADER(DMat3Property, glm::dmat3x3);
REGISTER_NUMERICALPROPERTY_HEADER(DMat3x4Property, glm::dmat3x4);
REGISTER_NUMERICALPROPERTY_HEADER(DMat4x2Property, glm::dmat4x2);
REGISTER_NUMERICALPROPERTY_HEADER(DMat4x3Property, glm::dmat4x3);
REGISTER_NUMERICALPROPERTY_HEADER(DMat4Property, glm::dmat4x4);

} // namespace properties
} // namespace openspace

#endif // __MATRIXPROPERTY_H__
