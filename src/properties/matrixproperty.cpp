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

#include "openspace/properties/matrixproperty.h"

#include <limits>

using std::numeric_limits;

namespace openspace {
namespace properties {

REGISTER_NUMERICALPROPERTY_SOURCE(Mat2Property, glm::mat2x2, glm::mat2x2(0),
    glm::mat2x2(numeric_limits<float>::lowest()),
    glm::mat2x2(numeric_limits<float>::max()), glm::mat2x2(0.01f));
REGISTER_NUMERICALPROPERTY_SOURCE(Mat2x3Property, glm::mat2x3, glm::mat2x3(0),
    glm::mat2x3(numeric_limits<float>::lowest()),
    glm::mat2x3(numeric_limits<float>::max()), glm::mat2x3(0.01f));
REGISTER_NUMERICALPROPERTY_SOURCE(Mat2x4Property, glm::mat2x4, glm::mat2x4(0),
    glm::mat2x4(numeric_limits<float>::lowest()),
    glm::mat2x4(numeric_limits<float>::max()), glm::mat2x4(0.01f));
REGISTER_NUMERICALPROPERTY_SOURCE(Mat3x2Property, glm::mat3x2, glm::mat3x2(0),
    glm::mat3x2(numeric_limits<float>::lowest()),
    glm::mat3x2(numeric_limits<float>::max()), glm::mat3x2(0.01f));
REGISTER_NUMERICALPROPERTY_SOURCE(Mat3Property, glm::mat3x3, glm::mat3x3(0),
    glm::mat3x3(numeric_limits<float>::lowest()),
    glm::mat3x3(numeric_limits<float>::max()), glm::mat3x3(0.01f));
REGISTER_NUMERICALPROPERTY_SOURCE(Mat3x4Property, glm::mat3x4, glm::mat3x4(0),
    glm::mat3x4(numeric_limits<float>::lowest()),
    glm::mat3x4(numeric_limits<float>::max()), glm::mat3x4(0.01f));
REGISTER_NUMERICALPROPERTY_SOURCE(Mat4x2Property, glm::mat4x2, glm::mat4x2(0),
    glm::mat4x2(numeric_limits<float>::lowest()),
    glm::mat4x2(numeric_limits<float>::max()), glm::mat4x2(0.01f));
REGISTER_NUMERICALPROPERTY_SOURCE(Mat4x3Property, glm::mat4x3, glm::mat4x3(0),
    glm::mat4x3(numeric_limits<float>::lowest()),
    glm::mat4x3(numeric_limits<float>::max()), glm::mat4x3(0.01f));
REGISTER_NUMERICALPROPERTY_SOURCE(Mat4Property, glm::mat4x4, glm::mat4x4(0),
    glm::mat4x4(numeric_limits<float>::lowest()),
    glm::mat4x4(numeric_limits<float>::max()), glm::mat4x4(0.01f));
REGISTER_NUMERICALPROPERTY_SOURCE(DMat2Property, glm::dmat2x2, glm::dmat2x2(0),
    glm::dmat2x2(numeric_limits<double>::lowest()),
    glm::dmat2x2(numeric_limits<double>::max()), glm::dmat2x2(0.01));
REGISTER_NUMERICALPROPERTY_SOURCE(DMat2x3Property, glm::dmat2x3, glm::dmat2x3(0),
    glm::dmat2x3(numeric_limits<double>::lowest()),
    glm::dmat2x3(numeric_limits<double>::max()), glm::dmat2x3(0.01));
REGISTER_NUMERICALPROPERTY_SOURCE(DMat2x4Property, glm::dmat2x4, glm::dmat2x4(0),
    glm::dmat2x4(numeric_limits<double>::lowest()),
    glm::dmat2x4(numeric_limits<double>::max()), glm::dmat2x4(0.01));
REGISTER_NUMERICALPROPERTY_SOURCE(DMat3x2Property, glm::dmat3x2, glm::dmat3x2(0),
    glm::dmat3x2(numeric_limits<double>::lowest()),
    glm::dmat3x2(numeric_limits<double>::max()), glm::dmat3x2(0.01));
REGISTER_NUMERICALPROPERTY_SOURCE(DMat3Property, glm::dmat3x3, glm::dmat3x3(0),
    glm::dmat3x3(numeric_limits<double>::lowest()),
    glm::dmat3x3(numeric_limits<double>::max()), glm::dmat3x3(0.01));
REGISTER_NUMERICALPROPERTY_SOURCE(DMat3x4Property, glm::dmat3x4, glm::dmat3x4(0),
    glm::dmat3x4(numeric_limits<double>::lowest()),
    glm::dmat3x4(numeric_limits<double>::max()), glm::dmat3x4(0.01));
REGISTER_NUMERICALPROPERTY_SOURCE(DMat4x2Property, glm::dmat4x2, glm::dmat4x2(0),
    glm::dmat4x2(numeric_limits<double>::lowest()),
    glm::dmat4x2(numeric_limits<double>::max()), glm::dmat4x2(0.01));
REGISTER_NUMERICALPROPERTY_SOURCE(DMat4x3Property, glm::dmat4x3, glm::dmat4x3(0),
    glm::dmat4x3(numeric_limits<double>::lowest()),
    glm::dmat4x3(numeric_limits<double>::max()), glm::dmat4x3(0.01));
REGISTER_NUMERICALPROPERTY_SOURCE(DMat4Property, glm::dmat4x4, glm::dmat4x4(0),
    glm::dmat4x4(numeric_limits<double>::lowest()),
    glm::dmat4x4(numeric_limits<double>::max()), glm::dmat4x4(0.01));




//REGISTER_NUMERICALPROPERTY_SOURCE(Vec2Property, glm::vec2, glm::vec2(0),
//    glm::vec2(numeric_limits<float>::lowest()), glm::vec2(numeric_limits<float>::max()),
//    glm::vec2(0.01f));
//REGISTER_NUMERICALPROPERTY_SOURCE(Vec3Property, glm::vec3, glm::vec3(0),
//    glm::vec3(numeric_limits<float>::lowest()), glm::vec3(numeric_limits<float>::max()),
//    glm::vec3(0.01f));
//REGISTER_NUMERICALPROPERTY_SOURCE(Vec4Property, glm::vec4, glm::vec4(0),
//    glm::vec4(numeric_limits<float>::lowest()), glm::vec4(numeric_limits<float>::max()),
//    glm::vec4(0.01f));
//REGISTER_NUMERICALPROPERTY_SOURCE(DVec2Property, glm::dvec2, glm::dvec2(0),
//    glm::dvec2(numeric_limits<double>::lowest()),
//    glm::dvec2(numeric_limits<double>::max()), glm::dvec2(0.01));
//REGISTER_NUMERICALPROPERTY_SOURCE(DVec3Property, glm::dvec3, glm::dvec3(0),
//    glm::dvec3(numeric_limits<double>::lowest()),
//    glm::dvec3(numeric_limits<double>::max()), glm::dvec3(0.01));
//REGISTER_NUMERICALPROPERTY_SOURCE(DVec4Property, glm::dvec4, glm::dvec4(0),
//    glm::dvec4(numeric_limits<double>::lowest()),
//    glm::dvec4(numeric_limits<double>::max()), glm::dvec4(0.01));
//REGISTER_NUMERICALPROPERTY_SOURCE(IVec2Property, glm::ivec2, glm::ivec2(0),
//    glm::ivec2(numeric_limits<int>::lowest()), glm::ivec2(numeric_limits<int>::max()),
//    glm::ivec2(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(IVec3Property, glm::ivec3, glm::ivec3(0),
//    glm::ivec3(numeric_limits<int>::lowest()), glm::ivec3(numeric_limits<int>::max()),
//    glm::ivec3(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(IVec4Property, glm::ivec4, glm::ivec4(0),
//    glm::ivec4(numeric_limits<int>::lowest()), glm::ivec4(numeric_limits<int>::max()),
//    glm::ivec4(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(UVec2Property, glm::uvec2, glm::uvec2(0),
//    glm::uvec2(numeric_limits<unsigned int>::lowest()),
//    glm::uvec2(numeric_limits<unsigned int>::max()), glm::uvec2(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(UVec3Property, glm::uvec3, glm::uvec3(0),
//    glm::uvec3(numeric_limits<unsigned int>::lowest()),
//    glm::uvec3(numeric_limits<unsigned int>::max()), glm::uvec3(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(UVec4Property, glm::uvec4, glm::uvec4(0),
//    glm::uvec4(numeric_limits<unsigned int>::lowest()),
//    glm::uvec4(numeric_limits<unsigned int>::max()), glm::uvec4(1));


//REGISTER_NUMERICALPROPERTY_SOURCE(CharProperty, char, char(0),
//    numeric_limits<char>::min(), numeric_limits<char>::max(), char(1));
////REGISTER_NUMERICALPROPERTY_SOURCE(Char16Property, char16_t, char16_t(0),
////    numeric_limits<char16_t>::min(), numeric_limits<char16_t>::max(), char16_t(1));
////REGISTER_NUMERICALPROPERTY_SOURCE(Char32Property, char32_t, char32_t(0),
////    numeric_limits<char32_t>::min(), numeric_limits<char32_t>::max(), char32_t(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(WCharProperty, wchar_t, wchar_t(0),
//    numeric_limits<wchar_t>::min(), numeric_limits<wchar_t>::max(), wchar_t(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(SignedCharProperty, signed char, signed char(0),
//    numeric_limits<signed char>::min(), numeric_limits<signed char>::max(),
//    signed char(0));
//REGISTER_NUMERICALPROPERTY_SOURCE(UCharProperty, unsigned char, unsigned char(0),
//    numeric_limits<unsigned char>::min(), numeric_limits<unsigned char>::max(),
//    unsigned char(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(ShortProperty, short, short(0),
//    numeric_limits<short>::min(), numeric_limits<short>::max(), short(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(UShortProperty, unsigned short, unsigned short(0),
//    numeric_limits<unsigned short>::min(), numeric_limits<unsigned short>::max(),
//    unsigned short(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(IntProperty, int, int(0),
//    numeric_limits<int>::min(), numeric_limits<int>::max(), int(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(UIntProperty, unsigned int, unsigned int(0),
//    numeric_limits<unsigned int>::min(), numeric_limits<unsigned int>::max(),
//    unsigned int(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(LongProperty, long, long(0),
//    numeric_limits<long>::min(), numeric_limits<long>::max(), long(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(ULongProperty, unsigned long, unsigned long(0),
//    numeric_limits<unsigned long>::min(), numeric_limits<unsigned long>::max(),
//    unsigned long(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(LongLongProperty, long long, long long(0),
//    numeric_limits<long long>::min(), numeric_limits<long long>::max(), long long(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(ULongLongProperty, unsigned long long,
//    unsigned long long(1), numeric_limits<unsigned long long>::min(),
//    numeric_limits<unsigned long long>::max(), unsigned long long(1));
//REGISTER_NUMERICALPROPERTY_SOURCE(FloatProperty, float, 0.f,
//    numeric_limits<float>::min(), numeric_limits<float>::max(), 0.01f);
//REGISTER_NUMERICALPROPERTY_SOURCE(DoubleProperty, double, 0.0,
//    numeric_limits<double>::min(), numeric_limits<double>::max(), 0.01);
//REGISTER_NUMERICALPROPERTY_SOURCE(LongDoubleProperty, long double, long double(0),
//    numeric_limits<long double>::min(), numeric_limits<long double>::max(),
//    long double(0.01f));


} // namespace properties
} // namespace openspace
