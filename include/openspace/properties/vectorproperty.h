/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#ifndef __VECTORPROPERTY_H__
#define __VECTORPROPERTY_H__

#include "openspace/properties/numericalproperty.h"

#include <ghoul/glm.h>

namespace openspace {
namespace properties {

REGISTER_TEMPLATEPROPERTY_HEADER(BVec2Property, glm::bvec2);
REGISTER_TEMPLATEPROPERTY_HEADER(BVec3Property, glm::bvec3);
REGISTER_TEMPLATEPROPERTY_HEADER(BVec4Property, glm::bvec4);

REGISTER_NUMERICALPROPERTY_HEADER(Vec2Property, glm::vec2);
REGISTER_NUMERICALPROPERTY_HEADER(Vec3Property, glm::vec3);
REGISTER_NUMERICALPROPERTY_HEADER(Vec4Property, glm::vec4);
REGISTER_NUMERICALPROPERTY_HEADER(DVec2Property, glm::dvec2);
REGISTER_NUMERICALPROPERTY_HEADER(DVec3Property, glm::dvec3);
REGISTER_NUMERICALPROPERTY_HEADER(DVec4Property, glm::dvec4);
REGISTER_NUMERICALPROPERTY_HEADER(IVec2Property, glm::ivec2);
REGISTER_NUMERICALPROPERTY_HEADER(IVec3Property, glm::ivec3);
REGISTER_NUMERICALPROPERTY_HEADER(IVec4Property, glm::ivec4);
REGISTER_NUMERICALPROPERTY_HEADER(UVec2Property, glm::uvec2);
REGISTER_NUMERICALPROPERTY_HEADER(UVec3Property, glm::uvec3);
REGISTER_NUMERICALPROPERTY_HEADER(UVec4Property, glm::uvec4);

} // namespace properties
} // namespace openspace

#endif // __INTPROPERTY_H__
