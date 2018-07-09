/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_SERVER___JSON_CONVERTERS___H__
#define __OPENSPACE_MODULE_SERVER___JSON_CONVERTERS___H__

#include <openspace/json.h>
#include <ghoul/glm.h>

namespace openspace::properties {

class Property;
class PropertyOwner;

void to_json(nlohmann::json& j, const Property& p);
void to_json(nlohmann::json& j, const Property* pP);
void to_json(nlohmann::json& j, const PropertyOwner& p);
void to_json(nlohmann::json& j, const PropertyOwner* p);

} // namespace openspace::properties

namespace openspace {

class Renderable;
class SceneGraphNode;

void to_json(nlohmann::json& j, const SceneGraphNode& n);
void to_json(nlohmann::json& j, const SceneGraphNode* pN);

void to_json(nlohmann::json& j, const Renderable& r);
void to_json(nlohmann::json& j, const Renderable* pR);

} // namespace openspace

namespace glm {

void to_json(nlohmann::json& j, const dvec3& v);

} // namespace glm

#endif // __OPENSPACE_MODULE_SERVER___JSON_CONVERTERS___H__
