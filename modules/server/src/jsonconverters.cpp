/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/server/include/jsonconverters.h>
#include <openspace/properties/property.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/rendering/renderable.h>
#include <ghoul/logging/logmanager.h>
#include <ext/json/json.hpp>

using json = nlohmann::json;

namespace openspace::properties {

void to_json(json &j, const Property &p) {
    j = {
        { "Description", json::parse(p.generateBaseDescription()) },
        { "Value", p.jsonValue() }
    };
    j["Description"]["description"] = p.description();
}

void to_json(json &j, const Property* pP) {
    j = *pP;
}

void to_json(json &j, const PropertyOwner &p) {
    j = {
        { "name", p.name() },
        { "description", p.description() },
        { "properties", p.properties() },
        { "subowners", p.propertySubOwners() },
        { "tag", p.tags() }
    };
}

void to_json(json &j, const PropertyOwner* p) {
    j = *p;
}

} // namespace openspace::properties

namespace openspace {

void to_json(json &j, const SceneGraphNode &n) {
    j = {
        { "name", n.name() },
        { "worldPosition", n.worldPosition() },
        { "position", n.position() },
        { "tags", n.tags() },

        { "propertiesCount", n.properties().size() },
        { "properties", n.properties() },

        { "subowners", n.propertySubOwners() }
    };
/*
    auto renderable = n.renderable();
    if (renderable != nullptr) {
        j["renderable"] = renderable;
    }*/

    auto parent = n.parent();
    if (parent != nullptr) {
        j["parent"] = {
            { "name", parent->name() }
        };
    }
}

void to_json(json &j, const SceneGraphNode* pN) {
    // Use reference converter instead of pointer
    j = *pN;
}

void to_json(json &j, const Renderable &r) {
    j = {
        { "properties", r.properties() },
        { "enabled", r.isEnabled() }
    };
}

void to_json(json &j, const Renderable* pR) {
    // Use reference converter instead of pointer
    j = *pR;
}


} // namespace openspace

namespace glm {

void to_json(json &j, const dvec3 &v) {
    j = {
        { "value", {v.x, v.y, v.z} },
        { "type", "vec3" }
    };
}

}
