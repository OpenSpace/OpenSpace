/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/logging/logmanager.h>

using json = nlohmann::json;

namespace openspace::properties {

void to_json(json& j, const Property& p) {
    const std::string description = p.generateJsonDescription();
    json desc = json::parse(description);

    const std::string value = p.jsonValue();
    json val = json::parse(value);

    j = {
        { "Description", desc },
        { "Value", val }
    };
    j["Description"]["description"] = p.description();
}

void to_json(json& j, const Property* pP) {
    j = *pP;
}

void to_json(json& j, const PropertyOwner& p) {
    j = {
        { "identifier", p.identifier() },
        { "guiName", p.guiName() },
        { "description", p.description() },
        { "properties", p.properties() },
        { "subowners", p.propertySubOwners() },
        { "tag", p.tags() }
    };
}

void to_json(json& j, const PropertyOwner* p) {
    j = *p;
}

} // namespace openspace::properties

namespace ghoul {

void to_json(json& j, const Dictionary& d) {
    json object;
    for (const std::string_view k : d.keys()) {
        const std::string key = std::string(k);
        if (d.hasValue<glm::dvec4>(key)) {
            const glm::dvec4 v = d.value<glm::dvec4>(key);
            object[key] = json::array({ v[0], v[1], v[2], v[3] });
        }
        else if (d.hasValue<glm::dvec3>(key)) {
            const glm::dvec3 v = d.value<glm::dvec3>(key);
            object[key] = json::array({ v[0], v[1], v[2] });
        }
        else if (d.hasValue<glm::dvec2>(key)) {
            const glm::dvec2 v = d.value<glm::dvec2>(key);
            object[key] = json::array({ v[0], v[1] });
        }
        else if (d.hasValue<double>(key)) {
            object[key] = d.value<double>(key);
        }
        else if (d.hasValue<int>(key)) {
            object[key] = d.value<int>(key);
        }
        else if (d.hasValue<std::string>(key)) {
            object[key] = d.value<std::string>(key);
        }
        else if (d.hasValue<bool>(key)) {
            object[key] = d.value<bool>(key);
        }
        else if (d.hasValue<Dictionary>(key)) {
            json child;
            to_json(child, d.value<Dictionary>(key));
            object[key] = child;
        }
        else {
            object[key] = nullptr;
        }
    }
    j = object;
}

void to_json(json& j, const Dictionary* d) {
    j = *d;
}

} // namespace ghoul

namespace openspace {

void to_json(json& j, const SceneGraphNode& n) {
    j = {
        { "identifier", n.identifier() },
        { "worldPosition", n.worldPosition() },
        { "position", n.position() },
        { "tags", n.tags() },

        { "propertiesCount", n.properties().size() },
        { "properties", n.properties() },

        { "subowners", n.propertySubOwners() }
    };

    SceneGraphNode* parent = n.parent();
    if (parent) {
        j["parent"] = {
            { "identifier", parent->identifier() }
        };
    }
}

void to_json(json& j, const SceneGraphNode* pN) {
    // Use reference converter instead of pointer
    j = *pN;
}

void to_json(json& j, const Renderable& r) {
    j = {
        { "properties", r.properties() },
        { "enabled", r.isEnabled() }
    };
}

void to_json(json& j, const Renderable* pR) {
    // Use reference converter instead of pointer
    j = *pR;
}

} // namespace openspace

namespace glm {

void to_json(json& j, const dvec3& v) {
    j = {
        { "value", { v.x, v.y, v.z } },
        { "type", "vec3" }
    };
}

} // namespace glm
