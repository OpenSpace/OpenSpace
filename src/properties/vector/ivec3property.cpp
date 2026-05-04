/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/properties/vector/ivec3property.h>

#include <ghoul/lua/lua_helper.h>

namespace openspace {

IVec3Property::IVec3Property(PropertyInfo info, glm::ivec3 value, glm::ivec3 minValue,
                             glm::ivec3 maxValue, glm::ivec3 stepValue)
    : NumericalProperty<glm::ivec3>(
        std::move(info),
        std::move(value),
        std::move(minValue),
        std::move(maxValue),
        std::move(stepValue)
    )
{}

std::string_view IVec3Property::className() const {
    return "IVec3Property";
}

ghoul::lua::LuaTypes IVec3Property::typeLua() const {
    return ghoul::lua::LuaTypes::Table;
}

void IVec3Property::getLuaValue(lua_State* state) const {
    ghoul::lua::push(state, _value);
}

glm::ivec3 IVec3Property::toValue(lua_State* state) const {
    return ghoul::lua::value<glm::ivec3>(state);
}

std::string IVec3Property::stringValue() const {
    return formatJson(_value);
}

nlohmann::json IVec3Property::Schema() {
    nlohmann::json metaData = NumericalProperty<glm::ivec3>::MetaDataSchema();
    metaData["$defs"]["ViewOptions"] = ViewOptionsSchema();
    metaData["properties"]["type"] = { { "const", "IVec3Property" } };
    metaData["properties"]["viewOptions"] = { { "$ref", "#/$defs/ViewOptions" } };
    metaData["required"].push_back("type");

    nlohmann::json typeDef = nlohmann::json::parse(R"(
        {
          "type": "object",
          "properties": {
            "uri": { "type": "string" },
            "value": {
              "type": "array",
              "items": { "type": "number" },
              "minItems": 3,
              "maxItems": 3
            }
          },
          "additionalProperties": false,
          "required": ["metaData", "uri", "value"]
        }
    )");
    nlohmann::json sharedDefs = extractDefs(metaData);
    typeDef["properties"]["metaData"] = metaData;

    nlohmann::json schema;
    schema["$defs"] = sharedDefs;
    schema["typedefs"]["IVec3Property"] = typeDef;
    return schema;
}

} // namespace openspace
