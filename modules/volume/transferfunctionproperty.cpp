/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/volume/transferfunctionproperty.h>
#include <ghoul/lua/ghoul_lua.h>

namespace openspace::properties {

TransferFunctionProperty::TransferFunctionProperty(Property::PropertyInfo info,
                                                   volume::TransferFunction value)
    : TemplateProperty<volume::TransferFunction>(std::move(info), std::move(value))
{}

std::string_view TransferFunctionProperty::className() const {
    return "TransferFunctionProperty";
}

ghoul::lua::LuaTypes TransferFunctionProperty::typeLua() const {
    return ghoul::lua::LuaTypes::Table;
}

void TransferFunctionProperty::getLuaValue(lua_State* state) const {
    _value.envelopesToLua(state);
}

volume::TransferFunction TransferFunctionProperty::toValue(lua_State* state) const {
    openspace::volume::TransferFunction tf;
    tf.setEnvelopesFromLua(state);
    return tf;
}

std::string TransferFunctionProperty::stringValue() const {
    return _value.serializedToString();
}

} // namespace openspace::properties
