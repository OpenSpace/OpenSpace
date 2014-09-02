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

#include <openspace/properties/stringproperty.h>

#include <ghoul/lua/ghoul_lua.h>

namespace openspace {
namespace properties {

StringProperty::StringProperty(std::string identifier, std::string guiName)
    : StringProperty(
            std::move(identifier), std::move(guiName),
            PropertyDelegate<TemplateProperty<std::string>>::defaultValue<std::string>())
{
}

StringProperty::StringProperty(std::string identifier, std::string guiName,
                                std::string value)
    : TemplateProperty(std::move(identifier), std::move(guiName), std::move(value))
{}

template <>
std::string PropertyDelegate<TemplateProperty<std::string>>::className() {
    return "StringProperty";
}

template <>
template <>
std::string PropertyDelegate<TemplateProperty<std::string>>::defaultValue<std::string>() {
    return "";
}

template <>
template <>
std::string PropertyDelegate<TemplateProperty<std::string>>::fromLuaValue<std::string>(
      lua_State* state, bool& success)
{
	success = lua_isstring(state, -1) == 1;
	if (success)
		return lua_tostring(state, -1);
	else
		return "";
}

template <>
template <>
bool PropertyDelegate<TemplateProperty<std::string>>::toLuaValue<std::string>(
      lua_State* state, std::string value)
{
	lua_pushstring(state, value.c_str());
	return true;
}

template <>
int PropertyDelegate<TemplateProperty<std::string>>::typeLua() {
	return LUA_TSTRING;
}

} // namespace properties
} // namespace openspace
