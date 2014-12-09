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

#include <openspace/properties/selectionproperty.h>

namespace {
	const std::string _loggerCat = "SelectionProperty";
}

namespace openspace {
namespace properties {

SelectionProperty::SelectionProperty(std::string identifier, std::string guiName)
	: TemplateProperty(std::move(identifier), std::move(guiName), std::vector<int>())
{}

void SelectionProperty::addOption(Option option) {
	// @COPY-N-PASTE from optionproperty.cpp, possible refactoring? ---abock
	for (const Option& o : _options) {
		if (o.value == option.value) {
			LWARNING("The value of option {" << o.value << " -> " << o.description <<
				"} was already registered when trying to add option {" << option.value <<
				" -> " << option.description << "}");
			return;
		}
	}
	_options.push_back(std::move(option));
}

const std::vector<SelectionProperty::Option>& SelectionProperty::options() const {
	return _options;
}

void SelectionProperty::setValue(std::vector<int> value) {
	_values = std::move(value);
}

template <>
std::string PropertyDelegate<TemplateProperty<std::vector<int>>>::className() {
	return "SelectionProperty";
}

template <>
template <>
std::vector<int> PropertyDelegate<TemplateProperty<std::vector<int>>>::fromLuaValue(lua_State* state, bool& success) {
	static const int KEY = -2;
	static const int VAL = -1;

	std::vector<int> result;

	if (!lua_istable(state, -1)) {
		LERROR("Parameter passed to the property is not a table");
		success = false;
		return result;
	}

	lua_pushnil(state);
	while (lua_next(state, -2) != 0) {
		int valueType = lua_type(state, VAL);

		if (lua_isnumber(state, VAL)) {
			int number = static_cast<int>(lua_tonumber(state, VAL));
			result.push_back(number);
		}
		else {
			success = false;
			return std::vector<int>();
		}

		lua_pop(state, 1);
	}

	success = true;
	return result;
}

template <>
template <>
bool PropertyDelegate<TemplateProperty<std::vector<int>>>::toLuaValue(lua_State* state, std::vector<int> value) {
	//@NOTE Untested ---abock
	lua_newtable(state);
	for (int i = 0; i < value.size(); ++i) {
		int v = value[i];
		lua_pushinteger(state, v);
		lua_setfield(state, -2, std::to_string(i).c_str());
	}
	return true;
}

template <>
int PropertyDelegate<TemplateProperty<std::vector<int>>>::typeLua() {
	return LUA_TTABLE;
}

//REGISTER_TEMPLATEPROPERTY_SOURCE(SelectionProperty, std::vector<int>, std::vector<int>(),
//	[](lua_State* state, bool& success) -> std::vector<int> {
//		static const int KEY = -2;
//		static const int VAL = -1;
//
//		std::vector<int> result;
//
//		if (!lua_istable(state, -1)) {
//			LERROR("Parameter passed to the property is not a table");
//			success = false;
//			return result;
//		}
//
//		lua_pushnil(state);
//		while (lua_next(state, -2) != 0) {
//			int valueType = lua_type(state, VAL);
//
//			if (lua_isnumber(state, VAL)) {
//				int number = static_cast<int>(lua_tonumber(state, VAL));
//				result.push_back(number);
//			}
//			else {
//				success = false;
//				return std::vector<int>();
//			}
//
//			lua_pop(state, 1);
//		}
//
//		success = true;
//		return result;
//	},
//	[](lua_State* state, const std::vector<int>& value) -> bool {
//		//@NOTE Untested ---abock
//		lua_newtable(state);
//		for (int i = 0; i < value.size(); ++i) {
//			int v = value[i];
//			lua_pushinteger(state, v);
//			lua_setfield(state, -2, std::to_string(i).c_str());
//		}
//		return true;
//	}, LUA_TTABLE
//);

} // namespace properties
} // namespace openspace
