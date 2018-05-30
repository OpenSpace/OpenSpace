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

#include <openspace/properties/selectionproperty.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>

namespace {
    constexpr const char* _loggerCat = "SelectionProperty";

    const char Delimiter = ',';
} // namespace

namespace openspace::properties {

const std::string SelectionProperty::OptionsKey = "Options";

SelectionProperty::SelectionProperty(Property::PropertyInfo info)
    : TemplateProperty(std::move(info), std::vector<int>())
{}

void SelectionProperty::addOption(Option option) {
    // @REFACTOR from optionproperty.cpp, possible refactoring? ---abock
    for (const Option& o : _options) {
        if (o.value == option.value) {
            LWARNINGC(
                "SelectionProperty",
                fmt::format("The value of option {{ {} -> {} }} was already registered "
                    "when trying to add option {{ {} -> {} }}",
                    o.value, o.description, option.value, option.description
                )
            );
            return;
        }
    }
    _options.push_back(std::move(option));
}

void SelectionProperty::removeOptions() {
    _options.clear();
}

const std::vector<SelectionProperty::Option>& SelectionProperty::options() const {
    return _options;
}

template <>
std::string PropertyDelegate<TemplateProperty<std::vector<int>>>::className() {
    return "SelectionProperty";
}

template <>
template <>
std::vector<int> PropertyDelegate<TemplateProperty<std::vector<int>>>::fromLuaValue(
                                                          lua_State* state, bool& success)
{
    static const int KEY = -2;
    static const int VAL = -1;

    std::vector<int> result;

    if (!lua_istable(state, VAL)) {
        LERROR("Parameter passed to the property is not a table");
        success = false;
        return result;
    }

    lua_pushnil(state);
    while (lua_next(state, KEY) != 0) {
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
bool PropertyDelegate<TemplateProperty<std::vector<int>>>::toLuaValue(
                                          lua_State* state, const std::vector<int>& value)
{
    //@NOTE Untested ---abock
    lua_newtable(state);
    for (size_t i = 0; i < value.size(); ++i) {
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

template <>
template <>
std::vector<int> PropertyDelegate<TemplateProperty<std::vector<int>>>::fromString(
                                                  const std::string& value, bool& success)
{
    std::string v = value;
    std::vector<int> result;
    size_t pos = 0;
    while ((pos = v.find(Delimiter)) != std::string::npos) {
        std::string token = v.substr(0, pos);
        result.push_back(std::stoi(token));
        v.erase(0, pos + 1); // 1: Delimiter.length()
    }
    success = true;
    return result;
}

template <>
template <>
bool PropertyDelegate<TemplateProperty<std::vector<int>>>::toString(
                                   std::string& outValue, const std::vector<int>& inValue)
{
    outValue = "[";
    for (int i : inValue) {
        outValue += std::to_string(i) + Delimiter;
    }
    outValue += "]";
    return true;
}

std::string SelectionProperty::generateAdditionalJsonDescription() const {
    std::string result = "{ \"" + OptionsKey + "\": [";
    for (size_t i = 0; i < _options.size(); ++i) {
        const Option& o = _options[i];
        result += "{";
        result +=  "\"" + std::to_string(o.value) + "\": \"" + o.description + "\"";
        result += "}";
        if (i != _options.size() - 1) {
            result += ",";
        }
    }

    result += "] }";
    return result;
}

} // namespace openspace::properties
