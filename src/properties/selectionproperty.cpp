/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <openspace/json.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/misc.h>
#include <string>

namespace {
    constexpr std::string_view _loggerCat = "SelectionProperty";

    constexpr std::string_view OptionsKey = "Options";
} // namespace

namespace openspace::properties {

SelectionProperty::SelectionProperty(Property::PropertyInfo info)
    : TemplateProperty(std::move(info), std::set<std::string>())
{}

std::string_view SelectionProperty::className() const {
    return "SelectionProperty";
}

int SelectionProperty::typeLua() const {
    return LUA_TTABLE;
}

void SelectionProperty::setValue(std::set<std::string> val) {
    ghoul_assert(!_options.empty(), "Cannot set selection before options have been set");

    if (val == _value) {
        return;
    }

    removeInvalidKeys(val);

    _value = std::move(val);
    notifyChangeListeners();
    _isValueDirty = true;
}

bool SelectionProperty::hasOption(const std::string& key) const {
    auto it = std::find(_options.begin(), _options.end(), key);
    return it != _options.end();
}

bool SelectionProperty::isSelected(const std::string& key) const {
    auto it = std::find(_value.begin(), _value.end(), key);
    return it != _value.end();
}

bool SelectionProperty::hasSelected() const {
    return !_value.empty();
}

const std::vector<std::string>& SelectionProperty::options() const {
    return _options;
}

void SelectionProperty::setOptions(const std::vector<std::string>& keys) {
    _options.clear();
    _options.reserve(keys.size());

    for (const std::string& key : keys) {
        if (!hasOption(key)) {
            _options.push_back(key);
        }
        else {
            LWARNING(fmt::format("Ignoring duplicated key '{}'", key));
        }
    }
    _options.shrink_to_fit();
    sortOptions();

    // In case we have a selection, remove non-existing options
    bool changed = removeInvalidKeys(_value);
    if (changed) {
        _isValueDirty = true;
    }

    notifyChangeListeners();
}

void SelectionProperty::addOption(const std::string& key) {
    if (hasOption(key)) {
        LWARNING(fmt::format("Cannot add option. Key '{}' already exists", key));
        return;
    }

    _options.push_back(key);
    sortOptions();
    notifyChangeListeners();
}

void SelectionProperty::clearSelection() {
    _value.clear();
    notifyChangeListeners();
    _isValueDirty = true;
}

void SelectionProperty::clearOptions() {
    _options.clear();
    clearSelection();
}

std::set<std::string> SelectionProperty::fromLuaConversion(lua_State* state,
                                                           bool& success) const
{
    constexpr int KEY = -2;
    constexpr int VAL = -1;

    if (!lua_istable(state, VAL)) {
        LERROR("Parameter passed to the property is not a table");
        success = false;
        return {};
    }

    std::set<std::string> result;
    lua_pushnil(state);
    while (lua_next(state, KEY) != 0) {
        if (lua_isstring(state, VAL)) {
            result.insert(lua_tostring(state, -1));
        }
        else {
            success = false;
            return {};
        }
        lua_pop(state, 1);
    }

    success = true;
    return result;
}

void SelectionProperty::toLuaConversion(lua_State* state) const {
    lua_newtable(state);
    int i = 1;
    for (const std::string& v : _value) {
        lua_pushinteger(state, i);
        lua_pushstring(state, v.c_str());
        lua_settable(state, -3);
        ++i;
    }
}

std::string SelectionProperty::toStringConversion() const {
    nlohmann::json json(_value);
    return json.dump();
}

void SelectionProperty::sortOptions() {
    std::sort(_options.begin(), _options.end());
}

bool SelectionProperty::removeInvalidKeys(std::set<std::string>& keys) {
    bool changed = false;
    std::set<std::string>::iterator it = keys.begin();
    while (it != keys.end()) {
        if (!hasOption(*it)) {
            LWARNING(fmt::format(
                "Key '{}' is not a valid option and is removed from selection", *it
            ));
            it = keys.erase(it);
            changed = true;
        }
        else {
            ++it;
        }
    }
    return changed;
}

std::string SelectionProperty::generateAdditionalJsonDescription() const {
    nlohmann::json optionsJson(_options);
    std::string result = "{ ";
    result += fmt::format("\"{}\": {}", OptionsKey, optionsJson.dump());
    result += " }";
    return result;
}

} // namespace openspace::properties
