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

#include "configuration/configurationmanager.h"

#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/filesystem/filesystem>
#include <ghoul/logging/logging>

#include <assert.h>
#include <fstream>

namespace {
    const std::string _loggerCat = "ConfigurationManager";
    const char* _configurationScript = "${SCRIPTS}/configurationmanager.lua";
    const char* _configurationTable = "config";
}

namespace openspace {

namespace helper {
    bool getValue(lua_State* state, const std::string& key, lua_Integer& value);
    bool getValue(lua_State* state, const std::string& key, lua_Unsigned& value);
    bool getValue(lua_State* state, const std::string& key, lua_Number& value);
    void setValue(lua_State* state, const std::string& key, const lua_Integer& value);
    void setValue(lua_State* state, const std::string& key, const lua_Unsigned& value);
    void setValue(lua_State* state, const std::string& key, const lua_Number& value);
}

ConfigurationManager::ConfigurationManager()
    : _state(nullptr)
{}

ConfigurationManager::~ConfigurationManager() {
    if (_state != nullptr) {
        LWARNING("ConfigurationManager was not deinitialized");
        deinitialize();
    }
}

bool ConfigurationManager::initialize(const std::string& configurationScript) {
    // TODO custom assert (ticket #5)
    assert(_state == nullptr);

    std::string script;
    if (configurationScript == "")
        script = _configurationScript;
    else
        script = configurationScript;

    LDEBUG("Create Lua state");
    _state = luaL_newstate();
    if (_state == nullptr) {
        LFATAL("Error creating new Lua state: Memory allocation error");
        return false;
    }
    LDEBUG("Open libraries");
    luaL_openlibs(_state);

    LDEBUG("Loading configuration script '" << script << "'");
    const int status = luaL_loadfile(_state, absPath(script).c_str());
    if (status != LUA_OK) {
        LFATAL("Error loading configuration script: " << lua_tostring(_state, -1));
        deinitialize();
        return false;
    }

    LDEBUG("Executing configuration script");
    if (lua_pcall(_state, 0, LUA_MULTRET, 0)) {
        LFATAL("Error executing configuration script: " << lua_tostring(_state, -1));
        deinitialize();
        return false;
    }

    // Sanity checks
    LDEBUG("Sanity check for 'loadConfiguration'");
    lua_getglobal(_state, "loadConfiguration");
    if (!lua_isfunction(_state, -1)) {
        LFATAL("Could not find function 'loadConfiguration' in configuration script");
        deinitialize();
        return false;
    }
    lua_pop(_state, 1);
    
    LDEBUG("Sanity check for the configuration table");
    lua_getglobal(_state, _configurationTable);
    if (!lua_istable(_state, -1)) {
        LERROR("Table '" << _configurationTable << "' not found in configuration script");
        deinitialize();
        return false;
    }
    lua_pop(_state, 1);

    LDEBUG("Sanity check for 'getValue'");
    lua_getglobal(_state, "getValue");
    if (!lua_isfunction(_state, -1)) {
        LFATAL("Could not find function 'getValue' in configuration script");
        deinitialize();
        return false;
    }
    lua_pop(_state, 1);

    LDEBUG("Sanity check for 'setValue'");
    lua_getglobal(_state, "setValue");
    if (!lua_isfunction(_state, -1)) {
        LFATAL("Could not find function 'setValue' in configuration script");
        deinitialize();
        return false;
    }
    lua_pop(_state, 1);
    return true;
}

void ConfigurationManager::deinitialize() {
    // TODO custom assert (ticket #5)
    assert(_state != nullptr);
    LDEBUG("Close Lua state");
    lua_close(_state);
    _state = nullptr;
}

void ConfigurationManager::loadConfiguration(const std::string& filename,
                                             const std::string& position)
{
    // TODO custom assert (ticket #5)
    assert(_state != nullptr);

    const std::string& absFilename = absPath(filename);

    lua_getglobal(_state, "loadConfiguration");
    lua_pushstring(_state, absFilename.c_str());

    const int status = lua_pcall(_state, 1, 0, 0);
    if (status != LUA_OK) {
        LERROR("Error loading configuration: " << lua_tostring(_state, -1));
        return;
    }
}

// TODO: We can replace this by using type_traits in a smart way

template<>
void ConfigurationManager::setValue(const std::string& key, const bool& value) {
    assert(_state != nullptr);

    lua_getglobal(_state, "setValue");
    lua_pushstring(_state, key.c_str());
    lua_pushboolean(_state, static_cast<int>(value));
    const int status = lua_pcall(_state, 2, 0, NULL);
    if (status != LUA_OK)
        LERROR("Error setting value '" << key << "'. Error: " << lua_tostring(_state, -1));
}

// character types
template <>
void ConfigurationManager::setValue(const std::string& key, const char& value) {
    lua_Integer val = static_cast<lua_Integer>(value);
    helper::setValue(_state, key, val);
}

template <>
void ConfigurationManager::setValue(const std::string& key, const signed char& value) {
    lua_Integer val = static_cast<lua_Integer>(value);
    helper::setValue(_state, key, val);
}

template <>
void ConfigurationManager::setValue(const std::string& key, const unsigned char& value) {
    lua_Unsigned val = static_cast<lua_Unsigned>(value);
    helper::setValue(_state, key, val);
}

template <>
void ConfigurationManager::setValue(const std::string& key, const wchar_t& value) {
    lua_Integer val = static_cast<lua_Integer>(value);
    helper::setValue(_state, key, val);
}

// integer types

template <>
void ConfigurationManager::setValue(const std::string& key, const short& value) {
    lua_Integer val = static_cast<lua_Integer>(value);
    helper::setValue(_state, key, val);
}

template <>
void ConfigurationManager::setValue(const std::string& key, const unsigned short& value) {
    lua_Unsigned val = static_cast<lua_Unsigned>(value);
    helper::setValue(_state, key, val);
}

template <>
void ConfigurationManager::setValue(const std::string& key, const int& value) {
    lua_Integer val = static_cast<lua_Integer>(value);
    helper::setValue(_state, key, val);
}

template <>
void ConfigurationManager::setValue(const std::string& key, const unsigned int& value) {
    lua_Unsigned val = static_cast<lua_Unsigned>(value);
    helper::setValue(_state, key, val);
}

template <>
void ConfigurationManager::setValue(const std::string& key, const long& value) {
    lua_Integer val = static_cast<lua_Integer>(value);
    helper::setValue(_state, key, val);
}

template <>
void ConfigurationManager::setValue(const std::string& key, const unsigned long& value) {
    lua_Unsigned val = static_cast<lua_Unsigned>(value);
    helper::setValue(_state, key, val);
}

template <>
void ConfigurationManager::setValue(const std::string& key, const long long& value) {
    lua_Integer val = static_cast<lua_Integer>(value);
    helper::setValue(_state, key, val);
}

template <>
void ConfigurationManager::setValue(const std::string& key, const unsigned long long& value) {
    lua_Unsigned val = static_cast<lua_Unsigned>(value);
    helper::setValue(_state, key, val);
}

template <>
void ConfigurationManager::setValue(const std::string& key, const float& value) {
    lua_Number val = static_cast<lua_Number>(value);
    helper::setValue(_state, key, val);
}

template <>
void ConfigurationManager::setValue(const std::string& key, const double& value) {
    lua_Number val = static_cast<lua_Number>(value);
    helper::setValue(_state, key, val);
}

template <>
void ConfigurationManager::setValue(const std::string& key, const long double& value) {
    lua_Number val = static_cast<lua_Number>(value);
    helper::setValue(_state, key, val);
}

template <>
void ConfigurationManager::setValue(const std::string& key, const std::string& value) {
    assert(_state != nullptr);

    lua_getglobal(_state, "setValue");
    lua_pushstring(_state, key.c_str());
    lua_pushstring(_state, value.c_str());
    const int status = lua_pcall(_state, 2, 0, NULL);
    if (status != LUA_OK)
        LERROR("Error setting value '" << key << "'. Error: " << lua_tostring(_state, -1));
}

//
// Get
//
template <>
bool ConfigurationManager::getValue(const std::string& key, bool& value) {
    assert(_state != nullptr);

    lua_getglobal(_state, "getValue");
    lua_pushstring(_state, key.c_str());
    const int status = lua_pcall(_state, 1, 1, NULL);
    if (status != LUA_OK) {
        LERROR("Error getting value '" << key << "'. Error: " << lua_tostring(_state, -1));
        return false;
    }
    if (lua_isnil(_state, -1)) {
        lua_pop(_state, 1);
        return false;
    } else {
        const int v = lua_toboolean(_state, -1);
        value = (v != 0);
        lua_pop(_state, 1);
        return true;
    }
}

template <>
bool ConfigurationManager::getValue(const std::string& key, char& value) {
    lua_Integer val;
    const bool result = helper::getValue(_state, key, val);
    if (result)
        value = static_cast<char>(val);
    return result;
}

template <>
bool ConfigurationManager::getValue(const std::string& key, signed char& value) {
    lua_Integer val;
    const bool result = helper::getValue(_state, key, val);
    if (result)
        value = static_cast<signed char>(val);
    return result;
}

template <>
bool ConfigurationManager::getValue(const std::string& key, unsigned char& value) {
    lua_Unsigned val;
    const bool result = helper::getValue(_state, key, val);
    if (result)
        value = static_cast<unsigned char>(val);
    return result;
}

template <>
bool ConfigurationManager::getValue(const std::string& key, wchar_t& value) {
    lua_Integer val;
    const bool result = helper::getValue(_state, key, val);
    if (result)
        value = static_cast<wchar_t>(val);
    return result;
}

template <>
bool ConfigurationManager::getValue(const std::string& key, short& value) {
    lua_Integer val;
    const bool result = helper::getValue(_state, key, val);
    if (result)
        value = static_cast<short>(val);
    return result;
}

template <>
bool ConfigurationManager::getValue(const std::string& key, unsigned short& value) {
    lua_Unsigned val;
    const bool result = helper::getValue(_state, key, val);
    if (result)
        value = static_cast<unsigned short>(val);
    return result;
}

template <>
bool ConfigurationManager::getValue(const std::string& key, int& value) {
    lua_Integer val;
    const bool result = helper::getValue(_state, key, val);
    if (result)
        value = static_cast<int>(val);
    return result;
}

template <>
bool ConfigurationManager::getValue(const std::string& key, unsigned int& value) {
    lua_Unsigned val;
    const bool result = helper::getValue(_state, key, val);
    if (result)
        value = static_cast<unsigned int>(val);
    return result;
}

template <>
bool ConfigurationManager::getValue(const std::string& key, long& value) {
    lua_Integer val;
    const bool result = helper::getValue(_state, key, val);
    if (result)
        value = static_cast<long>(val);
    return result;
}

template <>
bool ConfigurationManager::getValue(const std::string& key, unsigned long& value) {
    lua_Unsigned val;
    const bool result = helper::getValue(_state, key, val);
    if (result)
        value = static_cast<unsigned long>(val);
    return result;
}

template <>
bool ConfigurationManager::getValue(const std::string& key, long long& value) {
    lua_Integer val;
    const bool result = helper::getValue(_state, key, val);
    if (result)
        value = static_cast<long long>(val);
    return result;
}

template <>
bool ConfigurationManager::getValue(const std::string& key, unsigned long long& value) {
    lua_Unsigned val;
    const bool result = helper::getValue(_state, key, val);
    if (result)
        value = static_cast<unsigned long>(val);
    return result;
}

template <>
bool ConfigurationManager::getValue(const std::string& key, float& value) {
    lua_Number val;
    const bool result = helper::getValue(_state, key, val);
    if (result)
        value = static_cast<float>(val);
    return result;
}

template <>
bool ConfigurationManager::getValue(const std::string& key, double& value) {
    lua_Number val;
    const bool result = helper::getValue(_state, key, val);
    if (result)
        value = static_cast<double>(val);
    return result;
}

template <>
bool ConfigurationManager::getValue(const std::string& key, long double& value) {
    lua_Number val;
    const bool result = helper::getValue(_state, key, val);
    if (result)
        value = static_cast<long double>(val);
    return result;
}

template <>
bool ConfigurationManager::getValue(const std::string& key, std::string& value) {
    assert(_state != nullptr);

    lua_getglobal(_state, "getValue");
    lua_pushstring(_state, key.c_str());
    const int status = lua_pcall(_state, 1, 1, NULL);
    if (status != LUA_OK) {
        LERROR("Error getting value '" << key << "'. Error: " << lua_tostring(_state, -1));
        return false;
    }
    if (lua_isnil(_state, -1)) {
        lua_pop(_state, 1);
        return false;
    } else {
        const char* v = lua_tostring(_state, -1);
        value = std::string(v);
        lua_pop(_state, 1);
        return true;
    }
}

//
// Helper
//

namespace helper {

bool getValue(lua_State* state, const std::string& key, lua_Integer& value) {
    assert(state != nullptr);

    lua_getglobal(state, "getValue");
    lua_pushstring(state, key.c_str());
    const int status = lua_pcall(state, 1, 1, NULL);
    if (status != LUA_OK) {
        LERROR("Error getting value '" << key << "'. Error: " << lua_tostring(state, -1));
        return false;
    }
    if (lua_isnil(state, -1)) {
        lua_pop(state, 1);
        return false;
    } else {
        value = lua_tointeger(state, -1);
        lua_pop(state, 1);
        return true;
    }
}

bool getValue(lua_State* state, const std::string& key, lua_Unsigned& value) {
    assert(state != nullptr);

    lua_getglobal(state, "getValue");
    lua_pushstring(state, key.c_str());
    const int status = lua_pcall(state, 1, 1, NULL);
    if (status != LUA_OK) {
        LERROR("Error getting value '" << key << "'. Error: " << lua_tostring(state, -1));
        return false;
    }
    if (lua_isnil(state, -1)) {
        lua_pop(state, 1);
        return false;
    } else {
        value = lua_tounsigned(state, -1);
        lua_pop(state, 1);
        return true;
    }
}

bool getValue(lua_State* state, const std::string& key, lua_Number& value) {
    assert(state != nullptr);

    lua_getglobal(state, "getValue");
    lua_pushstring(state, key.c_str());
    const int status = lua_pcall(state, 1, 1, NULL);
    if (status != LUA_OK) {
        LERROR("Error getting value '" << key << "'. Error: " << lua_tostring(state, -1));
        return false;
    }
    if (lua_isnil(state, -1)) {
        lua_pop(state, 1);
        return false;
    } else {
        value = lua_tonumber(state, -1);
        lua_pop(state, 1);
        return true;
    }
}

void setValue(lua_State* state, const std::string& key, const lua_Integer& value) {
    assert(state != nullptr);

    lua_getglobal(state, "setValue");
    lua_pushstring(state, key.c_str());
    lua_pushinteger(state, value);
    const int status = lua_pcall(state, 2, 0, NULL);
    if (status != LUA_OK)
        LERROR("Error setting value '" << key << "'. Error: " << lua_tostring(state, -1));
}

void setValue(lua_State* state, const std::string& key, const lua_Unsigned& value) {
    assert(state != nullptr);

    lua_getglobal(state, "setValue");
    lua_pushstring(state, key.c_str());
    lua_pushunsigned(state, value);
    const int status = lua_pcall(state, 2, 0, NULL);
    if (status != LUA_OK)
        LERROR("Error setting value '" << key << "'. Error: " << lua_tostring(state, -1));
}

void setValue(lua_State* state, const std::string& key, const lua_Number& value) {
    assert(state != nullptr);

    lua_getglobal(state, "setValue");
    lua_pushstring(state, key.c_str());
    lua_pushnumber(state, value);
    const int status = lua_pcall(state, 2, 0, NULL);
    if (status != LUA_OK)
        LERROR("Error setting value '" << key << "'. Error: " << lua_tostring(state, -1));
}

};

} // namespace openspace

