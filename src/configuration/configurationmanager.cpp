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

#include <ghoul/filesystem/filesystem>
#include <ghoul/logging/logging>

#include <assert.h>
#include <fstream>

namespace {
    const std::string _loggerCat = "ConfigurationManager";
    const char* _configurationScript = "${SCRIPTS}/configurationmanager.lua";
    const char* _configurationTable = "config";
}

using namespace ghoul::lua;

namespace openspace {


ConfigurationManager::ConfigurationManager()
    : _state(nullptr)
{}

ConfigurationManager::~ConfigurationManager() {
    if (_state != nullptr) {
        LWARNING("ConfigurationManager was not deinitialized");
        deinitialize();
    }
}

bool ConfigurationManager::initialize() {
    // TODO custom assert (ticket #5)
    assert(_state == nullptr);

    LDEBUG("Create Lua state");
    _state = luaL_newstate();
    if (_state == nullptr) {
        LFATAL("Error creating new Lua state: Memory allocation error");
        return false;
    }
    LDEBUG("Open libraries");
    luaL_openlibs(_state);

    LDEBUG("Loading configuration script '" << _configurationScript << "'");
    const int status = luaL_loadfile(_state, absPath(_configurationScript).c_str());
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
    lua_getglobal(_state, "loadConfiguration");
    if (!lua_isfunction(_state, -1)) {
        LFATAL("Could not find function 'loadConfiguration' in script");
        deinitialize();
        return false;
    }
    lua_pop(_state, 1);
    lua_getglobal(_state, _configurationTable);
    if (!lua_istable(_state, -1)) {
        LERROR("Table '" << _configurationTable << "' not found in script");
        deinitialize();
        return false;
    }
    lua_pop(_state, 1);
    return true;
}

void ConfigurationManager::deinitialize() {
    // TODO custom assert (ticket #5)
    assert(_state != nullptr);
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

template <class T>
bool ConfigurationManager::getValue(const std::string& key, T& value) {
    // If none of the specializations fit, we don't know what to do
    LERROR("Unsupported type for key '" << key << "'");
    return false;
}

template <class T>
bool ConfigurationManager::setValue(const std::string& key, const T& value) {
    // If none of the specializations fit, we don't know what to do
    LERROR("Unsupported type for key '" << key << "'");
    return false;
}

bool getFullValue(lua_State* state, const std::string& key, LUA_INTEGER& value) {
    assert(state != nullptr);

    lua_getglobal(state, "getValue");
    lua_pushstring(state, key.c_str());
    lua_pcall(state, 1, 1, NULL);

    //lua_getglobal(state, _configurationTable);
    //lua_getfield(state, -1, key.c_str());
    //if (lua_isnil(state, -1)) {
    //    LWARNING("Key '" << key << "' not found");
    //    return false;
    //}
    //if (!lua_isnumber(state, -1)) {
    //    LWARNING("Value of key '" << key <<"' is not a number");
    //    return false;
    //}
    //value = lua_tointeger(state, -1);
    //lua_pop(state, lua_gettop(state));
    return true;
}

bool setFullValue(lua_State* state, const std::string& key, const LUA_INTEGER& value) {
    assert(state != nullptr);
    lua_getglobal(state, _configurationTable);
    lua_pushinteger(state, value);
    lua_setfield(state, -2, key.c_str());
    lua_pop(state, 1);
    return true;
}

// int
template <>
bool ConfigurationManager::setValue(const std::string& key, const int& value) {
    return setFullValue(_state, key, value);
}

template <>
bool ConfigurationManager::getValue(const std::string& key, int& value) {
    lua_getglobal(_state, "getValue");
    lua_pushstring(_state, key.c_str());
    lua_pcall(_state, 1, 1, NULL);
    if (lua_isnil(_state, -1)) {
        lua_pop(_state, 1);
        return false;
    } else {
        value = lua_tointeger(_state, -1);
        lua_pop(_state, 1);
        return true;
    }


    //lua_getglobal(_state, _configurationTable);
    //LUA_INTEGER val;
    //const bool result = getFullValue(_state, key, val);
    //if (result)
    //    value = static_cast<int>(val);
    //return result;
}

} // namespace openspace
