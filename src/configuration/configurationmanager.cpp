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
#include <ghoul/lua/ghoul_lua.h>

#include <assert.h>
#include <fstream>

namespace {
    const std::string _loggerCat = "ConfigurationManager";

    lua_State* _state = nullptr;
}

using namespace ghoul::lua;

namespace openspace {



ConfigurationManager::ConfigurationManager() {}


ConfigurationManager::~ConfigurationManager() {

}

void ConfigurationManager::initialize() {
    // TODO custom assert (ticket #5)
    assert(_state == nullptr);

    _state = luaL_newstate();
    luaL_openlibs(_state);
}

void ConfigurationManager::deinitialize() {
    // TODO custom assert (ticket #5)
    assert(_state != nullptr);
    lua_close(_state);
}


void ConfigurationManager::loadConfiguration(const std::string& filename,
                                             const std::string& position)
{
    // TODO custom assert (ticket #5)
    assert(_state != nullptr);

    // load 'filename', prepent 'return', execute string, modify stack and execute
    // separate script to apply the table of changes
    std::ifstream file(filename);
    if (!file.good()) {
        LERROR("Error opening file '" << filename << "': Error code:" << 
            std::endl << file.rdstate());
        return;
    }

    // load script code from 'filename' and prepend "return"
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string scriptText = "return " + buffer.str();

    const int loadResult = luaL_loadstring(_state, scriptText.c_str());
    if (loadResult != 0) {
        LERROR("Error loading script in file '" << filename << "'. Error:" <<
            std::endl << lua_tostring(_state, -1));
        return;
    }

    LINFO("before");
    lua_logStack(_state);

    const int execResult = lua_pcall(_state, 0, LUA_MULTRET, 0);
    if (execResult != 0) {
        LERROR("Error executing script in file '" << filename << "'. Error:" << 
            std::endl << lua_tostring(_state, -1));
        return;
    }
    LINFO("after");
    lua_logStack(_state);
    lua_logTable(_state);

    //lua_p

    const int load2Result = luaL_loadfile(_state, p("${SCRIPTS}/script.lua").c_str());
    if (load2Result != 0) {
        LERROR("Error loading script in file '" << filename << "'. Error:" <<
            std::endl << lua_tostring(_state, -1));
        return;
    }
    LINFO("afterloading");
    lua_logStack(_state);


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


} // namespace openspace
