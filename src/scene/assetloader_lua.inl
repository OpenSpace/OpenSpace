/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

namespace openspace {
namespace luascriptfunctions {

int importAsset(lua_State* state) {
    AssetLoader *assetLoader =
        reinterpret_cast<AssetLoader*>(lua_touserdata(state, lua_upvalueindex(1)));

    int nArguments = lua_gettop(state);
    SCRIPT_CHECK_ARGUMENTS("importAsset", state, 1, nArguments);

    std::string assetName = luaL_checkstring(state, -1);

    assetLoader->importAsset(assetName);
    return 0;
}

int unimportAsset(lua_State* state) {
    AssetLoader *assetLoader =
        reinterpret_cast<AssetLoader*>(lua_touserdata(state, lua_upvalueindex(1)));

    int nArguments = lua_gettop(state);
    SCRIPT_CHECK_ARGUMENTS("unimportAsset", state, 1, nArguments);

    std::string assetName = luaL_checkstring(state, -1);

    assetLoader->unimportAsset(assetName);
    return 0;
}

int synchronizeResource(lua_State* state) {
    AssetLoader *assetLoader =
        reinterpret_cast<AssetLoader*>(lua_touserdata(state, lua_upvalueindex(1)));

    int nArguments = lua_gettop(state);
    SCRIPT_CHECK_ARGUMENTS("addSynchronization", state, 2, nArguments);
    
    int referenceIndex = luaL_ref(state, LUA_REGISTRYINDEX);

    ghoul::Dictionary d;
    try {
        ghoul::lua::luaDictionaryFromState(state, d);
    }
    catch (const ghoul::lua::LuaFormatException& e) {
        LERRORC("addSynchronization", e.what());
        return luaL_error(state, "Error loading dictionary from lua state");
    }

    assetLoader->synchronizeResource(d, [state, referenceIndex](bool success) {
        lua_rawgeti(state, LUA_REGISTRYINDEX, referenceIndex);
        lua_pushboolean(state, success ? 1 : 0);
        lua_pcall(state, 1, 0, 0);
        luaL_unref(state, LUA_REGISTRYINDEX, referenceIndex);
    });
}

}
}
