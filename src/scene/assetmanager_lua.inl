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

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/globals.h>

namespace openspace::luascriptfunctions::asset {

int add(lua_State* state) {
    ghoul::lua::checkArgumentsAndThrow(state, 1, "lua::add");

    AssetManager* assetManager =
        reinterpret_cast<AssetManager*>(lua_touserdata(state, lua_upvalueindex(1)));

    const std::string& assetName = ghoul::lua::value<std::string>(
        state,
        1,
        ghoul::lua::PopValue::Yes
    );

    if (global::renderEngine.scene()) {
        assetManager->add(assetName);
    }
    else {
        // The scene might not exist yet if OpenSpace was started without specifying an
        // initial asset
        global::openSpaceEngine.scheduleLoadSingleAsset(assetName);
    }


    ghoul_assert(lua_gettop(state) == 0, "Incorrect number of items left on stack");
    return 0;
}

int remove(lua_State* state) {
    ghoul::lua::checkArgumentsAndThrow(state, 1, "lua::remove");

    AssetManager* assetManager =
        reinterpret_cast<AssetManager*>(lua_touserdata(state, lua_upvalueindex(1)));

    const std::string& assetName = ghoul::lua::value<std::string>(
        state,
        1,
        ghoul::lua::PopValue::Yes
    );
    assetManager->remove(assetName);

    ghoul_assert(lua_gettop(state) == 0, "Incorrect number of items left on stack");
    return 0;
}

int removeAll(lua_State* state) {
    ghoul::lua::checkArgumentsAndThrow(state, 0, "lua::removeAll");

    AssetManager* assetManager =
        reinterpret_cast<AssetManager*>(lua_touserdata(state, lua_upvalueindex(1)));

    assetManager->removeAll();

    return 0;
}

} // namespace openspace::luascriptfunctions
