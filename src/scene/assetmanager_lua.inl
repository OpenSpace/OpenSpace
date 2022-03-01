/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
#include <openspace/engine/globals.h>
#include <ghoul/lua/lua_helper.h>

namespace openspace::luascriptfunctions::asset {

int add(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::add");
    const std::string assetName = ghoul::lua::value<std::string>(L);
    global::openSpaceEngine->assetManager().add(assetName);
    return 0;
}

int remove(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::remove");
    const std::string assetName = ghoul::lua::value<std::string>(L);
    global::openSpaceEngine->assetManager().remove(assetName);
    return 0;
}

int isLoaded(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::isLoaded");
    const std::string assetName = ghoul::lua::value<std::string>(L);

    std::vector<const Asset*> as = global::openSpaceEngine->assetManager().allAssets();
    for (const Asset* a : as) {
        if (a->path() == assetName) {
            ghoul::lua::push(L, true);
            return 1;
        }
    }

    ghoul::lua::push(L, false);
    return 1;
}

int allAssets(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::allAssets");

    std::vector<const Asset*> as = global::openSpaceEngine->assetManager().allAssets();
    std::vector<std::string> res;
    res.reserve(as.size());
    for (const Asset* a : as) {
        res.push_back(a->path().string());
    }
    ghoul::lua::push(L, res);
    return 1;
}

} // namespace openspace::luascriptfunctions
