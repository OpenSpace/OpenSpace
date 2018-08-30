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

namespace openspace::luascriptfunctions {

int connect(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::connect");

    if (global::windowDelegate.isMaster()) {
        global::parallelPeer.connect();
    }

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int disconnect(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::disconnect");

    if (global::windowDelegate.isMaster()) {
        global::parallelPeer.connect();
    }

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int requestHostship(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::requestHostship");

    if (global::windowDelegate.isMaster()) {
        global::parallelPeer.requestHostship();
    }

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int resignHostship(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::resignHostship");

    if (global::windowDelegate.isMaster()) {
        global::parallelPeer.resignHostship();
    }

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

} // namespace openspace::luascriptfunctions
