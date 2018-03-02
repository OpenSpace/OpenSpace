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

int loadFile(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadFile");

    std::string missionFileName = ghoul::lua::checkStringAndPop(L);
    if (missionFileName.empty()) {
        return luaL_error(L, "filepath string is empty");
    }

    OsEng.scriptScheduler().loadScripts(
        ghoul::lua::loadDictionaryFromFile(missionFileName, L)
    );

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int loadScheduledScript(lua_State* L) {
    int nArguments = ghoul::lua::checkArgumentsAndThrow(
        L,
        { 2, 4 },
        "lua::loadScheduledScript"
    );

    if (nArguments == 2) {
        OsEng.scriptScheduler().loadScripts({
            {
                "1",
                ghoul::Dictionary {
                    { KeyTime, std::string(luaL_checkstring(L, -2)) },
                    { KeyForwardScript, std::string(luaL_checkstring(L, -1)) }
                }
            }
        });
    }
    else if (nArguments == 3) {
        OsEng.scriptScheduler().loadScripts({
            {
                "1",
                ghoul::Dictionary {
                    { KeyTime, std::string(luaL_checkstring(L, -3)) },
                    { KeyForwardScript, std::string(luaL_checkstring(L, -2)) },
                    { KeyBackwardScript, std::string(luaL_checkstring(L, -1)) }
                }
            }
        });
    }
    else if (nArguments == 4) {
        OsEng.scriptScheduler().loadScripts({
            {
                "1",
                ghoul::Dictionary {
                    { KeyTime, std::string(luaL_checkstring(L, -4)) },
                    { KeyForwardScript, std::string(luaL_checkstring(L, -3)) },
                    { KeyBackwardScript, std::string(luaL_checkstring(L, -2)) },
                    { KeyUniversalScript, std::string(luaL_checkstring(L, -1)) }
                }
            }
        });
    }

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int clear(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::clear");

    OsEng.scriptScheduler().clearSchedule();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

} // namespace openspace::luascriptfunction
