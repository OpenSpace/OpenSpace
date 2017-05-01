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

namespace luascriptfunctions {

int loadFile(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);
    }

    std::string missionFileName = luaL_checkstring(L, -1);
    if (missionFileName.empty()) {
        return luaL_error(L, "filepath string is empty");
    }

    OsEng.scriptScheduler().loadScripts(
        ghoul::lua::loadDictionaryFromFile(missionFileName, L)
    );
    
    return 0;
}

int loadScheduledScript(lua_State* L) {
    int nArguments = lua_gettop(L);
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
    else {
        return luaL_error(L, "Expected %i-%i arguments, got %i", 2, 4, nArguments);
    }

    return 0;
}

int clear(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 0) {
        return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);
    }

    OsEng.scriptScheduler().clearSchedule();

    return 0;
}

} // namespace luascriptfunction
