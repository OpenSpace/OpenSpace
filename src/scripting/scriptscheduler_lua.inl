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

#include <openspace/engine/globals.h>

namespace openspace::luascriptfunctions {

int loadFile(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadFile");

    const std::string& missionFileName = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );
    if (missionFileName.empty()) {
        return ghoul::lua::luaError(L, "filepath string is empty");
    }

    global::scriptScheduler.loadScripts(
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

    std::string time = ghoul::lua::value<std::string>(L, 1);
    std::string forwardScript = ghoul::lua::value<std::string>(L, 2);

    if (nArguments == 2) {
        global::scriptScheduler.loadScripts({
            {
                "1",
                ghoul::Dictionary {
                    { KeyTime, std::move(time) },
                    { KeyForwardScript, std::move(forwardScript) }
                }
            }
        });
    }
    else if (nArguments == 3) {
        std::string backwardScript = ghoul::lua::value<std::string>(L, 3);
        global::scriptScheduler.loadScripts({
            {
                "1",
                ghoul::Dictionary {
                    { KeyTime, std::move(time) },
                    { KeyForwardScript, std::move(forwardScript) },
                    { KeyBackwardScript, std::move(backwardScript) }
                }
            }
        });
    }
    else if (nArguments == 4) {
        std::string backwardScript = ghoul::lua::value<std::string>(L, 3);
        std::string universalScript = ghoul::lua::value<std::string>(L, 4);

        global::scriptScheduler.loadScripts({
            {
                "1",
                ghoul::Dictionary {
                    { KeyTime, std::move(time) },
                    { KeyForwardScript, std::move(forwardScript) },
                    { KeyBackwardScript, std::move(backwardScript) },
                    { KeyUniversalScript, std::move(universalScript) }
                }
            }
        });
    }

    lua_settop(L, 0);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int clear(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::clear");

    global::scriptScheduler.clearSchedule();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

} // namespace openspace::luascriptfunction
