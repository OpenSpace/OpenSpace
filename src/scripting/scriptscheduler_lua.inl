/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
    const std::string& fileName = ghoul::lua::value<std::string>(L);

    if (fileName.empty()) {
        return ghoul::lua::luaError(L, "filepath string is empty");
    }

    ghoul::Dictionary scriptsDict;
    scriptsDict.setValue("Scripts", ghoul::lua::loadDictionaryFromFile(fileName, L));
    documentation::testSpecificationAndThrow(
        scripting::ScriptScheduler::Documentation(),
        scriptsDict,
        "ScriptScheduler"
    );

    std::vector<scripting::ScriptScheduler::ScheduledScript> scripts;
    for (size_t i = 1; i <= scriptsDict.size(); ++i) {
        ghoul::Dictionary d = scriptsDict.value<ghoul::Dictionary>(std::to_string(i));

        scripting::ScriptScheduler::ScheduledScript script =
            scripting::ScriptScheduler::ScheduledScript(d);
        scripts.push_back(script);
    }

    global::scriptScheduler->loadScripts(scripts);
    return 0;
}

int loadScheduledScript(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 2, 5 }, "lua::loadScheduledScript");
    auto [time, forwardScript, backwardScript, universalScript, group] =
        ghoul::lua::values<
            std::string,
            std::string,
            std::optional<std::string>,
            std::optional<std::string>,
            std::optional<int>
        >(L);

    scripting::ScriptScheduler::ScheduledScript script;
    script.time = Time::convertTime(time);
    script.forwardScript = std::move(forwardScript);
    script.backwardScript = backwardScript.value_or(script.backwardScript);
    script.universalScript = universalScript.value_or(script.universalScript);
    script.group = group.value_or(script.group);

    std::vector<scripting::ScriptScheduler::ScheduledScript> scripts;
    scripts.push_back(std::move(script));
    global::scriptScheduler->loadScripts(scripts);
    return 0;
}

int setModeApplicationTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::setModeApplicationTime");
    global::scriptScheduler->setModeApplicationTime();
    return 0;
}

int setModeRecordedTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::setModeRecordedTime");
    global::scriptScheduler->setModeRecordedTime();
    return 0;
}

int setModeSimulationTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::setModeSimulationTime");
    global::scriptScheduler->setModeSimulationTime();
    return 0;
}

int clear(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 0, 1 }, "lua::clear");
    std::optional<int> group = ghoul::lua::value<std::optional<int>>(L);

    global::scriptScheduler->clearSchedule(group);
    return 0;
}

} // namespace openspace::luascriptfunction
