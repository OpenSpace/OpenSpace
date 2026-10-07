/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/lua/lua_helper.h>

using namespace openspace;

namespace {

/**
 * Load timed scripts from a Lua script file that returns a list of scheduled scripts.
 */
[[codegen::luawrap]] void loadFile(std::string fileName) {
    if (fileName.empty()) {
        throw lua::LuaError("Filepath string is empty");
    }

    Dictionary scriptsDict;
    scriptsDict.setValue("Scripts", lua::loadDictionaryFromFile(fileName));
    testSpecificationAndThrow(
        ScriptScheduler::Documentation(),
        scriptsDict,
        "ScriptScheduler"
    );

    std::vector<ScriptScheduler::ScheduledScript> scripts;
    for (size_t i = 1; i <= scriptsDict.size(); i++) {
        Dictionary d = scriptsDict.value<Dictionary>(std::to_string(i));

        ScriptScheduler::ScheduledScript script = ScriptScheduler::ScheduledScript(d);
        scripts.push_back(script);
    }

    global::scriptScheduler->loadScripts(scripts);
}

/**
 * Load a single scheduled script. The first argument is the time at which the scheduled
 * script is triggered, the second argument is the script that is executed in the forward
 * direction, the optional third argument is the script executed in the backwards
 * direction, and the optional last argument is the universal script, executed in either
 * direction. If a group is specified, it must be larger than 0.
 */
[[codegen::luawrap]] void loadScheduledScript(std::string time, std::string forwardScript,
                                              std::optional<std::string> backwardScript,
                                              std::optional<std::string> universalScript,
                                              std::optional<int> group)
{
    if (group.has_value() && *group < 0) {
        throw lua::LuaError("Only groups larger than 0 are allowed");
    }

    ScriptScheduler::ScheduledScript script;
    script.time = Time::convertTime(time);
    script.forwardScript = std::move(forwardScript);
    script.backwardScript = backwardScript.value_or(script.backwardScript);
    script.universalScript = universalScript.value_or(script.universalScript);
    script.group = group.value_or(script.group);

    std::vector<ScriptScheduler::ScheduledScript> scripts;
    scripts.push_back(std::move(script));
    global::scriptScheduler->loadScripts(scripts);
}

/**
 * Schedules a single execution of a script. If the specified `time` is passed, the
 * provided `script` is executed exactly once.
 */
[[codegen::luawrap]] void scheduleSingleShotScript(std::string time, std::string script) {
    // The main function is restricted to positive group ids, so we can use negative ones
    // for ourself. We start arbitrarily at -1073741824 (2**-30) counting away from 0.
    static int Counter = -1073741824;

    ScriptScheduler::ScheduledScript s;
    s.time = Time::convertTime(time);
    s.universalScript = std::format(
        "{};openspace.scriptScheduler.clear({})", script, Counter
    );
    s.group = Counter;
    Counter--;

    std::vector<ScriptScheduler::ScheduledScript> scripts;
    scripts.push_back(std::move(s));
    global::scriptScheduler->loadScripts(scripts);
}

/**
 * Clears all scheduled scripts.
 */
[[codegen::luawrap]] void clear(std::optional<int> group) {
    global::scriptScheduler->clearSchedule(group);
}

/**
 * Returns the list of all scheduled scripts.
 */
[[codegen::luawrap]] std::vector<Dictionary> scheduledScripts() {
    std::vector<ScriptScheduler::ScheduledScript> scripts =
        global::scriptScheduler->allScripts();

    std::vector<Dictionary> result;
    result.reserve(scripts.size());

    for (const ScriptScheduler::ScheduledScript& script : scripts) {
        Dictionary d;
        d.setValue("Time", script.time);
        if (!script.forwardScript.empty()) {
            d.setValue("ForwardScript", script.forwardScript);
        }
        if (!script.backwardScript.empty()) {
            d.setValue("BackwardScript", script.backwardScript);
        }
        if (!script.universalScript.empty()) {
            d.setValue("UniversalScript", script.universalScript);
        }
        d.setValue("Group", script.group);

        result.push_back(d);
    }

    return result;
}

} // namespace

#include "scriptscheduler_lua_codegen.cpp"
