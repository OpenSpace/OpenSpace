/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <ghoul/lua/lua_helper.h>

namespace {

// Load timed scripts from a Lua script file that returns a list of scheduled scripts.
[[codegen::luawrap]] void loadFile(std::string fileName) {
    using namespace openspace;

    if (fileName.empty()) {
        throw ghoul::lua::LuaError("Filepath string is empty");
    }

    ghoul::Dictionary scriptsDict;
    scriptsDict.setValue("Scripts", ghoul::lua::loadDictionaryFromFile(fileName));
    documentation::testSpecificationAndThrow(
        scripting::ScriptScheduler::Documentation(),
        scriptsDict,
        "ScriptScheduler"
    );

    std::vector<scripting::ScriptScheduler::ScheduledScript> scripts;
    for (size_t i = 1; i <= scriptsDict.size(); i++) {
        ghoul::Dictionary d = scriptsDict.value<ghoul::Dictionary>(std::to_string(i));

        scripting::ScriptScheduler::ScheduledScript script =
            scripting::ScriptScheduler::ScheduledScript(d);
        scripts.push_back(script);
    }

    global::scriptScheduler->loadScripts(scripts);
}

/**
 * Load a single scheduled script. The first argument is the time at which the scheduled
 * script is triggered, the second argument is the script that is executed in the forward
 * direction, the optional third argument is the script executed in the backwards
 * direction, and the optional last argument is the universal script, executed in either
 * direction.
 */
[[codegen::luawrap]] void loadScheduledScript(std::string time, std::string forwardScript,
                                              std::optional<std::string> backwardScript,
                                              std::optional<std::string> universalScript,
                                              std::optional<int> group)
{
    using namespace openspace;

    scripting::ScriptScheduler::ScheduledScript script;
    script.time = Time::convertTime(time);
    script.forwardScript = std::move(forwardScript);
    script.backwardScript = backwardScript.value_or(script.backwardScript);
    script.universalScript = universalScript.value_or(script.universalScript);
    script.group = group.value_or(script.group);

    std::vector<scripting::ScriptScheduler::ScheduledScript> scripts;
    scripts.push_back(std::move(script));
    global::scriptScheduler->loadScripts(scripts);
}

// Clears all scheduled scripts.
[[codegen::luawrap]] void clear(std::optional<int> group) {
    openspace::global::scriptScheduler->clearSchedule(group);
}

// Returns the list of all scheduled scripts
[[codegen::luawrap]] std::vector<ghoul::Dictionary> scheduledScripts() {
    using namespace openspace;

    std::vector<scripting::ScriptScheduler::ScheduledScript> scripts =
        global::scriptScheduler->allScripts();

    std::vector<ghoul::Dictionary> result;
    result.reserve(scripts.size());

    for (const scripting::ScriptScheduler::ScheduledScript& script : scripts) {
        ghoul::Dictionary d;
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

#include "scriptscheduler_lua_codegen.cpp"

} // namespace
