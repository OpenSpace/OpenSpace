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

#include <openspace/scripting/scriptscheduler.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/time.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* KeyTime = "Time";
    constexpr const char* KeyForwardScript = "ForwardScript";
    constexpr const char* KeyBackwardScript = "BackwardScript";
    constexpr const char* KeyUniversalScript = "Script";
} // namespace

#include "scriptscheduler_lua.inl"

namespace openspace::scripting {

documentation::Documentation ScriptScheduler::Documentation() {
    using namespace openspace::documentation;

    using TimeVerifier = StringVerifier;
    using LuaScriptVerifier = StringVerifier;

    return{
        "Scheduled Scripts",
        "core_scheduledscript",
        {
            {
                "*",
                new TableVerifier({
                    {
                        KeyTime,
                        new TimeVerifier,
                        Optional::No,
                        "The time at which, when the in game time passes it, the two "
                        "scripts will be executed. If the traversal is forwards (towards "
                        "+ infinity), the ForwardScript will be executed, otherwise the "
                        "BackwardScript will be executed instead."
                    },
                    {
                        KeyUniversalScript,
                        new LuaScriptVerifier,
                        Optional::Yes,
                        "The Lua script that will be executed when the specified time is "
                        "passed independent of its direction. This script will be "
                        "executed before the specific scripts if both versions are "
                        "specified"
                    },
                    {
                        KeyForwardScript,
                        new LuaScriptVerifier,
                        Optional::Yes,
                        "The Lua script that is executed when OpenSpace passes the time "
                        "in a forward direction."
                    },
                    {
                        KeyBackwardScript,
                        new LuaScriptVerifier,
                        Optional::Yes,
                        "The Lua script that is executed when OpenSpace passes the time "
                        "in a backward direction."
                    }
                }),
                Optional::No
            }
        }
    };
}

ScriptScheduler::ScheduledScript::ScheduledScript(const ghoul::Dictionary& dictionary) {
    const std::string& timeStr = dictionary.value<std::string>(KeyTime);
    time = Time::convertTime(timeStr);

    // If a universal script is specified, retrieve it and add a ; as a separator so that
    // it can be added to the other scripts
    std::string universal;
    dictionary.getValue(KeyUniversalScript, universal);
    if (!universal.empty()) {
        universal += ";";
    }

    if (dictionary.hasKeyAndValue<std::string>(KeyForwardScript)) {
        forwardScript =
            universal + dictionary.value<std::string>(KeyForwardScript);
    }

    if (dictionary.hasKeyAndValue<std::string>(KeyBackwardScript)) {
        backwardScript =
            universal + dictionary.value<std::string>(KeyBackwardScript);
    }
}

void ScriptScheduler::loadScripts(const ghoul::Dictionary& dictionary) {
    // Check if all of the scheduled scripts are formed correctly
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "ScriptScheduler"
    );

    // Create all the scheduled script first
    std::vector<ScheduledScript> scheduledScripts;
    for (size_t i = 1; i <= dictionary.size(); ++i) {
        const ghoul::Dictionary& timedScriptDict = dictionary.value<ghoul::Dictionary>(
            std::to_string(i)
        );
        scheduledScripts.emplace_back(timedScriptDict);
    }

    // Sort scripts by time; use a stable_sort as the user might have had an intention
    // specifying multiple scripts for the same time in a specific order
    std::stable_sort(
        scheduledScripts.begin(),
        scheduledScripts.end(),
        [](const ScheduledScript& lhs, const ScheduledScript& rhs) {
            return lhs.time < rhs.time;
        }
    );

    // Move the scheduled scripts into their SOA alignment
    // For the forward scripts, this is the forwards direction
    // For the backward scripts, we insert them in the opposite order so that we can still
    // return forward iterators to them in the progressTo method
    for (ScheduledScript& script : scheduledScripts) {
        _timings.push_back(script.time);

        _forwardScripts.push_back(std::move(script.forwardScript));

        _backwardScripts.insert(
            _backwardScripts.begin(),
            std::move(script.backwardScript)
        );
    }

    // Ensure _currentIndex and _currentTime is accurate after new scripts was added
    const double lastTime = _currentTime;
    rewind();
    progressTo(lastTime);

    ghoul_assert(
        (_timings.size() == _forwardScripts.size()) &&
        (_timings.size() == _backwardScripts.size()),
        "The SOA data structure has been mistreated and has different number of values"
    );
}

void ScriptScheduler::rewind() {
    _currentIndex = 0;
    _currentTime = -std::numeric_limits<double>::max();
}

void ScriptScheduler::clearSchedule() {
    rewind();
    _timings.clear();
    _forwardScripts.clear();
    _backwardScripts.clear();
}

std::pair<ScriptScheduler::ScriptIt, ScriptScheduler::ScriptIt>
ScriptScheduler::progressTo(double newTime)
{
    if (newTime == _currentTime) {
        return { _forwardScripts.end(), _forwardScripts.end() };
    }

    if (newTime > _currentTime) {
        // Moving forward in time; we need to find the highest entry in the timings
        // vector that is still smaller than the newTime
        size_t prevIndex = _currentIndex;
        const auto it = std::upper_bound(
            _timings.begin() + prevIndex, // We only need to start at the previous time
            _timings.end(),
            newTime
         );

        // How many values did we pass over?
        const ptrdiff_t n = std::distance(_timings.begin() + prevIndex, it);
        _currentIndex = static_cast<int>(prevIndex + n);

        // Update the new time
        _currentTime = newTime;

        return {
            _forwardScripts.begin() + prevIndex,
            _forwardScripts.begin() + _currentIndex
        };
    }
    else {
        // Moving backward in time; the need to find the lowest entry that is still bigger
        // than the newTime
        const size_t prevIndex = _currentIndex;
        const auto it = std::lower_bound(
            _timings.begin(),
            _timings.begin() + prevIndex, // We can stop at the previous time
            newTime
        );

        // How many values did we pass over?
        const ptrdiff_t n = std::distance(it, _timings.begin() + prevIndex);
        _currentIndex = static_cast<int>(prevIndex - n);

        // Update the new time
        _currentTime = newTime;

        return {
            _backwardScripts.begin() + (_timings.size() - prevIndex),
            _backwardScripts.begin() + (_timings.size() - _currentIndex)
        };
    }
}

double ScriptScheduler::currentTime() const {
    return _currentTime;
}

std::vector<ScriptScheduler::ScheduledScript> ScriptScheduler::allScripts() const {
    std::vector<ScheduledScript> result;
    for (size_t i = 0; i < _timings.size(); ++i) {
        ScheduledScript script;
        script.time = _timings[i];
        script.forwardScript = _forwardScripts[i];
        script.backwardScript = _backwardScripts[i];

        result.push_back(std::move(script));
    }
    return result;
}

LuaLibrary ScriptScheduler::luaLibrary() {
    return {
        "scriptScheduler",
        {
            {
                "loadFile",
                &luascriptfunctions::loadFile,
                {},
                "string",
                "Load timed scripts from a Lua script file that returns a list of "
                "scheduled scripts."
            },
            {
                "loadScheduledScript",
                &luascriptfunctions::loadScheduledScript,
                {},
                "string, string, (string, string)",
                "Load a single scheduled script. The first argument is the time at which "
                "the scheduled script is triggered, the second argument is the script "
                "that is executed in the forward direction, the optional third argument "
                "is the script executed in the backwards direction, and the optional "
                "last argument is the universal script, executed in either direction."

            },
            {
                "clear",
                &luascriptfunctions::clear,
                {},
                "",
                "Clears all scheduled scripts."
            },
        }
    };
}

} // namespace openspace::scripting
