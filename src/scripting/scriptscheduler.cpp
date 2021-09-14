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

#include <openspace/scripting/scriptscheduler.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/time.h>
#include <ghoul/logging/logmanager.h>

#include "scriptscheduler_lua.inl"

namespace {
    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "EnabledInfo",
        "Enabled",
        "This enables or disables the ScriptScheduler. If disabled, no scheduled scripts "
        "will be executed. If enabled, scheduled scripts will be executed at their given "
        "time as normal."
    };

    struct [[codegen::Dictionary(ScheduledScript)]] Parameters {
        // The time at which, when the in game time passes it, the two scripts will
        // be executed. If the traversal is forwards (towards + infinity), the
        // ForwardScript will be executed, otherwise the BackwardScript will be
        // executed instead
        std::string time;

        // The Lua script that will be executed when the specified time is passed
        // independent of its direction. This script will be executed before the
        // specific scripts if both versions are specified
        std::optional<std::string> script;

        // The Lua script that is executed when OpenSpace passes the time in a
        // forward direction
        std::optional<std::string> forwardScript;

        // The Lua script that is executed when OpenSpace passes the time in a
        // backward direction
        std::optional<std::string> backwardScript;
    };
#include "scriptscheduler_codegen.cpp"
} // namespace

namespace openspace::scripting {

documentation::Documentation ScriptScheduler::Documentation() {
    // @TODO (abock, 2021-03-25)  This is not really correct. This function currently
    // returns the documentation for the ScheduledScript, not for the ScriptScheduler
    // itself. This should be cleaned up a bit
    return codegen::doc<Parameters>("core_scheduledscript");
}

ScriptScheduler::ScriptScheduler()
    : properties::PropertyOwner({ "ScriptScheduler" })
    , _enabled(EnabledInfo, true)
{
    addProperty(_enabled);
}

ScriptScheduler::ScheduledScript::ScheduledScript(const ghoul::Dictionary& dict) {
    const Parameters p = codegen::bake<Parameters>(dict);

    time = Time::convertTime(p.time);
    forwardScript = p.forwardScript.value_or(forwardScript);
    backwardScript = p.backwardScript.value_or(backwardScript);
    universalScript = p.script.value_or(universalScript);
}

void ScriptScheduler::loadScripts(std::vector<ScheduledScript> scheduledScripts) {
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

        std::string forward =
            script.universalScript.empty() ?
            std::move(script.forwardScript) :
            std::move(script.universalScript) + ';' + std::move(script.forwardScript);
        _forwardScripts.push_back(forward);

        std::string backward =
            script.universalScript.empty() ?
            std::move(script.backwardScript) :
            std::move(script.universalScript) + ';' + std::move(script.backwardScript);
        _backwardScripts.insert(_backwardScripts.begin(), backward);
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
    if (!_enabled || newTime == _currentTime) {
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

void ScriptScheduler::setTimeReferenceMode(interaction::KeyframeTimeRef refType) {
    _timeframeMode = refType;
}

void ScriptScheduler::triggerPlaybackStart() {
    _playbackModeEnabled = true;
}

void ScriptScheduler::stopPlayback() {
    _playbackModeEnabled = false;
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

void ScriptScheduler::setModeApplicationTime() {
    _timeframeMode = interaction::KeyframeTimeRef::Relative_applicationStart;
}

void ScriptScheduler::setModeRecordedTime() {
    _timeframeMode = interaction::KeyframeTimeRef::Relative_recordedStart;
}

void ScriptScheduler::setModeSimulationTime() {
    _timeframeMode = interaction::KeyframeTimeRef::Absolute_simTimeJ2000;
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
                "setModeApplicationTime",
                &luascriptfunctions::setModeApplicationTime,
                {},
                "",
                "Sets the time reference for scheduled scripts to application time "
                "(seconds since OpenSpace application started)."
            },
            {
                "setModeRecordedTime",
                &luascriptfunctions::setModeRecordedTime,
                {},
                "",
                "Sets the time reference for scheduled scripts to the time since the "
                "recording was started (the same relative time applies to playback)."
            },
            {
                "setModeSimulationTime",
                &luascriptfunctions::setModeSimulationTime,
                {},
                "",
                "Sets the time reference for scheduled scripts to the simulated "
                "date & time (J2000 epoch seconds)."
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
