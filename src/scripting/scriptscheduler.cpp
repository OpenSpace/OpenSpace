/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/time.h>
#include <ghoul/logging/logmanager.h>

#include "scriptscheduler_lua.inl"

namespace {
    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "This enables or disables the ScriptScheduler. If disabled, no scheduled scripts "
        "will be executed. If enabled, scheduled scripts will be executed at their given "
        "time as normal.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ShouldRunAllTimeJumpInfo = {
        "ShouldRunAllTimeJump",
        "Should Run All Time Jump",
        "If 'true': In a time jump, all scheduled scripts between the old time and the "
        "new time is executed. If 'false': In a time jump, no scripts scheduled between "
        "the new time and the old time is executed.",
        openspace::properties::Property::Visibility::AdvancedUser
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

        // The group that this script belongs to, default group is 0
        std::optional<int> group;
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
    , _shouldRunAllTimeJump(ShouldRunAllTimeJumpInfo, true)
{
    addProperty(_enabled);
    addProperty(_shouldRunAllTimeJump);
}

ScriptScheduler::ScheduledScript::ScheduledScript(const ghoul::Dictionary& dict) {
    const Parameters p = codegen::bake<Parameters>(dict);

    time = Time::convertTime(p.time);
    forwardScript = p.forwardScript.value_or(forwardScript);
    backwardScript = p.backwardScript.value_or(backwardScript);
    universalScript = p.script.value_or(universalScript);
    group = p.group.value_or(group);
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

    for (ScheduledScript& script : scheduledScripts) {
        _scripts.push_back(std::move(script));
    }

    // Re-sort so it is always in sorted order in regards to time
    std::stable_sort(
        _scripts.begin(),
        _scripts.end(),
        [](const ScheduledScript& lhs, const ScheduledScript& rhs) {
            return lhs.time < rhs.time;
        }
    );

    // Ensure _currentIndex and _currentTime is accurate after new scripts was added
    const double lastTime = _currentTime;
    rewind();
    progressTo(lastTime);
}

void ScriptScheduler::rewind() {
    _currentIndex = 0;
    _currentTime = -std::numeric_limits<double>::max();
}

void ScriptScheduler::clearSchedule(std::optional<int> group) {
    if (group.has_value()) {
        for (auto it = _scripts.begin(); it < _scripts.end();) {
            if (it->group == *group) {
                it = _scripts.erase(it);
            }
            else {
                it++;
            }
        }

        // Ensure _currentIndex and _currentTime is accurate after scripts was removed
        const double lastTime = _currentTime;
        rewind();
        progressTo(lastTime);
    }
    else {
        rewind();
        _scripts.clear();
    }
}

std::vector<std::string> ScriptScheduler::progressTo(double newTime) {
    std::vector<std::string> result;
    if (!_enabled || newTime == _currentTime || _scripts.empty()) {
        // Update the new time
        _currentTime = newTime;
        return result;
    }

    if (newTime > _currentTime) {
        // Moving forward in time; we need to find the highest entry in the timings
        // vector that is still smaller than the newTime
        const size_t prevIndex = _currentIndex;
        const auto it = std::upper_bound(
            _scripts.begin() + prevIndex, // We only need to start at the previous time
            _scripts.end(),
            newTime,
            [](const double value, const ScheduledScript& item) {
                return value < item.time;
            }
         );

        // How many values did we pass over?
        const ptrdiff_t n = std::distance(_scripts.begin() + prevIndex, it);
        _currentIndex = static_cast<int>(prevIndex + n);

        // Update the new time
        _currentTime = newTime;

        // Construct result
        for (auto iter = _scripts.begin() + prevIndex;
            iter < (_scripts.begin() + _currentIndex);
            iter++)
        {
            std::string script = iter->universalScript.empty() ?
                iter->forwardScript :
                iter->universalScript + "; " + iter->forwardScript;
            result.push_back(std::move(script));
        }

        return result;
    }
    else {
        // Moving backward in time; the need to find the lowest entry that is still bigger
        // than the newTime
        const size_t prevIndex = _currentIndex;
        const auto it = std::lower_bound(
            _scripts.begin(),
            _scripts.begin() + prevIndex, // We can stop at the previous time
            newTime,
            [](const ScheduledScript& item, const double value) {
                return item.time < value;
            }
        );

        // How many values did we pass over?
        const ptrdiff_t n = std::distance(it, _scripts.begin() + prevIndex);
        _currentIndex = static_cast<int>(prevIndex - n);

        // Update the new time
        _currentTime = newTime;

        // Construct result
        const size_t startOffset = prevIndex == 0 ? prevIndex : prevIndex - 1;
        auto start = _scripts.begin() + startOffset;
        auto end = it;
        for (auto iter = start; iter != _scripts.end() && iter >= end; --iter) {
            std::string script = iter->universalScript.empty() ?
                iter->backwardScript :
                iter->universalScript + "; " + iter->backwardScript;
            result.push_back(std::move(script));

            if (iter == _scripts.begin()) {
                break;
            }
        }

        return result;
    }
}

void ScriptScheduler::setTimeReferenceMode(interaction::KeyframeTimeRef refType) {
    _timeframeMode = refType;
}

double ScriptScheduler::currentTime() const {
    return _currentTime;
}

void ScriptScheduler::setCurrentTime(double time) {
    // Ensure _currentIndex and _currentTime is accurate after time jump
    const std::vector<std::string> scheduledScripts = progressTo(time);

    if (_shouldRunAllTimeJump) {
        // Queue all scripts for the time jump
        for (const std::string& script : scheduledScripts) {
            global::scriptEngine->queueScript(
                script,
                scripting::ScriptEngine::ShouldBeSynchronized::Yes,
                scripting::ScriptEngine::ShouldSendToRemote::Yes
            );
        }
    }
}

std::vector<ScriptScheduler::ScheduledScript> ScriptScheduler::allScripts(
                                                           std::optional<int> group) const
{
    std::vector<ScheduledScript> result;
    for (const ScheduledScript& script : _scripts) {
        if (!group.has_value() || script.group == *group) {
            result.push_back(script);
        }
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
            codegen::lua::LoadFile,
            codegen::lua::LoadScheduledScript,
            codegen::lua::SetModeApplicationTime,
            codegen::lua::SetModeRecordedTime,
            codegen::lua::SetModeSimulationTime,
            codegen::lua::Clear,
            codegen::lua::ScheduledScripts
        }
    };
}

} // namespace openspace::scripting
