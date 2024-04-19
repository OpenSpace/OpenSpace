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

#include <modules/statemachine/include/statemachine.h>

#include <openspace/documentation/documentation.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <fstream>
#include <optional>

namespace {
    constexpr std::string_view _loggerCat = "StateMachine";

    struct [[codegen::Dictionary(StateMachine)]] Parameters {
        // A list of states
        std::vector<ghoul::Dictionary> states
            [[codegen::reference("statemachine_state")]];

        // A list of transitions between the different states
        std::vector<ghoul::Dictionary> transitions
            [[codegen::reference("statemachine_transition")]];

        // The initial state of the state machine. Defaults to the first in the list
        std::optional<std::string> startState;
    };
#include "statemachine_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation StateMachine::Documentation() {
    return codegen::doc<Parameters>("statemachine_statemachine");
}

StateMachine::StateMachine(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _states.reserve(p.states.size());
    for (const ghoul::Dictionary& s : p.states) {
        _states.emplace_back(s);
    }

    _transitions.reserve(p.transitions.size());
    for (const ghoul::Dictionary& t : p.transitions) {
        const Transition trans = Transition(t);

        // Check so transition has valid identifiers
        const bool foundFrom = findState(trans.from()) != -1;
        const bool foundTo = findState(trans.to()) != -1;

        if (foundFrom && foundTo) {
            _transitions.push_back(trans);
        }
        else {
            LERROR(std::format(
                "Invalid transition from '{}' to '{}'. One or both of the states do not "
                "exist in the state machine", trans.from(), trans.to()
            ));
        }
    }
    _transitions.shrink_to_fit();

    if (_transitions.empty()) {
        LWARNING("Created state machine without transitions");
    }

    if (_states.empty()) {
        LERROR("Created state machine with no states");
        return;
    }

    const std::string startState = p.startState.value_or(_states.front().name());
    setInitialState(startState);
}

void StateMachine::setInitialState(const std::string& initialState) {
    const int stateIndex = findState(initialState);

    if (stateIndex == -1) {
        LWARNING(std::format(
            "Attempting to initialize with undefined state '{}'", initialState
        ));
        return;
    }

    _currentStateIndex = stateIndex;
    currentState()->enter();
}

const State* StateMachine::currentState() const {
    if (_currentStateIndex == -1) {
        return nullptr;
    }
    return &_states[_currentStateIndex];
}

void StateMachine::transitionTo(const std::string& newState) {
    if (!currentState()) {
        LERROR(
            "Cannot perform transition as the machine is in no current state. First set "
            "an initial state"
        );
        return;
    }

    const int stateIndex = findState(newState);
    if (stateIndex == -1) {
        LWARNING(std::format(
            "Attempting to transition to undefined state '{}'", newState
        ));
        return;
    }

    const int transitionIndex = findTransitionTo(newState);
    if (transitionIndex == -1) {
        LWARNING(std::format(
            "Transition from '{}' to '{}' is undefined",
            currentState()->name(), newState
        ));
        return;
    }

    currentState()->exit();
    _transitions[transitionIndex].performAction();
    _currentStateIndex = stateIndex;
    currentState()->enter();
}

bool StateMachine::canTransitionTo(const std::string& state) const {
    const int transitionIndex = findTransitionTo(state);
    return transitionIndex != -1;
}

// Search if the transition from _currentState to newState exists.
// If yes then return the index to the transition, otherwise return -1
int StateMachine::findTransitionTo(const std::string& state) const {
    if (!currentState()) {
        return -1;
    }

    for (size_t i = 0; i < _transitions.size(); i++) {
        if (_transitions[i].from() == currentState()->name() &&
            _transitions[i].to() == state)
        {
            return static_cast<int>(i);
        }
    }
    return -1;
}

// Search if the state exist.
// If yes then return the index to the state, otherwise return -1
int StateMachine::findState(const std::string& state) const {
    for (size_t i = 0; i < _states.size(); i++) {
        if (_states[i].name() == state) {
            return static_cast<int>(i);
        }
    }
    return -1;
}

std::vector<std::string> StateMachine::possibleTransitions() const {
    std::vector<std::string> res;

    if (!currentState()) {
        return res;
    }

    res.reserve(_transitions.size());
    for (const Transition& transition : _transitions) {
        if (transition.from() == currentState()->name()) {
            res.push_back(transition.to());
        }
    }
    return res;
}

void StateMachine::saveToDotFile(const std::filesystem::path& filename) const {
    std::filesystem::path outputFile = filename;
    if (!outputFile.has_extension()) {
        outputFile.replace_extension(".dot");
    }

    std::ofstream file = std::ofstream(outputFile);
    if (!file.good()) {
        LERROR(std::format(
            "Error opening file '{}' for saving state machine dot file", outputFile
        ));
        return;
    }

    file << "digraph statemachine {\n";
    for (const State& s : _states) {
        file << std::format("\t{};\n", s.name());
    }
    for (const Transition& t : _transitions) {
        file << std::format("\t{} -> {};\n", t.from(), t.to());
    }
    file << "}\n";

    LINFO(std::format("Saved state machine to file: {}", outputFile));
}

} // namespace openspace
