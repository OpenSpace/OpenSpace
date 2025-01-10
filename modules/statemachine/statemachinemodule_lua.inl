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

namespace {

/**
 * Creates a state machine from a list of states and transitions. See State and Transition
 * documentation for details. The optional thrid argument is the identifier of the desired
 * initial state. If left out, the first state in the list will be used.
 */
[[codegen::luawrap]] void createStateMachine(ghoul::Dictionary states,
                                             ghoul::Dictionary transitions,
                                             std::optional<std::string> startState)
{
    using namespace openspace;
    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    module->initializeStateMachine(
        std::move(states),
        std::move(transitions),
        std::move(startState)
    );
}

// Destroys the current state machine and deletes all the memory.
[[codegen::luawrap]] void destroyStateMachine() {
    using namespace openspace;
    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    module->deinitializeStateMachine();
}

/**
 * Triggers a transition from the current state to the state with the given identifier.
 * Requires that the specified string corresponds to an existing state, and that a
 * transition between the two states exists.
 */
[[codegen::luawrap]] void goToState(std::string newState) {
    using namespace openspace;
    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    module->transitionTo(newState);
    LINFOC("StateMachine", "Transitioning to " + newState);
}

/**
 * Immediately sets the current state to the state with the given name, if it exists. This
 * is done without doing a transition and completely ignores the previous state.
 */
[[codegen::luawrap]] void setInitialState(std::string startState) {
    using namespace openspace;
    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    module->setInitialState(startState);
    LINFOC("StateMachine", "Initial state set to: " + startState);
}

// Returns the string name of the current state that the statemachine is in.
[[codegen::luawrap]] std::string currentState() {
    using namespace openspace;
    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    std::string currentState = module->currentState();
    return currentState;
}

/**
 * Returns a list with the identifiers of all the states that can be transitioned to from
 * the current state.
 */
[[codegen::luawrap]] std::vector<std::string> possibleTransitions() {
    using namespace openspace;
    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    std::vector<std::string> transitions = module->possibleTransitions();
    return transitions;
}

/**
 * Returns true if there is a defined transition between the current state and the given
 * string name of a state, otherwise false.
 */
[[codegen::luawrap]] bool canGoToState(std::string state) {
    using namespace openspace;
    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    bool canTransition = module->canGoToState(state);
    return canTransition;
}

/**
 * Prints information about the current state and possible transitions to the log.
 */
[[codegen::luawrap]] void printCurrentStateInfo() {
    using namespace openspace;
    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    if (module->hasStateMachine()) {
        std::string currentState = module->currentState();
        std::vector<std::string> transitions = module->possibleTransitions();
        LINFOC(
            "StateMachine",
            std::format(
                "Currently in state: '{}'. Can transition to states: [ {} ]",
                currentState,
                ghoul::join(transitions, ",")
            )
        );
    }
    else {
        LINFOC("StateMachine", "No state machine has been created");
    }
}

/**
 * Saves the current state machine to a .dot file as a directed graph. The resulting graph
 * can be rendered using external tools such as Graphviz. The first parameter is the name
 * of the file, and the second is an optional directory. If no directory is given, the
 * file is saved to the temp folder.
 */
[[codegen::luawrap]] void saveToDotFile(std::string filename,
                                        std::optional<std::string> directory)
{
    using namespace openspace;
    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    if (directory.has_value()) {
        module->saveToFile(filename, *directory);
    }
    else {
        module->saveToFile(filename);
    }
}

#include "statemachinemodule_lua_codegen.cpp"

} // namespace
