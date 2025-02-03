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

#include <modules/statemachine/include/state.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>

namespace {
    struct [[codegen::Dictionary(State)]] Parameters {
        // A string that will be used to identify the state. Cannot be the same as
        // any other state in the machine
        std::string identifier;

        // A string containing a Lua script that will be executed when the state
        // is entered, i.e on a transition from another state
        std::optional<std::string> enter;

        // A string containing a Lua script that will be executed when the state
        // is exited, i.e on a transition to another state
        std::optional<std::string> exit;
    };
#include "state_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation State::Documentation() {
    return codegen::doc<Parameters>("statemachine_state");
}

State::State(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _name = p.identifier;
    _enter = p.enter.value_or("");
    _exit = p.exit.value_or("");
}

void State::enter() const {
    global::scriptEngine->queueScript(_enter);
}

void State::exit() const {
    global::scriptEngine->queueScript(_exit);
}

std::string State::name() const {
    return _name;
}

} // namespace openspace
