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

#include <modules/statemachine/include/transition.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>

namespace {
    struct [[codegen::Dictionary(Transition)]] Parameters {
        // The identifier of the state that can trigger the transition
        std::string from;

        // The identifier of the state that the state machine will move to after the
        // transition
        std::string to;

        // A string containing a Lua script that will be executed when the transition
        // is triggered
        std::optional<std::string> action;
    };
#include "transition_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation Transition::Documentation() {
    return codegen::doc<Parameters>("statemachine_transition");
}

Transition::Transition(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _from = p.from;
    _to = p.to;
    _action = p.action.value_or(_action);
}

const std::string& Transition::from() const {
    return _from;
}

const std::string& Transition::to() const {
    return _to;
}

void Transition::performAction() const {
    global::scriptEngine->queueScript(_action);
}

} // namespace openspace
