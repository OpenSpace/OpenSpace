/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>

namespace {
    constexpr const char* FromStateKey = "From";
    constexpr const char* ToStateKey = "To";
    constexpr const char* ActionFunctionKey = "Action";
} // namespace

namespace openspace {

Transition::Transition(const ghoul::Dictionary& dictionary) {
    if (dictionary.hasKey(FromStateKey)) {
        _from = dictionary.value<std::string>(FromStateKey);
    }

    if (dictionary.hasKey(ToStateKey)) {
        _to = dictionary.value<std::string>(ToStateKey);
    }

    if (dictionary.hasKey(ActionFunctionKey)) {
        _action = dictionary.value<std::string>(ActionFunctionKey);
    }
}

std::string Transition::from() const {
    return _from;
}

std::string Transition::to() const {
    return _to;
}

void Transition::performAction() const {
    global::scriptEngine->queueScript(
        _action,
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

} // namespace openspace
