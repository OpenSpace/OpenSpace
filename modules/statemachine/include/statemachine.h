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

#ifndef __OPENSPACE_MODULE_STATEMACHINE___STATEMACHINE___H__
#define __OPENSPACE_MODULE_STATEMACHINE___STATEMACHINE___H__

#include <modules/statemachine/include/state.h>
#include <modules/statemachine/include/transition.h>
#include <vector>

namespace openspace {

namespace documentation { struct Documentation; }

class StateMachine {
public:
    StateMachine(const ghoul::Dictionary& dictionary);
    ~StateMachine() = default;

    void setInitialState(const std::string initialState);
    State* currentState() const;
    void transitionTo(const std::string newState);
    bool canGoTo(const std::string state) const;
    void setState(State& newState);

    static documentation::Documentation Documentation();

private:
    State* _currentState = nullptr;
    std::vector<State> _states;
    std::vector<Transition> _transitions;

    int findTransitionTo(const std::string state) const;
    int findState(const std::string state) const;
};

} // namespace openspace

#endif __OPENSPACE_MODULE_STATEMACHINE___STATEMACHINE___H__
