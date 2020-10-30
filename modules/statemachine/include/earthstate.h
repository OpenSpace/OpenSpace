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

#ifndef __OPENSPACE_MODULE_STATEMACHINE___EARTHSTATE___H__
#define __OPENSPACE_MODULE_STATEMACHINE___EARTHSTATE___H__

#include "modules/statemachine/include/state.h"

namespace openspace {

class State;
class StateMachine;

// All concrete States should be singletons
class EarthState : public State {
public:
    static State& getInstance();
    ~EarthState();
    EarthState(const EarthState&) = delete;
    EarthState(const EarthState&&) = delete;
    EarthState& operator= (const EarthState& other) = delete;
    EarthState& operator= (const EarthState&& other) = delete;

    // What should be done entering the state, while in the state and exiting the state
    void enter(openspace::StateMachine* statemachine) override;
    void activate(openspace::StateMachine* statemachine) override;
    void idle(openspace::StateMachine* statemachine) override;
    void exit(openspace::StateMachine* statemachine) override;
    bool isIdle() override;
    std::string name() override;

private:
    EarthState();
    bool _isIdle;
    std::string _name;
};

} // namespace openspace

#endif __OPENSPACE_MODULE_STATEMACHINE___EARTHSTATE___H__
