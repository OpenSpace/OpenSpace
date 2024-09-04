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

#ifndef __OPENSPACE_MODULE_STATEMACHINE___STATEMACHINEMODULE___H__
#define __OPENSPACE_MODULE_STATEMACHINE___STATEMACHINEMODULE___H__

#include <openspace/util/openspacemodule.h>

#include <modules/statemachine/include/statemachine.h>
#include <memory>
#include <optional>

namespace openspace {

class StateMachineModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "StateMachine";

    StateMachineModule();
    ~StateMachineModule() override = default;

    void initializeStateMachine(const ghoul::Dictionary& states,
        const ghoul::Dictionary& transitions,
        std::optional<std::string> startState = std::nullopt);
    void deinitializeStateMachine();

    bool hasStateMachine() const;

    // initializeStateMachine must have been called before
    void setInitialState(const std::string& initialState);
    std::string currentState() const;
    std::vector<std::string> possibleTransitions() const;
    void transitionTo(const std::string& newState);
    bool canGoToState(const std::string& state) const;

    /**
     * Save the state machine to a file given by the name and optional directory.
     * If no directory is given, the TEMP folder is used.
     */
    void saveToFile(const std::string& filename,
        std::string directory = "${TEMPORARY}/") const;

    scripting::LuaLibrary luaLibrary() const override;

    std::vector<documentation::Documentation> documentations() const override;

private:
    std::unique_ptr<StateMachine> _machine = nullptr;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_STATEMACHINE___STATEMACHINEMODULE___H__
