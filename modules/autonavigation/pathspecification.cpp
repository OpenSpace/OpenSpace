/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/autonavigation/pathspecification.h>

#include <modules/autonavigation/instruction.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "PathSpecification";

    constexpr const char* KeyInstructions = "Instructions";
    constexpr const char* KeyStopAtTargets = "StopAtTargets";
    constexpr const char* KeyStartState = "StartState";

    // Instruction Types
    constexpr const char* KeyType = "Type";
    constexpr const char* KeyTypeTargetNode = "Node";
    constexpr const char* KeyTypeNavigationState = "NavigationState";

} // namespace

namespace openspace::autonavigation {

using NavigationState = interaction::NavigationHandler::NavigationState;

PathSpecification::PathSpecification(const ghoul::Dictionary& dictionary) {
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "Path Specification"
    );

    ghoul::Dictionary instructions =
        dictionary.value<ghoul::Dictionary>(KeyInstructions);

    for (size_t i = 1; i <= instructions.size(); ++i) {
        ghoul::Dictionary insDict =
            instructions.value<ghoul::Dictionary>(std::to_string(i));

        if (!insDict.hasValue<std::string>(KeyType)) {
            throw ghoul::RuntimeError(
                "Each instruction must have a specified type."
            );
        }

        std::string type = insDict.value<std::string>(KeyType);
        tryReadInstruction(i, type, insDict);
    }

    if (dictionary.hasValue<bool>(KeyStopAtTargets)) {
        _stopAtTargets = dictionary.value<bool>(KeyStopAtTargets);
    }

    if (dictionary.hasValue<ghoul::Dictionary>(KeyStartState)) {
        auto navStateDict = dictionary.value<ghoul::Dictionary>(KeyStartState);

        try {
            openspace::documentation::testSpecificationAndThrow(
                NavigationState::Documentation(),
                navStateDict,
                "NavigationState"
            );
        }
        catch (ghoul::RuntimeError& e) {
            LERROR(fmt::format("Unable to read start navigation state. {}", e.message));
            return;
        }

        _startState = NavigationState(navStateDict);
    }
}

PathSpecification::PathSpecification(const TargetNodeInstruction instruction) {
    _instructions.push_back(std::make_unique<TargetNodeInstruction>(instruction));
}

const std::vector<std::unique_ptr<Instruction>>* PathSpecification::instructions() const {
    return &_instructions;
}

Instruction* PathSpecification::instruction(int i) const {
    return (_instructions.size() > i) ? _instructions[i].get() : nullptr;
}

const bool PathSpecification::stopAtTargets() const {
    return _stopAtTargets.value();
}

const bool PathSpecification::stopAtTargetsSpecified() const {
    return _stopAtTargets.has_value();
}

const NavigationState& PathSpecification::startState() const {
    return _startState.value();
}

const bool PathSpecification::hasStartState() const {
    return _startState.has_value();
}

documentation::Documentation PathSpecification::Documentation() {
    using namespace documentation;

    return {
        "Path Specification",
        "camera_path_specification",
        {
            {
                KeyInstructions,
                new TableVerifier,
                Optional::No,
                "A list of path instructions."
            },
            {
                KeyStopAtTargets,
                new BoolVerifier,
                Optional::Yes,
                "A bool that decides whether we should pause when reaching a target when playing a path."
            },
            {
                KeyStartState,
                new TableVerifier,
                Optional::Yes,
                "A navigation state that determines the start state for the camera path."
            },
        }
    };
}

// create correct type of instruction and present and throw error with useful
// error message if we failed.
void PathSpecification::tryReadInstruction(int index, std::string type,
                                           ghoul::Dictionary& dictionary)
{
    if (type == KeyTypeTargetNode) {
        try {
            _instructions.push_back(std::make_unique<TargetNodeInstruction>(dictionary));
        }
        catch (ghoul::RuntimeError& e) {
            throw ghoul::RuntimeError(
                fmt::format("Failed reading instruction {}: {}", index, e.message));
        }
    }
    else if (type == KeyTypeNavigationState) {
        try {
            _instructions.push_back(
                std::make_unique<NavigationStateInstruction>(dictionary));
        }
        catch (ghoul::RuntimeError& e) {
            throw ghoul::RuntimeError(
                fmt::format("Failed reading instruction {}: {}", index, e.message));
        }
    }
    else {
        throw ghoul::RuntimeError(fmt::format(
            "Failed reading instruction {}: Uknown instruction type '{}'",
            index, type)
        );
    }
}

} // namespace openspace::autonavigation
