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

    struct [[codegen::Dictionary(PathSpecification)]] Parameters {
        // A list of path instructions
        std::vector<ghoul::Dictionary> instructions 
            [[codegen::reference("autonavigation_pathinstruction")]];

        // Decides whether the path should be paused when reaching an intermediate target 
        std::optional<bool> stopAtTargets;

        // A navigation state that determines the start state for the camera path
        std::optional<ghoul::Dictionary> startState 
            [[codegen::reference("core_navigation_state")]];
    };
#include "pathspecification_codegen.cpp"

} // namespace

namespace openspace::autonavigation {

using NavigationState = interaction::NavigationHandler::NavigationState;

documentation::Documentation PathSpecification::Documentation() {
    return codegen::doc<Parameters>("autonavigation_pathspecification");
}

PathSpecification::PathSpecification(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    const std::vector<ghoul::Dictionary> instructions = p.instructions;
    int counter = 1; 
    for (const ghoul::Dictionary& dict : instructions) {
        try {
            _instructions.push_back(Instruction(dict));
        }
        catch (ghoul::RuntimeError& e) {
            LERROR(fmt::format("Failed reading instruction {}: {}", counter, e.message));
        }
        counter++;
    }

    _stopAtTargets = p.stopAtTargets;

    if (p.startState.has_value()) {
        _startState = NavigationState(p.startState.value());
    }
}

PathSpecification::PathSpecification(const Instruction instruction) {
    _instructions.push_back(instruction);
}

const std::vector<Instruction>& PathSpecification::instructions() const {
    return _instructions;
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

} // namespace openspace::autonavigation
