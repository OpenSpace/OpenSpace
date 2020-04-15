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

#ifndef __OPENSPACE_MODULE___PATHSPECIFICATION___H__
#define __OPENSPACE_MODULE___PATHSPECIFICATION___H__

#include <modules/autonavigation/instruction.h>
#include <openspace/documentation/documentation.h>
#include <openspace/interaction/navigationhandler.h>
#include <ghoul/glm.h>
#include <optional>

namespace openspace::autonavigation {

class PathSpecification {
    using NavigationState = interaction::NavigationHandler::NavigationState;

public:
    PathSpecification() = default;
    PathSpecification(const ghoul::Dictionary& dictionary);
    PathSpecification(const TargetNodeInstruction instruction);

    static documentation::Documentation Documentation();

    // Accessors
    const std::vector<std::unique_ptr<Instruction>>* instructions() const;
    Instruction* instruction(int i) const;
    const bool stopAtTargets() const; 
    const bool stopAtTargetsSpecified() const;
    const NavigationState& startState() const;
    const bool hasStartState() const;

private:
    void tryReadInstruction(int index, std::string type, ghoul::Dictionary& dictionary);

    std::vector<std::unique_ptr<Instruction>> _instructions;
    std::optional<bool> _stopAtTargets; 
    std::optional<NavigationState> _startState;

    // TODO: maxSpeed or speedFactor or something?
};

} // namespace openspace::autonavigation

#endif // __OPENSPACE_MODULE___PATHSPECIFICATION___H__
