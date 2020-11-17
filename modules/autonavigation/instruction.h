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

#ifndef __OPENSPACE_MODULE___PATHINSTRUCTION___H__
#define __OPENSPACE_MODULE___PATHINSTRUCTION___H__

#include <modules/autonavigation/waypoint.h>
#include <openspace/interaction/navigationhandler.h>
#include <optional>

namespace openspace::autonavigation {

struct Instruction {
    Instruction() = default;
    Instruction(const ghoul::Dictionary& dictionary);
    virtual ~Instruction();

    virtual std::vector<Waypoint> waypoints() const = 0;

    // TODO
    //static documentation::Documentation Documentation();

    std::optional<double> duration;

    std::optional<bool> stopAtTarget;

    // only relevant is stopAtTarget true
    std::optional<double> stopDuration;
    std::optional<std::string> stopBehavior;
};

struct TargetNodeInstruction : public Instruction {
    TargetNodeInstruction(const ghoul::Dictionary& dictionary);

    std::vector<Waypoint> waypoints() const override;
    bool setUpDirectionFromTarget() const;

    // TODO
    //static documentation::Documentation Documentation();

    std::string nodeIdentifier;
    std::optional<glm::dvec3> position; // relative to target node (model space)
    std::optional<double> height;
    std::optional<bool> useTargetUpDirection;
};

struct NavigationStateInstruction : public Instruction {
    using NavigationState = interaction::NavigationHandler::NavigationState;

    NavigationStateInstruction(const ghoul::Dictionary& dictionary);

    std::vector<Waypoint> waypoints() const override;

    // TODO
    //static documentation::Documentation Documentation();

    NavigationState navigationState;
};


} // namespace openspace::autonavigation

#endif // __OPENSPACE_MODULE___PATHINSTRUCTION___H__
