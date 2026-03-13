/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/navigation/orbitalnavigator/touchcamerastates.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/touchinputstate.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "TouchCameraStates";

    double validAngleDiff(double lastAngle, double currentAngle) {
        // Ifs used to set angles 359 + 1 = 0 and 0 - 1 = 359
        if (lastAngle > currentAngle + 1.5 * glm::pi<float>()) {
            return currentAngle + (2.0 * glm::pi<float>() - lastAngle);
        }
        else if (currentAngle > lastAngle + 1.5 * glm::pi<float>()) {
            return (2.0 * glm::pi<float>() - currentAngle) + lastAngle;
        }
        else {
            return currentAngle - lastAngle;
        }
    }
} // namespace

namespace openspace {

TouchCameraStates::TouchCameraStates(double sensitivity, double velocityScaleFactor)
    : OrbitalCameraStates(sensitivity, velocityScaleFactor)
    , _pinchInputs({
        TouchInputHolder(TouchInput(0, 0, glm::vec2(0.f), 0.0)),
        TouchInputHolder(TouchInput(0, 0, glm::vec2(0.f), 0.0))
    })
{}

TouchCameraStates::~TouchCameraStates() {}

void TouchCameraStates::updateVelocitiesFromInput(const TouchInputState& touchState,
                                                  double deltaTime)
{
    const std::vector<TouchInputHolder>& touchPoints = touchState.touchPoints();
    const std::vector<TouchInput>& lastProcessed = touchState.lastProcessedInputs();

    UpdateStates updateStates = computeVelocities(touchPoints, lastProcessed);
    updateVelocities(std::move(updateStates), deltaTime);

    resetAfterInput();
}

OrbitalCameraStates::UpdateStates
TouchCameraStates::computeVelocities(const std::vector<TouchInputHolder>& touchPoints,
                                     const std::vector<TouchInput>& lastProcessed)
{
    UpdateStates updateVelocities;

    if (touchPoints.empty()) {
        return updateVelocities;
    }

    const InteractionType action = interpretInteraction(touchPoints, lastProcessed);

#ifdef TOUCH_DEBUG_MODE
    const std::map<InteractionType, std::string> interactionNames = {
        { InteractionType::Rotation, "Rotation" },
        { InteractionType::Pinch, "Pinch" },
        { InteractionType::Pan, "Pan" },
        { InteractionType::Roll, "Roll" },
        { InteractionType::None, "None" }
    };
    LDEBUG(std::format("Interpreted interaction: {}", interactionNames.at(action)));
#endif // TOUCH_DEBUG_MODE

    const TouchInputHolder& currentInput = touchPoints.at(0);

    const glm::vec2 windowSize = glm::vec2(global::windowDelegate->currentWindowSize());
    const float aspectRatio = windowSize.x / windowSize.y;

    switch (action) {
        case InteractionType::Rotation: {
            // Add rotation velocity
            constexpr glm::dvec2 Scale = glm::dvec2(5.0);
            updateVelocities.globalRotation = Scale * _sensitivity * -glm::dvec2(
                currentInput.speedX(),
                currentInput.speedY()
            );
            break;
        }
        case InteractionType::Pinch: {
            // Add zooming velocity - dependent on distance difference between contact
            // points this/first frame
            const TouchInput& startFinger0 = _pinchInputs[0].firstInput();
            const TouchInput& startFinger1 = _pinchInputs[1].firstInput();
            const glm::dvec2 startVec0 = glm::dvec2(
                startFinger0.pos.x * aspectRatio,
                startFinger0.pos.y
            );
            const glm::dvec2 startVec1 = glm::dvec2(
                startFinger1.pos.x * aspectRatio,
                startFinger1.pos.y
            );
            double distToCentroidStart = glm::length(startVec0 - startVec1) / 2.0;

            const TouchInput& endFinger0 = _pinchInputs[0].latestInput();
            const TouchInput& endFinger1 = _pinchInputs[1].latestInput();
            const glm::dvec2 endVec0 = glm::dvec2(
                endFinger0.pos.x * aspectRatio,
                endFinger0.pos.y
            );
            const glm::dvec2 endVec1 = glm::dvec2(
                endFinger1.pos.x * aspectRatio,
                endFinger1.pos.y
            );
            double distToCentroidEnd = glm::length(endVec0 - endVec1) / 2.0;

            double zoomFactor = distToCentroidEnd - distToCentroidStart;

            const double Scale = 100.0;
            updateVelocities.zoom = Scale * zoomFactor * _sensitivity;

            break;
        }
        case InteractionType::Roll: {
            // Add global roll rotation velocity
            double rollFactor = 0.0;
            for (const TouchInputHolder& touchPoint : touchPoints) {
                TouchInput point = *std::find_if(
                    lastProcessed.begin(),
                    lastProcessed.end(),
                    [&touchPoint](const TouchInput& input) {
                        return touchPoint.holdsInput(input);
                    }
                );
                float lastAngle = point.angleToPos(_centroid);
                float currentAngle = touchPoint.latestInput().angleToPos(_centroid);
                rollFactor += validAngleDiff(lastAngle, currentAngle);
            }
            double avgRollFactor = rollFactor / static_cast<double>(touchPoints.size());

            const double Scale = 100.0;
            updateVelocities.globalRoll = Scale * -avgRollFactor * _sensitivity;
            break;
        }
        case InteractionType::Pan: {
            // Add local rotation velocity
            const glm::dvec2 Scale = glm::dvec2(10.0);

            // @TODO (2026-03-11, emmbr) Update scaling and gesture. Panning is currently
            // triggered using a three-finger gesture, which is often reserved for other
            // windows-related interaction, so I could not test it
            updateVelocities.localRotation = _sensitivity * Scale * glm::dvec2(
                currentInput.speedX(),
                currentInput.speedY()
            );

            break;
        }
        case InteractionType::NONE:
        default:
            break;
    }

    return updateVelocities;
}

TouchCameraStates::InteractionType
TouchCameraStates::interpretInteraction(const std::vector<TouchInputHolder>& inputs,
                                        const std::vector<TouchInput>& lastProcessed)
{
    ghoul_assert(!inputs.empty(), "Cannot interpret interaction of no input");

    if (inputs.size() != lastProcessed.size() || inputs.empty() || lastProcessed.empty()) {
        // Not a valid gesture. Probably just a tap.
        // @TODO (2026-03-11, emmbr) This code prevents a crash from happening, but
        // ideally we should instea dchange the code below so this check is not needed
        return InteractionType::None;
    }

    // A one finger gesture is always interpreted as a rotation
    if (inputs.size() == 1) {
        return InteractionType::Rotation;
    }

    // Handle multi-finger gestures

    // Compute centroid of current touch points
    glm::vec2 lastCentroid = _centroid;
    _centroid = glm::vec2(0.f);
    for (const TouchInputHolder& inputHolder : inputs) {
        _centroid += inputHolder.latestInput().pos;
    }
    _centroid /= static_cast<float>(inputs.size());

    // See if the distance between fingers changed - used in pan interpretation
    double dist = 0.0;
    double lastDist = 0.0;
    TouchInput distInput = inputs[0].latestInput();
    for (const TouchInputHolder& inputHolder : inputs) {
        const TouchInput& latestInput = inputHolder.latestInput();
        dist += glm::length(glm::dvec2(latestInput.pos) - glm::dvec2(distInput.pos));
        distInput = latestInput;
    }
    distInput = lastProcessed[0];
    for (const TouchInput& p : lastProcessed) {
        lastDist += glm::length(glm::dvec2(p.pos) - glm::dvec2(distInput.pos));
        distInput = p;
    }

    const float InterpretPanDistance = 0.015f;

    float avgDistance = static_cast<float>(std::abs(dist - lastDist));

    // If average distance between 3 fingers are constant we have panning
    if (avgDistance < InterpretPanDistance && inputs.size() == 3) {
        return InteractionType::PAN;
    }

    // Find the slowest moving finger - used in roll interpretation
    double minDiff = 1000.0;

    // Find if all fingers angles are high - used in roll interpretation
    double rollFactor = 0.0;

    for (const TouchInputHolder& inputHolder : inputs) {
        const auto it = std::find_if(
            lastProcessed.cbegin(),
            lastProcessed.cend(),
            [&inputHolder](const TouchInput& input) {
                return inputHolder.holdsInput(input);
            }
        );

        if (it == lastProcessed.cend()) {
            continue;
        }
        const TouchInput& latestInput = inputHolder.latestInput();
        const TouchInput& prevInput = *it;

        // Compute diff for slowest moving finger
        const glm::vec2 d = latestInput.pos - prevInput.pos;
        const double diff = d.x + d.y;
        if (!inputHolder.isMoving()) {
            minDiff = 0.0;
        }
        else if (std::abs(diff) < std::abs(minDiff)) {
            minDiff = diff;
        }

        // Compute roll factor, as the difference in angle to centroid between frames
        float lastAngle = prevInput.angleToPos(_centroid);
        float currentAngle = inputHolder.latestInput().angleToPos(_centroid);

        double res = validAngleDiff(lastAngle, currentAngle);

        const float RollAngleThreshold = 0.025f;
        if (std::abs(res) < RollAngleThreshold) {
            rollFactor = 1000.0; // @TODO: Figure out this magic number
        }
        else {
            rollFactor += res;
        }
    }

    float normalizedCentroidDist = glm::distance(_centroid, lastCentroid);
    normalizedCentroidDist /= static_cast<float>(inputs.size());

    const float InputStillThreshold = 0.0005f;
    const float CentroidStillThreshold = 0.0018f;

    // We have roll if one finger is still, or the total roll angles around the centroid
    // is over RollAngleThreshold (CentroidStillThreshold is used to avoid
    // misinterpretations)
    if (std::abs(minDiff) < InputStillThreshold ||
        (std::abs(rollFactor) < 100.0 && normalizedCentroidDist < CentroidStillThreshold))
    {
        return InteractionType::Roll;
    }
    else {
        // Check for pinch gesture
        const bool isSameInput0 = _pinchInputs[0].holdsInput(inputs[0].latestInput());
        const bool isSameInput1 = _pinchInputs[1].holdsInput(inputs[1].latestInput());
        if (isSameInput0 && isSameInput1) {
            _pinchInputs[0].tryAddInput(inputs[0].latestInput());
            _pinchInputs[1].tryAddInput(inputs[1].latestInput());
        }
        else {
            _pinchInputs[0] = TouchInputHolder(inputs[0].latestInput());
            _pinchInputs[1] = TouchInputHolder(inputs[1].latestInput());
        }
        return InteractionType::Pinch;
    }
}

void TouchCameraStates::resetAfterInput() {
    _pinchInputs[0].clearInputs();
    _pinchInputs[1].clearInputs();
}

} // namespace openspace
