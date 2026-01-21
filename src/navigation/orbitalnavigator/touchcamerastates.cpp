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

namespace openspace::interaction {

TouchCameraStates::TouchCameraStates(double sensitivity, double velocityScaleFactor)
    : OrbitalCameraStates(sensitivity, velocityScaleFactor)
    , _pinchInputs({
        TouchInputHolder(TouchInput(0, 0, 0.f, 0.f, 0.0)),
        TouchInputHolder(TouchInput(0, 0, 0.f, 0.f, 0.0))
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
        { InteractionType::ROTATION, "Rotation" },
        { InteractionType::PINCH, "Pinch" },
        { InteractionType::PAN, "Pan" },
        { InteractionType::ROLL, "Roll" },
        { InteractionType::NONE, "None" }
    };
    LDEBUG(std::format("Interpreted interaction: {}", interactionNames.at(action)));
#endif // TOUCH_DEBUG_MODE

    const TouchInputHolder& currentInput = touchPoints.at(0);

    const glm::ivec2 windowSize = global::windowDelegate->currentWindowSize();
    const float aspectRatio =
        static_cast<float>(windowSize.x) / static_cast<float>(windowSize.y);

    switch (action) {
        case InteractionType::ROTATION: {
            // Add rotation velocity
            glm::dvec2 scale = glm::dvec2(5.0);
            updateVelocities.globalRotation = scale * _sensitivity * -glm::dvec2(
                currentInput.speedX(),
                currentInput.speedY()
            );
            break;
        }
        case InteractionType::PINCH: {
            // Add zooming velocity - dependent on distance difference between contact
            // points this/first frame
            const TouchInput& startFinger0 = _pinchInputs[0].firstInput();
            const TouchInput& startFinger1 = _pinchInputs[1].firstInput();
            const glm::dvec2 startVec0 = glm::dvec2(startFinger0.x * aspectRatio, startFinger0.y);
            const glm::dvec2 startVec1 = glm::dvec2(startFinger1.x * aspectRatio, startFinger1.y);
            double distToCentroidStart = glm::length(startVec0 - startVec1) / 2.0;

            const TouchInput& endFinger0 = _pinchInputs[0].latestInput();
            const TouchInput& endFinger1 = _pinchInputs[1].latestInput();
            const glm::dvec2 endVec0 = glm::dvec2(endFinger0.x * aspectRatio, endFinger0.y);
            const glm::dvec2 endVec1 = glm::dvec2(endFinger1.x * aspectRatio, endFinger1.y);
            double distToCentroidEnd = glm::length(endVec0 - endVec1) / 2.0;

            double zoomFactor = distToCentroidEnd - distToCentroidStart;

            // @TODO: Update so zoom keeps going when fingers are held?
            // Now, no movement => interpreted as roll

            double scale = 100.0;
            updateVelocities.zoom = scale * zoomFactor * _sensitivity;

            break;
        }
        case InteractionType::ROLL: {
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
                float lastAngle = point.angleToPos(_centroid.x, _centroid.y);
                float currentAngle = touchPoint.latestInput().angleToPos(
                    _centroid.x,
                    _centroid.y
                );
                rollFactor += validAngleDiff(lastAngle, currentAngle);
            }
            double avgRollFactor = rollFactor / static_cast<double>(touchPoints.size());

            //double scale = 2.75; // From old sensitivity settings
            double scale = 100.0;
            updateVelocities.globalRoll = scale * -avgRollFactor * _sensitivity;
            // TODO: Local roll?

            break;
        }
        case InteractionType::PAN: {
            // Add local rotation velocity
            glm::dvec2 scale = glm::dvec2(10.0);

            // TODO: Update scale and gesture
            updateVelocities.localRotation = _sensitivity * glm::dvec2(
                currentInput.speedX(),
                currentInput.speedY()
            );

            break;
        }
        case InteractionType::NONE:
            break;
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
        // @TODO: Update code so that we dont have to do this check (currently there is a
        // risk of crashing below)
        return InteractionType::NONE;
    }

    // A one finger gesture is always interpreted as a rotation
    if (inputs.size() == 1) {
        return InteractionType::ROTATION;
    }

    // Handle multi-finger gestures

    // Compute centroid of current touch points
    glm::vec2 lastCentroid = _centroid;
    _centroid = glm::vec2(0.f, 0.f);
    for (const TouchInputHolder& inputHolder : inputs) {
        _centroid += glm::vec2(
            inputHolder.latestInput().x,
            inputHolder.latestInput().y
        );
    }
    _centroid /= static_cast<float>(inputs.size());

    // See if the distance between fingers changed - used in pan interpretation
    double dist = 0.0;
    double lastDist = 0.0;
    TouchInput distInput = inputs[0].latestInput();
    for (const TouchInputHolder& inputHolder : inputs) {
        const TouchInput& latestInput = inputHolder.latestInput();
        dist += glm::length(glm::dvec2(
            latestInput.x - distInput.x,
            latestInput.y - distInput.y
        ));
        distInput = latestInput;
    }
    distInput = lastProcessed[0];
    for (const TouchInput& p : lastProcessed) {
        lastDist += glm::length(glm::dvec2(
            p.x - distInput.x,
            p.y - distInput.y
        ));
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
        double diff = latestInput.x - prevInput.x + latestInput.y - prevInput.y;
        if (!inputHolder.isMoving()) {
            minDiff = 0.0;
        }
        else if (std::abs(diff) < std::abs(minDiff)) {
            minDiff = diff;
        }

        // Compute roll factor, as the difference in angle to centroid between frames
        float lastAngle = prevInput.angleToPos(_centroid.x, _centroid.y);
        float currentAngle =
            inputHolder.latestInput().angleToPos(_centroid.x, _centroid.y);

        double res = validAngleDiff(lastAngle, currentAngle);

        const float RollAngleThreshold = 0.025f;
        if (std::abs(res) < RollAngleThreshold) {
            rollFactor = 1000.0; // @TOD: Figure out this magic number
        }
        else {
            rollFactor += res;
        }
    }

    float normalizedCentroidDistance = glm::distance(_centroid, lastCentroid);
    normalizedCentroidDistance /= static_cast<float>(inputs.size());

    const float InputStillThreshold = 0.0005f;
    const float CentroidStillThreshold = 0.0018f;

    // We have roll if one finger is still, or the total roll angles around the
    // centroid is over RollAngleThreshold (CentroidStillThreshold is used to void
    // misinterpretations)
    if (std::abs(minDiff) < InputStillThreshold ||
        (std::abs(rollFactor) < 100.0 && normalizedCentroidDistance < CentroidStillThreshold))
    {
        return InteractionType::ROLL;
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
        return InteractionType::PINCH;
    }
}

void TouchCameraStates::resetAfterInput() {
    _pinchInputs[0].clearInputs();
    _pinchInputs[1].clearInputs();
}

} // namespace openspace::interaction
