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
{}

TouchCameraStates::~TouchCameraStates() {}

void TouchCameraStates::updateVelocitiesFromInput(const TouchInputState& touchState,
                                                  double deltaTime)
{
    const std::vector<TouchInputHolder>& touchPoints = touchState.touchPoints();
    const std::vector<TouchInput>& lastProcessed = touchState.lastProcessedInputs();

    UpdateStates updateStates = computeVelocities(touchPoints, lastProcessed);
    updateVelocities(std::move(updateStates), deltaTime);
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
            glm::dvec2 diff = currentInput.latestInput().pos - currentInput.firstInput().pos;
            constexpr glm::dvec2 Scale = glm::dvec2(5.0);
            updateVelocities.globalRotation = Scale * _sensitivity * -diff;
            break;
        }
        case InteractionType::Pinch: {
            // Add zooming velocity - dependent on distance difference between contact
            // points this/first frame
            const TouchInput& startFinger0 = touchPoints[0].firstInput();
            const TouchInput& startFinger1 = touchPoints[1].firstInput();
            const glm::dvec2 startVec0 = glm::dvec2(
                startFinger0.pos.x * aspectRatio,
                startFinger0.pos.y
            );
            const glm::dvec2 startVec1 = glm::dvec2(
                startFinger1.pos.x * aspectRatio,
                startFinger1.pos.y
            );
            double distToCentroidStart = glm::length(startVec0 - startVec1) / 2.0;

            const TouchInput& endFinger0 = touchPoints[0].latestInput();
            const TouchInput& endFinger1 = touchPoints[1].latestInput();
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
                const auto it = std::find_if(
                    lastProcessed.begin(),
                    lastProcessed.end(),
                    [&touchPoint](const TouchInput& input) {
                        return touchPoint.holdsInput(input);
                    }
                );

                if (it == lastProcessed.end()) {
                    continue;
                }

                const TouchInput& point = *it;
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
            const glm::dvec2 Scale = glm::dvec2(3.0);

            updateVelocities.localRotation = _sensitivity * Scale * glm::dvec2(
                currentInput.speedX(),
                currentInput.speedY()
            );

            break;
        }
        case InteractionType::None:
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
        // ideally we should instead change the code below so this check is not needed
        return InteractionType::None;
    }

    // A one-finger gesture is always interpreted as a rotation
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
    float avgDistance = static_cast<float>(std::abs(dist - lastDist));


    // Compare movement direction of first two fingers (pinch and pan)
    const TouchInput& current0 = inputs[0].latestInput();
    const TouchInput& current1 = inputs[1].latestInput();

    const TouchInput& first0 = inputs[0].firstInput();
    const TouchInput& first1 = inputs[1].firstInput();

    glm::vec2 dir0 = current0.pos - first0.pos;
    glm::vec2 dir1 = current1.pos - first1.pos;

    // Check for pinch gesture - detect movement in opposite directions
    if (inputs.size() == 2) {
        // Normalize directions relative to centroid
        glm::vec2 toCentroid0 = glm::normalize(_centroid - current0.pos);
        glm::vec2 toCentroid1 = glm::normalize(_centroid - current1.pos);

        float dot0 = glm::dot(glm::normalize(dir0), toCentroid0);
        float dot1 = glm::dot(glm::normalize(dir1), toCentroid1);

        // Check if both moving toward or away from centroid (opposite directions)
        const float OppositeThreshold = 0.3f;
        if ((dot0 > OppositeThreshold && dot1 > OppositeThreshold) ||
            (dot0 < -OppositeThreshold && dot1 < -OppositeThreshold))
        {
            return InteractionType::Pinch;
        }

        // Check if the vectors point in the same direction, by simply checking the dot
        // product between the direction vectors (dot product is 1 => exactly the same)
        const float SameThreshold = 0.3f;
        const float InterpretPanDistance = 0.2f;

        float dot = glm::dot(glm::normalize(dir0), glm::normalize(dir1));
        if (avgDistance < InterpretPanDistance && std::abs(dot - 1.f) < SameThreshold) {
            return InteractionType::Pan;
        }
    }

    float normalizedCentroidDist = glm::distance(_centroid, lastCentroid);
    normalizedCentroidDist /= static_cast<float>(inputs.size());





    // Find if all fingers angles are high - used in roll interpretation
    double rollFactor = 0.0;

    // Find the slowest moving finger - used in roll interpretation
    double minDiff = 1000.0;

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



    // We have roll if one finger is still, or the total roll angles around the centroid
    // is over RollAngleThreshold (CentroidStillThreshold is used to avoid
    // misinterpretations)
    const float InputStillThreshold = 0.0005f;
    const float CentroidStillThreshold = 0.0018f;
    if (std::abs(minDiff) < InputStillThreshold ||
        (std::abs(rollFactor) < 100.0 && normalizedCentroidDist < CentroidStillThreshold))
    {
        return InteractionType::Roll;
    }

    return InteractionType::None;
}

} // namespace openspace
