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
    using namespace openspace;
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

    glm::dvec2 normalizeByAspectRatio(glm::dvec2 vec) {
        const glm::vec2 windowSize =
            glm::vec2(global::windowDelegate->currentWindowSize());

        const float aspectRatio = windowSize.x / windowSize.y;
        return glm::dvec2(vec.x * aspectRatio, vec.y);
    };

    double computeRollFactor(const glm::dvec2& centroid,
                             const std::vector<TouchInputHolder>& touchPoints,
                             const std::vector<TouchInput>& lastProcessed)
    {
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

            const TouchInput& lastTouchPoint = *it;

            float firstAngle = lastTouchPoint.angleToPos(centroid);
            float currentAngle = touchPoint.latestInput().angleToPos(centroid);
            rollFactor += validAngleDiff(firstAngle, currentAngle);
        }
        return rollFactor;
     };
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

    const TouchInputHolder& currentInput = touchPoints[0];

    const TouchInput& startFinger0 = touchPoints[0].firstInput();
    const TouchInput& endFinger0 = touchPoints[0].latestInput();

    switch (action) {
        case InteractionType::Rotation: {
            // Add rotation velocity
            glm::dvec2 diff = normalizeByAspectRatio(glm::dvec2(
                endFinger0.pos - startFinger0.pos
            ));
            constexpr glm::dvec2 Scale = glm::dvec2(3.0);
            updateVelocities.globalRotation = _sensitivity * Scale * -diff;
            break;
        }
        case InteractionType::Pinch: {
            // Add zooming velocity
            ghoul_assert(touchPoints.size() > 1, "Pinch needs at least two touch points");

            const TouchInput& startFinger1 = touchPoints[1].firstInput();
            const TouchInput& endFinger1 = touchPoints[1].latestInput();

            glm::dvec2 diffStart = normalizeByAspectRatio(glm::dvec2(
                startFinger0.pos - startFinger1.pos
            ));
            glm::dvec2 diffEnd = normalizeByAspectRatio(glm::dvec2(
                endFinger0.pos - endFinger1.pos
            ));
            double zoomFactor = glm::length(diffEnd) - glm::length(diffStart);

            constexpr double ZoomScale = 5.0;
            updateVelocities.zoom = _sensitivity * ZoomScale * zoomFactor;

            // Also apply rolling of fingers as part of the zooming gesture, so we can
            // orient the view while zooming
            double rollFactor = computeRollFactor(_centroid, touchPoints, lastProcessed);
            constexpr double RollScale = 50.0;
            updateVelocities.globalRoll = _sensitivity * RollScale * -rollFactor;
            break;
        }
        case InteractionType::Roll: {
            // Apply global rotation velocity
            // Note that whe a pure roll gesture was detected, we don't want to zoom
            double rollFactor = computeRollFactor(_centroid, touchPoints, lastProcessed);
            constexpr double Scale = 50.0;
            updateVelocities.globalRoll = _sensitivity * Scale * -rollFactor;
            break;
        }
        case InteractionType::Pan: {
            // Add local rotation velocity
            ghoul_assert(touchPoints.size() > 1, "Pan needs at least two touch points");

            // Here we don't use the difference between the first and current position,
            // but instead the speed of the movement, to allow for more responsive panning
            glm::dvec2 speed = normalizeByAspectRatio(glm::dvec2(
                currentInput.speedX(),
                currentInput.speedY()
            ));

            constexpr glm::dvec2 Scale = glm::dvec2(0.8);
            updateVelocities.localRotation = _sensitivity * Scale * speed;
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

    // Two fingers: Pinch and pan
    if (inputs.size() == 2) {
        // Compare movement direction of first two fingers
        const TouchInput& current0 = inputs[0].latestInput();
        const TouchInput& current1 = inputs[1].latestInput();

        const TouchInput& first0 = inputs[0].firstInput();
        const TouchInput& first1 = inputs[1].firstInput();

        glm::vec2 dir0 = current0.pos - first0.pos;
        glm::vec2 dir1 = current1.pos - first1.pos;

        // Check distance between the fingers
        float dist = glm::length(current1.pos - current0.pos);
        float distStart = glm::length(first1.pos - first0.pos);
        float distanceDiff = std::abs(dist - distStart);

        // Check that both fingers are moving - otherwise, dot products would be nan
        constexpr float MoveEpsilon = 0.01f;
        bool isFingerMoving0 = glm::length(dir0) > MoveEpsilon;
        bool isFingerMoving1 = glm::length(dir1) > MoveEpsilon;
        bool areBothFingersMoving = isFingerMoving0 && isFingerMoving1;

        // Pinch: movement in opposite directions (checked against the centroid).

        // Normalize directions relative to centroid
        glm::vec2 toCentroid0 = glm::normalize(_centroid - current0.pos);
        glm::vec2 toCentroid1 = glm::normalize(_centroid - current1.pos);

        float dot0 = isFingerMoving0 ? glm::dot(glm::normalize(dir0), toCentroid0) : 0.f;
        float dot1 = isFingerMoving1 ? glm::dot(glm::normalize(dir1), toCentroid1) : 0.f;

        // Check if both moving toward or away from centroid (opposite directions)
        const float OppositeThreshold = 0.3f;
        if ((dot0 > OppositeThreshold && dot1 > OppositeThreshold) ||
            (dot0 < -OppositeThreshold && dot1 < -OppositeThreshold))
        {
            return InteractionType::Pinch;
        }

        // Pan: movement in same direction

        // Check if the vectors point in the same direction, by simply checking the dot
        // product between the direction vectors (dot product is 1 => exactly the same)
        constexpr float SameThreshold = 0.3f;
        constexpr float InterpretPanDistance = 0.2f;

        float dot = glm::dot(glm::normalize(dir0), glm::normalize(dir1));

        if (areBothFingersMoving && distanceDiff < InterpretPanDistance &&
            std::abs(dot - 1.f) < SameThreshold)
        {
            return InteractionType::Pan;
        }
    }

    // Rolling may use two fingers or more

    // We have roll if one finger is still, or the total roll angles around the centroid
    // is over a given threshold (all fingers are moving)

    // 1) Find the slowest moving finger - to find potentially still fingers
    double minDiff = 1000.0;
    for (const TouchInputHolder& inputHolder : inputs) {
        if (!inputHolder.isMoving()) {
            minDiff = 0.0;
            break; // Found a still finger, no need to continue
        }

        const auto it = std::find_if(
            lastProcessed.cbegin(),
            lastProcessed.cend(),
            [&inputHolder](const TouchInput& input) {
                return inputHolder.holdsInput(input);
            }
        );

        if (it != lastProcessed.cend()) {
            const glm::vec2 d = inputHolder.latestInput().pos - it->pos;
            minDiff = std::min(minDiff, static_cast<double>(std::abs(d.x + d.y)));
        }
    }

    const float InputStillThreshold = 0.0005f;
    bool hasStillFinger = std::abs(minDiff) < InputStillThreshold;
    if (hasStillFinger) {
        return InteractionType::Roll;
    }


    // 2: All fingers are rotating around a still centroid
    constexpr float CentroidStillThreshold = 0.0018f;
    float normalizedCentroidDist = glm::distance(_centroid, lastCentroid);
    normalizedCentroidDist /= static_cast<float>(inputs.size());
    bool isCentroidStill = normalizedCentroidDist < CentroidStillThreshold;

    bool allFingersRotating = true;
    constexpr float RollAngleThreshold = 0.025f;

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

        float lastAngle = it->angleToPos(_centroid);
        float currentAngle = inputHolder.latestInput().angleToPos(_centroid);
        double angleDiff = validAngleDiff(lastAngle, currentAngle);

        if (std::abs(angleDiff) < RollAngleThreshold) {
            // Found a finger that is not rolling, no need to continue
            allFingersRotating = false;
            break;
        }
    }

    if (allFingersRotating && isCentroidStill) {
        return InteractionType::Roll;
    }

    return InteractionType::None;
}

} // namespace openspace
