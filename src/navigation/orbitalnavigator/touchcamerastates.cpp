/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

    // Code for lower-right corner double-tap to zoom-out
    // @TODO: Make a setting to enable/disable this feature, whihc would also communicate its existence
    if (touchState.isDoubleTap()) {
        const glm::vec2 res = global::windowDelegate->currentWindowSize();
        const glm::vec2 pos = touchPoints[0].latestInput().screenCoordinates(res);

        const float bottomCornerSizeForZoomTap_fraction = 0.08f;
        const int zoomTapThresholdX = static_cast<int>(
            res.x * (1.f - bottomCornerSizeForZoomTap_fraction)
        );
        const int zoomTapThresholdY = static_cast<int>(
            res.y * (1.f - bottomCornerSizeForZoomTap_fraction)
        );

        const bool isTapInLowerRightCorner =
            (std::abs(pos.x) > zoomTapThresholdX && std::abs(pos.y) > zoomTapThresholdY);

        if (isTapInLowerRightCorner) {
            _zoomOutTap = true;
        }
    }

    UpdateStates updateStates = computeVelocities(touchPoints, lastProcessed);
    updateVelocities(std::move(updateStates), deltaTime);

    resetAfterInput();
}

OrbitalCameraStates::UpdateStates
TouchCameraStates::computeVelocities(const std::vector<TouchInputHolder>& list,
                                     const std::vector<TouchInput>& lastProcessed)
{
    UpdateStates updateVelocities;

    if (list.empty()) {
        return updateVelocities;
    }

    const InteractionType action = interpretInteraction(list, lastProcessed);

#ifdef TOUCH_DEBUG_MODE
    const std::map<InteractionType, std::string> interactionNames = {
        { InteractionType::ROTATION, "Rotation" },
        { InteractionType::PINCH, "Pinch" },
        { InteractionType::PAN, "Pan" },
        { InteractionType::ROLL, "Roll" },
        { InteractionType::ZOOM_OUT, "Zoom out" },
        { InteractionType::NONE, "None" }
    };
    LDEBUG(std::format("Interpreted interaction: {}", interactionNames.at(action)));
#endif // TOUCH_DEBUG_MODE

    const TouchInputHolder& inputHolder = list.at(0);
    const glm::ivec2 windowSize = global::windowDelegate->currentWindowSize();
    const float aspectRatio =
        static_cast<float>(windowSize.x) / static_cast<float>(windowSize.y);

    switch (action) {
    case InteractionType::ROTATION: {
        // Add rotation velocity
        //glm::dvec2 scale = glm::dvec2(0.08, 0.045); // From old sensitivity settings
        glm::dvec2 scale = glm::dvec2(10.0);
        updateVelocities.globalRotation = scale * _sensitivity * -glm::dvec2(
            inputHolder.speedX(),
            inputHolder.speedY()
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

        double scale = 11.0; // From old sensitivity settings
        updateVelocities.zoom = scale * zoomFactor * _sensitivity;

        break;
    }
    case InteractionType::ROLL: {
        // Add global roll rotation velocity
        double rollFactor = std::accumulate(
            list.begin(),
            list.end(),
            0.0,
            [this, &lastProcessed](double diff, const TouchInputHolder& holder) {
                TouchInput point = *std::find_if(
                    lastProcessed.begin(),
                    lastProcessed.end(),
                    [&holder](const TouchInput& input) {
                        return holder.holdsInput(input);
                    }
                );
                double res = diff;
                float lastAngle = point.angleToPos(_centroid.x, _centroid.y);
                float currentAngle = holder.latestInput().angleToPos(
                    _centroid.x,
                    _centroid.y
                );

                // TODO: Simplify this computation

                // Ifs used to set angles 359 + 1 = 0 and 0 - 1 = 359
                if (lastAngle > currentAngle + 1.5 * glm::pi<float>()) {
                    res += currentAngle + (2 * glm::pi<float>() - lastAngle);
                }
                else if (currentAngle > lastAngle + 1.5 * glm::pi<float>()) {
                    res += (2 * glm::pi<float>() - currentAngle) + lastAngle;
                }
                else {
                    res += currentAngle - lastAngle;
                }
                return res;
            }
        ) / list.size();

        //double scale = 2.75; // From old sensitivity settings
        double scale = 16.0;
        updateVelocities.globalRoll = scale * -rollFactor * _sensitivity;
        // TODO: Local roll?

        break;
    }
    case InteractionType::PAN: {
        // Add local rotation velocity
        //glm::dvec2 scale = glm::dvec2(0.08, 0.045); // From old sensitivity settings
        glm::dvec2 scale = glm::dvec2(10.0);
        // TODO: Update scale and gesture
        updateVelocities.localRotation = _sensitivity * glm::dvec2(
            inputHolder.speedX(),
            inputHolder.speedY()
        );

        break;
    }
    case InteractionType::ZOOM_OUT: {
        // Instanteneous zoom out from current if double tap occurred in bottom right
        // corner
        double scale = 100.0; // Impulse! => high value
        updateVelocities.zoom = -scale * _sensitivity;
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

    if (_zoomOutTap) {
        return InteractionType::ZOOM_OUT;
    }

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
    double dist = 0;
    double lastDist = 0;
    TouchInput distInput = inputs[0].latestInput();
    for (const TouchInputHolder& inputHolder : inputs) {
        const TouchInput& latestInput = inputHolder.latestInput();
        dist += glm::length(
            glm::dvec2(latestInput.x, latestInput.y) -
            glm::dvec2(distInput.x, distInput.y)
        );
        distInput = latestInput;
    }
    distInput = lastProcessed[0];
    for (const TouchInput& p : lastProcessed) {
        lastDist += glm::length(glm::dvec2(p.x, p.y) -
            glm::dvec2(distInput.x, distInput.y));
        distInput = p;
    }
    // Find the slowest moving finger - used in roll interpretation
    double minDiff = 1000.0;
    for (const TouchInputHolder& inputHolder : inputs) {
        const auto it = std::find_if(
            lastProcessed.cbegin(),
            lastProcessed.cend(),
            [&inputHolder](const TouchInput& input) {
                return inputHolder.holdsInput(input);
            });

        if (it == lastProcessed.cend()) {
            continue;
        }
        const TouchInput& latestInput = inputHolder.latestInput();
        const TouchInput& prevInput = *it;

        double diff = latestInput.x - prevInput.x + latestInput.y - prevInput.y;

        if (!inputHolder.isMoving()) {
            minDiff = 0.0;
        }
        else if (std::abs(diff) < std::abs(minDiff)) {
            minDiff = diff;
        }
    }

    // Find if all fingers angles are high - used in roll interpretation
    double rollOn = std::accumulate(
        inputs.begin(),
        inputs.end(),
        0.0,
        [this, &lastProcessed](double diff, const TouchInputHolder& inputHolder) {
            const TouchInput& lastPoint = *std::find_if(
                lastProcessed.begin(),
                lastProcessed.end(),
                [&inputHolder](const TouchInput& input) {
                    return inputHolder.holdsInput(input);
                }
            );

            double res = 0.0;
            float lastAngle = lastPoint.angleToPos(_centroid.x, _centroid.y);
            float currentAngle =
                inputHolder.latestInput().angleToPos(_centroid.x, _centroid.y);

            // TODO: Simplify this computation
            if (lastAngle > currentAngle + 1.5f * glm::pi<float>()) {
                res = currentAngle + (2.f * glm::pi<float>() - lastAngle);
            }
            else if (currentAngle > lastAngle + 1.5f * glm::pi<float>()) {
                res = (2.f * glm::pi<float>() - currentAngle) + lastAngle;
            }
            else {
                res = currentAngle - lastAngle;
            }

            const float RollAngleThreshold = 0.025f;
            if (std::abs(res) < RollAngleThreshold) {
                return 1000.0;
            }
            else {
                return (diff + res);
            }
        }
    );

    double normalizedCentroidDistance = glm::distance(
        _centroid,
        lastCentroid
    ) / inputs.size();

    const float InterpretPanDistance = 0.015f;
    const float InputStillThreshold = 0.0005f;
    const float CentroidStillThreshold = 0.0018f;

    float avgDistance = static_cast<float>(std::abs(dist - lastDist));
    // If average distance between 3 fingers are constent we have panning
    if (avgDistance < InterpretPanDistance && inputs.size() == 3) {
        return InteractionType::PAN;
    }

    // We have roll if one finger is still, or the total roll angles around the
    // centroid is over RollAngleThreshold (CentroidStillThreshold is used to void
    // misinterpretations)
    if (std::abs(minDiff) < InputStillThreshold ||
        (std::abs(rollOn) < 100.0 && normalizedCentroidDistance < CentroidStillThreshold))
    {
        return InteractionType::ROLL;
    }
    else {
        const bool sameInput0 = _pinchInputs[0].holdsInput(inputs[0].latestInput());
        const bool sameInput1 = _pinchInputs[1].holdsInput(inputs[1].latestInput());
        if (sameInput0 && sameInput1) {
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
    _zoomOutTap = false;
}

} // namespace openspace::interaction
