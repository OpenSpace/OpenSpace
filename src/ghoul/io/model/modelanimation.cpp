/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#include <ghoul/io/model/modelanimation.h>

#include <glm/gtx/quaternion.hpp>
#include <limits>
#include <utility>

namespace ghoul::io {

ModelAnimation::ModelAnimation(std::string name, double duration)
    : _name(std::move(name))
    , _duration(duration)
{}

void ModelAnimation::setTimeScale(float timeScale) {
    const double duration = _duration / _timeScale;
    _timeScale = timeScale;
    _duration = duration * _timeScale;
}

void ModelAnimation::animate(std::vector<ModelNode>& nodes, double now, bool enabled) {
    // Animation out of scope or disabled
    if (!enabled || now > _duration || now < 0) {
        if (_wasActive) {
            reset(nodes);
            _wasActive = false;
        }
        return;
    }
    _wasActive = true;

    // Find keyframes
    for (const ModelAnimation::NodeAnimation& nodeAnimation : _nodeAnimations) {
        // Position
        glm::vec3 currPos = glm::vec3(0.f);
        if (nodeAnimation.positions.size() > 1) {
            double prevPosTime = -std::numeric_limits<double>::max();
            glm::vec3 prevPos = glm::vec3(0.f);
            double nextPosTime = std::numeric_limits<double>::max();
            glm::vec3 nextPos = glm::vec3(0.f);
            bool interpolate = true;

            for (const ModelAnimation::PositionKeyframe& kf : nodeAnimation.positions) {
                const double diff = (kf.time * _timeScale) - now;

                // Exact on a keyframe
                if (std::abs(diff) < std::numeric_limits<double>::epsilon()) {
                    currPos = kf.position;
                    interpolate = false;
                }
                // Previous keyframe
                else if (diff < 0.0 && diff > (prevPosTime - now)) {
                    prevPosTime = kf.time * _timeScale;
                    prevPos = kf.position;
                }
                // Next keyframe
                else if (diff > 0.0 && diff < (nextPosTime - now)) {
                    nextPosTime = kf.time * _timeScale;
                    nextPos = kf.position;
                }
            }

            if (interpolate) {
                const double blend = (now - prevPosTime) / (nextPosTime - prevPosTime);
                currPos = glm::mix(prevPos, nextPos, static_cast<float>(blend));
            }
        }
        else if (!nodeAnimation.positions.empty()) {
            currPos = nodeAnimation.positions[0].position;
        }
        else {
            currPos = glm::vec3(0.f);
        }

        // Rotation
        glm::quat currRot = glm::quat(1.f, 0.f, 0.f, 0.f);
        if (nodeAnimation.rotations.size() > 1) {
            double prevRotTime = -std::numeric_limits<double>::max();
            glm::quat prevRot = glm::quat(1.f, 0.f, 0.f, 0.f);
            double nextRotTime = std::numeric_limits<double>::max();
            glm::quat nextRot = glm::quat(1.f, 0.f, 0.f, 0.f);
            bool interpolate = true;

            for (const ModelAnimation::RotationKeyframe& kf : nodeAnimation.rotations) {
                const double diff = (kf.time * _timeScale) - now;

                // Exact on a keyframe
                if (std::abs(diff) < std::numeric_limits<double>::epsilon()) {
                    currRot = kf.rotation;
                    interpolate = false;
                }
                // Previous keyframe
                else if (diff < 0.0 && diff > (prevRotTime - now)) {
                    prevRotTime = kf.time * _timeScale;
                    prevRot = kf.rotation;
                }
                // Next keyframe
                else if (diff > 0.0 && diff < (nextRotTime - now)) {
                    nextRotTime = kf.time * _timeScale;
                    nextRot = kf.rotation;
                }
            }

            if (interpolate) {
                const double blend = (now - prevRotTime) / (nextRotTime - prevRotTime);
                currRot = glm::slerp(prevRot, nextRot, static_cast<float>(blend));
            }
        }
        else if (!nodeAnimation.rotations.empty()) {
            currRot = nodeAnimation.rotations[0].rotation;
        }
        else {
            currRot = glm::quat(0.f, 0.f, 0.f, 0.f);
        }

        // Scale
        glm::vec3 currScale = glm::vec3(0.f);
        if (nodeAnimation.scales.size() > 1) {
            double prevScaleTime = -std::numeric_limits<double>::max();
            glm::vec3 prevScale = glm::vec3(0.f);
            double nextScaleTime = std::numeric_limits<double>::max();
            glm::vec3 nextScale = glm::vec3(0.f);
            bool interpolate = true;

            for (const ModelAnimation::ScaleKeyframe& scale : nodeAnimation.scales) {
                const double diff = (scale.time * _timeScale) - now;

                // Exact on a keyframe
                if (std::abs(diff) < std::numeric_limits<double>::epsilon()) {
                    currScale = scale.scale;
                    interpolate = false;
                }
                // Previous keyframe
                else if (diff < 0.0 && diff > (prevScaleTime - now)) {
                    prevScaleTime = scale.time * _timeScale;
                    prevScale = scale.scale;
                }
                // Next keyframe
                else if (diff > 0.0 && diff < (nextScaleTime - now)) {
                    nextScaleTime = scale.time * _timeScale;
                    nextScale = scale.scale;
                }
            }

            if (interpolate) {
                const double blend =
                    (now - prevScaleTime) / (nextScaleTime - prevScaleTime);
                currScale = glm::mix(prevScale, nextScale, static_cast<float>(blend));
            }
        }
        else if (!nodeAnimation.scales.empty()) {
            currScale = nodeAnimation.scales[0].scale;
        }
        else {
            currScale = glm::vec3(1.f);
        }

        glm::mat4 animationTransform = glm::mat4(1.f);
        animationTransform =
            glm::translate(glm::mat4(1.f), currPos) *
            glm::toMat4(currRot) *
            glm::scale(animationTransform, currScale);

        nodes[nodeAnimation.node].setAnimation(animationTransform);
    }
}

void ModelAnimation::reset(std::vector<ModelNode>& nodes) {
    for (const ModelAnimation::NodeAnimation& nodeAnimation : _nodeAnimations) {
        nodes[nodeAnimation.node].setAnimation(nodes[nodeAnimation.node].transform());
    }
}

std::vector<ModelAnimation::NodeAnimation>& ModelAnimation::nodeAnimations() {
    return _nodeAnimations;
}

const std::vector<ModelAnimation::NodeAnimation>& ModelAnimation::nodeAnimations() const {
    return _nodeAnimations;
}

std::string ModelAnimation::name() const {
    return _name;
}

double ModelAnimation::duration() const {
    return _duration;
}

float ModelAnimation::timeScale() const {
    return _timeScale;
}

} // namespace ghoul::io
