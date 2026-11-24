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

#include <openspace/io/model/modelanimation.h>

#include <openspace/io/model/modelnode.h>
#include <openspace/logging/logmanager.h>
#include <glm/gtx/quaternion.hpp>
#include <limits>
#include <utility>

namespace openspace::io {

ModelAnimation::ModelAnimation(std::string name, double duration)
    : _name(std::move(name))
    , _duration(duration)
{
    _finalBoneTransforms = std::vector<glm::mat4>(100, glm::mat4(1.f));
}

void ModelAnimation::setTimeScale(float timeScale) {
    const double duration = _duration / _timeScale;
    _timeScale = timeScale;
    _duration = duration * _timeScale;
}

void ModelAnimation::animate(double now, bool enabled) {
    // Animation out of scope or disabled
    if (!enabled || now > _duration || now < 0) {
        if (_wasActive) {
            reset();
            _wasActive = false;
        }
        return;
    }
    _wasActive = true;

    if (_mode == AnimationMode::Node) {
        for (ModelAnimation::NodeAnimation& nodeAnimation : _nodeAnimations) {
            glm::mat4 transform = computeCurrentTransform(
                now,
                nodeAnimation.positions,
                nodeAnimation.rotations,
                nodeAnimation.scales
            );
            nodeAnimation.currentTransform = std::move(transform);
        }
    }
    else if (_mode == AnimationMode::Bone) {
        for (ModelAnimation::BoneAnimation& boneAnimation : _boneAnimations) {
            glm::mat4 transform = computeCurrentTransform(
                now,
                boneAnimation.positions,
                boneAnimation.rotations,
                boneAnimation.scales
            );
            boneAnimation.currentBoneTransform = std::move(transform);
        }
    }
}

void ModelAnimation::reset() {
    for (ModelAnimation::NodeAnimation& nodeAnimation : _nodeAnimations) {
        nodeAnimation.currentTransform = glm::mat4(1.f);
    }

    for (ModelAnimation::BoneAnimation& boneAnimation : _boneAnimations) {
        boneAnimation.currentBoneTransform = glm::mat4(1.f);
    }
}

ModelAnimation::AnimationMode ModelAnimation::mode() const {
    return _mode;
}

void ModelAnimation::setMode(AnimationMode mode) {
    _mode = mode;
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

std::vector<ModelAnimation::NodeAnimation>& ModelAnimation::nodeAnimations() {
    return _nodeAnimations;
}

const std::vector<ModelAnimation::NodeAnimation>& ModelAnimation::nodeAnimations() const {
    return _nodeAnimations;
}

std::vector<ModelAnimation::BoneAnimation>& ModelAnimation::boneAnimations() {
    return _boneAnimations;
}

const std::vector<ModelAnimation::BoneAnimation>&ModelAnimation::boneAnimations() const {
    return _boneAnimations;
}

ModelAnimation::BoneAnimation* ModelAnimation::findBoneAnimation(
                                                                std::string_view boneName)
{
    for (ModelAnimation::BoneAnimation& boneAnimation : _boneAnimations) {
        if (boneAnimation.boneName == boneName) {
            return &boneAnimation;
        }
    }
    return nullptr;
}

const ModelAnimation::BoneAnimation* ModelAnimation::findBoneAnimation(
                                                          std::string_view boneName) const
{
    for (const ModelAnimation::BoneAnimation& boneAnimation : _boneAnimations) {
        if (boneAnimation.boneName == boneName) {
            return &boneAnimation;
        }
    }
    return nullptr;
}

void ModelAnimation::setBoneMap(std::map<std::string, io::ModelMesh::Bone*> boneMap) {
    _boneMap = boneMap;
}

void ModelAnimation::computeFinalBoneTransforms(const std::vector<ModelNode>& nodes) {
    if (_mode != AnimationMode::Bone || nodes.empty() || _boneMap.empty()) {
        return;
    }

    computeFinalBoneTransformsRecursive(nodes, nodes[0], glm::mat4(1.f));
}

const std::vector<glm::mat4>& ModelAnimation::finalBoneTransforms() const {
    return _finalBoneTransforms;
}

std::vector<glm::mat4>& ModelAnimation::finalBoneTransforms() {
    return _finalBoneTransforms;
}

glm::mat4 ModelAnimation::computeCurrentTransform(double now,
                                           const std::vector<PositionKeyframe>& positions,
                                           const std::vector<RotationKeyframe>& rotations,
                                           const std::vector<ScaleKeyframe>& scales) const
{
    // Position
    glm::vec3 currPos = glm::vec3(0.f);
    if (positions.size() > 1) {
        double prevPosTime = -std::numeric_limits<double>::max();
        glm::vec3 prevPos = glm::vec3(0.f);
        double nextPosTime = std::numeric_limits<double>::max();
        glm::vec3 nextPos = glm::vec3(0.f);
        bool interpolate = true;

        for (const ModelAnimation::PositionKeyframe& kf : positions) {
            const double diff = (kf.time * _timeScale) - now;

            // Exact on a keyframe
            if (std::abs(diff) < std::numeric_limits<double>::epsilon()) {
                currPos = kf.position;
                interpolate = false;
            }
            // Previous keyframe
            else if (diff < 0.0 && diff >(prevPosTime - now)) {
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
    else if (!positions.empty()) {
        currPos = positions[0].position;
    }
    else {
        currPos = glm::vec3(0.f);
    }

    // Rotation
    glm::quat currRot = glm::quat(1.f, 0.f, 0.f, 0.f);
    if (rotations.size() > 1) {
        double prevRotTime = -std::numeric_limits<double>::max();
        glm::quat prevRot = glm::quat(1.f, 0.f, 0.f, 0.f);
        double nextRotTime = std::numeric_limits<double>::max();
        glm::quat nextRot = glm::quat(1.f, 0.f, 0.f, 0.f);
        bool interpolate = true;

        for (const ModelAnimation::RotationKeyframe& kf : rotations) {
            const double diff = (kf.time * _timeScale) - now;

            // Exact on a keyframe
            if (std::abs(diff) < std::numeric_limits<double>::epsilon()) {
                currRot = kf.rotation;
                interpolate = false;
            }
            // Previous keyframe
            else if (diff < 0.0 && diff >(prevRotTime - now)) {
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
    else if (!rotations.empty()) {
        currRot = rotations[0].rotation;
    }
    else {
        currRot = glm::quat(0.f, 0.f, 0.f, 0.f);
    }

    // Scale
    glm::vec3 currScale = glm::vec3(0.f);
    if (scales.size() > 1) {
        double prevScaleTime = -std::numeric_limits<double>::max();
        glm::vec3 prevScale = glm::vec3(0.f);
        double nextScaleTime = std::numeric_limits<double>::max();
        glm::vec3 nextScale = glm::vec3(0.f);
        bool interpolate = true;

        for (const ModelAnimation::ScaleKeyframe& scale : scales) {
            const double diff = (scale.time * _timeScale) - now;

            // Exact on a keyframe
            if (std::abs(diff) < std::numeric_limits<double>::epsilon()) {
                currScale = scale.scale;
                interpolate = false;
            }
            // Previous keyframe
            else if (diff < 0.0 && diff >(prevScaleTime - now)) {
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
    else if (!scales.empty()) {
        currScale = scales[0].scale;
    }
    else {
        currScale = glm::vec3(1.f);
    }

    glm::mat4 animationTransform = glm::mat4(1.f);
    animationTransform =
        glm::translate(glm::mat4(1.f), currPos) *
        glm::toMat4(currRot) *
        glm::scale(animationTransform, currScale);

    return animationTransform;
}

void ModelAnimation::computeFinalBoneTransformsRecursive(
                                                      const std::vector<ModelNode>& nodes,
                                                                    const ModelNode& node,
                                                           glm::mat4 parentTransformation)
{
    // TODO: Include the bone offset matrix here so we do not need to apply that later
    const ModelAnimation::BoneAnimation* boneAnimation = findBoneAnimation(node.name());
    glm::mat4 finalTransform = parentTransformation * node.transform();

    if (boneAnimation && boneAnimation->boneIndex >= 0 && boneAnimation->boneIndex < 100) {
        // NOTE: Parents are needed here
        finalTransform = parentTransformation * boneAnimation->currentBoneTransform;

        io::ModelMesh::Bone*  bone = _boneMap.at(boneAnimation->boneName);
        if (!bone) {
            LERRORC("ModelAnimation", std::format(
                "BoneAnimation with bone '{}' does not refer to any bone in the mesh",
                boneAnimation->boneName
            ));
            _finalBoneTransforms[boneAnimation->boneIndex] = parentTransformation * node.transform();
        }
        else {
            // NOTE: no transposing bone->boneTransform
            _finalBoneTransforms[boneAnimation->boneIndex] = finalTransform * bone->boneTransform;
        }
    }

    for (int child : node.children()) {
        computeFinalBoneTransformsRecursive(
            nodes,
            nodes[child],
            finalTransform
        );
    }
}

} // namespace openspace::io
