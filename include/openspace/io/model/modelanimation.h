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

#ifndef __OPENSPACE_CORE___MODELANIMATION___H__
#define __OPENSPACE_CORE___MODELANIMATION___H__

#include <openspace/glm.h>
#include <openspace/io/model/modelmesh.h>
#include <string>
#include <vector>

namespace openspace::io {

class ModelNode;

class ModelAnimation {
public:
    enum class AnimationMode {
        Node = 0,
        Bone
    };

    struct PositionKeyframe {
        glm::vec3 position = glm::vec3(0.f);
        double time = 0.0;
    };

    struct RotationKeyframe {
        glm::quat rotation = glm::quat(1.f, 0.f, 0.f, 0.f);
        double time = 0.0;
    };

    struct ScaleKeyframe {
        glm::vec3 scale = glm::vec3(0.f);
        double time = 0.0;
    };

    struct NodeAnimation {
        int node = -1;
        std::vector<PositionKeyframe> positions;
        std::vector<RotationKeyframe> rotations;
        std::vector<ScaleKeyframe> scales;
        glm::mat4 currentTransform = glm::mat4(1.f);
    };

    struct BoneAnimation {
        std::string boneName;
        int boneIndex = -1;
        std::vector<PositionKeyframe> positions;
        std::vector<RotationKeyframe> rotations;
        std::vector<ScaleKeyframe> scales;
        glm::mat4 currentBoneTransform = glm::mat4(1.f);
    };

    ModelAnimation(std::string name, double duration);

    ModelAnimation(ModelAnimation&&) noexcept = default;
    ~ModelAnimation() noexcept = default;

    void setTimeScale(float timeScale);
    void animate(double now, bool enabled);
    void reset();

    AnimationMode mode() const;
    void setMode(AnimationMode mode);
    std::string name() const;
    double duration() const;
    float timeScale() const;

    std::vector<NodeAnimation>& nodeAnimations();
    const std::vector<NodeAnimation>& nodeAnimations() const;

    std::vector<BoneAnimation>& boneAnimations();
    const std::vector<BoneAnimation>&boneAnimations() const;
    BoneAnimation* findBoneAnimation(std::string_view boneName);
    const BoneAnimation* findBoneAnimation(std::string_view boneName) const;
    void setBoneMap(std::map<std::string, io::ModelMesh::Bone*> boneMap);
    void computeFinalBoneTransforms(const std::vector<ModelNode>& nodes);
    const std::vector<glm::mat4>& finalBoneTransforms() const;
    std::vector<glm::mat4>& finalBoneTransforms();

private:
    glm::mat4 computeCurrentTransform(double now,
        const std::vector<PositionKeyframe>& positions,
        const std::vector<RotationKeyframe>& rotations,
        const std::vector<ScaleKeyframe>& scales) const;

    void computeFinalBoneTransformsRecursive(const std::vector<ModelNode>& nodes,
        const ModelNode& node, glm::mat4 parentTransformation);

    AnimationMode _mode = AnimationMode::Node;
    std::string _name;
    double _duration;
    float _timeScale = 1.f;
    bool _wasActive = false;

    std::vector<NodeAnimation> _nodeAnimations;

    std::vector<BoneAnimation> _boneAnimations;
    std::map<std::string, io::ModelMesh::Bone*> _boneMap;
    std::vector<glm::mat4> _finalBoneTransforms;
};

} // namespace openspace::io

#endif // __OPENSPACE_CORE___MODELANIMATION___H__
