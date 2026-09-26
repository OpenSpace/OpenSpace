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

#ifndef __GHOUL___MODELANIMATION___H__
#define __GHOUL___MODELANIMATION___H__

#include <ghoul/io/model/modelnode.h>
#include <ghoul/glm.h>
#include <string>
#include <vector>

namespace ghoul::io {

class ModelAnimation {
public:
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
        int node = 0;
        std::vector<PositionKeyframe> positions;
        std::vector<RotationKeyframe> rotations;
        std::vector<ScaleKeyframe> scales;
    };

    ModelAnimation(std::string name, double duration);

    ModelAnimation(ModelAnimation&&) noexcept = default;
    ~ModelAnimation() noexcept = default;

    void setTimeScale(float timeScale);
    void animate(std::vector<ModelNode>& nodes, double now, bool enabled);
    void reset(std::vector<ModelNode>& nodes);

    std::vector<NodeAnimation>& nodeAnimations();
    const std::vector<NodeAnimation>& nodeAnimations() const;
    std::string name() const;
    double duration() const;
    float timeScale() const;

private:
    std::string _name;
    double _duration;
    float _timeScale = 1.f;
    std::vector<NodeAnimation> _nodeAnimations;
    bool _wasActive = false;
};

} // namespace ghoul::io

#endif // __GHOUL___MODELANIMATION___H__
