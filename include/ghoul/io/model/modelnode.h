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

#ifndef __GHOUL___MODELNODE___H__
#define __GHOUL___MODELNODE___H__

#include <ghoul/io/model/modelmesh.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/glm.h>
#include <array>
#include <vector>

namespace ghoul::io {

class ModelNode {
public:
    ModelNode(std::string name, glm::mat4 transform, std::vector<io::ModelMesh> meshes);

    ModelNode(ModelNode&&) noexcept = default;
    ~ModelNode() noexcept = default;

    void setParent(int parent);
    void setChildren(std::vector<int> children);
    void addChild(int child);
    void setAnimation(const glm::mat4& animation);
    void updateCustomTransform(const glm::dmat4& customTransform);

    std::vector<io::ModelMesh>& meshes();
    const std::vector<io::ModelMesh>& meshes() const;
    int parent() const;
    std::vector<int>& children();
    const std::vector<int>& children() const;
    glm::mat4 transform() const;
    glm::mat4 animationTransform() const;
    glm::mat4 customTransform() const;
    bool hasAnimation() const;
    bool hasCustomTransform() const;
    std::string name() const;

private:
    // `glm::mat4` is not noexcept move constructable, use an array instead for transform
    // Array is column major
    std::array<GLfloat, 16> _transform = {
        1.f, 0.f, 0.f, 0.f,
        0.f, 1.f, 0.f, 0.f,
        0.f, 0.f, 1.f, 0.f,
        0.f, 0.f, 0.f, 1.f
    };
    std::array<GLfloat, 16> _animationTransform = {
        1.f, 0.f, 0.f, 0.f,
        0.f, 1.f, 0.f, 0.f,
        0.f, 0.f, 1.f, 0.f,
        0.f, 0.f, 0.f, 1.f
    };
    std::array<GLfloat, 16> _customTransform = {
        1.f, 0.f, 0.f, 0.f,
        0.f, 1.f, 0.f, 0.f,
        0.f, 0.f, 1.f, 0.f,
        0.f, 0.f, 0.f, 1.f
    };
    std::vector<ModelMesh> _meshes;
    int _parent = -1;
    std::vector<int> _children;
    bool _hasAnimation = false;
    bool _hasCustomTransform = false;
    std::string _name;
};

} // namespace ghoul::io

#endif // __GHOUL___MODELNODE___H__
