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

#ifndef __OPENSPACE_CORE___SHADOWMAPPING___H__
#define __OPENSPACE_CORE___SHADOWMAPPING___H__

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <vector>

namespace ghoul { class Dictionary; }
namespace openspace {
    namespace documentation { struct Documentation; }
    class SceneGraphNode;
} // namespace openspace

namespace openspace::shadowmapping {

struct ShadowInfo {
    const SceneGraphNode* lightsource = nullptr;
    std::vector<const SceneGraphNode*> targets;
    GLuint depthMap = 0;
    glm::ivec2 depthMapResolution = glm::ivec2(0);
    GLuint fbo = 0;
    glm::dmat4 viewProjectionMatrix = glm::dmat4(1.0);
};

// This drop-in class is representing that an object is capable of shadowing another
// object
class Shadower {
public:
    explicit Shadower(const ghoul::Dictionary& dictionary);
    virtual ~Shadower() = default;

    bool isCastingShadow() const;
    void setLightSource(const SceneGraphNode* lightSource);
    const SceneGraphNode* lightSource() const;
    void setShadowGroup(std::string shadowGroup);
    const std::string& shadowGroup() const;
    double shadowFrustumSize() const;

    virtual glm::dvec3 center() const = 0;

    virtual void renderForDepthMap(const glm::dmat4& vp) const = 0;

    static documentation::Documentation Documentation();

protected:
    properties::BoolProperty _castShadow;
    const SceneGraphNode* _lightSource = nullptr;
    std::string _shadowGroup;

    properties::FloatProperty _frustumSize;
    bool _hasFrustumSize = false;
};

// This drop-in class is representing that an object can be shadowed by other another
// object
class Shadowee {
public:
    void addShadower(const Shadower* shadower);
    void removeShadower(const Shadower* shadower);

protected:
    std::vector<const Shadower*> _shadowers;
    bool _isShadowersDirty = false;
};

} // namespace openspace::shadowmapping

#endif // __OPENSPACE_CORE___SHADOWMAPPING___H__
