/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___RINGSCOMPONENT___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___RINGSCOMPONENT___H__

#include <openspace/properties/propertyowner.h>

#include <modules/globebrowsing/src/shadowcomponent.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec4property.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/uniformcache.h>

namespace ghoul { class Dictionary; }
namespace ghoul::filesystem { class File; }
namespace ghoul::opengl { class ProgramObject; }

namespace openspace {
    struct RenderData;
    struct UpdateData;

namespace documentation { struct Documentation; }

class RingsComponent : public properties::PropertyOwner {
public:
    enum RenderPass {
        GeometryOnly,
        GeometryAndShading
    };

    RingsComponent(const ghoul::Dictionary& dictionary);

    void initialize();
    void initializeGL();
    void deinitializeGL();

    bool isReady() const;

    void draw(const RenderData& data, const RingsComponent::RenderPass renderPass,
        const ShadowComponent::ShadowMapData& shadowData = {}
    );
    void update(const UpdateData& data);

    static documentation::Documentation Documentation();

    bool isEnabled() const;

private:
    void loadTexture();
    void createPlane();
    void compileShadowShader();

    properties::StringProperty _texturePath;
    properties::FloatProperty _size;
    properties::Vec2Property _offset;
    properties::FloatProperty _nightFactor;
    properties::FloatProperty _transparency;
    properties::BoolProperty _enabled;
    properties::FloatProperty _zFightingPercentage;
    properties::IntProperty _nShadowSamples;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
    std::unique_ptr<ghoul::opengl::ProgramObject> _geometryOnlyShader;
    UniformCache(modelViewProjectionMatrix, textureOffset, transparency, nightFactor,
        sunPosition, ringTexture, shadowMatrix, shadowMapTexture, zFightingPercentage
    ) _uniformCache;
    UniformCache(modelViewProjectionMatrix, textureOffset, ringTexture
    ) _geomUniformCache;
    std::unique_ptr<ghoul::opengl::Texture> _texture;
    std::unique_ptr<ghoul::filesystem::File> _textureFile;

    ghoul::Dictionary _ringsDictionary;
    bool _textureIsDirty = false;
    GLuint _quad = 0;
    GLuint _vertexPositionBuffer = 0;
    bool _planeIsDirty = false;

    glm::vec3 _sunPosition = glm::vec3(0.f);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___RINGSCOMPONENT___H__
