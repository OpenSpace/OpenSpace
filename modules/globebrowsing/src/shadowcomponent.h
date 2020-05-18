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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___SHADOWCOMPONENT___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___SHADOWCOMPONENT___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/util/camera.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/uniformcache.h>
#include <string>
#include <sstream>

namespace ghoul { class Dictionary; }
namespace ghoul::filesystem { class File; }
namespace ghoul::opengl { class ProgramObject; }

namespace openspace {
    struct RenderData;
    struct UpdateData;

namespace documentation { struct Documentation; }

class ShadowComponent : public properties::PropertyOwner {
public:
    struct ShadowMapData {
        glm::dmat4 shadowMatrix;
        GLuint shadowDepthTexture;
    };

    ShadowComponent(const ghoul::Dictionary& dictionary);

    void initialize();
    void initializeGL();
    void deinitializeGL();
    //bool deinitialize();

    bool isReady() const;

    RenderData begin(const RenderData& data);
    void end();
    void update(const UpdateData& data);

    static documentation::Documentation Documentation();

    bool isEnabled() const;

    ShadowComponent::ShadowMapData shadowMapData() const;

    void setViewDepthMap(bool enable);

private:
    void createDepthTexture();
    void createShadowFBO();
    void updateDepthTexture();

    // Debug
    void saveDepthBuffer();

    ShadowMapData _shadowData;

    // Texture coords in [0, 1], while clip coords in [-1, 1]
    const glm::dmat4 _toTextureCoordsMatrix = glm::dmat4(
        glm::dvec4(0.5, 0.0, 0.0, 0.0),
        glm::dvec4(0.0, 0.5, 0.0, 0.0),
        glm::dvec4(0.0, 0.0, 1.0, 0.0),
        glm::dvec4(0.5, 0.5, 0.0, 1.0)
    );

    // DEBUG
    properties::TriggerProperty _saveDepthTexture;
    properties::IntProperty _distanceFraction;
    properties::BoolProperty _enabled;

    ghoul::Dictionary _shadowMapDictionary;

    int _shadowDepthTextureHeight = 4096;
    int _shadowDepthTextureWidth = 4096;
    bool _dynamicDepthTextureRes = true;

    GLuint _shadowDepthTexture = 0;
    GLuint _positionInLightSpaceTexture = 0;
    GLuint _shadowFBO = 0;
    GLint _defaultFBO = 0;
    GLint _mViewport[4];

    GLboolean _faceCulling;
    GLboolean _polygonOffSet;
    GLboolean _depthIsEnabled;
    GLboolean _blendIsEnabled = false;

    GLenum _faceToCull;
    GLenum _depthFunction;

    GLfloat _polygonOffSetFactor;
    GLfloat _polygonOffSetUnits;
    GLfloat _colorClearValue[4];
    GLfloat _depthClearValue;

    glm::vec3 _sunPosition = glm::vec3(0.f);

    glm::dmat4 _shadowMatrix = glm::dmat4(1.0);

    glm::dvec3 _cameraPos = glm::dvec3(0.0);
    glm::dvec3 _cameraFocus = glm::dvec3(0.0);
    glm::dquat _cameraRotation = glm::dquat(1.0, 0.0, 0.0, 0.0);

    std::stringstream _serializedCamera;

    std::unique_ptr<Camera> _lightCamera;

    // DEBUG
    bool _executeDepthTextureSave = false;
    bool _viewDepthMap = false;
    std::unique_ptr<ghoul::opengl::ProgramObject> _renderDMProgram;
    GLuint _quadVAO = 0u;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___SHADOWCOMPONENT___H__
