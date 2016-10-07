/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __PROJECTIONCOMPONENT_H__
#define __PROJECTIONCOMPONENT_H__

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/util/spicemanager.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/texture.h>

namespace ghoul {
namespace opengl {

class ProgramObject;

} // namespace opengl
} // namespace ghoul

namespace openspace {

class ProjectionComponent : public properties::PropertyOwner {
public:
    ProjectionComponent();

    bool initialize();
    bool deinitialize();

    bool isReady() const;

    bool initializeProjectionSettings(const ghoul::Dictionary& dictionary);
    bool initializeParser(const ghoul::Dictionary& dictionary);

    ghoul::opengl::Texture& depthTexture();
    void imageProjectBegin();
    void imageProjectEnd();
    void depthMapRenderBegin();
    void depthMapRenderEnd();


    void update();

    bool auxiliaryRendertarget();
    bool depthRendertarget();

    std::shared_ptr<ghoul::opengl::Texture> loadProjectionTexture(
        const std::string& texturePath,
        bool isPlaceholder = false
    );
    
    glm::mat4 computeProjectorMatrix(
        const glm::vec3 loc, glm::dvec3 aim, const glm::vec3 up,
        const glm::dmat3& instrumentMatrix,
        float fieldOfViewY, 
        float aspectRatio,
        float nearPlane,
        float farPlane,
        glm::vec3& boreSight
    );

    bool doesPerformProjection() const;
    bool needsClearProjection() const;
    float projectionFading() const;

    bool needsShadowMap() const;

    void clearAllProjections();

    ghoul::opengl::Texture& projectionTexture() const;

    std::string projectorId() const;
    std::string projecteeId() const;
    std::string instrumentId() const;
    SpiceManager::AberrationCorrection aberration() const;

    float fieldOfViewY() const;
    float aspectRatio() const;

private:
    bool generateProjectionLayerTexture(const glm::ivec2& size);
    bool generateDepthTexture(const glm::ivec2& size);

protected:
    properties::BoolProperty _performProjection;
    properties::BoolProperty _clearAllProjections;
    properties::FloatProperty _projectionFading;

    properties::IVec2Property _textureSize;
    properties::TriggerProperty _applyTextureSize;
    bool _textureSizeDirty;

    std::unique_ptr<ghoul::opengl::Texture> _projectionTexture;
    std::shared_ptr<ghoul::opengl::Texture> _placeholderTexture;

    float _projectionTextureAspectRatio;

    std::string _instrumentID;
    std::string _projectorID;
    std::string _projecteeID;
    SpiceManager::AberrationCorrection _aberration;
    std::vector<std::string> _potentialTargets;
    float _fovy;
    float _aspectRatio;

    GLuint _fboID;
    GLuint _depthFboID;

    GLint _defaultFBO;
    GLint _viewport[4];

    struct {
        bool isEnabled;
        std::unique_ptr<ghoul::opengl::Texture> texture;
    } _shadowing;

    struct {
        bool isEnabled;
        GLuint fbo;
        GLuint vao;
        GLuint vbo;
        std::unique_ptr<ghoul::opengl::ProgramObject> program;
        std::unique_ptr<ghoul::opengl::Texture> texture;
        std::unique_ptr<ghoul::opengl::Texture> stencilTexture;
    } _dilation;
};

} // namespace openspace

#endif // __PROJECTIONCOMPONENT_H__
