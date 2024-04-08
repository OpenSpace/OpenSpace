/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___PROJECTIONCOMPONENT___H__
#define __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___PROJECTIONCOMPONENT___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul { class Dictionary; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }

class ProjectionComponent : public properties::PropertyOwner {
public:
    ProjectionComponent();

    void initialize(const std::string& identifier, const ghoul::Dictionary& dictionary);
    bool initializeGL();
    void deinitialize();

    bool isReady() const;

    ghoul::opengl::Texture& depthTexture() const;
    void imageProjectBegin();
    void imageProjectEnd();
    void depthMapRenderBegin();
    void depthMapRenderEnd();

    void update() const;

    bool auxiliaryRendertarget();
    bool depthRendertarget();

    std::shared_ptr<ghoul::opengl::Texture> loadProjectionTexture(
        const std::filesystem::path& texturePath, bool isPlaceholder = false);

    glm::mat4 computeProjectorMatrix(const glm::vec3& loc, const glm::dvec3& aim,
        const glm::vec3& up, const glm::dmat3& instrumentMatrix, float fieldOfViewY,
        float aspectRatio, float nearPlane, float farPlane, glm::vec3& boreSight);

    bool doesPerformProjection() const;
    bool needsClearProjection() const;
    bool needsMipMapGeneration() const;
    float projectionFading() const;

    bool needsShadowMap() const;

    void clearAllProjections();
    void generateMipMap();

    ghoul::opengl::Texture& projectionTexture() const;

    std::string projectorId() const;
    std::string projecteeId() const;
    std::string instrumentId() const;
    SpiceManager::AberrationCorrection aberration() const;

    float fieldOfViewY() const;
    float aspectRatio() const;

    static documentation::Documentation Documentation();

private:
    bool generateProjectionLayerTexture(const glm::ivec2& size);
    bool generateDepthTexture(const glm::ivec2& size);

protected:
    properties::BoolProperty _performProjection;
    properties::BoolProperty _clearAllProjections;
    properties::FloatProperty _projectionFading;

    properties::IVec2Property _textureSize;
    properties::TriggerProperty _applyTextureSize;
    bool _textureSizeDirty = false;
    bool _mipMapDirty = false;

    std::unique_ptr<ghoul::opengl::Texture> _projectionTexture;
    std::shared_ptr<ghoul::opengl::Texture> _placeholderTexture;

    float _projectionTextureAspectRatio = 1.f;

    std::string _instrumentID;
    std::string _projectorID;
    std::string _projecteeID;
    SpiceManager::AberrationCorrection _aberration;
    std::vector<std::string> _potentialTargets;
    float _fovy = -1.f;
    float _aspectRatio = -1.f;

    GLuint _fboID = 0;
    GLuint _depthFboID = 0;

    GLint _defaultFBO = 0;
    GLint _viewport[4] = { 0, 0, 0, 0};

    struct {
        bool isEnabled = false;
        std::unique_ptr<ghoul::opengl::Texture> texture;
    } _shadowing;

    struct {
        bool isEnabled = false;
        GLuint fbo = 0;
        GLuint vao = 0;
        GLuint vbo = 0;
        std::unique_ptr<ghoul::opengl::ProgramObject> program;
        std::unique_ptr<ghoul::opengl::Texture> texture;
        std::unique_ptr<ghoul::opengl::Texture> stencilTexture;
    } _dilation;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___PROJECTIONCOMPONENT___H__
