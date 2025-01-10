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

#ifndef __OPENSPACE_MODULE_GALAXY___RENDERABLEGALAXY___H__
#define __OPENSPACE_MODULE_GALAXY___RENDERABLEGALAXY___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/optionproperty.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <filesystem>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace volume { template <typename T> class RawVolume; }

class GalaxyRaycaster;
struct RenderData;

class RenderableGalaxy : public Renderable {
public:
    explicit RenderableGalaxy(const ghoul::Dictionary& dictionary);
    ~RenderableGalaxy() override = default;

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;
    bool isReady() const override;
    void render(const RenderData& data, RendererTasks& tasks) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    void renderPoints(const RenderData& data);
    void renderBillboards(const RenderData& data);

    struct Result {
        bool success;
        std::vector<glm::vec3> positions;
        std::vector<glm::vec3> color;
    };
    Result loadPointFile();
    Result loadCachedFile(const std::filesystem::path& file);

    glm::vec3 _volumeSize = glm::vec3(0.f);
    glm::vec3 _pointScaling = glm::vec3(0.f);
    properties::BoolProperty _volumeRenderingEnabled;
    properties::BoolProperty _starRenderingEnabled;
    properties::FloatProperty _stepSize;
    properties::FloatProperty _absorptionMultiply;
    properties::FloatProperty _emissionMultiply;
    properties::OptionProperty _starRenderingMethod;
    properties::FloatProperty _enabledPointsRatio;
    properties::Vec3Property _rotation;
    properties::FloatProperty _downScaleVolumeRendering;
    properties::FloatProperty _numberOfRayCastingSteps;

    std::unique_ptr<ghoul::opengl::Texture> _pointSpreadFunctionTexture;
    std::unique_ptr<ghoul::filesystem::File> _pointSpreadFunctionFile;

    std::filesystem::path _volumeFilename;
    glm::ivec3 _volumeDimensions = glm::ivec3(0);
    std::filesystem::path _pointsFilename;
    std::filesystem::path _pointSpreadFunctionTexturePath;
    std::filesystem::path _raycastingShader;

    std::unique_ptr<GalaxyRaycaster> _raycaster;
    std::unique_ptr<volume::RawVolume<glm::tvec4<GLubyte>>> _volume;
    std::unique_ptr<ghoul::opengl::Texture> _texture;
    glm::mat4 _pointTransform = glm::mat4(1.f);
    glm::vec3 _aspect = glm::vec3(0.f);
    float _opacityCoefficient = 0.f;

    std::unique_ptr<ghoul::opengl::ProgramObject> _pointsProgram;
    std::unique_ptr<ghoul::opengl::ProgramObject> _billboardsProgram;
    UniformCache(
        modelMatrix, viewProjectionMatrix, eyePosition, opacityCoefficient
    ) _uniformCachePoints;
    UniformCache(
        modelMatrix, viewProjectionMatrix, cameraUp, eyePosition, psfTexture
    ) _uniformCacheBillboards;
    std::vector<float> _pointsData;
    size_t _nPoints = 0;
    GLuint _pointsVao = 0;
    GLuint _positionVbo = 0;
    GLuint _colorVbo = 0;

    std::vector<glm::vec3> _pointPositionsCache;
    std::vector<glm::vec3> _pointColorsCache;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GALAXY___RENDERABLEGALAXY___H__
