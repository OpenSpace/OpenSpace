/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace volume { template <typename T> class RawVolume; }

class GalaxyRaycaster;
struct RenderData;

class RenderableGalaxy : public Renderable {
public:
    explicit RenderableGalaxy(const ghoul::Dictionary& dictionary);
    virtual ~RenderableGalaxy() = default;

    void initializeGL() override;
    void deinitializeGL() override;
    bool isReady() const override;
    void render(const RenderData& data, RendererTasks& tasks) override;
    void update(const UpdateData& data) override;

private:
    void renderPoints(const RenderData& data);
    void renderBillboards(const RenderData& data);
    float safeLength(const glm::vec3& vector) const;

    struct Result {
        bool success;
        std::vector<glm::vec3> positions;
        std::vector<glm::vec3> color;
    };
    Result loadPointFile(const std::string& file);
    Result loadCachedFile(const std::string& file);

    glm::vec3 _volumeSize;
    glm::vec3 _pointScaling;
    properties::BoolProperty _volumeRenderingEnabled;
    properties::BoolProperty _starRenderingEnabled;
    properties::FloatProperty _stepSize;
    properties::FloatProperty _absorptionMultiply;
    properties::FloatProperty _emissionMultiply;
    properties::OptionProperty _starRenderingMethod;
    properties::FloatProperty _enabledPointsRatio;
    properties::Vec3Property _translation;
    properties::Vec3Property _rotation;
    properties::FloatProperty _downScaleVolumeRendering;
    properties::FloatProperty _numberOfRayCastingSteps;

    std::unique_ptr<ghoul::opengl::Texture> _pointSpreadFunctionTexture;
    std::unique_ptr<ghoul::filesystem::File> _pointSpreadFunctionFile;

    std::string _volumeFilename;
    glm::ivec3 _volumeDimensions;
    std::string _pointsFilename;
    std::string _pointSpreadFunctionTexturePath;

    std::unique_ptr<GalaxyRaycaster> _raycaster;
    std::unique_ptr<volume::RawVolume<glm::tvec4<GLubyte>>> _volume;
    std::unique_ptr<ghoul::opengl::Texture> _texture;
    glm::mat4 _pointTransform;
    glm::vec3 _aspect;
    float _opacityCoefficient;

    std::unique_ptr<ghoul::opengl::ProgramObject> _pointsProgram;
    std::unique_ptr<ghoul::opengl::ProgramObject> _billboardsProgram;
    UniformCache(
        modelMatrix, cameraViewProjectionMatrix, eyePosition,
        opacityCoefficient
    ) _uniformCachePoints;
    UniformCache(
        modelMatrix, cameraViewProjectionMatrix,
        cameraUp, eyePosition, psfTexture
    ) _uniformCacheBillboards;
    std::vector<float> _pointsData;
    size_t _nPoints;
    GLuint _pointsVao;
    GLuint _positionVbo;
    GLuint _colorVbo;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GALAXY___RENDERABLEGALAXY___H__
