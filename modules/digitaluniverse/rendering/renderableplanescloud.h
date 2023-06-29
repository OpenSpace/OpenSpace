/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEPLANESCLOUD___H__
#define __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEPLANESCLOUD___H__

#include <openspace/rendering/renderable.h>

#include <modules/space/labelscomponent.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <filesystem>
#include <functional>
#include <unordered_map>

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

// (x, y, z, w, s, t) * 6 = 36

namespace documentation { struct Documentation; }

class RenderablePlanesCloud : public Renderable {
public:
    explicit RenderablePlanesCloud(const ghoul::Dictionary& dictionary);
    ~RenderablePlanesCloud() override = default;

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    struct PlaneAggregate {
        int textureIndex;
        int numberOfPlanes;
        GLuint vao;
        GLuint vbo;
        std::vector<GLfloat> planesCoordinates;
    };

    void deleteDataGPUAndCPU();
    void createPlanes();
    void renderPlanes(const RenderData& data, const glm::dmat4& modelViewTransform,
        const glm::dmat4& projectionTransform, float fadeInVariable);

    void loadTextures();

    bool _hasSpeckFile = false;
    bool _dataIsDirty = true;
    bool _hasLabels = false;

    properties::FloatProperty _scaleFactor;
    properties::BoolProperty _drawElements;
    properties::OptionProperty _blendMode;
    properties::Vec2Property _fadeInDistances;
    properties::BoolProperty _disableFadeInDistance;
    properties::FloatProperty _planeMinSize;
    properties::OptionProperty _renderOption;

    ghoul::opengl::ProgramObject* _program = nullptr;
    UniformCache(
        modelViewProjectionTransform, alphaValue, fadeInValue, galaxyTexture
    ) _uniformCache;
    std::unordered_map<int, std::unique_ptr<ghoul::opengl::Texture>> _textureMap;
    std::unordered_map<int, std::string> _textureFileMap;
    std::unordered_map<int, PlaneAggregate> _planesMap;

    std::filesystem::path _speckFile;
    std::filesystem::path _texturesPath;
    std::string _luminosityVar;

    DistanceUnit _unit = DistanceUnit::Parsec;

    speck::Dataset _dataset;

    // Everything related to the labels is handled by LabelsComponent
    std::unique_ptr<LabelsComponent> _labels;

    float _sluminosity = 1.f;

    glm::dmat4 _transformationMatrix = glm::dmat4(1.0);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEPLANESCLOUD___H__
