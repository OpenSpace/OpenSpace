/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec4property.h>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>

#include <functional>
#include <unordered_map>

namespace ghoul::filesystem { class File; }
namespace ghoul::fontrendering { class Font; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

// (x, y, z, w, s, t) * 6 = 36
const int PLANES_VERTEX_DATA_SIZE = 36;

namespace documentation { struct Documentation; }

class RenderablePlanesCloud : public Renderable {
public:
    explicit RenderablePlanesCloud(const ghoul::Dictionary& dictionary);
    ~RenderablePlanesCloud() = default;

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    enum Unit {
        Meter = 0,
        Kilometer = 1,
        Parsec = 2,
        Kiloparsec = 3,
        Megaparsec = 4,
        Gigaparsec = 5,
        GigalightYears = 6
    };

    struct PlaneAggregate {
        int textureIndex;
        int numberOfPlanes;
        GLuint vao;
        GLuint vbo;
        std::vector<GLfloat> planesCoordinates;
    };

    void deleteDataGPUAndCPU();
    void createPlanes();
    void renderPlanes(const RenderData& data, const glm::dmat4& modelViewMatrix,
        const glm::dmat4& projectionMatrix, float fadeInVariable);
    void renderLabels(const RenderData& data,
        const glm::dmat4& modelViewProjectionMatrix, const glm::dvec3& orthoRight,
        const glm::dvec3& orthoUp, float fadeInVarible);

    bool loadData();
    bool loadTextures();
    bool readSpeckFile();
    bool readLabelFile();
    bool loadCachedFile(const std::string& file);
    bool saveCachedFile(const std::string& file) const;

    bool _hasSpeckFile = false;
    bool _dataIsDirty = true;
    bool _textColorIsDirty = true;
    bool _hasLabel = false;
    bool _labelDataIsDirty = true;

    int _textMinSize = 0;
    int _textMaxSize = 200;
    int _planeStartingIndexPos = 0;
    int _textureVariableIndex = 0;

    properties::FloatProperty _alphaValue;
    properties::FloatProperty _scaleFactor;
    properties::Vec4Property _textColor;
    properties::FloatProperty _textSize;
    properties::BoolProperty _drawElements;
    properties::OptionProperty _blendMode;
    properties::Vec2Property _fadeInDistance;
    properties::BoolProperty _disableFadeInDistance;
    properties::FloatProperty _planeMinSize;

    // DEBUG:
    properties::OptionProperty _renderOption;

    ghoul::opengl::ProgramObject* _program = nullptr;
    UniformCache(modelViewProjectionTransform, alphaValue, fadeInValue,
        galaxyTexture) _uniformCache;
    std::shared_ptr<ghoul::fontrendering::Font> _font = nullptr;
    std::unordered_map<int, std::unique_ptr<ghoul::opengl::Texture>> _textureMap;
    std::unordered_map<int, std::string> _textureFileMap;
    std::unordered_map<int, PlaneAggregate> _planesMap;

    std::string _speckFile;
    std::string _labelFile;
    std::string _texturesPath;
    std::string _luminosityVar;

    Unit _unit = Parsec;

    std::vector<float> _fullData;
    std::vector<std::pair<glm::vec3, std::string>> _labelData;
    std::unordered_map<std::string, int> _variableDataPositionMap;

    int _nValuesPerAstronomicalObject = 0;

    float _sluminosity = 1.f;

    glm::dmat4 _transformationMatrix = glm::dmat4(1.0);
};


} // namespace openspace

#endif // __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEPLANESCLOUD___H__
