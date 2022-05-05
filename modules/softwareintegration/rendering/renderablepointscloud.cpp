/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/softwareintegration/rendering/renderablepointscloud.h>

#include <modules/softwareintegration/softwareintegrationmodule.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <fstream>
#include <optional>

namespace {
    constexpr const char* _loggerCat = "PointsCloud";

    constexpr const std::array<const char*, 8> UniformNames = {
        "color", "opacity", "size", "modelMatrix", "cameraUp",
        "cameraViewProjectionMatrix", "eyePosition", "sizeOption"
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The color of the points."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "The size of the points."
    };

    constexpr openspace::properties::Property::PropertyInfo ToggleVisibilityInfo = {
        "ToggleVisibility",
        "Toggle Visibility",
        "Enables/Disables the drawing of points."
    };

    constexpr openspace::properties::Property::PropertyInfo DataInfo = {
        "Data",
        "Data",
        "Data to use for the positions of the points, given in Parsec."
    };

    constexpr openspace::properties::Property::PropertyInfo DataStorageKeyInfo = {
        "DataStorageKey",
        "Data Storage Key",
        "Key used to access a dataset in the module's centralized storage. Used for "
        "big datasets with lots of points."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeOptionInfo = {
        "SizeOption",
        "Size Option",
        "This value determines how the size of the data points are rendered."
    };

    struct [[codegen::Dictionary(RenderablePointsCloud)]] Parameters {
        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color;

        // [[codegen::verbatim(SizeInfo.description)]]
        std::optional<float> size;

        // [[codegen::verbatim(ToggleVisibilityInfo.description)]]
        std::optional<bool> toggleVisiblity;

        // [[codegen::verbatim(DataInfo.description)]]
        std::optional<std::vector<glm::vec3>> data;

        // [[codegen::verbatim(DataStorageKeyInfo.description)]]
        std::optional<std::string> dataStorageKey;

        enum class SizeOption {
            Uniform,
            NonUniform
        };
        // [[codegen::verbatim(SizeOptionInfo.description)]]
        std::optional<SizeOption> sizeOption;
    };
#include "renderablepointscloud_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderablePointsCloud::Documentation() {
    return codegen::doc<Parameters>("softwareintegration_renderable_pointscloud");
}

RenderablePointsCloud::RenderablePointsCloud(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _color(ColorInfo, glm::vec3(0.5f), glm::vec3(0.f), glm::vec3(1.f))
    , _size(SizeInfo, 1.f, 0.f, 150.f)
    , _isVisible(ToggleVisibilityInfo, true)
    , _sizeOption(SizeOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _color = p.color.value_or(_color);
    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_color);

    // Check if a key to data stored in the module's centralized memory was included
    _nValuesPerPoint = 3;
    _dataStorageKey = p.dataStorageKey.value();

    _size = p.size.value_or(_size);
    addProperty(_size);

    _isVisible = p.toggleVisiblity.value_or(_isVisible);
    addProperty(_isVisible);

    addProperty(_opacity);

    _sizeOption.addOptions({
        { SizeOption::Uniform, "Uniform" },
        { SizeOption::NonUniform, "NonUniform" }
    });
    if (p.sizeOption.has_value()) {
        switch (*p.sizeOption) {
            case Parameters::SizeOption::Uniform:
                _sizeOption = SizeOption::Uniform;
                break;
            case Parameters::SizeOption::NonUniform:
                _sizeOption = SizeOption::NonUniform;
                break;
        }
    }
    _sizeOption.onChange([&] { _isDirty = true; });
    addProperty(_sizeOption);
}

bool RenderablePointsCloud::isReady() const {
    return _shaderProgram && (!_fullData.empty());
}

void RenderablePointsCloud::initialize() {
    loadData();
}

void RenderablePointsCloud::initializeGL() {
    _shaderProgram = global::renderEngine->buildRenderProgram(
        "PointsCloud",
        absPath("${MODULE_SOFTWAREINTEGRATION}/shaders/point_vs.glsl"),
        absPath("${MODULE_SOFTWAREINTEGRATION}/shaders/point_fs.glsl"),
        absPath("${MODULE_SOFTWAREINTEGRATION}/shaders/point_ge.glsl")
    );

    ghoul::opengl::updateUniformLocations(*_shaderProgram, _uniformCache, UniformNames);
}

void RenderablePointsCloud::deinitializeGL() {
    glDeleteVertexArrays(1, &_vertexArrayObjectID);
    _vertexArrayObjectID = 0;

    glDeleteBuffers(1, &_vertexBufferObjectID);
    _vertexBufferObjectID = 0;

    if (_shaderProgram) {
        global::renderEngine->removeRenderProgram(_shaderProgram.get());
        _shaderProgram = nullptr;
    }
}

void RenderablePointsCloud::render(const RenderData& data, RendererTasks&) {
    if (_fullData.empty()) {
        return;
    }

    if (!_isVisible) {
        return;
    }
    
    _shaderProgram->activate();

    glm::dvec3 eyePosition = glm::dvec3(
        glm::inverse(data.camera.combinedViewMatrix()) * glm::dvec4(0.0, 0.0, 0.0, 1.0)
    );
    _shaderProgram->setUniform(_uniformCache.eyePosition, eyePosition);

    glm::dvec3 cameraUp = data.camera.lookUpVectorWorldSpace();
    _shaderProgram->setUniform(_uniformCache.cameraUp, cameraUp);

    glm::dmat4 modelMatrix =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), data.modelTransform.scale);
    _shaderProgram->setUniform(_uniformCache.modelMatrix, modelMatrix);

    glm::dmat4 projectionMatrix = glm::dmat4(data.camera.projectionMatrix());
    glm::dmat4 cameraViewProjectionMatrix = projectionMatrix *
                                            data.camera.combinedViewMatrix();
    _shaderProgram->setUniform(
        _uniformCache.cameraViewProjectionMatrix,
        cameraViewProjectionMatrix
    );

    _shaderProgram->setUniform(_uniformCache.color, _color);
    _shaderProgram->setUniform(_uniformCache.opacity, _opacity);
    _shaderProgram->setUniform(_uniformCache.size, _size);
    _shaderProgram->setUniform(_uniformCache.sizeOption, _sizeOption);

    // Changes GL state:
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDepthMask(false);

    glBindVertexArray(_vertexArrayObjectID);
    const GLsizei nPoints = static_cast<GLsizei>(_fullData.size() / _nValuesPerPoint);
    glDrawArrays(GL_POINTS, 0, nPoints);

    glBindVertexArray(0);
    _shaderProgram->deactivate();

    // Restores GL State
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}

void RenderablePointsCloud::update(const UpdateData&) {
    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shaderProgram, _uniformCache, UniformNames);
    }

    if (!_isDirty) {
        return;
    }

    LDEBUG("Regenerating data");

    // @TODO (emmbr26 2021-02-11) This 'createDataSlice'step doesn't really seem
    // necessary, but could rather be combined with the loadData() step?
    createDataSlice();

    int size = static_cast<int>(_slicedData.size());

    if (_vertexArrayObjectID == 0) {
        glGenVertexArrays(1, &_vertexArrayObjectID);
        LDEBUG(fmt::format("Generating Vertex Array id '{}'", _vertexArrayObjectID));
    }
    if (_vertexBufferObjectID == 0) {
        glGenBuffers(1, &_vertexBufferObjectID);
        LDEBUG(fmt::format("Generating Vertex Buffer Object id '{}'", _vertexBufferObjectID));
    }

    glBindVertexArray(_vertexArrayObjectID);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferObjectID);
    glBufferData(
        GL_ARRAY_BUFFER,
        size * sizeof(float),
        _slicedData.data(),
        GL_STATIC_DRAW
    );

    GLint positionAttribute = _shaderProgram->attributeLocation("in_position");

    glEnableVertexAttribArray(positionAttribute);
    glVertexAttribPointer(
        positionAttribute,
        4,
        GL_FLOAT,
        GL_FALSE,
        0,
        nullptr
    );

    glBindVertexArray(0);

    _isDirty = false;
}

void RenderablePointsCloud::createDataSlice() {
    _slicedData.clear();
    _slicedData.reserve(4 * (_fullData.size() / _nValuesPerPoint));

    auto addPosition = [&](const glm::vec4& pos) {
        for (int j = 0; j < 4; ++j) {
            _slicedData.push_back(pos[j]);
        }
    };

    for (size_t i = 0; i < _fullData.size(); i += _nValuesPerPoint) {
        glm::dvec4 transformedPos = glm::dvec4(
            _fullData[i + 0],
            _fullData[i + 1],
            _fullData[i + 2],
            1.0
        );
        // W-normalization
        transformedPos /= transformedPos.w;
        transformedPos *= openspace::distanceconstants::Parsec;
        addPosition(transformedPos);
    }
}

void RenderablePointsCloud::loadData() {
    if (!_dataStorageKey.has_value()) {
        LWARNING("No point data found");
        return;
    }

    // @TODO: potentially combine point data with additional data about the points,
    // such as luminosity

    // Fetch data from module's centralized storage
    auto module = global::moduleEngine->module<SoftwareIntegrationModule>();
    _fullData = module->fetchData(_dataStorageKey.value());

    _isDirty = true;
}

} // namespace openspace
