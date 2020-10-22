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

#include <modules/softwareintegration/rendering/renderablepointscloud.h>

#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>

#include <fstream>

namespace {
    constexpr const char* KeyData = "Data";
    constexpr const char* KeyLuminosity = "Luminosity";
    constexpr const char* KeyVelocity = "Velocity";
    constexpr const char* ProgramName = "shaderProgram";
    constexpr const char* _loggerCat = "PointsCloud";

    constexpr int8_t CurrentCacheVersion = 1;

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The color of the points."
    };

    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
        "Opacity",
        "Opacity",
        "Determines the transparency of the points, where 1 is completely opaque "
        "and 0 fully transparent."
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
} // namespace

namespace openspace {

    documentation::Documentation RenderablePointsCloud::Documentation() {
        using namespace documentation;
        return {
            "RenderablePointsCloud",
            "softwareintegration_renderable_pointscloud",
            {
                {
                    "Type",
                    new StringEqualVerifier("RenderablePointsCloud"),
                    Optional::No
                },
                {
                    ColorInfo.identifier,
                    new DoubleVector3Verifier,
                    Optional::Yes,
                    ColorInfo.description
                },
                {
                    OpacityInfo.identifier,
                    new DoubleVerifier,
                    Optional::Yes,
                    OpacityInfo.description
                },
                {
                    SizeInfo.identifier,
                    new DoubleVerifier,
                    Optional::Yes,
                    SizeInfo.description
                },
                {
                    ToggleVisibilityInfo.identifier,
                    new BoolVerifier,
                    Optional::Yes,
                    ToggleVisibilityInfo.description
                }
            }
        };
    }

    RenderablePointsCloud::RenderablePointsCloud(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary)
        , _color(
            ColorInfo,
            glm::vec3(0.5f, 0.5, 0.5f),
            glm::vec3(0.f),
            glm::vec3(1.f)
        )
        , _opacity(OpacityInfo, 0.5f, 0.f, 1.f)
        , _size(SizeInfo, 1.f, 0.f, 150.f)
        , _toggleVisibility(ToggleVisibilityInfo, true)
    {
        documentation::testSpecificationAndThrow(
            Documentation(),
            dictionary,
            "RenderablePointsCloud"
        );

        if (dictionary.hasKey(ColorInfo.identifier)) {
            _color = dictionary.value<glm::vec3>(ColorInfo.identifier);
        }
        _color.setViewOption(properties::Property::ViewOptions::Color);
        addProperty(_color);

        if (dictionary.hasKey(KeyData)) {
            ghoul::Dictionary pointDataDict = dictionary.value<ghoul::Dictionary>(
                    KeyData
                );
            for (int i = 0; i < static_cast<int>(pointDataDict.size()); ++i) {
                _pointData.push_back(
                    { pointDataDict.value<glm::vec3>(std::to_string(i + 1)) }
                );
            }
            _hasPointData = true;
        }

        if (dictionary.hasKey(KeyLuminosity)) {
            ghoul::Dictionary luminosityDataDict = dictionary.value<ghoul::Dictionary>(
                    KeyLuminosity
                );
            for (int i = 0; i < static_cast<int>(luminosityDataDict.size()); ++i) {
                _luminosityData.push_back(
                    { luminosityDataDict.value<float>(std::to_string(i + 1)) }
                );
            }
            _hasLuminosityData = true;
        }

        if (dictionary.hasKey(KeyVelocity)) {
            ghoul::Dictionary velocityDataDict = dictionary.value<ghoul::Dictionary>(
                    KeyVelocity
                );
            for (int i = 0; i < static_cast<int>(velocityDataDict.size()); ++i) {
                _velocityData.push_back(
                    { velocityDataDict.value<float>(std::to_string(i + 1)) }
                );
            }
            _hasVelocityData = true;
        }

        if (dictionary.hasKey(OpacityInfo.identifier)) {
            _opacity = static_cast<float>(
                dictionary.value<double>(OpacityInfo.identifier));
        }
        addProperty(_opacity);

        if (dictionary.hasKey(SizeInfo.identifier)) {
            _size = static_cast<float>(
                dictionary.value<double>(SizeInfo.identifier));
        }
        addProperty(_size);

        if (dictionary.hasKey(ToggleVisibilityInfo.identifier)) {
            _toggleVisibility = dictionary.value<bool>(ToggleVisibilityInfo.identifier);
        }
        _toggleVisibility.onChange([&]() { _hasPointData = !_hasPointData; });
        addProperty(_toggleVisibility);
    }

    bool RenderablePointsCloud::isReady() const {
        return ((_shaderProgram != nullptr) && (!_fullData.empty()));
    }

    void RenderablePointsCloud::initialize() { 
        bool isSuccessful = loadData();
        if (!isSuccessful) {
            throw ghoul::RuntimeError("Error loading data");
        }
    }

    void RenderablePointsCloud::initializeGL() {
        _shaderProgram = global::renderEngine.buildRenderProgram(
            "PointsCloud",
            absPath("${MODULE_SOFTWAREINTEGRATION}/shaders/point_vs.glsl"),
            absPath("${MODULE_SOFTWAREINTEGRATION}/shaders/point_fs.glsl")
        );
    }

    void RenderablePointsCloud::deinitializeGL() {
        glDeleteVertexArrays(1, &_vertexArrayObjectID);
        _vertexArrayObjectID = 0;

        glDeleteBuffers(1, &_vertexBufferObjectID);
        _vertexBufferObjectID = 0;

        if (_shaderProgram) {
            global::renderEngine.removeRenderProgram(_shaderProgram.get());
            _shaderProgram = nullptr;
        }
    }

    void RenderablePointsCloud::render(const RenderData& data, RendererTasks&) {
        if (_fullData.empty()) {
            return;
        }

        if (_hasPointData && _toggleVisibility) {
            _shaderProgram->activate();

            glm::dmat4 modelTransform =
                glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
                glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
                glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

            glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

            _shaderProgram->setUniform("modelViewTransform", modelViewTransform);
            _shaderProgram->setUniform(
                "MVPTransform",
                glm::dmat4(data.camera.projectionMatrix()) * modelViewTransform
            );

            _shaderProgram->setUniform("color", _color);
            _shaderProgram->setUniform("opacity", _opacity);
            _shaderProgram->setUniform("size", _size);

            // Changes GL state:
            glEnablei(GL_BLEND, 0);
            glDepthMask(false);
            glBlendEquation(GL_FUNC_ADD);
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
            glEnable(GL_PROGRAM_POINT_SIZE); // Enable gl_PointSize in vertex

            glBindVertexArray(_vertexArrayObjectID);
            const GLsizei nPoints = static_cast<GLsizei>(_fullData.size() / _nValuesPerPoints);
            glDrawArrays(GL_POINTS, 0, nPoints);

            glBindVertexArray(0);
            _shaderProgram->deactivate();

            // Restores GL State
            global::renderEngine.openglStateCache().resetBlendState();
            global::renderEngine.openglStateCache().resetDepthState();
        }
    }

    void RenderablePointsCloud::update(const UpdateData&) {        
        if (!_isDirty) {
            return;
        }

        if (_hasPointData) {
            LDEBUG("Regenerating data");

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
        }
        
        glBindVertexArray(0);

        _isDirty = false;
    }

    void RenderablePointsCloud::createDataSlice() {
        _slicedData.clear();
        _slicedData.reserve(4 * (_fullData.size() / _nValuesPerPoints));

        auto addPosition = [&](const glm::vec4& pos) {
            for (int j = 0; j < 4; ++j) {
                _slicedData.push_back(pos[j]);
            }
        };

        for (size_t i = 0; i < _fullData.size(); i += _nValuesPerPoints) {
            glm::dvec4 transformedPos = _transformationMatrix * glm::dvec4(
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

    bool RenderablePointsCloud::loadData() {
        bool isSuccessful = true;
        _slicedData.clear();
        _fullData.clear();

        isSuccessful &= readPointData();
        if (!isSuccessful) {
            return false;
        }

        if (!_hasPointData) {
            isSuccessful = true;
        }

        return isSuccessful;
    }

    bool RenderablePointsCloud::readPointData() {
        if (!_hasPointData) {
            LERROR("No point data found");
            return false;
        }

        _nValuesPerPoints = 3;

        int dataSize = _pointData.size();
        std::vector<float> values(_nValuesPerPoints);

        for (int i = 0; i < dataSize; i++) {

            for (int j = 0; j < _nValuesPerPoints; j++) {
                values.push_back(_pointData[i][j]);
            }
        }

        _fullData.insert(_fullData.end(), values.begin(), values.end());

        return true;
    }

} // namespace openspace
