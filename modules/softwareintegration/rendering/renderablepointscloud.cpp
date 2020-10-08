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

#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/updatestructures.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>

#include <fstream>

namespace {
    constexpr const char* ProgramName = "shaderProgram";
    constexpr const char* _loggerCat = "PointsCloud";
    constexpr const char* KeyFile = "File";

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
                    KeyFile,
                    new StringVerifier,
                    Optional::Yes,
                    "The path to the SPECK file that contains information about the "
                    "astronomical object being rendered."
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

        if (dictionary.hasKey(KeyFile)) {
            _speckFile = absPath(dictionary.value<std::string>(KeyFile));
            _hasSpeckFile = true;
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
        _toggleVisibility.onChange([&]() { _hasSpeckFile = !_hasSpeckFile; });
        addProperty(_toggleVisibility);
    }

    bool RenderablePointsCloud::isReady() const {
        return ((_shaderProgram != nullptr) && (!_fullData.empty()));
    }

    void RenderablePointsCloud::initialize() {
        const bool isSuccessful = loadData();
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
        if (_hasSpeckFile && _toggleVisibility) {
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
        }
    }

    void RenderablePointsCloud::update(const UpdateData&) {
        if (!_isDirty) {
            return;
        }
        if (_hasSpeckFile) {
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

    bool RenderablePointsCloud::loadData() {
        bool isSuccessful = true;
        _slicedData.clear();
        _fullData.clear();

        isSuccessful &= loadSpeckData();

        if (!_hasSpeckFile) {
            isSuccessful = true;
        }

        return isSuccessful;
    }

    bool RenderablePointsCloud::loadSpeckData() {
        if (!_hasSpeckFile) {
            LERROR(fmt::format("No speckFile found"));
            return true;
        }
        if (!FileSys.fileExists(absPath(_speckFile))) {
            LERROR(fmt::format("No path to speckFile found {}", _speckFile));
            return false;
        };

        bool isSuccessful = true;
        std::string cachedFile = FileSys.cacheManager()->cachedFilename(
            _speckFile,
            ghoul::filesystem::CacheManager::Persistent::Yes
        );

        const bool hasCachedFile = FileSys.fileExists(cachedFile);
        if (hasCachedFile) {
            LINFO(fmt::format(
                "Cached file '{}' used for Speck file '{}'",
                cachedFile, _speckFile
            ));

            isSuccessful = loadCachedFile(cachedFile);
            if (isSuccessful) {
                return true;
            }
            else {
                FileSys.cacheManager()->removeCacheFile(_speckFile);
                // Intentional fall-through to the 'else' to generate the cache
                // file for the next run
            }
        }
        else {
            LINFO(fmt::format("Cache for Speck file '{}' not found", _speckFile));
        }
        LINFO(fmt::format("Loading Speck file '{}'", _speckFile));

        isSuccessful = readSpeckFile();
        if (!isSuccessful) {
            return false;
        }

        LINFO("Saving cache");
        isSuccessful &= saveCachedFile(cachedFile);
        return isSuccessful;
    }

    bool RenderablePointsCloud::readSpeckFile() {
        std::ifstream file(_speckFile);
        if (!file.good()) {
            LERROR(fmt::format("Failed to open Speck file '{}'", _speckFile));
            return false;
        }

        _nValuesPerPoints = 0;

        // The beginning of the speck file has a header that either contains comments
        // (signaled by a preceding '#') or information about the structure of the file
        // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
        std::string line;
        while (true) {
            std::getline(file, line);

            // Guard against wrong line endings (copying files from Windows to Mac) causes
            // lines to have a final \r
            if (!line.empty() && line.back() == '\r') {
                line = line.substr(0, line.length() - 1);
            }

            if (line.empty() || line[0] == '#') {
                continue;
            }

            if (line.substr(0, 7) != "datavar")
            {
                // Started reading data
                break;
            }

            if (line.substr(0, 7) == "datavar") {
                // datavar lines are structured as follows:
                // datavar # description
                // where # is the index of the data variable; so if we repeatedly overwrite
                // the 'nValues' variable with the latest index, we will end up with the total
                // number of values (+3 since X Y Z are not counted in the Speck file index)
                std::stringstream str(line);

                std::string dummy;
                str >> dummy; // command
                str >> _nValuesPerPoints; // variable index
                dummy.clear();
                str >> dummy; // variable name

                // We want the number, but the index is 0 based
                _nValuesPerPoints += 1;
            }
        }

        _nValuesPerPoints += 3; // X Y Z are not counted in the Speck file indices

        do {
            // Guard against wrong line endings (copying files from Windows to Mac) causes
            // lines to have a final \r
            if (!line.empty() && line.back() == '\r') {
                line = line.substr(0, line.length() - 1);
            }

            if (line.empty()) {
                std::getline(file, line);
                continue;
            }
            else if (line[0] == '#') {
                std::getline(file, line);
                continue;
            }

            std::stringstream str(line);
            std::vector<float> values(_nValuesPerPoints);

            for (int i = 0; i < _nValuesPerPoints; ++i) {
                str >> values[i];
            }

            _fullData.insert(_fullData.end(), values.begin(), values.end());

            // reads new line
            std::getline(file, line);
        } while (!file.eof());

        return true;
    }

    bool RenderablePointsCloud::loadCachedFile(const std::string& file) {
        std::ifstream fileStream(file, std::ifstream::binary);
        if (!fileStream.good()) {
            LERROR(fmt::format("Error opening file '{}' for loading cache file", file));
            return false;
        }
        int8_t version = 0;
        fileStream.read(reinterpret_cast<char*>(&version), sizeof(int8_t));
        if (version != CurrentCacheVersion) {
            LINFO("The format of the cached file has changed: deleting old cache");
            fileStream.close();
            FileSys.deleteFile(file);
            return false;
        }

        int32_t nValues = 0;
        fileStream.read(reinterpret_cast<char*>(&nValues), sizeof(int32_t));
        fileStream.read(
            reinterpret_cast<char*>(&_nValuesPerPoints),
            sizeof(int32_t)
        );

        _fullData.resize(nValues);
        fileStream.read(
            reinterpret_cast<char*>(&_fullData[0]),
            nValues * sizeof(_fullData[0])
        );

        bool isSuccessful = fileStream.good();
        return isSuccessful;
    }

    bool RenderablePointsCloud::saveCachedFile(const std::string& file) const {
        std::ofstream fileStream(file, std::ofstream::binary);
        if (!fileStream.good()) {
            LERROR(fmt::format("Error opening file '{}' for save cache file", file));
            return false;
        }
        fileStream.write(reinterpret_cast<const char*>(&CurrentCacheVersion), sizeof(int8_t));

        int32_t nValues = static_cast<int32_t>(_fullData.size());
        if (nValues == 0) {
            LERROR("Error writing cache: No values were loaded");
            return false;
        }
        fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));

        int32_t nValuesPerPoints = static_cast<int32_t>(
            _nValuesPerPoints
            );
        fileStream.write(
            reinterpret_cast<const char*>(&nValuesPerPoints),
            sizeof(int32_t)
        );

        size_t nBytes = nValues * sizeof(_fullData[0]);
        fileStream.write(reinterpret_cast<const char*>(&_fullData[0]), nBytes);

        return fileStream.good();
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

} // namespace openspace
