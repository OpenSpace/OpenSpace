/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <modules/space/rendering/renderableconstellationlines.h>

#include <openspace/documentation/documentation.h>
#include <openspace/util/updatestructures.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/misc.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <array>
#include <filesystem>
#include <fstream>
#include <optional>

namespace {
    constexpr std::string_view _loggerCat = "RenderableConstellationLines";

    constexpr std::array<const char*, 4> UniformNames = {
        "modelViewTransform", "projectionTransform", "alphaValue", "color"
    };

    constexpr openspace::properties::Property::PropertyInfo DrawElementsInfo = {
        "DrawElements",
        "Draw Elements",
        "Enables/Disables the drawing of the constellations"
    };

    constexpr openspace::properties::Property::PropertyInfo ConstellationUnitInfo = {
        "ConstellationUnit",
        "Constellation Unit",
        "The unit used for the constellation data"
    };

    constexpr openspace::properties::Property::PropertyInfo ConstellationColorInfo = {
        "ConstellationColor",
        "Constellation colors",
        "The defined colors for the constellations to be rendered"
    };

    struct [[codegen::Dictionary(RenderableConstellationLines)]] Parameters {
        // The path to the SPECK file that contains constellation lines data
        std::filesystem::path file;

        enum class [[codegen::map(openspace::DistanceUnit)]] Unit {
            Meter [[codegen::key("m")]],
            Kilometer [[codegen::key("Km")]],
            Parsec [[codegen::key("pc")]],
            Kiloparsec [[codegen::key("Kpc")]],
            Megaparsec [[codegen::key("Mpc")]],
            Gigaparsec [[codegen::key("Gpc")]],
            Gigalightyear [[codegen::key("Gly")]]
        };
        // [[codegen::verbatim(ConstellationUnitInfo.description)]]
        std::optional<Unit> constellationUnit;

        // [[codegen::verbatim(ConstellationColorInfo.description)]]
        std::optional<std::vector<glm::vec3>> constellationColor;
    };
#include "renderableconstellationlines_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation RenderableConstellationLines::Documentation() {
    return codegen::doc<Parameters>("space_renderable_constellationlines");
}

RenderableConstellationLines::RenderableConstellationLines(
                                                      const ghoul::Dictionary& dictionary)
    : RenderableConstellation(dictionary)
    , _drawElements(DrawElementsInfo, true)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _speckFile = absPath(p.file.string()).string();
    _hasSpeckFile = true;
    _drawElements.onChange([&]() { _hasSpeckFile = !_hasSpeckFile; });
    addProperty(_drawElements);

    if (p.constellationUnit.has_value()) {
        _constellationUnit = codegen::map<DistanceUnit>(*p.constellationUnit);
    }
    else {
        _constellationUnit = DistanceUnit::Meter;
    }

    if (p.constellationColor.has_value()) {
        std::vector<glm::vec3> ops = *p.constellationColor;
        for (size_t i = 0; i < ops.size(); ++i) {
            _constellationColorMap.insert({ static_cast<int>(i) + 1, ops[i] });
        }
    }
}

void RenderableConstellationLines::selectionPropertyHasChanged() {
    // If no values are selected (the default), we want to show all constellations
    if (!_constellationSelection.hasSelected()) {
        for (std::pair<const int, ConstellationLine>& pair :
            _renderingConstellationsMap)
        {
            pair.second.isEnabled = true;
        }
    }
    else {
        // Enable all constellations that are selected
        for (std::pair<const int, ConstellationLine>& pair :
            _renderingConstellationsMap)
        {
            pair.second.isEnabled =
                _constellationSelection.isSelected(pair.second.name);
        }
    }
}

bool RenderableConstellationLines::isReady() const {
    if (!_hasLabel) {
        return _program && !_renderingConstellationsMap.empty();
    }
    return _program && !_renderingConstellationsMap.empty() &&
        !_labelset.entries.empty();
}

void RenderableConstellationLines::initialize() {
    RenderableConstellation::initialize();

    bool success = loadData();
    if (!success) {
        throw ghoul::RuntimeError("Error loading data");
    }

    if (!_assetSelectedConstellations.empty()) {
        const std::vector<std::string> options = _constellationSelection.options();
        std::set<std::string> selectedConstellations;

        for (const std::string& s : _assetSelectedConstellations) {
            const auto it = std::find(options.begin(), options.end(), s);
            if (it == options.end()) {
                // The user has specified a constellation name that doesn't exist
                LWARNINGC(
                    "RenderableConstellation",
                    fmt::format("Option '{}' not found in list of constellations", s)
                );
            }
            else {
                selectedConstellations.insert(s);
            }
        }
        _constellationSelection = selectedConstellations;
    }
}

void RenderableConstellationLines::initializeGL() {
    _program = global::renderEngine->buildRenderProgram(
        "RenderableConstellationLines",
        absPath("${MODULE_SPACE}/shaders/constellationlines_vs.glsl"),
        absPath("${MODULE_SPACE}/shaders/constellationlines_fs.glsl")
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);

    createConstellations();
}

void RenderableConstellationLines::deinitializeGL() {
    for (const std::pair<const int, ConstellationLine>& pair :
        _renderingConstellationsMap)
    {
        glDeleteVertexArrays(1, &pair.second.vaoArray);
        glDeleteBuffers(1, &pair.second.vboArray);
    }

    if (_program) {
        global::renderEngine->removeRenderProgram(_program.get());
        _program = nullptr;
    }
}

void RenderableConstellationLines::renderConstellations(const RenderData&,
                                                        const glm::dmat4& modelViewMatrix,
                                                       const glm::dmat4& projectionMatrix)
{
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glDepthMask(false);
    glEnable(GL_DEPTH_TEST);

    _program->activate();

    _program->setUniform(_uniformCache.modelViewTransform, modelViewMatrix);
    _program->setUniform(_uniformCache.projectionTransform, projectionMatrix);
    _program->setUniform(_uniformCache.alphaValue, opacity());

    for (const std::pair<const int, ConstellationLine>& pair :
        _renderingConstellationsMap)
    {
        if (!pair.second.isEnabled) {
            continue;
        }

        _program->setUniform(
            _uniformCache.color,
            _constellationColorMap[pair.second.colorIndex]
        );

        glBindVertexArray(pair.second.vaoArray);

        glLineWidth(_lineWidth);
        glDrawArrays(GL_LINE_STRIP, 0, pair.second.numV);
        global::renderEngine->openglStateCache().resetLineState();
    }

    glBindVertexArray(0);
    _program->deactivate();

    // Restores GL State
    global::renderEngine->openglStateCache().resetDepthState();
    global::renderEngine->openglStateCache().resetBlendState();
}

void RenderableConstellationLines::render(const RenderData& data, RendererTasks& tasks) {
    const glm::dmat4 modelMatrix =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    const glm::dmat4 modelViewMatrix = data.camera.combinedViewMatrix() * modelMatrix;
    const glm::dmat4 projectionMatrix = data.camera.projectionMatrix();

    if (_hasSpeckFile) {
        renderConstellations(data, modelViewMatrix, projectionMatrix);
    }

    RenderableConstellation::render(data, tasks);
}

void RenderableConstellationLines::update(const UpdateData&) {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
    }
}

bool RenderableConstellationLines::loadData() {
    bool success = false;
    if (_hasSpeckFile) {
        LINFO(fmt::format("Loading Speck file {}", std::filesystem::path(_speckFile)));
        success = readSpeckFile();
        if (!success) {
            return false;
        }
    }

    return success;
}

bool RenderableConstellationLines::readSpeckFile() {
    std::ifstream file(_speckFile);
    if (!file.good()) {
        LERROR(fmt::format(
            "Failed to open Speck file {}", std::filesystem::path(_speckFile)
        ));
        return false;
    }

    const float scale = static_cast<float>(toMeter(_constellationUnit));
    double maxRadius = 0.0;

    int lineIndex = 0;

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line;
    while (true) {
        std::getline(file, line);

        if (file.eof()) {
            break;
        }

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (!line.empty() && line.back() == '\r') {
            line = line.substr(0, line.length() - 1);
        }

        if (line.empty() || line[0] == '#') {
            continue;
        }

        std::size_t found = line.find("mesh");
        if (found == std::string::npos) {
            continue;
        }
        else {
            // mesh lines are structured as follows:
            // mesh -c colorindex {
            // colorindex is the index of the color for the mesh
            std::stringstream str(line);

            ConstellationLine constellationLine;
            constellationLine.lineIndex = lineIndex;

            std::string dummy;
            str >> dummy; // mesh command
            dummy.clear();
            str >> dummy; // color index command?
            do {
                if (dummy == "-c") {
                    str >> constellationLine.colorIndex; // color index command
                }
                dummy.clear();
                str >> dummy;
            } while (dummy != "{");

            std::getline(file, line);

            // Read the identifier
            std::stringstream id(line);
            std::string identifier;

            id >> dummy; // id command
            dummy.clear();
            std::getline(id, identifier); // identifier
            ghoul::trimWhitespace(identifier);
            std::string name = constellationFullName(identifier);
            if (!name.empty()) {
                constellationLine.name = name;
            }

            // Read the number of vertices
            std::getline(file, line);
            std::stringstream dim(line);
            dim >> constellationLine.numV;

            // We can now read the vertices data:
            for (int l = 0; l < constellationLine.numV; ++l) {
                std::getline(file, line);
                if (line.substr(0, 1) == "}") {
                    break;
                }

                std::stringstream lineData(line);

                // Try to read three values for the position
                glm::vec3 pos;
                bool success = true;
                for (int i = 0; i < 3; ++i) {
                    GLfloat value;
                    lineData >> value;
                    bool errorReading = lineData.rdstate() & std::ifstream::failbit;
                    if (errorReading) {
                        success = false;
                        break;
                    }

                    GLfloat scaledValue = value * scale;
                    pos[i] = scaledValue;
                    constellationLine.vertices.push_back(scaledValue);
                }

                if (!success) {
                    LERROR(fmt::format(
                        "Failed reading position on line {} of mesh {} in file: '{}'. "
                        "Stopped reading constellation data", l, lineIndex, _speckFile
                    ));
                    break;
                }

                // Check if new max radius
                const double r = glm::length(glm::dvec3(pos));
                maxRadius = std::max(maxRadius, r);
            }

            std::getline(file, line);
            if (line.substr(0, 1) == "}") {
                _renderingConstellationsMap.insert({ lineIndex++, constellationLine });
            }
            else {
                return false;
            }
        }
    }
    setBoundingSphere(maxRadius);

    return true;
}

void RenderableConstellationLines::createConstellations() {
    if (!(_dataIsDirty && _hasSpeckFile)) {
        return;
    }
    LDEBUG("Creating constellations");

    for (std::pair<const int, ConstellationLine>& p : _renderingConstellationsMap) {
        GLuint vao;
        glGenVertexArrays(1, &vao);
        p.second.vaoArray = vao;

        GLuint vbo;
        glGenBuffers(1, &vbo);
        p.second.vboArray = vbo;

        glBindVertexArray(vao);
        glBindBuffer(GL_ARRAY_BUFFER, vbo);
        glBufferData(
            GL_ARRAY_BUFFER,
            p.second.vertices.size() * sizeof(GLfloat),
            p.second.vertices.data(),
            GL_STATIC_DRAW
        );
        // in_position
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);
    }

    glBindVertexArray(0);

    _dataIsDirty = false;
}

} // namespace openspace
