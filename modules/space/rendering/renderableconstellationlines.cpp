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

#include <modules/space/rendering/renderableconstellationlines.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <scn/scan.h>
#include <array>
#include <filesystem>
#include <fstream>
#include <optional>

namespace {
    constexpr std::string_view _loggerCat = "RenderableConstellationLines";

    constexpr std::array<const char*, 4> UniformNames = {
        "modelViewTransform", "projectionTransform", "opacity", "color"
    };

    constexpr openspace::properties::Property::PropertyInfo SpeckInfo = {
        "File",
        "Constellation Data File Path",
        "The file that contains the data for the constellation lines",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DrawElementsInfo = {
        "DrawElements",
        "Draw Elements",
        "Enables/Disables the drawing of the constellations",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo UnitInfo = {
        "Unit",
        "Unit",
        "The distance unit used for the constellation lines data",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorsInfo = {
        "Colors",
        "Constellation Colors",
        "The defined colors for the constellations to be rendered. There can be several "
        "groups of constellaitons that can have distinct colors.",
        openspace::properties::Property::Visibility::User
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
        // [[codegen::verbatim(UnitInfo.description)]]
        std::optional<Unit> unit;

        // [[codegen::verbatim(ColorsInfo.description)]]
        std::optional<std::vector<glm::vec3>> colors;
    };
#include "renderableconstellationlines_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation RenderableConstellationLines::Documentation() {
    return codegen::doc<Parameters>("space_renderable_constellationlines");
}

RenderableConstellationLines::RenderableConstellationLines(
                                                      const ghoul::Dictionary& dictionary)
    : RenderableConstellationsBase(dictionary)
    , _drawElements(DrawElementsInfo, true)
    , _speckFile(SpeckInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    // Avoid reading files here, instead do it in multithreaded initialize()
    _speckFile = absPath(p.file.string()).string();
    _speckFile.onChange([this]() { loadData(); });
    addProperty(_speckFile);

    addProperty(_drawElements);

    if (p.unit.has_value()) {
        _constellationUnit = codegen::map<DistanceUnit>(*p.unit);
    }
    else {
        _constellationUnit = DistanceUnit::Meter;
    }

    if (p.colors.has_value()) {
        std::vector<glm::vec3> ops = *p.colors;
        for (size_t i = 0; i < ops.size(); i++) {
            _constellationColorMap.insert({ static_cast<int>(i) + 1, ops[i] });
        }
    }
}

void RenderableConstellationLines::selectionPropertyHasChanged() {
    using ConstellationKeyValuePair = std::pair<const int, ConstellationLine>;

    // If no values are selected (the default), we want to show all constellations
    if (!_selection.hasSelected()) {
        for (ConstellationKeyValuePair& pair : _renderingConstellationsMap) {
            pair.second.isEnabled = true;
        }

        if (_hasLabels) {
            for (dataloader::Labelset::Entry& e : _labels->labelSet().entries) {
                e.isEnabled = true;
            }
        }
    }
    else {
        // Enable all constellations that are selected
        for (ConstellationKeyValuePair& pair : _renderingConstellationsMap) {
            const bool isSelected = _selection.isSelected(pair.second.name);
            pair.second.isEnabled = isSelected;

            if (_hasLabels) {
                for (dataloader::Labelset::Entry& e : _labels->labelSet().entries) {
                    if (constellationFullName(e.identifier) == pair.second.name) {
                        e.isEnabled = isSelected;
                        break;
                    }
                }
            }
        }
    }
}

bool RenderableConstellationLines::isReady() const {
    const bool isReady = _program && !_renderingConstellationsMap.empty();

    // If we have labels, they also need to be loaded
    if (_hasLabels) {
        return isReady && RenderableConstellationsBase::isReady();
    }
    return isReady;
}

void RenderableConstellationLines::initialize() {
    RenderableConstellationsBase::initialize();

    loadData();

    if (!_assetSelection.empty()) {
        const std::vector<std::string> options = _selection.options();
        std::set<std::string> selectedConstellations;

        for (const std::string& s : _assetSelection) {
            auto it = std::find(options.begin(), options.end(), s);
            if (it == options.end()) {
                // Test if the provided name was an identifier instead of the full name
                it = std::find(
                    options.begin(),
                    options.end(),
                    constellationFullName(s)
                );

                if (it == options.end()) {
                    // The user has specified a constellation name that doesn't exist
                    LWARNING(std::format(
                        "Option '{}' not found in list of constellations", s
                    ));
                }
                else {
                    selectedConstellations.insert(constellationFullName(s));
                }
            }
            else {
                selectedConstellations.insert(s);
            }
        }
        _selection = selectedConstellations;
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
    using ConstellationKeyValuePair = std::pair<const int, ConstellationLine>;
    for (const ConstellationKeyValuePair& pair : _renderingConstellationsMap)
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
    _program->setUniform(_uniformCache.opacity, opacity());

    using ConstellationKeyValuePair = std::pair<const int, ConstellationLine>;
    for (const ConstellationKeyValuePair& pair : _renderingConstellationsMap)
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
    const glm::dmat4 modelViewTransform = calcModelViewTransform(data);
    const glm::dmat4 projectionTransform = data.camera.projectionMatrix();

    if (_drawElements) {
        renderConstellations(data, modelViewTransform, projectionTransform);
    }

    RenderableConstellationsBase::render(data, tasks);
}

void RenderableConstellationLines::update(const UpdateData&) {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
    }
}

bool RenderableConstellationLines::loadData() {
    const bool success = readSpeckFile();
    if (!success) {
        throw ghoul::RuntimeError("Error loading data");
    }
    return success;
}

bool RenderableConstellationLines::readSpeckFile() {
    if (_speckFile.value().empty()) {
        return false;
    }
    std::filesystem::path fileName = absPath(_speckFile);

    LINFO(std::format("Loading Speck file '{}'", fileName));
    std::ifstream file(fileName);
    if (!file.good()) {
        LERROR(std::format("Failed to open Speck file '{}'", fileName));
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
        ghoul::getline(file, line);

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

        if (const size_t found = line.find("mesh");  found == std::string::npos) {
            continue;
        }

        // mesh lines are structured as follows:
        // mesh -c colorindex {
        // colorindex is the index of the color for the mesh
        std::stringstream str(line);

        ConstellationLine constellationLine;
        constellationLine.lineIndex = lineIndex;

        std::string dummy;
        str >> dummy; // mesh command
        dummy.clear();
        str >> dummy; // color index command
        do {
            if (dummy == "-c") {
                str >> constellationLine.colorIndex; // color index
            }
            else {
                LWARNING(std::format(
                    "Unknown command '{}' found in constellation file '{}'",
                    dummy, fileName
                ));
            }
            dummy.clear();
            str >> dummy;
        }
        while (dummy != "{");

        ghoul::getline(file, line);

        // Read the identifier
        std::stringstream id(line);
        std::string identifier;

        id >> dummy; // id command
        dummy.clear();
        ghoul::getline(id, identifier); // identifier
        ghoul::trimWhitespace(identifier);
        constellationLine.name = constellationFullName(identifier);

        // Read the number of vertices
        ghoul::getline(file, line);
        std::stringstream dim(line);
        dim >> constellationLine.numV;

        // We can now read the vertices data:
        for (int l = 0; l < constellationLine.numV; ++l) {
            ghoul::getline(file, line);
            if (line.substr(0, 1) == "}") {
                break;
            }

            // Try to read three values for the position
            glm::vec3 pos;
            auto reading = scn::scan<float, float, float>(line, "{} {} {}");
            if (reading) {
                std::tie(pos.x, pos.y, pos.z) = reading->values();
                pos *= scale;
                constellationLine.vertices.push_back(pos.x);
                constellationLine.vertices.push_back(pos.y);
                constellationLine.vertices.push_back(pos.z);
            }
            else {
                LERROR(std::format(
                    "Failed reading position on line {} of mesh {} in file '{}'. "
                    "Stopped reading constellation data", l, lineIndex, fileName
                ));
            }

            // Check if new max radius
            const double r = glm::length(glm::dvec3(pos));
            maxRadius = std::max(maxRadius, r);
        }

        ghoul::getline(file, line);
        if (line.substr(0, 1) == "}") {
            _renderingConstellationsMap.insert({ lineIndex++, constellationLine });
        }
        else {
            return false;
        }
    }
    setBoundingSphere(maxRadius);

    return true;
}

void RenderableConstellationLines::createConstellations() {
    LDEBUG("Creating constellations");

    for (std::pair<const int, ConstellationLine>& p : _renderingConstellationsMap) {
        GLuint vao = 0;
        glGenVertexArrays(1, &vao);
        p.second.vaoArray = vao;

        GLuint vbo = 0;
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
}

} // namespace openspace
