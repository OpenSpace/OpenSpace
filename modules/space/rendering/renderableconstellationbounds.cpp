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

#include <modules/space/rendering/renderableconstellationbounds.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/misc.h>
#include <ghoul/opengl/programobject.h>
#include <fstream>
#include <optional>
#include "SpiceUsr.h"

namespace {
    constexpr float convertHrsToRadians(float rightAscension) {
        // 360 degrees / 24h = 15 degrees/h
        return glm::radians(rightAscension * 15);
    }

    constexpr openspace::properties::Property::PropertyInfo VertexInfo = {
        "File",
        "Vertex File Path",
        "The file pointed to with this value contains the vertex locations of the "
        "constellations"
    };

    constexpr openspace::properties::Property::PropertyInfo ConstellationInfo = {
        "ConstellationFile",
        "Constellation File Path",
        "Specifies the file that contains the mapping between constellation "
        "abbreviations and full name of the constellation. If this value is empty, the "
        "abbreviations are used as the full names"
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color of constellation lines",
        "Specifies the color of the constellation lines. The lines are always drawn at "
        "full opacity"
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "The line width of the constellation bounds"
    };

    constexpr openspace::properties::Property::PropertyInfo SelectionInfo = {
        "ConstellationSelection",
        "Constellation Selection",
        "The constellations that are selected are displayed on the celestial sphere"
    };

    struct [[codegen::Dictionary(RenderableConstellationBounds)]] Parameters {
        // [[codegen::verbatim(VertexInfo.description)]]
        std::string file;

        // [[codegen::verbatim(ConstellationInfo.description)]]
        std::optional<std::string> constellationFile;

        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(SelectionInfo.description)]]
        std::optional<std::vector<std::string>> constellationSelection;
    };
#include "renderableconstellationbounds_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableConstellationBounds::Documentation() {
    return codegen::doc<Parameters>("space_renderable_constellationbounds");
}

RenderableConstellationBounds::RenderableConstellationBounds(
    const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _vertexFilename(VertexInfo)
    , _constellationFilename(ConstellationInfo)
    , _color(ColorInfo, glm::vec3(1.f, 0.f, 0.f), glm::vec3(0.f), glm::vec3(1.f))
    , _lineWidth(LineWidthInfo, 2.f, 1.f, 32.f)
    , _constellationSelection(SelectionInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _vertexFilename.onChange([&](){ loadVertexFile(); });
    addProperty(_vertexFilename);
    _vertexFilename = p.file;

    _constellationFilename.onChange([&](){ loadConstellationFile(); });
    _constellationFilename = p.constellationFile.value_or(_constellationFilename);
    addProperty(_constellationFilename);

    _color.setViewOption(properties::Property::ViewOptions::Color);
    _color = p.color.value_or(_color);
    addProperty(_color);

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    fillSelectionProperty();
    _constellationSelection.onChange([this]() { selectionPropertyHasChanged(); });
    addProperty(_constellationSelection);

    if (p.constellationSelection.has_value()) {
        const std::vector<std::string> options = _constellationSelection.options();

        std::set<std::string> selectedNames;
        for (const std::string& s : *p.constellationSelection) {
            const auto it = std::find(options.begin(), options.end(), s);
            if (it == options.end()) {
                // The user has specified a constellation name that doesn't exist
                LWARNINGC(
                    "RenderableConstellationBounds",
                    fmt::format("Option '{}' not found in list of constellations", s)
                );
            }
            else {
                selectedNames.insert(s);
            }
        }
        _constellationSelection = selectedNames;
    }
}

void RenderableConstellationBounds::initializeGL() {
    _program = global::renderEngine->buildRenderProgram(
        "ConstellationBounds",
        absPath("${MODULE_SPACE}/shaders/constellationbounds_vs.glsl"),
        absPath("${MODULE_SPACE}/shaders/constellationbounds_fs.glsl")
    );

    glGenVertexArrays(1, &_vao);
    glBindVertexArray(_vao);

    glGenBuffers(1, &_vbo);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexValues.size() * 3 * sizeof(float),
        &_vertexValues[0],
        GL_STATIC_DRAW
    );

    GLint positionAttrib = _program->attributeLocation("in_position");
    glEnableVertexAttribArray(positionAttrib);
    glVertexAttribPointer(positionAttrib, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

    glBindVertexArray(0);
}

void RenderableConstellationBounds::deinitializeGL() {
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;

    if (_program) {
        global::renderEngine->removeRenderProgram(_program.get());
        _program = nullptr;
    }
}

bool RenderableConstellationBounds::isReady() const {
    return (_vao != 0) && (_vbo != 0) && _program;
}

void RenderableConstellationBounds::render(const RenderData& data, RendererTasks&) {
    _program->activate();

    _program->setUniform("campos", glm::vec4(data.camera.positionVec3(), 1.f));
    _program->setUniform("objpos", glm::vec4(data.modelTransform.translation, 0.f));
    _program->setUniform("camrot", glm::mat4(data.camera.viewRotationMatrix()));
    _program->setUniform("scaling", glm::vec2(1.f, 0.f));

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));


    _program->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _program->setUniform("ModelTransform", glm::mat4(modelTransform));
    _program->setUniform("color", _color);

    glLineWidth(_lineWidth);

    glBindVertexArray(_vao);
    for (const ConstellationBound& bound : _constellationBounds) {
        if (bound.isEnabled) {
            glDrawArrays(GL_LINE_LOOP, bound.startIndex, bound.nVertices);
        }
    }
    glBindVertexArray(0);
    _program->deactivate();
}

bool RenderableConstellationBounds::loadVertexFile() {
    if (_vertexFilename.value().empty()) {
        return false;
    }

    std::filesystem::path fileName = absPath(_vertexFilename);
    std::ifstream file;
    file.open(fileName);
    if (!file.good()) {
        return false;
    }

    ConstellationBound currentBound;
    currentBound.constellationAbbreviation = "";

    std::string currentLine;
    int currentLineNumber = 1;

    // Overview of the reading algorithm:
    // We keep an active ConstellationBound (currentBound) and update it until we read
    // a new constellation name, at which point the currentBound is stored away, a new,
    // empty ConstellationBound is created and set at the currentBound
    while (file.good()) {
        std::getline(file, currentLine);
        if (currentLine.empty()) {
            continue;
        }

        // @CHECK: Is this the best way of doing this? ---abock
        std::stringstream s(currentLine);
        float ra;
        s >> ra;

        float dec;
        s >> dec;

        std::string constellationName;
        s >> constellationName;

        if (!s.good()) {
            // If this evaluates to true, the stream was not completely filled, which
            // means that the line was incomplete, so there was an error
            LERRORC(
                "RenderableConstellationBounds",
                fmt::format(
                    "Error reading file {} at line #{}", fileName, currentLineNumber
                )
            );
            break;
        }

        // Did we arrive at a new constellation?
        if (constellationName != currentBound.constellationAbbreviation) {
            // Store how many vertices we read during the active time of the constellation
            currentBound.nVertices = static_cast<GLsizei>(
                _vertexValues.size() - currentBound.startIndex
            );
            // Store the constellation and start a new one
            _constellationBounds.push_back(currentBound);
            currentBound = ConstellationBound();
            currentBound.isEnabled = true;
            currentBound.constellationAbbreviation = constellationName;
            currentBound.constellationFullName = constellationName;
            currentBound.startIndex = static_cast<GLsizei>(_vertexValues.size());
        }

        // The file format stores the right ascension in hours, while SPICE expects them
        // to be in radians
        ra = convertHrsToRadians(ra);

        // Likewise, the declination is stored in degrees and needs to be converted
        dec = glm::radians(dec);

        // Convert the (right ascension, declination) to rectangular coordinates)
        // The 1.0 is the distance of the celestial sphere, we will scale that in the
        // render function
        double rectangularValues[3];
        radrec_c(1.0, ra, dec, rectangularValues);

        // Add the new vertex to our list of vertices
        _vertexValues.push_back({
            static_cast<float>(rectangularValues[0]),
            static_cast<float>(rectangularValues[1]),
            static_cast<float>(rectangularValues[2])
        });
        ++currentLineNumber;
    }

    // Due to the way we read the file, the first (empty) constellation bounds will not
    // contain any valid values. So we have to remove it
    _constellationBounds.erase(_constellationBounds.begin());

    // And we still have the one value that was left when we exited the loop
    currentBound.nVertices = static_cast<GLsizei>(
        _vertexValues.size() - currentBound.startIndex
    );
    _constellationBounds.push_back(currentBound);

    return true;
}

bool RenderableConstellationBounds::loadConstellationFile() {
    if (_constellationFilename.value().empty()) {
        return true;
    }

    std::ifstream file;
    file.exceptions(std::ifstream::goodbit);
    file.open(absPath(_constellationFilename));

    std::string line;
    int index = 0;
    while (file.good()) {
        std::getline(file, line);
        if (line.empty()) {
            continue;
        }

        std::string abbreviation;
        std::stringstream s(line);
        s >> abbreviation;

        const auto it = std::find_if(
            _constellationBounds.begin(),
            _constellationBounds.end(),
            [abbreviation](const ConstellationBound& bound) {
                return bound.constellationAbbreviation == abbreviation;
            }
        );
        if (it == _constellationBounds.end()) {
            LERRORC(
                "RenderableConstellationBounds",
                fmt::format("Could not find constellation '{}' in list", abbreviation)
            );
            return false;
        }

        // Update the constellations full name
        std::string fullName;
        std::getline(s, fullName);
        ghoul::trimWhitespace(fullName);
        it->constellationFullName = std::move(fullName);

        ++index;
    }

    return true;
}

void RenderableConstellationBounds::fillSelectionProperty() {
    for (int i = 0 ; i < static_cast<int>(_constellationBounds.size()); ++i) {
        const ConstellationBound& bound = _constellationBounds[i];
        _constellationSelection.addOption(bound.constellationFullName);
    }
}

void RenderableConstellationBounds::selectionPropertyHasChanged() {
    // If no values are selected (the default), we want to show all constellations
    if (!_constellationSelection.hasSelected()) {
        for (ConstellationBound& b : _constellationBounds) {
            b.isEnabled = true;
        }
    }
    else {
        // Enable all constellations that are selected
        for (ConstellationBound& b : _constellationBounds) {
            b.isEnabled = _constellationSelection.isSelected(b.constellationFullName);
        }
    }
}

} // namespace openspace
