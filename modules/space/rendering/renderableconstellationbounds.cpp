/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/filesystem/filesystem.h>

#include <fstream>
#define _USE_MATH_DEFINES
#include <math.h>

#include "SpiceUsr.h"
#include "SpiceZpr.h"

namespace {
    const char* KeyVertexFile = "File";
    const char* KeyConstellationFile = "ConstellationFile";
    const char* KeyReferenceFrame = "ReferenceFrame";


    const char* DefaultReferenceFrame = "J2000";

    float deg2rad(float deg) {
        return static_cast<float>((deg / 360.f) * 2.f * M_PI);
    }
    float convertHrsToRadians(float rightAscension) {
        // 360 degrees / 24h = 15 degrees/h
        return deg2rad(rightAscension * 15);
    }
} // namespace

namespace openspace {

documentation::Documentation RenderableConstellationBounds::Documentation() {
    using namespace documentation;
    return {
        "RenderableConstellationBounds",
        "space_renderable_constellationbounds",
        {
            {
                KeyVertexFile,
                new StringVerifier,
                "Specifies the file containing the bounds information about the "
                "constellation locations.",
                Optional::No
            },
            {
                KeyConstellationFile,
                new StringVerifier,
                "Specifies the file that contains the mapping between constellation "
                "abbreviations and full name of the constellation. If the file is "
                "omitted, the abbreviations are used as the full names.",
                Optional::Yes
            }
        }
    };
}


RenderableConstellationBounds::RenderableConstellationBounds(
    const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _vertexFilename("")
    , _constellationFilename("")
    , _distance({ "Distance", "Distance to the celestial Sphere", "" }, 15.f, 0.f, 30.f) // @TODO Missing documentation
    , _constellationSelection({ "ConstellationSelection", "Constellation Selection", "" }) // @TODO Missing documentation
    , _vao(0)
    , _vbo(0)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableConstellationBounds"
    );

    _vertexFilename = dictionary.value<std::string>(KeyVertexFile);

    if (dictionary.hasKey(KeyConstellationFile)) {
        _constellationFilename = dictionary.value<std::string>(KeyConstellationFile);
    }

    addProperty(_distance);
    addProperty(_constellationSelection);
    _constellationSelection.onChange(
        [this]() { selectionPropertyHasChanged(); }
    );
}

bool RenderableConstellationBounds::initialize() {
    _program = OsEng.renderEngine().buildRenderProgram("ConstellationBounds",
        "${MODULE_SPACE}/shaders/constellationbounds_vs.glsl",
        "${MODULE_SPACE}/shaders/constellationbounds_fs.glsl");

    bool loadSuccess = loadVertexFile();
    if (!loadSuccess) {
        return false;
    }
    loadSuccess = loadConstellationFile();
    if (!loadSuccess) {
        return false;
    }

    fillSelectionProperty();

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
    glVertexAttribPointer(positionAttrib, 3, GL_FLOAT, GL_FALSE, 0, 0);

    glBindVertexArray(0);

    return true;
}

bool RenderableConstellationBounds::deinitialize() {
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;

    if (_program) {
        OsEng.renderEngine().removeRenderProgram(_program);
        _program = nullptr;
    }

    return true;
}

bool RenderableConstellationBounds::isReady() const {
    return (_vao != 0) && (_vbo != 0) && _program;
}

void RenderableConstellationBounds::render(const RenderData& data, RendererTasks&) {
    _program->activate();

    setPscUniforms(*_program.get(), data.camera, data.position);

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));


    _program->setUniform("exponent", _distance);
    _program->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _program->setUniform("ModelTransform", glm::mat4(modelTransform));

    glBindVertexArray(_vao);
    for (const ConstellationBound& bound : _constellationBounds) {
        if (bound.isEnabled) {
            glDrawArrays(
                GL_LINE_LOOP,
                static_cast<GLsizei>(bound.startIndex),
                static_cast<GLsizei>(bound.nVertices)
            );
        }
    }
    glBindVertexArray(0);
    _program->deactivate();
}

bool RenderableConstellationBounds::loadVertexFile() {
    if (_vertexFilename.empty()) {
        return false;
    }

    std::string fileName = absPath(_vertexFilename);
    std::ifstream file;
    file.exceptions(std::ifstream::goodbit);
    file.open(fileName);

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
                "Error reading file '" << fileName << "' at line #" << currentLineNumber
            );
            break;
        }

        // Did we arrive at a new constellation?
        if (constellationName != currentBound.constellationAbbreviation) {
            // Store how many vertices we read during the active time of the constellation
            currentBound.nVertices = (_vertexValues.size() - currentBound.startIndex);
            // Store the constellation and start a new one
            _constellationBounds.push_back(currentBound);
            currentBound = ConstellationBound();
            currentBound.isEnabled = true;
            currentBound.constellationAbbreviation = constellationName;
            currentBound.constellationFullName = constellationName;
            currentBound.startIndex = _vertexValues.size();
        }

        // The file format stores the right ascension in hours, while SPICE expects them
        // to be in radians
        ra = convertHrsToRadians(ra);

        // Likewise, the declination is stored in degrees and needs to be converted
        dec = deg2rad(dec);

        // Convert the (right ascension, declination) to rectangular coordinates)
        // The 1.0 is the distance of the celestial sphere, we will scale that in the
        // render function
        double rectangularValues[3];
        radrec_c(1.0, ra, dec, rectangularValues);

        // Add the new vertex to our list of vertices
        _vertexValues.push_back({{
            static_cast<float>(rectangularValues[0]),
            static_cast<float>(rectangularValues[1]),
            static_cast<float>(rectangularValues[2])
        }});
        ++currentLineNumber;
    }

    // Due to the way we read the file, the first (empty) constellation bounds will not
    // contain any valid values. So we have to remove it
    _constellationBounds.erase(_constellationBounds.begin());

    // And we still have the one value that was left when we exited the loop
    currentBound.nVertices = (_vertexValues.size() - currentBound.startIndex);
    _constellationBounds.push_back(currentBound);

    return true;
}

bool RenderableConstellationBounds::loadConstellationFile() {
    if (_constellationFilename.empty()) {
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

        auto it = std::find_if(
            _constellationBounds.begin(),
            _constellationBounds.end(),
            [abbreviation](const ConstellationBound& bound) {
                return bound.constellationAbbreviation == abbreviation;
            }
        );
        if (it == _constellationBounds.end()) {
            LERRORC(
                "RenderableConstellationBounds",
                "Could not find constellation '" << abbreviation << "' in list"
            );
            return false;
        }

        // Update the constellations full name
        s >> it->constellationFullName;
        ++index;
    }

    return true;
}

void RenderableConstellationBounds::fillSelectionProperty() {
    // Each constellation is associated with its position in the array as this is unique
    // and will be constant during the runtime
    for (int i = 0 ; i < static_cast<int>(_constellationBounds.size()); ++i) {
        const ConstellationBound& bound = _constellationBounds[i];
        _constellationSelection.addOption( { i, bound.constellationFullName } );
    }
}

void RenderableConstellationBounds::selectionPropertyHasChanged() {
    const std::vector<int>& values = _constellationSelection;
    // If no values are selected (the default), we want to show all constellations
    if (values.size() == 0) {
        for (ConstellationBound& b : _constellationBounds) {
            b.isEnabled = true;
        }
    }
    else {
        // In the worst case, this algorithm runs with 2 * nConstellations, which is
        // acceptable as the number of constellations is < 100
        // First disable all constellations
        for (ConstellationBound& b : _constellationBounds) {
            b.isEnabled = false;
        }
        // then re-enable the ones for which we have indices
        for (int value : values) {
            _constellationBounds[value].isEnabled = true;
        }
    }
}

} // namespace openspace
