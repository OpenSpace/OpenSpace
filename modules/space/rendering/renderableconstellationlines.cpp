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

#include <modules/digitaluniverse/digitaluniversemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/misc/misc.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <array>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <optional>

namespace {
    constexpr std::string_view _loggerCat = "RenderableConstellationLines";

    constexpr std::array<const char*, 4> UniformNames = {
        "modelViewTransform", "projectionTransform", "alphaValue", "color"
    };

    constexpr int RenderOptionViewDirection = 0;
    constexpr int RenderOptionPositionNormal = 1;

    constexpr openspace::properties::Property::PropertyInfo DrawElementsInfo = {
        "DrawElements",
        "Draw Elements",
        "Enables/Disables the drawing of the astronomical objects"
    };

    constexpr openspace::properties::Property::PropertyInfo MeshColorInfo = {
        "MeshColor",
        "Meshes colors",
        "The defined colors for the meshes to be rendered"
    };

    struct [[codegen::Dictionary(RenderableConstellationLines)]] Parameters {
        // The path to the SPECK file that contains information about the astronomical
        // object being rendered
        std::string file;

        enum class [[codegen::map(openspace::DistanceUnit)]] Unit {
            Meter [[codegen::key("m")]],
            Kilometer [[codegen::key("Km")]],
            Parsec [[codegen::key("pc")]],
            Kiloparsec [[codegen::key("Kpc")]],
            Megaparsec [[codegen::key("Mpc")]],
            Gigaparsec [[codegen::key("Gpc")]],
            Gigalightyear [[codegen::key("Gly")]]
        };
        std::optional<Unit> unit;

        // [[codegen::verbatim(MeshColorInfo.description)]]
        std::optional<std::vector<glm::vec3>> meshColor;
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

    _speckFile = absPath(p.file).string();
    _hasSpeckFile = true;
    _drawElements.onChange([&]() { _hasSpeckFile = !_hasSpeckFile; });
    addProperty(_drawElements);

    if (p.unit.has_value()) {
        _unit = codegen::map<DistanceUnit>(*p.unit);
    }
    else {
        _unit = DistanceUnit::Meter;
    }

    if (p.meshColor.has_value()) {
        std::vector<glm::vec3> ops = *p.meshColor;
        for (size_t i = 0; i < ops.size(); ++i) {
            _meshColorMap.insert({ static_cast<int>(i) + 1, ops[i] });
        }
    }
}

void RenderableConstellationLines::selectionPropertyHasChanged() {
    // If no values are selected (the default), we want to show all constellations
    if (!_constellationSelection.hasSelected()) {
        for (std::pair<const int, RenderingMesh>& pair : _renderingMeshesMap) {
            pair.second.isEnabled = true;
        }
    }
    else {
        // Enable all constellations that are selected
        for (std::pair<const int, RenderingMesh>& pair : _renderingMeshesMap) {
            pair.second.isEnabled =
                _constellationSelection.isSelected(pair.second.identifier);
        }
    }
}

bool RenderableConstellationLines::isReady() const {
    return (_program != nullptr) && !_renderingMeshesMap.empty() &&
        !_labelset.entries.empty();
}

void RenderableConstellationLines::initialize() {
    RenderableConstellation::initialize();

    bool success = loadData();
    if (!success) {
        throw ghoul::RuntimeError("Error loading data");
    }
}

void RenderableConstellationLines::initializeGL() {
    _program = DigitalUniverseModule::ProgramObjectManager.request(
        "RenderableConstellationLines",
        []() {
            return global::renderEngine->buildRenderProgram(
                "RenderableConstellationLines",
                absPath("${MODULE_SPACE}/shaders/constellationlines_vs.glsl"),
                absPath("${MODULE_SPACE}/shaders/constellationlines_fs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);

    createMeshes();
}

void RenderableConstellationLines::deinitialize() {
}

void RenderableConstellationLines::deinitializeGL() {
    for (const std::pair<const int, RenderingMesh>& pair : _renderingMeshesMap) {
        for (int i = 0; i < pair.second.numU; ++i) {
            glDeleteVertexArrays(1, &pair.second.vaoArray[i]);
            glDeleteBuffers(1, &pair.second.vboArray[i]);
        }
    }

    DigitalUniverseModule::ProgramObjectManager.release(
        "RenderableConstellationLines",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
}

void RenderableConstellationLines::renderMeshes(const RenderData&,
                                                const glm::dmat4& modelViewMatrix,
                                                const glm::dmat4& projectionMatrix)
{
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    //glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDepthMask(false);
    glEnable(GL_DEPTH_TEST);

    _program->activate();

    _program->setUniform(_uniformCache.modelViewTransform, modelViewMatrix);
    _program->setUniform(_uniformCache.projectionTransform, projectionMatrix);
    _program->setUniform(_uniformCache.alphaValue, opacity());

    for (const std::pair<const int, RenderingMesh>& pair : _renderingMeshesMap) {
        if (!pair.second.isEnabled) {
            continue;
        }

        _program->setUniform(_uniformCache.color, _meshColorMap[pair.second.colorIndex]);
        for (size_t i = 0; i < pair.second.vaoArray.size(); ++i) {
            glBindVertexArray(pair.second.vaoArray[i]);
            switch (pair.second.style) {
                case Solid:
                    break;
                case Wire:
                    glLineWidth(_lineWidth);
                    glDrawArrays(GL_LINE_STRIP, 0, pair.second.numV);
                    global::renderEngine->openglStateCache().resetLineState();
                    break;
                case Point:
                    glDrawArrays(GL_POINTS, 0, pair.second.numV);
                    break;
                default:
                    break;
            }
        }
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
        renderMeshes(data, modelViewMatrix, projectionMatrix);
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

    const float scale = static_cast<float>(toMeter(_unit));
    double maxRadius = 0.0;

    int meshIndex = 0;

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
            // mesh -t texnum -c colorindex -s style {
            // where textnum is the index of the texture;
            // colorindex is the index of the color for the mesh
            // and style is solid, wire or point (for now we support only wire)
            std::stringstream str(line);

            RenderingMesh mesh;
            mesh.meshIndex = meshIndex;

            std::string dummy;
            str >> dummy; // mesh command
            dummy.clear();
            str >> dummy; // texture index command?
            do {
                if (dummy == "-t") {
                    dummy.clear();
                    str >> mesh.textureIndex; // texture index
                }
                else if (dummy == "-c") {
                    dummy.clear();
                    str >> mesh.colorIndex; // color index command
                }
                else if (dummy == "-s") {
                    dummy.clear();
                    str >> dummy; // style value command
                    if (dummy == "solid") {
                        mesh.style = Solid;
                    }
                    else if (dummy == "wire") {
                        mesh.style = Wire;
                    }
                    else if (dummy == "point") {
                        mesh.style = Point;
                    }
                    else {
                        mesh.style = INVALID;
                        break;
                    }
                }
                dummy.clear();
                str >> dummy;
            } while (dummy != "{");

            std::getline(file, line);
            std::stringstream dimOrName(line);
            std::string dummyU, dummyV;

            // Try to read name of mesh if it exist
            dimOrName >> dummyU;             // numU or "id"
            std::getline(dimOrName, dummyV); // numV or the identifier of the mesh

            if (dummyU == "id") {
                ghoul::trimWhitespace(dummyV);
                mesh.identifier = constellationFullName(dummyV);

                // Dimensions are specified in the next line as usual
                std::getline(file, line);
                std::stringstream dim(line);
                dim >> mesh.numU; // numU
                dim >> mesh.numV; // numV
            }
            else {
                mesh.numU = stoi(dummyU);
                mesh.numV = stoi(dummyV);
            }

            // We can now read the vertices data:
            for (int l = 0; l < mesh.numU * mesh.numV; ++l) {
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
                    mesh.vertices.push_back(scaledValue);
                }

                if (!success) {
                    LERROR(fmt::format(
                        "Failed reading position on line {} of mesh {} in file: '{}'. "
                        "Stopped reading mesh data", l, meshIndex, _speckFile
                    ));
                    break;
                }

                // Check if new max radius
                const double r = glm::length(glm::dvec3(pos));
                maxRadius = std::max(maxRadius, r);

                // OLD CODE:
                // (2022-03-23, emmbr)  None of our files included texture coordinates,
                // and if they would they would still not be used by the shader
                //for (int i = 0; i < 7; ++i) {
                //    GLfloat value;
                //    lineData >> value;
                //    bool errorReading = lineData.rdstate() & std::ifstream::failbit;
                //    if (!errorReading) {
                //        mesh.vertices.push_back(value);
                //    }
                //    else {
                //        break;
                //    }
                //}
            }

            std::getline(file, line);
            if (line.substr(0, 1) == "}") {
                _renderingMeshesMap.insert({ meshIndex++, mesh });
            }
            else {
                return false;
            }
        }
    }
    setBoundingSphere(maxRadius);

    return true;
}

void RenderableConstellationLines::createMeshes() {
    if (!(_dataIsDirty && _hasSpeckFile)) {
        return;
    }
    LDEBUG("Creating planes");

    for (std::pair<const int, RenderingMesh>& p : _renderingMeshesMap) {
        for (int i = 0; i < p.second.numU; ++i) {
            GLuint vao;
            glGenVertexArrays(1, &vao);
            p.second.vaoArray.push_back(vao);

            GLuint vbo;
            glGenBuffers(1, &vbo);
            p.second.vboArray.push_back(vbo);

            glBindVertexArray(vao);
            glBindBuffer(GL_ARRAY_BUFFER, vbo);
            //glBufferData(GL_ARRAY_BUFFER, it->second.numV * sizeof(GLfloat),
            glBufferData(
                GL_ARRAY_BUFFER,
                p.second.vertices.size() * sizeof(GLfloat),
                &p.second.vertices[0],
                GL_STATIC_DRAW
            );
            // in_position
            glEnableVertexAttribArray(0);
            // (2022-03-23, emmbr) This code was actually never used. We only read three
            // values per line and did not handle any texture cooridnates, even if there
            // would have been some in the file
            //// U and V may not be given by the user
            //if (p.second.vertices.size() / (p.second.numU * p.second.numV) > 3) {
            //    glVertexAttribPointer(
            //        0,
            //        3,
            //        GL_FLOAT,
            //        GL_FALSE,
            //        sizeof(GLfloat) * 5,
            //        reinterpret_cast<GLvoid*>(sizeof(GLfloat) * i * p.second.numV)
            //    );

            //    // texture coords
            //    glEnableVertexAttribArray(1);
            //    glVertexAttribPointer(
            //        1,
            //        2,
            //        GL_FLOAT,
            //        GL_FALSE,
            //        sizeof(GLfloat) * 7,
            //        reinterpret_cast<GLvoid*>(sizeof(GLfloat) * 3 * i * p.second.numV)
            //    );
            //}
            //else { // no U and V:
                glVertexAttribPointer(
                    0,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    0,
                    reinterpret_cast<GLvoid*>(sizeof(GLfloat) * 3 * i * p.second.numV)
                );
            //}
        }

        // Grid: we need columns
        if (p.second.numU > 1) {
            for (int i = 0; i < p.second.numV; ++i) {
                GLuint cvao;
                glGenVertexArrays(1, &cvao);
                p.second.vaoArray.push_back(cvao);

                GLuint cvbo;
                glGenBuffers(1, &cvbo);
                p.second.vboArray.push_back(cvbo);

                glBindVertexArray(cvao);
                glBindBuffer(GL_ARRAY_BUFFER, cvbo);
                glBufferData(
                    GL_ARRAY_BUFFER,
                    p.second.vertices.size() * sizeof(GLfloat),
                    &p.second.vertices[0],
                    GL_STATIC_DRAW
                );
                // in_position
                glEnableVertexAttribArray(0);
                // U and V may not be given by the user
                if (p.second.vertices.size() / (p.second.numU * p.second.numV) > 3) {
                    glVertexAttribPointer(
                        0,
                        3,
                        GL_FLOAT,
                        GL_FALSE,
                        p.second.numV * sizeof(GLfloat) * 5,
                        reinterpret_cast<GLvoid*>(sizeof(GLfloat) * i)
                    );

                    // texture coords
                    glEnableVertexAttribArray(1);
                    glVertexAttribPointer(
                        1,
                        2,
                        GL_FLOAT,
                        GL_FALSE,
                        p.second.numV * sizeof(GLfloat) * 7,
                        reinterpret_cast<GLvoid*>(sizeof(GLfloat) * 3 * i)
                    );
                }
                else { // no U and V:
                    glVertexAttribPointer(
                        0,
                        3,
                        GL_FLOAT,
                        GL_FALSE,
                        p.second.numV * sizeof(GLfloat) * 3,
                        reinterpret_cast<GLvoid*>(sizeof(GLfloat) * 3 * i)
                    );
                }
            }
        }
    }

    glBindVertexArray(0);

    _dataIsDirty = false;
}

} // namespace openspace
