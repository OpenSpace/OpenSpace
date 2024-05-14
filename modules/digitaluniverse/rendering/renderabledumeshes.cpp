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

#include <modules/digitaluniverse/rendering/renderabledumeshes.h>

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
#include <ghoul/misc/stringhelper.h>
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
    constexpr std::string_view _loggerCat = "RenderableDUMeshes";

    constexpr int RenderOptionViewDirection = 0;
    constexpr int RenderOptionPositionNormal = 1;

    constexpr openspace::properties::Property::PropertyInfo TextColorInfo = {
        "TextColor",
        "Text Color",
        "The text color for the astronomical object.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo TextOpacityInfo = {
        "TextOpacity",
        "Text Opacity",
        "Determines the transparency of the text label, where 1 is completely opaque "
        "and 0 fully transparent.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo TextSizeInfo = {
        "TextSize",
        "Text Size",
        "The text size for the astronomical object labels.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LabelFileInfo = {
        "LabelFile",
        "Label File",
        "The path to the label file that contains information about the astronomical "
        "objects being rendered.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo LabelMinMaxSizeInfo = {
        "TextMinMaxSize",
        "Text Min/Max Size",
        "The minimum and maximum size (in pixels) of the text for the labels for the "
        "astronomical objects being rendered.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "If the DU mesh is of wire type, this value determines the width of the lines.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo DrawElementsInfo = {
        "DrawElements",
        "Draw Elements",
        "Enables/Disables the drawing of the astronomical objects.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo DrawLabelInfo = {
        "DrawLabels",
        "Draw Labels",
        "Determines whether labels should be drawn or hidden.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo MeshColorInfo = {
        "MeshColor",
        "Meshes colors",
        "The defined colors for the meshes to be rendered.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo RenderOptionInfo = {
        "RenderOption",
        "Render Option",
        "Debug option for rendering of billboards and texts.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableDUMeshes)]] Parameters {
        // The path to the SPECK file that contains information about the astronomical
        // object being rendered.
        std::string file;

        // [[codegen::verbatim(DrawLabelInfo.description)]]
        std::optional<bool> drawLabels;

        enum class [[codegen::map(openspace::DistanceUnit)]] Unit {
            Meter [[codegen::key("m")]],
            Kilometer [[codegen::key("Km")]],
            Parsec [[codegen::key("pc")]],
            Kiloparsec [[codegen::key("Kpc")]],
            Megaparsec [[codegen::key("Mpc")]],
            Gigaparsec [[codegen::key("Gpc")]],
            Gigalightyear [[codegen::key("Gly")]]
        };
        // The unit used when interpreting the positions in the dataset.
        std::optional<Unit> unit;

        // [[codegen::verbatim(TextColorInfo.description)]]
        std::optional<glm::vec3> textColor [[codegen::color()]];

        // [[codegen::verbatim(TextOpacityInfo.description)]]
        std::optional<float> textOpacity;

        // [[codegen::verbatim(TextSizeInfo.description)]]
        std::optional<float> textSize;

        // [[codegen::verbatim(LabelFileInfo.description)]]
        std::optional<std::string> labelFile;

        // [[codegen::verbatim(LabelMinMaxSizeInfo.description)]]
        std::optional<glm::ivec2> textMinMaxSize;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(MeshColorInfo.description)]]
        std::optional<std::vector<glm::vec3>> meshColor;
    };
#include "renderabledumeshes_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation RenderableDUMeshes::Documentation() {
    return codegen::doc<Parameters>("digitaluniverse_renderabledumeshes");
}

RenderableDUMeshes::RenderableDUMeshes(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _textColor(TextColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _textOpacity(TextOpacityInfo, 1.f, 0.f, 1.f)
    , _textSize(TextSizeInfo, 8.f, 0.5f, 24.f)
    , _drawElements(DrawElementsInfo, true)
    , _drawLabels(DrawLabelInfo, false)
    , _textMinMaxSize(
        LabelMinMaxSizeInfo,
        glm::ivec2(8, 500),
        glm::ivec2(0),
        glm::ivec2(1000)
    )
    , _lineWidth(LineWidthInfo, 2.f, 1.f, 16.f)
    , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    _speckFile = absPath(p.file);
    _drawElements.onChange([this]() { _hasSpeckFile = !_hasSpeckFile; });
    addProperty(_drawElements);

    _renderOption.addOption(RenderOptionViewDirection, "Camera View Direction");
    _renderOption.addOption(RenderOptionPositionNormal, "Camera Position Normal");
    // @TODO (abock. 2021-01-31) In the other DU classes, this is done with an enum, and
    // doing it based on the fisheye rendering seems a bit brittle?
    if (global::windowDelegate->isFisheyeRendering()) {
        _renderOption = RenderOptionPositionNormal;
    }
    else {
        _renderOption = RenderOptionViewDirection;
    }
    addProperty(_renderOption);

    if (p.unit.has_value()) {
        _unit = codegen::map<DistanceUnit>(*p.unit);
    }
    else {
        _unit = DistanceUnit::Meter;
    }

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    if (p.labelFile.has_value()) {
        _labelFile = absPath(*p.labelFile);
        _hasLabel = true;

        _drawLabels = p.drawLabels.value_or(_drawLabels);
        addProperty(_drawLabels);

        _textColor = p.textColor.value_or(_textColor);
        _hasLabel = p.textColor.has_value();
        _textColor.setViewOption(properties::Property::ViewOptions::Color);
        addProperty(_textColor);
        _textColor.onChange([this]() { _textColorIsDirty = true; });

        _textOpacity = p.textOpacity.value_or(_textOpacity);
        addProperty(_textOpacity);

        _textSize = p.textSize.value_or(_textSize);
        addProperty(_textSize);

        _textMinMaxSize = p.textMinMaxSize.value_or(_textMinMaxSize);
        _textMinMaxSize.setViewOption(properties::Property::ViewOptions::MinMaxRange);
        addProperty(_textMinMaxSize);
    }

    if (p.meshColor.has_value()) {
        std::vector<glm::vec3> ops = *p.meshColor;
        for (size_t i = 0; i < ops.size(); i++) {
            _meshColorMap.insert({ static_cast<int>(i) + 1, ops[i] });
        }
    }
}

bool RenderableDUMeshes::isReady() const {
    return (_program != nullptr) &&
        (!_renderingMeshesMap.empty() || (!_labelset.entries.empty()));
}

void RenderableDUMeshes::initialize() {
    const bool success = loadData();
    if (!success) {
        throw ghoul::RuntimeError("Error loading data");
    }
}

void RenderableDUMeshes::initializeGL() {
    _program = DigitalUniverseModule::ProgramObjectManager.request(
        "RenderableDUMeshes",
        []() {
            return global::renderEngine->buildRenderProgram(
                "RenderableDUMeshes",
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/dumesh_vs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/dumesh_fs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache);

    createMeshes();

    if (_hasLabel) {
        if (!_font) {
            constexpr int FontSize = 50;
            _font = global::fontManager->font(
                "Mono",
                static_cast<float>(FontSize),
                ghoul::fontrendering::FontManager::Outline::Yes,
                ghoul::fontrendering::FontManager::LoadGlyphs::No
            );
        }
    }
}

void RenderableDUMeshes::deinitializeGL() {
    for (const std::pair<const int, RenderingMesh>& pair : _renderingMeshesMap) {
        for (int i = 0; i < pair.second.numU; i++) {
            glDeleteVertexArrays(1, &pair.second.vaoArray[i]);
            glDeleteBuffers(1, &pair.second.vboArray[i]);
        }
    }

    DigitalUniverseModule::ProgramObjectManager.release(
        "RenderableDUMeshes",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
}

void RenderableDUMeshes::renderMeshes(const RenderData&,
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
        _program->setUniform(_uniformCache.color, _meshColorMap[pair.second.colorIndex]);
        for (size_t i = 0; i < pair.second.vaoArray.size(); i++) {
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

void RenderableDUMeshes::renderLabels(const RenderData& data,
                                      const glm::dmat4& modelViewProjectionMatrix,
                                      const glm::vec3& orthoRight,
                                      const glm::vec3& orthoUp)
{
    const float scale = static_cast<float>(toMeter(_unit));

    const ghoul::fontrendering::FontRenderer::ProjectedLabelsInformation labelInfo = {
        .enableDepth = true,
        .enableFalseDepth = false,
        .scale = std::pow(10.f, _textSize),
        .renderType = _renderOption,
        .minSize = _textMinMaxSize.value().x,
        .maxSize = _textMinMaxSize.value().y,
        .mvpMatrix = modelViewProjectionMatrix,
        .orthoRight = orthoRight,
        .orthoUp = orthoUp,
        .cameraPos = data.camera.positionVec3(),
        .cameraLookUp = data.camera.lookUpVectorWorldSpace()
    };

    const glm::vec4 textColor = glm::vec4(glm::vec3(_textColor), _textOpacity);

    for (const dataloader::Labelset::Entry& e : _labelset.entries) {
        glm::vec3 scaledPos(e.position);
        scaledPos *= scale;
        ghoul::fontrendering::FontRenderer::defaultProjectionRenderer().render(
            *_font,
            scaledPos,
            e.text,
            textColor,
            labelInfo
        );
    }
}

void RenderableDUMeshes::render(const RenderData& data, RendererTasks&) {
    const glm::dmat4 modelMatrix =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    const glm::dmat4 modelViewMatrix = data.camera.combinedViewMatrix() * modelMatrix;
    const glm::dmat4 projectionMatrix = data.camera.projectionMatrix();
    const glm::dmat4 modelViewProjectionMatrix = projectionMatrix * modelViewMatrix;

    const glm::vec3 lookup = data.camera.lookUpVectorWorldSpace();
    const glm::vec3 viewDirection = data.camera.viewDirectionWorldSpace();
    glm::vec3 right = glm::cross(viewDirection, lookup);
    const glm::vec3 up = glm::cross(right, viewDirection);

    const glm::dmat4 worldToModelTransform = glm::inverse(modelMatrix);
    glm::vec3 orthoRight = glm::normalize(
        glm::vec3(worldToModelTransform * glm::vec4(right, 0.f))
    );

    if (orthoRight == glm::vec3(0.0)) {
        const glm::vec3 otherVector = glm::vec3(lookup.y, lookup.x, lookup.z);
        right = glm::cross(viewDirection, otherVector);
        orthoRight = glm::normalize(
            glm::vec3(worldToModelTransform * glm::vec4(right, 0.f))
        );
    }

    if (_hasSpeckFile) {
        renderMeshes(data, modelViewMatrix, projectionMatrix);
    }

    if (_drawLabels && _hasLabel) {
        const glm::vec3 orthoUp = glm::normalize(
            glm::vec3(worldToModelTransform * glm::dvec4(up, 0.0))
        );
        renderLabels(data, modelViewProjectionMatrix, orthoRight, orthoUp);
    }
}

void RenderableDUMeshes::update(const UpdateData&) {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_program, _uniformCache);
    }
}

bool RenderableDUMeshes::loadData() {
    bool success = false;
    if (_hasSpeckFile) {
        LINFO(std::format("Loading Speck file '{}'", _speckFile));
        success = readSpeckFile();
        if (!success) {
            return false;
        }
    }

    if (!_labelFile.empty()) {
        _labelset = dataloader::label::loadFileWithCache(_labelFile);
    }

    return success;
}

bool RenderableDUMeshes::readSpeckFile() {
    std::ifstream file(_speckFile);
    if (!file.good()) {
        LERROR(std::format("Failed to open Speck file '{}'", _speckFile));
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

        const size_t found = line.find("mesh");
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

            ghoul::getline(file, line);
            std::stringstream dim(line);
            dim >> mesh.numU >> mesh.numV;

            // We can now read the vertices data:
            for (int l = 0; l < mesh.numU * mesh.numV; ++l) {
                ghoul::getline(file, line);
                if (line.substr(0, 1) == "}") {
                    break;
                }

                std::stringstream lineData(line);

                // Try to read three values for the position
                glm::vec3 pos;
                bool success = true;
                for (int i = 0; i < 3; i++) {
                    GLfloat value = 0.f;
                    lineData >> value;
                    const bool errorReading = lineData.rdstate() & std::ifstream::failbit;
                    if (errorReading) {
                        success = false;
                        break;
                    }

                    const GLfloat scaledValue = value * scale;
                    pos[i] = scaledValue;
                    mesh.vertices.push_back(scaledValue);
                }

                if (!success) {
                    LERROR(std::format(
                        "Failed reading position on line {} of mesh {} in file '{}'. "
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
                //for (int i = 0; i < 7; i++) {
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

            ghoul::getline(file, line);
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

void RenderableDUMeshes::createMeshes() {
    if (!(_dataIsDirty && _hasSpeckFile)) {
        return;
    }
    LDEBUG("Creating planes");

    for (std::pair<const int, RenderingMesh>& p : _renderingMeshesMap) {
        for (int i = 0; i < p.second.numU; i++) {
            GLuint vao = 0;
            glGenVertexArrays(1, &vao);
            p.second.vaoArray.push_back(vao);

            GLuint vbo = 0;
            glGenBuffers(1, &vbo);
            p.second.vboArray.push_back(vbo);

            glBindVertexArray(vao);
            glBindBuffer(GL_ARRAY_BUFFER, vbo);
            //glBufferData(GL_ARRAY_BUFFER, it->second.numV * sizeof(GLfloat),
            glBufferData(
                GL_ARRAY_BUFFER,
                p.second.vertices.size() * sizeof(GLfloat),
                p.second.vertices.data(),
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
            for (int i = 0; i < p.second.numV; i++) {
                GLuint cvao = 0;
                glGenVertexArrays(1, &cvao);
                p.second.vaoArray.push_back(cvao);

                GLuint cvbo = 0;
                glGenBuffers(1, &cvbo);
                p.second.vboArray.push_back(cvbo);

                glBindVertexArray(cvao);
                glBindBuffer(GL_ARRAY_BUFFER, cvbo);
                glBufferData(
                    GL_ARRAY_BUFFER,
                    p.second.vertices.size() * sizeof(GLfloat),
                    p.second.vertices.data(),
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
