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
#include <ghoul/misc/templatefactory.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <array>
#include <fstream>
#include <cstdint>

namespace {
    constexpr const char* _loggerCat = "RenderableDUMeshes";
    constexpr const char* ProgramObjectName = "RenderableDUMeshes";

    constexpr const std::array<const char*, 4> UniformNames = {
        "modelViewTransform", "projectionTransform", "alphaValue", "color"
    };

    constexpr const char* KeyFile = "File";
    constexpr const char* keyColor = "Color";
    constexpr const char* keyUnit = "Unit";
    constexpr const char* MeterUnit = "m";
    constexpr const char* KilometerUnit = "Km";
    constexpr const char* ParsecUnit = "pc";
    constexpr const char* KiloparsecUnit = "Kpc";
    constexpr const char* MegaparsecUnit = "Mpc";
    constexpr const char* GigaparsecUnit = "Gpc";
    constexpr const char* GigalightyearUnit = "Gly";

    constexpr const int RenderOptionViewDirection = 0;
    constexpr const int RenderOptionPositionNormal = 1;

    constexpr const int8_t CurrentCacheVersion = 1;
    constexpr const double PARSEC = 0.308567756E17;

    constexpr openspace::properties::Property::PropertyInfo TransparencyInfo = {
        "Transparency",
        "Transparency",
        "This value is a multiplicative factor that is applied to the transparency of "
        "all point."
    };

    constexpr openspace::properties::Property::PropertyInfo TextColorInfo = {
        "TextColor",
        "Text Color",
        "The text color for the astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo TextSizeInfo = {
        "TextSize",
        "Text Size",
        "The text size for the astronomical object labels."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelFileInfo = {
        "LabelFile",
        "Label File",
        "The path to the label file that contains information about the astronomical "
        "objects being rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelMinSizeInfo = {
        "TextMinSize",
        "Text Min Size",
        "The minimal size (in pixels) of the text for the labels for the astronomical "
        "objects being rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelMaxSizeInfo = {
        "TextMaxSize",
        "Text Max Size",
        "The maximum size (in pixels) of the text for the labels for the astronomical "
        "objects being rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "If the DU mesh is of wire type, this value determines the width of the lines"
    };

    constexpr openspace::properties::Property::PropertyInfo DrawElementsInfo = {
        "DrawElements",
        "Draw Elements",
        "Enables/Disables the drawing of the astronomical objects."
    };

    constexpr openspace::properties::Property::PropertyInfo DrawLabelInfo = {
        "DrawLabels",
        "Draw Labels",
        "Determines whether labels should be drawn or hidden."
    };

    constexpr openspace::properties::Property::PropertyInfo TransformationMatrixInfo = {
        "TransformationMatrix",
        "Transformation Matrix",
        "Transformation matrix to be applied to each astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo MeshColorInfo = {
        "MeshColor",
        "Meshes colors",
        "The defined colors for the meshes to be rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo RenderOptionInfo = {
        "RenderOption",
        "Render Option",
        "Debug option for rendering of billboards and texts."
    };
}  // namespace

namespace openspace {

documentation::Documentation RenderableDUMeshes::Documentation() {
    using namespace documentation;
    return {
        "RenderableDUMeshes",
        "digitaluniverse_renderabledumeshes",
        {
            {
                "Type",
                new StringEqualVerifier("RenderableDUMeshes"),
                Optional::No
            },
            {
                KeyFile,
                new StringVerifier,
                Optional::No,
                "The path to the SPECK file that contains information about the "
                "astronomical object being rendered."
            },
            {
                keyColor,
                new Vector3Verifier<float>,
                Optional::Yes,
                "Astronomical Object Color (r,g,b)."
            },
            {
                TransparencyInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                TransparencyInfo.description
            },
            /*{
                ScaleFactorInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                ScaleFactorInfo.description
            },*/
            {
                DrawLabelInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                DrawLabelInfo.description
            },
            {
                TextColorInfo.identifier,
                new DoubleVector4Verifier,
                Optional::Yes,
                TextColorInfo.description
            },
            {
                TextSizeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                TextSizeInfo.description
            },
            {
                LabelFileInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                LabelFileInfo.description
            },
            {
                LabelMinSizeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LabelMinSizeInfo.description
            },
            {
                LabelMaxSizeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LabelMaxSizeInfo.description
            },
            {
                LineWidthInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LineWidthInfo.description
            },
            {
                TransformationMatrixInfo.identifier,
                new Matrix4x4Verifier<double>,
                Optional::Yes,
                TransformationMatrixInfo.description
            },
            {
                MeshColorInfo.identifier,
                new Vector3ListVerifier<float>,
                Optional::No,
                MeshColorInfo.description
            },
        }
    };
}


RenderableDUMeshes::RenderableDUMeshes(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _alphaValue(TransparencyInfo, 1.f, 0.f, 1.f)
    //, _scaleFactor(ScaleFactorInfo, 1.f, 0.f, 64.f)
    , _textColor(TextColorInfo, glm::vec4(1.f), glm::vec4(0.f), glm::vec4(1.f))
    , _textSize(TextSizeInfo, 8.f, 0.5f, 24.f)
    , _drawElements(DrawElementsInfo, true)
    , _drawLabels(DrawLabelInfo, false)
    , _textMinSize(LabelMinSizeInfo, 8.f, 0.5f, 24.f)
    , _textMaxSize(LabelMaxSizeInfo, 500.f, 0.f, 1000.f)
    , _lineWidth(LineWidthInfo, 2.f, 0.f, 16.f)
    , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableDUMeshes"
    );

    if (dictionary.hasKey(KeyFile)) {
        _speckFile = absPath(dictionary.value<std::string>(KeyFile));
        _hasSpeckFile = true;
        _drawElements.onChange([&]() { _hasSpeckFile = !_hasSpeckFile; });
        addProperty(_drawElements);
    }

    _renderOption.addOption(RenderOptionViewDirection, "Camera View Direction");
    _renderOption.addOption(RenderOptionPositionNormal, "Camera Position Normal");
    if (global::windowDelegate.isFisheyeRendering()) {
        _renderOption = RenderOptionPositionNormal;
    }
    else {
        _renderOption = RenderOptionViewDirection;
    }
    addProperty(_renderOption);

    if (dictionary.hasKey(keyUnit)) {
        std::string unit = dictionary.value<std::string>(keyUnit);
        if (unit == MeterUnit) {
            _unit = Meter;
        }
        else if (unit == KilometerUnit) {
            _unit = Kilometer;
        }
        else if (unit == ParsecUnit) {
            _unit = Parsec;
        }
        else if (unit == KiloparsecUnit) {
            _unit = Kiloparsec;
        }
        else if (unit == MegaparsecUnit) {
            _unit = Megaparsec;
        }
        else if (unit == GigaparsecUnit) {
            _unit = Gigaparsec;
        }
        else if (unit == GigalightyearUnit) {
            _unit = GigalightYears;
        }
        else {
            LWARNING("No unit given for RenderableDUMeshes. Using meters as units.");
            _unit = Meter;
        }
    }

    /*if (dictionary.hasKey(keyColor)) {
        _pointColor = dictionary.value<glm::vec3>(keyColor);
    }
    addProperty(_pointColor);*/

    if (dictionary.hasKey(TransparencyInfo.identifier)) {
        _alphaValue = static_cast<float>(
            dictionary.value<double>(TransparencyInfo.identifier)
        );
    }
    addProperty(_alphaValue);

    /*if (dictionary.hasKey(ScaleFactorInfo.identifier)) {
        _scaleFactor = static_cast<float>(
            dictionary.value<double>(ScaleFactorInfo.identifier)
        );
    }
    addProperty(_scaleFactor);*/

    if (dictionary.hasKeyAndValue<double>(LineWidthInfo.identifier)) {
        _lineWidth = static_cast<float>(
            dictionary.value<double>(LineWidthInfo.identifier)
        );
    }
    addProperty(_lineWidth);

    if (dictionary.hasKey(DrawLabelInfo.identifier)) {
        _drawLabels = dictionary.value<bool>(DrawLabelInfo.identifier);
    }
    addProperty(_drawLabels);

    if (dictionary.hasKey(LabelFileInfo.identifier)) {
        _labelFile = absPath(dictionary.value<std::string>(LabelFileInfo.identifier));
        _hasLabel = true;

        if (dictionary.hasKey(TextColorInfo.identifier)) {
            _textColor = dictionary.value<glm::vec4>(TextColorInfo.identifier);
            _hasLabel = true;
        }
        _textColor.setViewOption(properties::Property::ViewOptions::Color);
        addProperty(_textColor);
        _textColor.onChange([&]() { _textColorIsDirty = true; });


        if (dictionary.hasKey(TextSizeInfo.identifier)) {
            _textSize = dictionary.value<float>(TextSizeInfo.identifier);
        }
        addProperty(_textSize);

        if (dictionary.hasKey(LabelMinSizeInfo.identifier)) {
            _textMinSize = floor(dictionary.value<float>(LabelMinSizeInfo.identifier));
        }
        addProperty(_textMinSize);

        if (dictionary.hasKey(LabelMaxSizeInfo.identifier)) {
            _textMaxSize = floor(dictionary.value<float>(LabelMaxSizeInfo.identifier));
        }
        addProperty(_textMaxSize);
    }

    if (dictionary.hasKey(TransformationMatrixInfo.identifier)) {
        _transformationMatrix = dictionary.value<glm::dmat4>(
            TransformationMatrixInfo.identifier
        );
    }

    if (dictionary.hasKey(MeshColorInfo.identifier)) {
        ghoul::Dictionary colorDict = dictionary.value<ghoul::Dictionary>(
            MeshColorInfo.identifier
        );
        for (int i = 0; i < static_cast<int>(colorDict.size()); ++i) {
            _meshColorMap.insert(
                { i + 1, colorDict.value<glm::vec3>(std::to_string(i + 1)) }
            );
        }
    }

    setRenderBin(Renderable::RenderBin::Opaque);
}

bool RenderableDUMeshes::isReady() const {
    return (_program != nullptr) &&
        (!_renderingMeshesMap.empty() || (!_labelData.empty()));
}

void RenderableDUMeshes::initializeGL() {
    _program = DigitalUniverseModule::ProgramObjectManager.request(
        ProgramObjectName,
        []() {
            return global::renderEngine.buildRenderProgram(
                "RenderableDUMeshes",
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/dumesh_vs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/dumesh_fs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);

    bool success = loadData();
    if (!success) {
        throw ghoul::RuntimeError("Error loading data");
    }

    createMeshes();

    if (_hasLabel) {
        if (!_font) {
            constexpr const int FontSize = 50;
            _font = global::fontManager.font(
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
        for (int i = 0; i < pair.second.numU; ++i) {
            glDeleteVertexArrays(1, &pair.second.vaoArray[i]);
            glDeleteBuffers(1, &pair.second.vboArray[i]);
        }
    }

    DigitalUniverseModule::ProgramObjectManager.release(
        ProgramObjectName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );
}

void RenderableDUMeshes::renderMeshes(const RenderData&,
                                      const glm::dmat4& modelViewMatrix,
                                      const glm::dmat4& projectionMatrix)
{
    // Saving current OpenGL state
    GLfloat lineWidth = 1.0f;
    glGetFloatv(GL_LINE_WIDTH, &lineWidth);

    GLboolean blendEnabled = glIsEnabledi(GL_BLEND, 0);

    GLenum blendEquationRGB;
    glGetIntegerv(GL_BLEND_EQUATION_RGB, &blendEquationRGB);

    GLenum blendEquationAlpha;
    glGetIntegerv(GL_BLEND_EQUATION_ALPHA, &blendEquationAlpha);

    GLenum blendDestAlpha;
    glGetIntegerv(GL_BLEND_DST_ALPHA, &blendDestAlpha);

    GLenum blendDestRGB;
    glGetIntegerv(GL_BLEND_DST_RGB, &blendDestRGB);

    GLenum blendSrcAlpha;
    glGetIntegerv(GL_BLEND_SRC_ALPHA, &blendSrcAlpha);

    GLenum blendSrcRGB;
    glGetIntegerv(GL_BLEND_SRC_RGB, &blendSrcRGB);

    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    //glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDepthMask(false);
    glEnable(GL_DEPTH_TEST);

    _program->activate();

    _program->setUniform(_uniformCache.modelViewTransform, modelViewMatrix);
    _program->setUniform(_uniformCache.projectionTransform, projectionMatrix);
    _program->setUniform(_uniformCache.alphaValue, _alphaValue);
    //_program->setUniform(_uniformCache.scaleFactor, _scaleFactor);

    for (const std::pair<const int, RenderingMesh>& pair : _renderingMeshesMap) {
        _program->setUniform(_uniformCache.color, _meshColorMap[pair.second.colorIndex]);
        for (size_t i = 0; i < pair.second.vaoArray.size(); ++i) {
            glBindVertexArray(pair.second.vaoArray[i]);
            switch (pair.second.style) {
                case Solid:
                    break;
                case Wire:
                    glLineWidth(_lineWidth);
                    glDrawArrays(GL_LINE_STRIP, 0, pair.second.numV);
                    glLineWidth(lineWidth);
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

    // Restores blending state
    glBlendEquationSeparate(blendEquationRGB, blendEquationAlpha);
    glBlendFuncSeparate(blendSrcRGB, blendDestRGB, blendSrcAlpha, blendDestAlpha);

    glDepthMask(true);

    if (!blendEnabled) {
        glDisablei(GL_BLEND, 0);
    }
}

void RenderableDUMeshes::renderLabels(const RenderData& data,
                                      const glm::dmat4& modelViewProjectionMatrix,
                                      const glm::vec3& orthoRight,
                                      const glm::vec3& orthoUp)
{
    float scale = 0.f;
    switch (_unit) {
        case Meter:
            scale = 1.f;
            break;
        case Kilometer:
            scale = 1e3f;
            break;
        case Parsec:
            scale = static_cast<float>(PARSEC);
            break;
        case Kiloparsec:
            scale = static_cast<float>(1e3 * PARSEC);
            break;
        case Megaparsec:
            scale = static_cast<float>(1e6 * PARSEC);
            break;
        case Gigaparsec:
            scale = static_cast<float>(1e9 * PARSEC);
            break;
        case GigalightYears:
            scale = static_cast<float>(306391534.73091 * PARSEC);
            break;
    }

    ghoul::fontrendering::FontRenderer::ProjectedLabelsInformation labelInfo;
    labelInfo.orthoRight = orthoRight;
    labelInfo.orthoUp = orthoUp;
    labelInfo.minSize = static_cast<int>(_textMinSize);
    labelInfo.maxSize = static_cast<int>(_textMaxSize);
    labelInfo.cameraPos = data.camera.positionVec3();
    labelInfo.cameraLookUp = data.camera.lookUpVectorWorldSpace();
    labelInfo.renderType = _renderOption;
    labelInfo.mvpMatrix = modelViewProjectionMatrix;
    labelInfo.scale = pow(10.f, _textSize);
    labelInfo.enableDepth = true;
    labelInfo.enableFalseDepth = false;

    for (const std::pair<glm::vec3, std::string>& pair : _labelData) {
        //glm::vec3 scaledPos(_transformationMatrix * glm::dvec4(pair.first, 1.0));
        glm::vec3 scaledPos(pair.first);
        scaledPos *= scale;
        ghoul::fontrendering::FontRenderer::defaultProjectionRenderer().render(
            *_font,
            scaledPos,
            pair.second,
            _textColor,
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
        glm::vec3(worldToModelTransform * glm::vec4(right, 0.0))
    );

    if (orthoRight == glm::vec3(0.0)) {
        glm::vec3 otherVector(lookup.y, lookup.x, lookup.z);
        right = glm::cross(viewDirection, otherVector);
        orthoRight = glm::normalize(
            glm::vec3(worldToModelTransform * glm::vec4(right, 0.0))
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
        ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
    }
}

bool RenderableDUMeshes::loadData() {
    bool success = false;
    if (_hasSpeckFile) {
        // I disabled the cache as it didn't work on Mac --- abock
        // std::string cachedFile = FileSys.cacheManager()->cachedFilename(
        //     _speckFile,
        //     ghoul::filesystem::CacheManager::Persistent::Yes
        // );

        // bool hasCachedFile = FileSys.fileExists(cachedFile);
        // if (hasCachedFile) {
        //     LINFO(
        //         "Cached file '" << cachedFile <<
        //         "' used for Speck file '" << _speckFile << "'"
        //     );

        //     success = loadCachedFile(cachedFile);
        //     if (!success) {
        //         FileSys.cacheManager()->removeCacheFile(_speckFile);
        //         // Intentional fall-through to the 'else' to generate the cache
        //         // file for the next run
        //     }
        // }
        // else
        // {
        //     LINFO("Cache for Speck file '" << _speckFile << "' not found");
        LINFO(fmt::format("Loading Speck file '{}'", _speckFile));

        success = readSpeckFile();
        if (!success) {
            return false;
        }
    }

    std::string labelFile = _labelFile;
    if (!labelFile.empty()) {
        // I disabled the cache as it didn't work on Mac --- abock
        // std::string cachedFile = FileSys.cacheManager()->cachedFilename(
        //     labelFile,
        //     ghoul::filesystem::CacheManager::Persistent::Yes
        // );
        // bool hasCachedFile = FileSys.fileExists(cachedFile);
        // if (hasCachedFile) {
        //     LINFO(
        //         "Cached file '" << cachedFile << "' used for Label file '" <<
        //         labelFile << "'"
        //     );

        //     success &= loadCachedFile(cachedFile);
        //     if (!success) {
        //         FileSys.cacheManager()->removeCacheFile(labelFile);
        //         // Intentional fall-through to the 'else' to generate the cache
        //         // file for the next run
        //     }
        // }
        // else {
        //     LINFO("Cache for Label file '" << labelFile << "' not found");
        LINFO(fmt::format("Loading Label file '{}'", labelFile));

        success &= readLabelFile();
        if (!success) {
            return false;
        }

        // }
    }

    return success;
}

bool RenderableDUMeshes::readSpeckFile() {
    std::ifstream file(_speckFile);
    if (!file.good()) {
        LERROR(fmt::format("Failed to open Speck file '{}'", _speckFile));
        return false;
    }

    int meshIndex = 0;

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line;
    while (true) {
        std::streampos position = file.tellg();
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
        //if (line.substr(0, 4) != "mesh") {
            // we read a line that doesn't belong to the header, so we have to jump back
            // before the beginning of the current line
            //file.seekg(position);
            //break;
            continue;
        }
        else {

        //if (line.substr(0, 4) == "mesh") {
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
            std::stringstream dim(line);
            dim >> mesh.numU; // numU
            dim >> mesh.numV; // numV

            // We can now read the vertices data:
            for (int l = 0; l < mesh.numU * mesh.numV; ++l) {
                std::getline(file, line);
                if (line.substr(0, 1) != "}") {
                    std::stringstream lineData(line);
                    for (int i = 0; i < 7; ++i) {
                        GLfloat value;
                        lineData >> value;
                        bool errorReading = lineData.rdstate() & std::ifstream::failbit;
                        if (!errorReading) {
                            mesh.vertices.push_back(value);
                        }
                        else {
                            break;
                        }
                    }
                }
                else {
                    break;
                }
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

    return true;
}

bool RenderableDUMeshes::readLabelFile() {
    std::ifstream file(_labelFile);
    if (!file.good()) {
        LERROR(fmt::format("Failed to open Label file '{}'", _labelFile));
        return false;
    }

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line;
    while (true) {
        std::streampos position = file.tellg();
        std::getline(file, line);

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (!line.empty() && line.back() == '\r') {
            line = line.substr(0, line.length() - 1);
        }


        if (line.empty() || line[0] == '#') {
            continue;
        }

        if (line.substr(0, 9) != "textcolor") {
            // we read a line that doesn't belong to the header, so we have to jump back
            // before the beginning of the current line
            file.seekg(position);
            continue;
        }

        if (line.substr(0, 9) == "textcolor") {
            // textcolor lines are structured as follows:
            // textcolor # description
            // where # is color text defined in configuration file
            std::stringstream str(line);

            // TODO: handle cases of labels with different colors
            break;
        }
    }

    do {
        std::vector<float> values(_nValuesPerAstronomicalObject);

        std::getline(file, line);

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (!line.empty() && line.back() == '\r') {
            line = line.substr(0, line.length() - 1);
        }

        if (line.empty()) {
            continue;
        }

        std::stringstream str(line);

        glm::vec3 position = glm::vec3(0.f);
        for (int j = 0; j < 3; ++j) {
            str >> position[j];
        }

        std::string dummy;
        str >> dummy; // text keyword

        std::string label;
        str >> label;
        dummy.clear();

        while (str >> dummy) {
            label += " " + dummy;
            dummy.clear();
        }

        glm::vec3 transformedPos = glm::vec3(
            _transformationMatrix * glm::dvec4(position, 1.0)
        );
        _labelData.emplace_back(std::make_pair(transformedPos, label));

    } while (!file.eof());

    return true;
}

bool RenderableDUMeshes::loadCachedFile(const std::string& file) {
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
        reinterpret_cast<char*>(&_nValuesPerAstronomicalObject),
        sizeof(int32_t)
    );

    _fullData.resize(nValues);
    fileStream.read(
        reinterpret_cast<char*>(&_fullData[0]),
        nValues * sizeof(_fullData[0])
    );

    bool success = fileStream.good();
    return success;
}

bool RenderableDUMeshes::saveCachedFile(const std::string& file) const {
    std::ofstream fileStream(file, std::ofstream::binary);
    if (!fileStream.good()) {
        LERROR(fmt::format("Error opening file '{}' for save cache file", file));
        return false;
    }

    fileStream.write(
        reinterpret_cast<const char*>(&CurrentCacheVersion),
        sizeof(int8_t)
    );

    const int32_t nValues = static_cast<int32_t>(_fullData.size());
    if (nValues == 0) {
        LERROR("Error writing cache: No values were loaded");
        return false;
    }
    fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));

    const int32_t nValuesPerAstronomicalObject = static_cast<int32_t>(
        _nValuesPerAstronomicalObject
    );
    fileStream.write(
        reinterpret_cast<const char*>(&nValuesPerAstronomicalObject),
        sizeof(int32_t)
    );

    const size_t nBytes = nValues * sizeof(_fullData[0]);
    fileStream.write(reinterpret_cast<const char*>(&_fullData[0]), nBytes);

    const bool success = fileStream.good();
    return success;
}

void RenderableDUMeshes::createMeshes() {
    if (!(_dataIsDirty && _hasSpeckFile)) {
        return;
    }
    LDEBUG("Creating planes");

    for (std::pair<const int, RenderingMesh>& p : _renderingMeshesMap) {
        float scale = 0.f;
        switch (_unit) {
            case Meter:
                scale = 1.f;
                break;
            case Kilometer:
                scale = 1e3f;
                break;
            case Parsec:
                scale = static_cast<float>(PARSEC);
                break;
            case Kiloparsec:
                scale = static_cast<float>(1e3 * PARSEC);
                break;
            case Megaparsec:
                scale = static_cast<float>(1e6 * PARSEC);
                break;
            case Gigaparsec:
                scale = static_cast<float>(1e9 * PARSEC);
                break;
            case GigalightYears:
                scale = static_cast<float>(306391534.73091 * PARSEC);
                break;
        }

        for (GLfloat& v : p.second.vertices) {
            v *= scale;
        }

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
            // U and V may not be given by the user
            if (p.second.vertices.size() / (p.second.numU * p.second.numV) > 3) {
                glVertexAttribPointer(
                    0,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    sizeof(GLfloat) * 5,
                    reinterpret_cast<GLvoid*>(sizeof(GLfloat) * i * p.second.numV)
                );

                // texture coords
                glEnableVertexAttribArray(1);
                glVertexAttribPointer(
                    1,
                    2,
                    GL_FLOAT,
                    GL_FALSE,
                    sizeof(GLfloat) * 7,
                    reinterpret_cast<GLvoid*>(sizeof(GLfloat) * 3 * i * p.second.numV)
                );
            }
            else { // no U and V:
                glVertexAttribPointer(
                    0,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    0,
                    reinterpret_cast<GLvoid*>(sizeof(GLfloat) * 3 * i * p.second.numV)
                );
            }
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
