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

#include <modules/digitaluniverse/rendering/renderabledumeshes.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

#include <glm/gtx/string_cast.hpp>
#include <glm/glm.hpp>

#include <array>
#include <fstream>
#include <stdint.h>

namespace {
    const char* _loggerCat        = "RenderableDUMeshes";
    const char* KeyFile           = "File";
    const char* keyColor          = "Color";
    const char* keyUnit           = "Unit";
    const char* MeterUnit         = "m";
    const char* KilometerUnit     = "Km";
    const char* ParsecUnit        = "pc";
    const char* KiloparsecUnit    = "Kpc";
    const char* MegaparsecUnit    = "Mpc";
    const char* GigaparsecUnit    = "Gpc";
    const char* GigalightyearUnit = "Gly";

    const int8_t CurrentCacheVersion = 1;
    const float PARSEC = 0.308567756E17;
    
    static const openspace::properties::Property::PropertyInfo TransparencyInfo = {
        "Transparency",
        "Transparency",
        "This value is a multiplicative factor that is applied to the transparency of "
        "all point."
    };

    static const openspace::properties::Property::PropertyInfo ScaleFactorInfo = {
        "ScaleFactor",
        "Scale Factor",
        "This value is used as a multiplicative factor that is applied to the apparent "
        "size of each point."
    };

    static const openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "This value is used to define the color of the astronomical object."
    }; 

    static const openspace::properties::Property::PropertyInfo TextColorInfo = {
        "TextColor",
        "Text Color",
        "The text color for the astronomical object."
    };

    static const openspace::properties::Property::PropertyInfo TextSizeInfo = {
        "TextSize",
        "Text Size",
        "The text size for the astronomical object labels."
    };

    static const openspace::properties::Property::PropertyInfo LabelFileInfo = {
        "LabelFile",
        "Label File",
        "The path to the label file that contains information about the astronomical "
        "objects being rendered."
    };

    static const openspace::properties::Property::PropertyInfo LabelMinSizeInfo = {
        "TextMinSize",
        "Text Min Size",
        "The minimal size (in pixels) of the text for the labels for the astronomical "
        "objects being rendered."
    };

    static const openspace::properties::Property::PropertyInfo DrawElementsInfo = {
        "DrawElements",
        "Draw Elements",
        "Enables/Disables the drawing of the astronomical objects."
    };

    static const openspace::properties::Property::PropertyInfo DrawLabelInfo = {
        "DrawLabels",
        "Draw Labels",
        "Determines whether labels should be drawn or hidden."
    };

    static const openspace::properties::Property::PropertyInfo TransformationMatrixInfo = {
        "TransformationMatrix",
        "Transformation Matrix",
        "Transformation matrix to be applied to each astronomical object."
    };

    static const openspace::properties::Property::PropertyInfo MeshColorInfo = {
        "MeshColor",
        "Meshes colors",
        "The defined colors for the meshes to be rendered."
    };

    static const openspace::properties::Property::PropertyInfo RenderOptionInfo = {
        "RenderOptionInfo",
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
                "The path to the SPECK file that contains information about the astronomical "
                "object being rendered."
            },
            { 
                keyColor,
                new Vector3Verifier<float>,
                Optional::No,
                "Astronomical Object Color (r,g,b)."
            },
            {
                TransparencyInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                TransparencyInfo.description
            },
            {
                ScaleFactorInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                ScaleFactorInfo.description
            },
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
                new IntVerifier,
                Optional::Yes,
                LabelMinSizeInfo.description
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
    , _hasSpeckFile(false)
    , _dataIsDirty(true)
    , _textColorIsDirty(true)
    , _hasLabel(false)
    , _labelDataIsDirty(true)
    , _textMinSize(0)
    , _alphaValue(TransparencyInfo, 1.f, 0.f, 1.f)
    , _scaleFactor(ScaleFactorInfo, 1.f, 0.f, 64.f)
    //, _pointColor(ColorInfo, glm::vec3(1.f, 0.4f, 0.2f), glm::vec3(0.f, 0.f, 0.f), glm::vec3(1.0f, 1.0f, 1.0f))
    , _drawLabels(DrawLabelInfo, false)
    , _textColor(
        TextColorInfo,
        glm::vec4(1.0f, 1.0, 1.0f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _textSize(TextSizeInfo, 8.0, 0.5, 24.0)        
    , _drawElements(DrawElementsInfo, true)
    , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _program(nullptr)
    , _fontRenderer(nullptr)
    , _font(nullptr)
    , _speckFile("")
    , _labelFile("")
    , _unit(Parsec)
    , _nValuesPerAstronomicalObject(0)        
{
    using File = ghoul::filesystem::File;

    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableDUMeshes"
    );
        
    if (dictionary.hasKey(KeyFile)) {
        _speckFile = absPath(dictionary.value<std::string>(KeyFile));
        _hasSpeckFile = true;
        _drawElements.onChange([&]() {
            _hasSpeckFile = _hasSpeckFile == true ? false : true; });
        addProperty(_drawElements);
    }

    // DEBUG:
    _renderOption.addOption(0, "Camera View Direction");
    _renderOption.addOption(1, "Camera Position Normal");
    _renderOption.addOption(2, "Screen center Position Normal");
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

    if (dictionary.hasKey(ScaleFactorInfo.identifier)) {
        _scaleFactor = static_cast<float>(
            dictionary.value<double>(ScaleFactorInfo.identifier)
            );
    }
    addProperty(_scaleFactor);
    
    if (dictionary.hasKey(DrawLabelInfo.identifier)) {
        _drawLabels = dictionary.value<bool>(DrawLabelInfo.identifier);
    }
    addProperty(_drawLabels);

    if (dictionary.hasKey(LabelFileInfo.identifier)) {
        _labelFile = absPath(dictionary.value<std::string>(
            LabelFileInfo.identifier
            ));
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
            _textMinSize = static_cast<int>(dictionary.value<float>(LabelMinSizeInfo.identifier));
        }
    }

    if (dictionary.hasKey(TransformationMatrixInfo.identifier)) {
        _transformationMatrix = dictionary.value<glm::dmat4>(TransformationMatrixInfo.identifier);
    }

    if (dictionary.hasKey(MeshColorInfo.identifier)) {
        ghoul::Dictionary colorDic = dictionary.value<ghoul::Dictionary>(
            MeshColorInfo.identifier
            );
        for (int i = 0; i < colorDic.size(); ++i) {
            _meshColorMap.insert({ i + 1,
                colorDic.value<glm::vec3>(std::to_string(i + 1)) });
        }

    }
}

bool RenderableDUMeshes::isReady() const {
    return (_program != nullptr) && (!_renderingMeshesMap.empty() || (!_labelData.empty()));
}

void RenderableDUMeshes::initialize() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    _program = renderEngine.buildRenderProgram("RenderableDUMeshes",
        "${MODULE_DIGITALUNIVERSE}/shaders/dumesh_vs.glsl",
        "${MODULE_DIGITALUNIVERSE}/shaders/dumesh_fs.glsl");

    bool success = loadData();
    if (!success) {
        throw ghoul::RuntimeError("Error loading data");
        return;
    }

    createMeshes();
        
    if (_hasLabel) {
        if (_fontRenderer == nullptr)
            _fontRenderer = std::unique_ptr<ghoul::fontrendering::FontRenderer>(
                ghoul::fontrendering::FontRenderer::createProjectionSubjectText());
        if (_font == nullptr) {
            size_t _fontSize = 30;
            _font = OsEng.fontManager().font("Mono", static_cast<float>(_fontSize),
                ghoul::fontrendering::FontManager::Outline::Yes, ghoul::fontrendering::FontManager::LoadGlyphs::No);
        }
    }
}

void RenderableDUMeshes::deinitialize() {
    for (auto pair : _renderingMeshesMap) {
        for (int i = 0; i < pair.second.numU; ++i) {
            glDeleteVertexArrays(1, &pair.second.vaoArray[i]);
            glDeleteBuffers(1, &pair.second.vboArray[i]);
        }            
    }
        
    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_program) {
        renderEngine.removeRenderProgram(_program);
        _program = nullptr;
    }
}

void RenderableDUMeshes::renderMeshes(const RenderData& data, const glm::dmat4& modelViewMatrix,
    const glm::dmat4& projectionMatrix) {
    // Saving current OpenGL state
    GLboolean blendEnabled = glIsEnabled(GL_BLEND);
    GLenum blendEquationRGB;
    GLenum blendEquationAlpha;
    GLenum blendDestAlpha;
    GLenum blendDestRGB;
    GLenum blendSrcAlpha;
    GLenum blendSrcRGB;

    glGetIntegerv(GL_BLEND_EQUATION_RGB, &blendEquationRGB);
    glGetIntegerv(GL_BLEND_EQUATION_ALPHA, &blendEquationAlpha);
    glGetIntegerv(GL_BLEND_DST_ALPHA, &blendDestAlpha);
    glGetIntegerv(GL_BLEND_DST_RGB, &blendDestRGB);
    glGetIntegerv(GL_BLEND_SRC_ALPHA, &blendSrcAlpha);
    glGetIntegerv(GL_BLEND_SRC_RGB, &blendSrcRGB);

    glEnable(GL_BLEND);
    //glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDepthMask(false);

    _program->activate();

    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _program->setIgnoreUniformLocationError(IgnoreError::Yes);

    _program->setUniform("modelViewProjectionTransform", glm::dmat4(projectionMatrix) * modelViewMatrix);
    _program->setUniform("alphaValue", _alphaValue);
    _program->setUniform("scaleFactor", _scaleFactor);                

    for (auto pair : _renderingMeshesMap) {
        _program->setUniform("color", _meshColorMap[pair.second.colorIndex]);
        for (int i = 0; i < pair.second.vaoArray.size(); ++i) {
            glBindVertexArray(pair.second.vaoArray[i]);
            switch (pair.second.style)
            {
            case Solid:
                break;
            case Wire:
                glDrawArrays(GL_LINE_STRIP, 0, pair.second.numV);
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

    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _program->setIgnoreUniformLocationError(IgnoreError::No);
    _program->deactivate();

    // Restores blending state
    glBlendEquationSeparate(blendEquationRGB, blendEquationAlpha);
    glBlendFuncSeparate(blendSrcRGB, blendDestRGB, blendSrcAlpha, blendDestAlpha);

    glDepthMask(true);

    if (!blendEnabled) {
        glDisable(GL_BLEND);
    }
}

void RenderableDUMeshes::renderLabels(const RenderData& data, const glm::dmat4& modelViewProjectionMatrix,
    const glm::vec3& orthoRight, const glm::vec3& orthoUp) {
    RenderEngine& renderEngine = OsEng.renderEngine();

    _fontRenderer->setFramebufferSize(renderEngine.renderingResolution());

    float scale = 0.0;
    switch (_unit) {
    case Meter:
        scale = 1.0;
        break;
    case Kilometer:
        scale = 1e3;
        break;
    case Parsec:
        scale = PARSEC;
        break;
    case Kiloparsec:
        scale = 1e3 * PARSEC;
        break;
    case Megaparsec:
        scale = 1e6 * PARSEC;
        break;
    case Gigaparsec:
        scale = 1e9 * PARSEC;
        break;
    case GigalightYears:
        scale = 306391534.73091 * PARSEC;
        break;
    }

    for (const auto pair : _labelData) {
        //glm::vec3 scaledPos(_transformationMatrix * glm::dvec4(pair.first, 1.0));
        glm::vec3 scaledPos(pair.first);
        scaledPos *= scale;
        _fontRenderer->render(
            *_font,
            scaledPos,
            _textColor,
            pow(10.0, _textSize.value()),
            _textMinSize,
            modelViewProjectionMatrix,
            orthoRight,
            orthoUp,
            data.camera.positionVec3(),
            data.camera.lookUpVectorWorldSpace(),
            _renderOption.value(),
            "%s",
            pair.second.c_str());
    }
}

void RenderableDUMeshes::render(const RenderData& data, RendererTasks&) {
    glm::dmat4 modelMatrix =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    glm::dmat4 modelViewMatrix = data.camera.combinedViewMatrix() * modelMatrix;
    glm::mat4 projectionMatrix = data.camera.projectionMatrix();
    glm::dmat4 modelViewProjectionMatrix = glm::dmat4(projectionMatrix) * modelViewMatrix;

    glm::vec3 lookup = data.camera.lookUpVectorWorldSpace();
    glm::vec3 viewDirection = data.camera.viewDirectionWorldSpace();
    glm::vec3 right = glm::cross(viewDirection, lookup);
    glm::vec3 up = glm::cross(right, viewDirection);

    glm::dmat4 worldToModelTransform = glm::inverse(modelMatrix);
    glm::vec3 orthoRight = glm::normalize(glm::vec3(worldToModelTransform * glm::vec4(right, 0.0)));
    glm::vec3 orthoUp = glm::normalize(glm::vec3(worldToModelTransform * glm::vec4(up, 0.0)));

    if (_hasSpeckFile) {
        renderMeshes(data, modelViewMatrix, projectionMatrix);
    }

    if (_drawLabels && _hasLabel) {
        renderLabels(data, modelViewProjectionMatrix, orthoRight, orthoUp);
    }
}

void RenderableDUMeshes::update(const UpdateData&) {}

bool RenderableDUMeshes::loadData() {
    bool success = false;
    if (_hasSpeckFile) {
        std::string _file = _speckFile;
        // I disabled the cache as it didn't work on Mac --- abock
        // std::string cachedFile = FileSys.cacheManager()->cachedFilename(
        //     _file,
        //     ghoul::filesystem::CacheManager::Persistent::Yes
        // );

        // bool hasCachedFile = FileSys.fileExists(cachedFile);
        // //if (hasCachedFile) {
        // //    LINFO("Cached file '" << cachedFile << "' used for Speck file '" << _file << "'");

        // //    success = loadCachedFile(cachedFile);
        // //    if (!success) {
        // //        FileSys.cacheManager()->removeCacheFile(_file);
        // //        // Intentional fall-through to the 'else' computation to generate the cache
        // //        // file for the next run
        // //    }
        // //}
        // //else 
        // {
        //     LINFO("Cache for Speck file '" << _file << "' not found");
            LINFO("Loading Speck file '" << _file << "'");

            success = readSpeckFile();
            if (!success) {
                return false;
            }

            // LINFO("Saving cache");
            //success &= saveCachedFile(cachedFile);
        // }
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
        //     LINFO("Cached file '" << cachedFile << "' used for Label file '" << labelFile << "'");

        //     success &= loadCachedFile(cachedFile);
        //     if (!success) {
        //         FileSys.cacheManager()->removeCacheFile(labelFile);
        //         // Intentional fall-through to the 'else' computation to generate the cache
        //         // file for the next run
        //     }
        // }
        // else {
        //     LINFO("Cache for Label file '" << labelFile << "' not found");
            LINFO("Loading Label file '" << labelFile << "'");

            success &= readLabelFile();
            if (!success) {
                return false;
            }

        // }
    }

    return success;
}

bool RenderableDUMeshes::readSpeckFile() {
    std::string _file = _speckFile;
    std::ifstream file(_file);
    if (!file.good()) {
        LERROR("Failed to open Speck file '" << _file << "'");
        return false;
    }

    int meshIndex = 0;

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line = "";
    while (true) {
        std::streampos position = file.tellg();
        std::getline(file, line);
            
        if (file.eof()) {
            break;
        }

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r 
        if (!line.empty() && line.back() == '\r') {
            line = line.substr(0, line.length() -1);
        }

        if (line.empty() || line[0] == '#') {
            continue;
        }

        if (line.substr(0, 4) != "mesh") {
            // we read a line that doesn't belong to the header, so we have to jump back
            // before the beginning of the current line
            file.seekg(position);
            break;
        }

        if (line.substr(0, 4) == "mesh") {
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
    std::string _file = _labelFile;
    std::ifstream file(_file);
    if (!file.good()) {
        LERROR("Failed to open Label file '" << _file << "'");
        return false;
    }

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line = "";
    while (true) {
        std::streampos position = file.tellg();
        std::getline(file, line);

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r 
        if (!line.empty() && line.back() == '\r') {
            line = line.substr(0, line.length() -1);
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
            line = line.substr(0, line.length() -1);
        }

        if (line.empty()) {
            continue;
        }

        std::stringstream str(line);

        glm::vec3 position;
        for (auto j = 0; j < 3; ++j) {
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

        glm::vec3 transformedPos = glm::vec3(_transformationMatrix * glm::dvec4(position, 1.0));
        _labelData.push_back(std::make_pair(transformedPos, label));

    } while (!file.eof());

    return true;
}

bool RenderableDUMeshes::loadCachedFile(const std::string& file) {
    std::ifstream fileStream(file, std::ifstream::binary);
    if (fileStream.good()) {
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
        fileStream.read(reinterpret_cast<char*>(&_nValuesPerAstronomicalObject), sizeof(int32_t));

        _fullData.resize(nValues);
        fileStream.read(reinterpret_cast<char*>(&_fullData[0]),
            nValues * sizeof(_fullData[0]));

        bool success = fileStream.good();
        return success;
    }
    else {
        LERROR("Error opening file '" << file << "' for loading cache file");
        return false;
    }
}

bool RenderableDUMeshes::saveCachedFile(const std::string& file) const {
    std::ofstream fileStream(file, std::ofstream::binary);
    if (fileStream.good()) {
        fileStream.write(reinterpret_cast<const char*>(&CurrentCacheVersion),
            sizeof(int8_t));

        int32_t nValues = static_cast<int32_t>(_fullData.size());
        if (nValues == 0) {
            LERROR("Error writing cache: No values were loaded");
            return false;
        }
        fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));

        int32_t nValuesPerAstronomicalObject = static_cast<int32_t>(_nValuesPerAstronomicalObject);
        fileStream.write(reinterpret_cast<const char*>(&nValuesPerAstronomicalObject), sizeof(int32_t));

        size_t nBytes = nValues * sizeof(_fullData[0]);
        fileStream.write(reinterpret_cast<const char*>(&_fullData[0]), nBytes);

        bool success = fileStream.good();
        return success;
    }
    else {
        LERROR("Error opening file '" << file << "' for save cache file");
        return false;
    }
}

void RenderableDUMeshes::createMeshes() {
    if (_dataIsDirty && _hasSpeckFile) {
        LDEBUG("Creating planes");


        std::unordered_map<int, RenderingMesh>::iterator it = _renderingMeshesMap.begin();
        std::unordered_map<int, RenderingMesh>::iterator itEnd = _renderingMeshesMap.end();

        for (; it != itEnd; ++it) {                
            float scale = 0.0;
            switch (_unit) {
            case Meter:
                scale = 1.0;
                break;
            case Kilometer:
                scale = 1e3;
                break;
            case Parsec:
                scale = PARSEC;
                break;
            case Kiloparsec:
                scale = 1e3 * PARSEC;
                break;
            case Megaparsec:
                scale = 1e6 * PARSEC;
                break;
            case Gigaparsec:
                scale = 1e9 * PARSEC;
                break;
            case GigalightYears:
                scale = 306391534.73091 * PARSEC;
                break;
            }

            for (int v = 0; v < it->second.vertices.size(); ++v) {
                it->second.vertices[v] *= scale;
            }
                                
            for (int i = 0; i < it->second.numU; ++i) {
                GLuint vao, vbo;
                glGenVertexArrays(1, &vao);
                glGenBuffers(1, &vbo);
                it->second.vaoArray.push_back(vao);
                it->second.vboArray.push_back(vbo);

                glBindVertexArray(vao);
                glBindBuffer(GL_ARRAY_BUFFER, vbo);
                //glBufferData(GL_ARRAY_BUFFER, it->second.numV * sizeof(GLfloat),
                glBufferData(GL_ARRAY_BUFFER, it->second.vertices.size() * sizeof(GLfloat),
                    &it->second.vertices[0], GL_STATIC_DRAW);
                // in_position
                glEnableVertexAttribArray(0);
                // U and V may not be given by the user
                if (it->second.vertices.size() / (it->second.numU * it->second.numV) > 3) {
                    glVertexAttribPointer(
                        0,
                        3,
                        GL_FLOAT,
                        GL_FALSE,
                        sizeof(GLfloat) * 5,
                        reinterpret_cast<GLvoid*>(sizeof(GLfloat) * i * it->second.numV)
                    );

                    // texture coords
                    glEnableVertexAttribArray(1);
                    glVertexAttribPointer(
                        1,
                        2,
                        GL_FLOAT,
                        GL_FALSE,
                        sizeof(GLfloat) * 7,
                        reinterpret_cast<GLvoid*>(sizeof(GLfloat) * 3 * i * it->second.numV)
                    );
                }
                else { // no U and V:
                    glVertexAttribPointer(
                        0,
                        3,
                        GL_FLOAT,
                        GL_FALSE,
                        0,
                        reinterpret_cast<GLvoid*>(sizeof(GLfloat) * 3 * i * it->second.numV)
                    );
                }                    
            }    

            // Grid: we need columns
            if (it->second.numU > 1) {
                for (int i = 0; i < it->second.numV; ++i) {
                    GLuint cvao, cvbo;
                    glGenVertexArrays(1, &cvao);
                    glGenBuffers(1, &cvbo);
                    it->second.vaoArray.push_back(cvao);
                    it->second.vboArray.push_back(cvbo);

                    glBindVertexArray(cvao);
                    glBindBuffer(GL_ARRAY_BUFFER, cvbo);
                    glBufferData(GL_ARRAY_BUFFER, it->second.vertices.size() * sizeof(GLfloat),
                        &it->second.vertices[0], GL_STATIC_DRAW);
                    // in_position
                    glEnableVertexAttribArray(0);
                    // U and V may not be given by the user
                    if (it->second.vertices.size() / (it->second.numU * it->second.numV) > 3) {
                        glVertexAttribPointer(
                            0,
                            3,
                            GL_FLOAT,
                            GL_FALSE,
                            it->second.numV * sizeof(GLfloat) * 5,
                            reinterpret_cast<GLvoid*>(sizeof(GLfloat) * i)
                        );

                        // texture coords
                        glEnableVertexAttribArray(1);
                        glVertexAttribPointer(
                            1,
                            2,
                            GL_FLOAT,
                            GL_FALSE,
                            it->second.numV * sizeof(GLfloat) * 7,
                            reinterpret_cast<GLvoid*>(sizeof(GLfloat) * 3 * i)
                        );
                    }
                    else { // no U and V:
                        glVertexAttribPointer(
                            0,
                            3,
                            GL_FLOAT,
                            GL_FALSE,
                            it->second.numV * sizeof(GLfloat) * 3,
                            reinterpret_cast<GLvoid*>(sizeof(GLfloat) * 3 * i)
                        );
                    }
                }                    
            }
        }                

        glBindVertexArray(0);

        _dataIsDirty = false;
    }

    if (_hasLabel && _labelDataIsDirty) {
        _labelDataIsDirty = false;
    }        
}

} // namespace openspace
