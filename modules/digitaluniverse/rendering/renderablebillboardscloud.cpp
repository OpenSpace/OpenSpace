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

#include <modules/digitaluniverse/rendering/renderablebillboardscloud.h>

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

#include <array>
#include <fstream>
#include <stdint.h>
#include <locale>
#include <string>

namespace {
    const char* _loggerCat        = "RenderableBillboardsCloud";
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
    const double PARSEC = 0.308567756E17;

    static const openspace::properties::Property::PropertyInfo SpriteTextureInfo = {
        "Texture",
        "Point Sprite Texture",
        "The path to the texture that should be used as the point sprite."        
    };
    
    static const openspace::properties::Property::PropertyInfo TransparencyInfo = {
        "Transparency",
        "Transparency",
        "This value is a multiplicative factor that is applied to the transparency of "
        "all points."
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

    static const openspace::properties::Property::PropertyInfo ColorMapInfo = {
        "ColorMap",
        "Color Map File",
        "The path to the color map file of the astronomical onject."
    };

    static const openspace::properties::Property::PropertyInfo PolygonSidesInfo = {
        "PolygonSides",
        "Polygon Sides",
        "The number of sides for the polygon used to represent the astronomical onject."
    };
}  // namespace

namespace openspace {

    documentation::Documentation RenderableBillboardsCloud::Documentation() {
        using namespace documentation;
        return {
            "RenderableBillboardsCloud",
            "digitaluniverse_RenderableBillboardsCloud",
            {
                {
                    "Type",
                    new StringEqualVerifier("RenderableBillboardsCloud"),
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
                    SpriteTextureInfo.identifier,
                    new StringVerifier,
                    Optional::Yes,
                    SpriteTextureInfo.description
                },
                {
                    TransparencyInfo.identifier,
                    new DoubleVerifier,
                    Optional::No,
                    TransparencyInfo.description
                },
                {
                    ScaleFactorInfo.identifier,
                    new DoubleVerifier,
                    Optional::Yes,
                    ScaleFactorInfo.description
                },
                {
                    ColorMapInfo.identifier,
                    new StringVerifier,
                    Optional::Yes,
                    ColorMapInfo.description
                },
                {
                    PolygonSidesInfo.identifier,
                    new IntVerifier,
                    Optional::Yes,
                    PolygonSidesInfo.description
                },

            }
        };
    }


    RenderableBillboardsCloud::RenderableBillboardsCloud(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary)
        , _dataIsDirty(true)
        , _hasSpriteTexture(false)
        , _spriteTextureIsDirty(true)
        , _hasColorMapFile(false)
        , _hasPolygon(false)
        , _polygonSides(0)
        , _pTexture(0)
        , _tTexture(0)
        , _alphaValue(TransparencyInfo, 1.f, 0.f, 1.f)
        , _scaleFactor(ScaleFactorInfo, 1.f, 0.f, 600.f)
        , _pointColor(ColorInfo, glm::vec3(1.f, 0.4f, 0.2f), glm::vec3(0.f, 0.f, 0.f), glm::vec3(1.0f, 1.0f, 1.0f))
        , _spriteTexturePath(SpriteTextureInfo)
        , _polygonTexture(nullptr)
        , _spriteTexture(nullptr)
        , _program(nullptr)
        , _speckFile("")
        , _colorMapFile("")
        , _unit(Parsec)
        , _nValuesPerAstronomicalObject(0)
        , _vao(0)
        , _vbo(0)
    {
        using File = ghoul::filesystem::File;

        documentation::testSpecificationAndThrow(
            Documentation(),
            dictionary,
            "RenderableBillboardsCloud"
        );
        
        _speckFile = absPath(dictionary.value<std::string>(KeyFile));
        
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
                LWARNING("No unit given for RenderableBillboardsCloud. Using meters as units.");
                _unit = Meter;
            }
        }

        if (dictionary.hasKey(keyColor)) {
            _pointColor = dictionary.value<glm::vec3>(keyColor);
        }
        addProperty(_pointColor);

        if (dictionary.hasKey(SpriteTextureInfo.identifier)) {
            _spriteTexturePath = absPath(dictionary.value<std::string>(
                SpriteTextureInfo.identifier
                ));
            _spriteTextureFile = std::make_unique<File>(_spriteTexturePath);

            _spriteTexturePath.onChange([&] { _spriteTextureIsDirty = true; });
            _spriteTextureFile->setCallback(
                [&](const File&) { _spriteTextureIsDirty = true; }
            );
            addProperty(_spriteTexturePath);

            _hasSpriteTexture = true;
        }

        if (dictionary.hasKey(ColorMapInfo.identifier)) {
            _colorMapFile = absPath(dictionary.value<std::string>(
                ColorMapInfo.identifier
                ));
            _hasColorMapFile = true;
        }

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

        if (dictionary.hasKey(PolygonSidesInfo.identifier)) {
            _polygonSides = static_cast<float>(
                dictionary.value<double>(PolygonSidesInfo.identifier)
                );
            _hasPolygon = true;
        }

    }

    bool RenderableBillboardsCloud::isReady() const {
        return (_program != nullptr) && (!_fullData.empty());
    }

    void RenderableBillboardsCloud::initialize() {
        RenderEngine& renderEngine = OsEng.renderEngine();
        
        _program = renderEngine.buildRenderProgram("RenderableBillboardsCloud",
            "${MODULE_DIGITALUNIVERSE}/shaders/billboard2_vs.glsl",
            "${MODULE_DIGITALUNIVERSE}/shaders/billboard2_fs.glsl",
            "${MODULE_DIGITALUNIVERSE}/shaders/billboard2_gs.glsl");
                
        bool success = loadData();
        if (!success) {
            throw ghoul::RuntimeError("Error loading data");
        }

        if (_hasPolygon) {
            createPolygonTexture();
        }
    }

    void RenderableBillboardsCloud::deinitialize() {
        glDeleteBuffers(1, &_vbo);
        _vbo = 0;
        glDeleteVertexArrays(1, &_vao);
        _vao = 0;

        RenderEngine& renderEngine = OsEng.renderEngine();
        if (_program) {
            renderEngine.removeRenderProgram(_program);
            _program = nullptr;
        }

        if (_hasSpriteTexture) {
            _spriteTexture = nullptr;
        }

        if (_hasPolygon) {
            _polygonTexture = nullptr;
            glDeleteTextures(1, &_pTexture);
        }
    }

    void RenderableBillboardsCloud::render(const RenderData& data, RendererTasks&) {
        glDepthMask(false);

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
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);

        _program->activate();

        using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
        _program->setIgnoreUniformLocationError(IgnoreError::Yes);
        
        /*glm::dmat4 modelMatrix = glm::dmat4(1.0);*/

        glm::dmat4 modelMatrix =
            glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
            glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
            glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

        glm::dmat4 modelViewMatrix = data.camera.combinedViewMatrix() * modelMatrix;
        glm::mat4 viewMatrix = data.camera.viewMatrix();
        glm::mat4 projectionMatrix = data.camera.projectionMatrix();

        _program->setUniform("screenSize", glm::vec2(OsEng.renderEngine().renderingResolution()));
        _program->setUniform("projection", projectionMatrix);
        _program->setUniform("modelViewTransform", modelViewMatrix);
        _program->setUniform("modelViewProjectionTransform", glm::dmat4(projectionMatrix) * modelViewMatrix);
        //_program->setUniform("campos", data.camera.positionVec3());

        _program->setUniform("minBillboardSize", 1.f); // in pixels
        _program->setUniform("color", _pointColor);
        _program->setUniform("sides", 4);
        _program->setUniform("alphaValue", _alphaValue);
        _program->setUniform("scaleFactor", _scaleFactor);

        glm::vec3 lookup = data.camera.lookUpVectorWorldSpace();
        glm::vec3 viewDirection = data.camera.viewDirectionWorldSpace();
        glm::vec3 right = glm::cross(viewDirection, lookup);
        glm::vec3 up = glm::cross(right, viewDirection);

        glm::dmat4 worldToModelTransform = glm::inverse(modelMatrix);
        _program->setUniform("up", glm::normalize(glm::vec3(worldToModelTransform * glm::vec4(up, 0.0))));
        _program->setUniform("right", glm::normalize(glm::vec3(worldToModelTransform * glm::vec4(right, 0.0))));
        
        ghoul::opengl::TextureUnit spriteTextureUnit;
        if (_hasSpriteTexture) {            
            spriteTextureUnit.activate();
            _spriteTexture->bind();
            _program->setUniform("spriteTexture", spriteTextureUnit);
        }

        ghoul::opengl::TextureUnit polygonTextureUnit;
        if (_hasPolygon) {
            polygonTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _pTexture);
            _program->setUniform("polygonTexture", polygonTextureUnit);
            _program->setUniform("hasPolygon", _hasPolygon);
        }


        if (_hasColorMapFile) {
            _program->setUniform("hasColorMap", true);
        }
        else {
            _program->setUniform("hasColorMap", false);
        }
     
        glBindVertexArray(_vao);
        const GLsizei nAstronomicalObjects = static_cast<GLsizei>(_fullData.size() / _nValuesPerAstronomicalObject);
        glDrawArrays(GL_POINTS, 0, nAstronomicalObjects);
        
        glBindVertexArray(0);
        using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
        _program->setIgnoreUniformLocationError(IgnoreError::No);
        _program->deactivate();

        // Restores blending state
        glBlendEquationSeparate(blendEquationRGB, blendEquationAlpha);
        glBlendFuncSeparate(blendSrcRGB, blendDestRGB, blendSrcAlpha, blendDestAlpha);
        
        if (!blendEnabled) {
            glDisable(GL_BLEND);            
        }
                   
        glDepthMask(true);
    }

    void RenderableBillboardsCloud::update(const UpdateData&) {  
        if (_dataIsDirty) {
            LDEBUG("Regenerating data");

            createDataSlice();

            int size = static_cast<int>(_slicedData.size());

            if (_vao == 0) {
                glGenVertexArrays(1, &_vao);
                LDEBUG("Generating Vertex Array id '" << _vao << "'");
            }
            if (_vbo == 0) {
                glGenBuffers(1, &_vbo);
                LDEBUG("Generating Vertex Buffer Object id '" << _vbo << "'");
            }

            glBindVertexArray(_vao);
            glBindBuffer(GL_ARRAY_BUFFER, _vbo);
            glBufferData(
                GL_ARRAY_BUFFER,
                size * sizeof(float),
                &_slicedData[0],
                GL_STATIC_DRAW
            );
            GLint positionAttrib = _program->attributeLocation("in_position");
            
            if (_hasColorMapFile) {
                
                const size_t nAstronomicalObjects = _fullData.size() / _nValuesPerAstronomicalObject;
                const size_t nValues = _slicedData.size() / nAstronomicalObjects;
                GLsizei stride = static_cast<GLsizei>(sizeof(float) * nValues);
                
                glEnableVertexAttribArray(positionAttrib);
                glVertexAttribPointer(
                    positionAttrib,
                    4,
                    GL_FLOAT,
                    GL_FALSE,
                    sizeof(float)*8,
                    nullptr
                );

                GLint colorMapAttrib = _program->attributeLocation("in_colormap");
                glEnableVertexAttribArray(colorMapAttrib);
                glVertexAttribPointer(
                    colorMapAttrib,
                    4,
                    GL_FLOAT,
                    GL_FALSE,
                    sizeof(float) * 8,
                    reinterpret_cast<void*>(sizeof(float)*4)
                );
            }
            else {                                                
                glEnableVertexAttribArray(positionAttrib);
                glVertexAttribPointer(
                    positionAttrib,
                    4,
                    GL_FLOAT,
                    GL_FALSE,
                    0,
                    nullptr
                );
            }

            glBindVertexArray(0);

            _dataIsDirty = false;
        }

        if (_hasSpriteTexture && _spriteTextureIsDirty) {
            LDEBUG("Reloading Sprite Texture");
            _spriteTexture = nullptr;
            if (_spriteTexturePath.value() != "") {
                _spriteTexture = ghoul::io::TextureReader::ref().loadTexture(absPath(_spriteTexturePath));
                if (_spriteTexture) {
                    LDEBUG("Loaded texture from '" << absPath(_spriteTexturePath) << "'");
                    _spriteTexture->uploadTexture();
                }
                _spriteTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

                _spriteTextureFile = std::make_unique<ghoul::filesystem::File>(
                    _spriteTexturePath);
                _spriteTextureFile->setCallback(
                    [&](const ghoul::filesystem::File&) { _spriteTextureIsDirty = true; }
                    );
            }
            _spriteTextureIsDirty = false;            
        }
    }

    bool RenderableBillboardsCloud::loadData() {
        std::string _file = _speckFile;
        std::string cachedFile = FileSys.cacheManager()->cachedFilename(
            _file,
            ghoul::filesystem::CacheManager::Persistent::Yes
        );

        bool hasCachedFile = FileSys.fileExists(cachedFile);
        if (hasCachedFile) {
            LINFO("Cached file '" << cachedFile << "' used for Speck file '" << _file << "'");

            bool success = loadCachedFile(cachedFile);
            if (success) {
                if (_hasColorMapFile) {
                    success &= readColorMapFile();
                }
                return success;
            }
            else {
                FileSys.cacheManager()->removeCacheFile(_file);
                // Intentional fall-through to the 'else' computation to generate the cache
                // file for the next run
            }
        }
        else {
            LINFO("Cache for Speck file '" << _file << "' not found");
        }
        LINFO("Loading Speck file '" << _file << "'");

        bool success = readSpeckFile();
        if (!success) {
            return false;
        }

        LINFO("Saving cache");
        success = saveCachedFile(cachedFile);

        if (_hasColorMapFile) {
            success &= readColorMapFile();
        }

        return success;
    }

    bool RenderableBillboardsCloud::readSpeckFile() {
        std::string _file = _speckFile;
        std::ifstream file(_file);
        if (!file.good()) {
            LERROR("Failed to open Speck file '" << _file << "'");
            return false;
        }

        _nValuesPerAstronomicalObject = 0;

        // The beginning of the speck file has a header that either contains comments
        // (signaled by a preceding '#') or information about the structure of the file
        // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
        std::string line = "";
        while (true) {
            std::streampos position = file.tellg();
            std::getline(file, line);
            
            if (line[0] == '#' || line.empty()) {
                continue;
            }

            if (line.substr(0, 7) != "datavar" &&
                line.substr(0, 10) != "texturevar" &&
                line.substr(0, 7) != "texture")
            {
                // we read a line that doesn't belong to the header, so we have to jump back
                // before the beginning of the current line
                file.seekg(position);
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
                str >> dummy;
                str >> _nValuesPerAstronomicalObject;
                _nValuesPerAstronomicalObject += 1; // We want the number, but the index is 0 based
            }
        }

        _nValuesPerAstronomicalObject += 3; // X Y Z are not counted in the Speck file indices

        do {
            std::vector<float> values(_nValuesPerAstronomicalObject);

            std::getline(file, line);
            
            if (line.size() == 0)
                continue;
            
            std::stringstream str(line);

            for (int i = 0; i < _nValuesPerAstronomicalObject; ++i) {
                str >> values[i];
            }

            _fullData.insert(_fullData.end(), values.begin(), values.end());
        } while (!file.eof());

        return true;
    }

    bool RenderableBillboardsCloud::readColorMapFile() {
        std::string _file = _colorMapFile;
        std::ifstream file(_file);
        if (!file.good()) {
            LERROR("Failed to open Color Map file '" << _file << "'");
            return false;
        }
 
        std::size_t numberOfColors = 0;

        // The beginning of the speck file has a header that either contains comments
        // (signaled by a preceding '#') or information about the structure of the file
        // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
        std::string line = "";
        while (true) {
            std::streampos position = file.tellg();
            std::getline(file, line);

            if (line[0] == '#' || line.empty()) {
                continue;
            }

            // Initial number of colors
            std::locale loc;
            if (std::isdigit(line[0], loc)) {
                std::string::size_type sz;
                numberOfColors = std::stoi(line, &sz);
                break;
            }
            else if (file.eof()) {
                return false;
            }            
        }
        
        for (auto i = 0; i < numberOfColors; ++i) {
            std::getline(file, line);
            std::stringstream str(line);
            
            glm::vec4 color;
            for (auto j = 0; j < 4; ++j) {
                str >> color[j];
            }

            _colorMapData.push_back(color);
        }
        
        return true;
    }

    bool RenderableBillboardsCloud::loadCachedFile(const std::string& file) {
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

    bool RenderableBillboardsCloud::saveCachedFile(const std::string& file) const {
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
    
    void RenderableBillboardsCloud::createDataSlice() {
        _slicedData.clear();
        if (_hasColorMapFile) {
            _slicedData.reserve(8 * (_fullData.size() / _nValuesPerAstronomicalObject));
        }
        else {
            _slicedData.reserve(4 * (_fullData.size()/_nValuesPerAstronomicalObject));
        }

        int colorIndex = 0;
        for (size_t i = 0; i < _fullData.size(); i += _nValuesPerAstronomicalObject) {
            glm::vec3 p = glm::vec3(_fullData[i + 0], _fullData[i + 1], _fullData[i + 2]);
         
            /*
            // Converting untis
            if (_unit == Kilometer) {
                p *= 1E3;
            } 
            else if (_unit == Parsec) {
                p *= PARSEC;
            }
            else if (_unit == Kiloparsec) {
                p *= 1E3 * PARSEC;
            }
            else if (_unit == Megaparsec) {
                p *= 1E6 * PARSEC;
            }
            else if (_unit == Gigaparsec) {
                p *= 1E9 * PARSEC;
            }
            else if (_unit == GigalightYears) {
                p *= 306391534.73091 * PARSEC;
            }
            */

            glm::vec4 position(p, static_cast<float>(_unit));
            //glm::dvec4 position(p, 1.0);

            if (_hasColorMapFile) {
                for (auto j = 0; j < 4; ++j) {
                    _slicedData.push_back(position[j]);
                }
                for (auto j = 0; j < 4; ++j) {
                    _slicedData.push_back(_colorMapData[colorIndex][j]);
                }
            }
            else {
                for (auto j = 0; j < 4; ++j) {
                    _slicedData.push_back(position[j]);
                }
            }
            
            colorIndex = (colorIndex == (_colorMapData.size() - 1)) ? 0 : colorIndex + 1;
        }
    }

    void RenderableBillboardsCloud::createPolygonTexture() {
        LDEBUG("Creating Polygon Texture");

        glGenTextures(1, &_pTexture);
        glBindTexture(GL_TEXTURE_2D, _pTexture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        // Stopped using a buffer object for GL_PIXEL_UNPACK_BUFFER
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 256,
            256, 0, GL_RGBA, GL_BYTE, nullptr);

        renderToTexture(std::bind(&openspace::RenderableBillboardsCloud::loadPolygonGeometryForRendering, 
            this),
            std::bind(&openspace::RenderableBillboardsCloud::renderPolygonGeometry, 
            this, std::placeholders::_1),
            _pTexture, 256, 256);          
    }

    void RenderableBillboardsCloud::createTextTexture() {
        glGenTextures(1, &_tTexture);
        glBindTexture(GL_TEXTURE_2D, _pTexture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        // Stopped using a buffer object for GL_PIXEL_UNPACK_BUFFER
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 256,
            256, 0, GL_RGBA, GL_BYTE, nullptr);

        renderToTexture(std::bind(&openspace::RenderableBillboardsCloud::loadTextGeometryForRendering,
            this),
            std::bind(&openspace::RenderableBillboardsCloud::renderTextgonGeometry,
                this, std::placeholders::_1),
            _pTexture, 256, 256);
    }

    void RenderableBillboardsCloud::renderToTexture(
        std::function<GLuint(void)> geometryLoadingFunction,
        std::function<void(GLuint)> renderFunction,
        GLuint textureToRenderTo, GLuint textureWidth, GLuint textureHeight) {
        LDEBUG("Rendering to Texture");
        
        // Saves initial Application's OpenGL State
        GLint defaultFBO;
        GLint viewport[4];
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);
        glGetIntegerv(GL_VIEWPORT, viewport);

        GLuint calcFBO;
        glGenFramebuffers(1, &calcFBO);
        glBindFramebuffer(GL_FRAMEBUFFER, calcFBO);
        GLenum drawBuffers[1] = { GL_COLOR_ATTACHMENT0 };
        glDrawBuffers(1, drawBuffers);

        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, textureToRenderTo, 0);
        if (glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
            LERROR("Framework not built. Polygon Texture");
            GLenum fbErr = glCheckFramebufferStatus(GL_FRAMEBUFFER);
            switch (fbErr) {
            case GL_FRAMEBUFFER_UNDEFINED:
                LERROR("Indefined framebuffer.");
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
                LERROR("Incomplete, missing attachement.");
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:
                LERROR("Framebuffer doesn't have at least one image attached to it.");
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER:
                LERROR("Returned if the value of GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is GL_NONE \
                        for any color attachment point(s) named by GL_DRAW_BUFFERi.");
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER:
                LERROR("Returned if GL_READ_BUFFER is not GL_NONE and the value of \
                        GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is GL_NONE for the color attachment point \
                        named by GL_READ_BUFFER.");
                break;
            case GL_FRAMEBUFFER_UNSUPPORTED:
                LERROR("Returned if the combination of internal formats of the attached images \
                        violates an implementation - dependent set of restrictions.");
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE:
                LERROR("Returned if the value of GL_RENDERBUFFE_r_samples is not the same for all \
                        attached renderbuffers; if the value of GL_TEXTURE_SAMPLES is the not same for all \
                        attached textures; or , if the attached images are a mix of renderbuffers and textures, \
                        the value of GL_RENDERBUFFE_r_samples does not match the value of GL_TEXTURE_SAMPLES.");
                LERROR("Returned if the value of GL_TEXTURE_FIXED_SAMPLE_LOCATIONS is not the same \
                        for all attached textures; or , if the attached images are a mix of renderbuffers and \
                        textures, the value of GL_TEXTURE_FIXED_SAMPLE_LOCATIONS is not GL_TRUE for all attached textures.");
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS:
                LERROR("Returned if any framebuffer attachment is layered, and any populated attachment \
                        is not layered, or if all populated color attachments are not from textures of the same target.");
                break;
            default:
                LDEBUG("No error found checking framebuffer: Polygon Texture");
                std::cout << "=== OK ===" << std::endl;
                break;
            }
        }
        glViewport(0, 0, textureWidth, textureHeight);

        RenderEngine& renderEngine = OsEng.renderEngine();

       
        GLuint pointVao = geometryLoadingFunction();
        renderFunction(pointVao);
        
        // Restores Applications' OpenGL State
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
        glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);

        //glDeleteBuffers(1, &pointVbo);
        glDeleteVertexArrays(1, &pointVao);
        glDeleteFramebuffers(1, &calcFBO);
    }

    GLuint RenderableBillboardsCloud::loadPolygonGeometryForRendering() {
        GLuint pointVao, pointVbo;
        glGenVertexArrays(1, &pointVao);
        glGenBuffers(1, &pointVbo);
        glBindVertexArray(pointVao);
        glBindBuffer(GL_ARRAY_BUFFER, pointVbo);

        const GLfloat vertex_data[] = {
            //      x      y     z     w
            0.0f, 0.0f, 0.0f, 1.0f,
        };

        glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
        glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 4, reinterpret_cast<GLvoid*>(0));
        glEnableVertexAttribArray(0);
        glBindVertexArray(0);

        return pointVao;
    }

    void RenderableBillboardsCloud::renderPolygonGeometry(GLuint vao) {
        RenderEngine& renderEngine = OsEng.renderEngine();

        std::unique_ptr<ghoul::opengl::ProgramObject> program = ghoul::opengl::ProgramObject::Build("RenderableBillboardsCloud_Polygon",
            "${MODULE_DIGITALUNIVERSE}/shaders/billboardpolygon_vs.glsl",
            "${MODULE_DIGITALUNIVERSE}/shaders/billboardpolygon_fs.glsl",
            "${MODULE_DIGITALUNIVERSE}/shaders/billboardpolygon_gs.glsl");

        program->activate();
        static const float black[] = { 0.0f, 0.0f, 0.0f, 0.0f };
        glClearBufferfv(GL_COLOR, 0, black);

        program->setUniform("sides", _polygonSides);
        program->setUniform("polygonColor", _pointColor);

        glBindVertexArray(vao);
        glDrawArrays(GL_POINTS, 0, 1);
        glBindVertexArray(0);

        if (true) {
            saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, std::string("polygon_texture.ppm"),
                256, 256);

        }
        program->deactivate();
    }

    GLuint RenderableBillboardsCloud::loadTextGeometryForRendering() {
        GLuint textVao = 0;
        return textVao;
    }

    void RenderableBillboardsCloud::renderTextgonGeometry(GLuint vao) {
       /* size_t _fontSize = 50;
        std::shared_ptr<ghoul::fontrendering::Font> _font = OsEng.fontManager().font("Mono", static_cast<float>(_fontSize));
        _fontRenderer = std::unique_ptr<ghoul::fontrendering::FontRenderer>(ghoul::fontrendering::FontRenderer::createDefault());
        _fontRenderer->setFramebufferSize(glm::vec2(256.0f,256.0f));
        
        std::string unit = "m";        
        glm::vec2 textPosition;
        textPosition.x = 0;
        textPosition.y = _fontSize / 2.f;
        
        glm::vec4 color(1.0, 1.0, 1.0, 1.0);

        ;

        _fontRenderer->render(
            *_font,
            textPosition,
            color,
            " %.0f %s",
            10.0, unit.c_str()
        );*/
    }

    void RenderableBillboardsCloud::saveTextureToPPMFile(const GLenum color_buffer_attachment,
        const std::string & fileName, const int width, const int height) const {
        std::fstream ppmFile;

        ppmFile.open(fileName.c_str(), std::fstream::out);
        if (ppmFile.is_open()) {
            unsigned char * pixels = new unsigned char[width*height * 3];
            for (int t = 0; t < width*height * 3; ++t)
                pixels[t] = 255;

            if (color_buffer_attachment != GL_DEPTH_ATTACHMENT) {
                glReadBuffer(color_buffer_attachment);
                glReadPixels(0, 0, width, height, GL_RGB, GL_UNSIGNED_BYTE, pixels);

            }
            else {
                glReadPixels(0, 0, width, height, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE, pixels);
            }

            ppmFile << "P3" << std::endl;
            ppmFile << width << " " << height << std::endl;
            ppmFile << "255" << std::endl;

            std::cout << "\n\nFILE\n\n";
            int k = 0;
            for (int i = 0; i < width; i++) {
                for (int j = 0; j < height; j++) {
                    ppmFile << (unsigned int)pixels[k] << " " << (unsigned int)pixels[k + 1] << " " << (unsigned int)pixels[k + 2] << " ";
                    k += 3;
                }
                ppmFile << std::endl;
            }
            delete[] pixels;

            ppmFile.close();
        }
    }
    

} // namespace openspace
