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

#include <array>
#include <fstream>
#include <stdint.h>

namespace {
    const char* _loggerCat        = "RenderablePoints";
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
}  // namespace

namespace openspace {

    documentation::Documentation RenderableDUMeshes::Documentation() {
        using namespace documentation;
        return {
            "RenderablePoints",
            "digitaluniverse_renderablepoints",
            {
                {
                    "Type",
                    new StringEqualVerifier("RenderablePoints"),
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
                }
            }
        };
    }


    RenderableDUMeshes::RenderableDUMeshes(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary)
        , _dataIsDirty(true)
        , _alphaValue(TransparencyInfo, 1.f, 0.f, 1.f)
        , _scaleFactor(ScaleFactorInfo, 1.f, 0.f, 64.f)
        , _pointColor(ColorInfo, glm::vec3(1.f, 0.4f, 0.2f), glm::vec3(0.f, 0.f, 0.f), glm::vec3(1.0f, 1.0f, 1.0f))
        , _program(nullptr)
        , _speckFile("")
        , _unit(Parsec)
        , _nValuesPerAstronomicalObject(0)
        , _vao(0)
        , _vbo(0)
    {
        using File = ghoul::filesystem::File;

        documentation::testSpecificationAndThrow(
            Documentation(),
            dictionary,
            "RenderableDUMeshes"
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
                LWARNING("No unit given for RenderableDUMeshes. Using meters as units.");
                _unit = Meter;
            }
        }

        if (dictionary.hasKey(keyColor)) {
            _pointColor = dictionary.value<glm::vec3>(keyColor);
        }
        addProperty(_pointColor);

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

    }

    bool RenderableDUMeshes::isReady() const {
        return (_program != nullptr) && (!_fullData.empty());
    }

    void RenderableDUMeshes::initialize() {
        RenderEngine& renderEngine = OsEng.renderEngine();
        _program = renderEngine.buildRenderProgram("RenderableDUMeshes",
            "${MODULE_DIGITALUNIVERSE}/shaders/points_vs.glsl",
            "${MODULE_DIGITALUNIVERSE}/shaders/points_fs.glsl");// ,
            //"${MODULE_DIGITALUNIVERSE}/shaders/points_ge.glsl");

        bool success = loadData();
        if (!success) {
            throw ghoul::RuntimeError("Error loading data");
        }
    }

    void RenderableDUMeshes::deinitialize() {
        glDeleteBuffers(1, &_vbo);
        _vbo = 0;
        glDeleteVertexArrays(1, &_vao);
        _vao = 0;

        RenderEngine& renderEngine = OsEng.renderEngine();
        if (_program) {
            renderEngine.removeRenderProgram(_program);
            _program = nullptr;
        }
    }

    void RenderableDUMeshes::render(const RenderData& data, RendererTasks&) {
        glDepthMask(false);
        _program->activate();

        glm::dmat4 modelMatrix = glm::dmat4(1.0);
        
        using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
        _program->setIgnoreUniformLocationError(IgnoreError::Yes);
        _program->setUniform("modelViewProjectionTransform", glm::dmat4(data.camera.projectionMatrix()) * 
            data.camera.combinedViewMatrix() * modelMatrix);

        _program->setUniform("color", _pointColor);
        _program->setUniform("alphaValue", _alphaValue);
        _program->setUniform("scaleFactor", _scaleFactor);
        
        //setPscUniforms(*_program.get(), data.camera, data.position);
        //_program->setUniform("scaling", scaling);
        

        glEnable(GL_PROGRAM_POINT_SIZE);
        glBindVertexArray(_vao);
        const GLsizei nAstronomicalObjects = static_cast<GLsizei>(_fullData.size() / _nValuesPerAstronomicalObject);
        glDrawArrays(GL_POINTS, 0, nAstronomicalObjects);
        
        glDisable(GL_PROGRAM_POINT_SIZE);
        glBindVertexArray(0);
        using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
        _program->setIgnoreUniformLocationError(IgnoreError::No);
        _program->deactivate();

        glDepthMask(true);
    }

    void RenderableDUMeshes::update(const UpdateData&) {
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
                size * sizeof(double),
                &_slicedData[0],
                GL_STATIC_DRAW
            );

            GLint positionAttrib = _program->attributeLocation("in_position");
            
            const size_t nAstronomicalObjects = _fullData.size() / _nValuesPerAstronomicalObject;
            const size_t nValues = _slicedData.size() / nAstronomicalObjects;

            GLsizei stride = static_cast<GLsizei>(sizeof(double) * nValues);

            glEnableVertexAttribArray(positionAttrib);
            glVertexAttribLPointer(
                positionAttrib,
                4,
                GL_DOUBLE,
                0,
                nullptr
            );
            
            glBindBuffer(GL_ARRAY_BUFFER, 0);
            glBindVertexArray(0);

            _dataIsDirty = false;
        }
    }

    bool RenderableDUMeshes::loadData() {
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
                return true;
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

        return success;
    }

    bool RenderableDUMeshes::readSpeckFile() {
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
            std::stringstream str(line);

            for (int i = 0; i < _nValuesPerAstronomicalObject; ++i) {
                str >> values[i];
            }

            _fullData.insert(_fullData.end(), values.begin(), values.end());
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
    
    void RenderableDUMeshes::createDataSlice() {
        _slicedData.clear();

        for (size_t i = 0; i < _fullData.size(); i += _nValuesPerAstronomicalObject) {
            glm::dvec3 p = glm::dvec3(_fullData[i + 0], _fullData[i + 1], _fullData[i + 2]);

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

            glm::dvec4 position(p, 1.0);

            for (auto j = 0; j < 4; ++j) {
                _slicedData.push_back(position[j]);
            }                        
        }
    }
    

} // namespace openspace
