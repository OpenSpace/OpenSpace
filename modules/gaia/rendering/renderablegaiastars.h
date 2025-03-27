/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_GAIA___RENDERABLEGAIASTARS___H__
#define __OPENSPACE_MODULE_GAIA___RENDERABLEGAIASTARS___H__

#include <openspace/rendering/renderable.h>

#include <modules/gaia/rendering/octreemanager.h>
#include <openspace/properties/list/stringlistproperty.h>
#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/ivec2property.h>
#include <ghoul/opengl/bufferbinding.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }

class RenderableGaiaStars : public Renderable {
public:
    explicit RenderableGaiaStars(const ghoul::Dictionary& dictionary);
    ~RenderableGaiaStars() override = default;

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    /**
     * Reads data file in format defined by FileReaderOption.
     *
     * \return `true` if data was successfully read
     */
    bool readDataFile();

    /**
     * Reads a FITS file by using FitsFileReader.readFitsFile() and constructs an octree.
     *
     * \return The number of stars read
     */
    int readFitsFile(const std::filesystem::path& filePath);

    /**
     * Read a SPECK file by using FitsFileReader.readSpeckFile() and constructs an octree.
     *
     * \return The number of stars read
     */
    int readSpeckFile(const std::filesystem::path& filePath);

    /**
     * Reads a preprocessed binary file and constructs an octree.
     *
     * \return The number of stars read
     */
    int readBinaryRawFile(const std::filesystem::path& filePath);

    /**
     * Reads a pre-constructed octree, with all data, from a binary file.
     *
     * \return The number of stars read
     */
    int readBinaryOctreeFile(const std::filesystem::path& filePath);

    /**
     * Reads the structure of a pre-constructed octree from a binary file, without any
     * data.
     *
     * \return The number of stars read
     */
    int readBinaryOctreeStructureFile(const std::filesystem::path& folderPath);

    /**
     * Checks for any OpenGL errors and reports these to the log if _reportGlErrors is
     * set to true.
     */
    void checkGlErrors(const std::string& identifier) const;

    properties::StringProperty _filePath;
    std::unique_ptr<ghoul::filesystem::File> _dataFile;
    bool _dataIsDirty = true;
    bool _buffersAreDirty = true;
    bool _shadersAreDirty = false;

    properties::StringProperty _pointSpreadFunctionTexturePath;
    std::unique_ptr<ghoul::opengl::Texture> _pointSpreadFunctionTexture;
    std::unique_ptr<ghoul::filesystem::File> _pointSpreadFunctionFile;
    bool _pointSpreadFunctionTextureIsDirty = true;

    properties::StringProperty _colorTexturePath;
    std::unique_ptr<ghoul::opengl::Texture> _colorTexture;
    std::unique_ptr<ghoul::filesystem::File> _colorTextureFile;
    bool _colorTextureIsDirty = true;

    properties::FloatProperty _luminosityMultiplier;
    properties::FloatProperty _magnitudeBoost;
    properties::FloatProperty _cutOffThreshold;
    properties::FloatProperty _sharpness;
    properties::FloatProperty _billboardSize;
    properties::FloatProperty _closeUpBoostDist;
    properties::IntProperty _tmPointFilterSize;
    properties::FloatProperty _tmPointSigma;
    properties::IVec2Property _additionalNodes;
    properties::FloatProperty _tmPointPixelWeightThreshold;
    properties::FloatProperty _lodPixelThreshold;

    properties::Vec2Property _posXThreshold;
    properties::Vec2Property _posYThreshold;
    properties::Vec2Property _posZThreshold;
    properties::Vec2Property _gMagThreshold;
    properties::Vec2Property _bpRpThreshold;
    properties::Vec2Property _distThreshold;

    properties::IntProperty _firstRow;
    properties::IntProperty _lastRow;
    properties::StringListProperty _columnNamesList;
    std::vector<std::string> _columnNames;
    properties::OptionProperty _fileReaderOption;
    properties::OptionProperty _renderMode;
    properties::OptionProperty _shaderOption;
    properties::IntProperty _nRenderedStars;
    // LongLongProperty doesn't show up in menu, use FloatProperty instead.
    properties::FloatProperty _cpuRamBudgetProperty;
    properties::FloatProperty _gpuStreamBudgetProperty;
    properties::FloatProperty _maxGpuMemoryPercent;
    properties::FloatProperty _maxCpuMemoryPercent;

    properties::BoolProperty _reportGlErrors;

    std::unique_ptr<ghoul::opengl::ProgramObject> _program;
    UniformCache(model, view, cameraPos, cameraLookUp, viewScaling, projection,
        renderOption, luminosityMultiplier, magnitudeBoost, cutOffThreshold,
        sharpness, billboardSize, closeUpBoostDist, screenSize, psfTexture,
        time, colorTexture, nChunksToRender, valuesPerStar, maxStarsPerNode)
        _uniformCache;

    UniformCache(posXThreshold, posYThreshold, posZThreshold, gMagThreshold,
        bpRpThreshold, distThreshold) _uniformFilterCache;

    std::unique_ptr<ghoul::opengl::ProgramObject> _programTM;
    UniformCache(renderedTexture, screenSize, filterSize, sigma, pixelWeightThreshold,
        projection) _uniformCacheTM;
    std::unique_ptr<ghoul::opengl::Texture> _fboTexture;

    OctreeManager _octreeManager;
    std::unique_ptr<ghoul::opengl::BufferBinding<
        ghoul::opengl::bufferbinding::Buffer::ShaderStorage>> _ssboIdxBinding;
    std::unique_ptr<ghoul::opengl::BufferBinding<
        ghoul::opengl::bufferbinding::Buffer::ShaderStorage>> _ssboDataBinding;

    std::vector<int> _accumulatedIndices;
    size_t _nRenderValuesPerStar = 0;
    int _nStarsToRender = 0;
    bool _firstDrawCalls = true;
    glm::dquat _previousCameraRotation = glm::dquat(1.0, 0.0, 0.0, 0.0);
    bool _useVBO = false;
    long long _cpuRamBudgetInBytes = 0;
    long long _totalDatasetSizeInBytes = 0;
    long long _gpuMemoryBudgetInBytes = 0;
    long long _maxStreamingBudgetInBytes = 0;
    size_t _chunkSize = 0;

    GLuint _vao = 0;
    GLuint _vaoEmpty = 0;
    GLuint _vboPos = 0;
    GLuint _vboCol = 0;
    GLuint _vboVel = 0;
    GLuint _ssboIdx = 0;
    GLuint _ssboData = 0;
    GLuint _vaoQuad = 0;
    GLuint _vboQuad = 0;
    GLuint _fbo = 0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GAIA___RENDERABLEGAIASTARS___H__
