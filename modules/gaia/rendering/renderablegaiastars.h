/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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
#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/ivec2property.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/bufferbinding.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <filesystem>
#include <memory>

namespace ghoul::filesystem { class File; }

namespace openspace {

class RenderableGaiaStars : public Renderable {
public:
    explicit RenderableGaiaStars(const ghoul::Dictionary& dictionary);
    ~RenderableGaiaStars() override = default;

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static openspace::Documentation Documentation();

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

    StringProperty _filePath;
    std::unique_ptr<ghoul::filesystem::File> _dataFile;

    StringProperty _colorTexturePath;
    std::unique_ptr<ghoul::opengl::Texture> _colorTexture;
    std::unique_ptr<ghoul::filesystem::File> _colorTextureFile;

    FloatProperty _luminosityMultiplier;
    FloatProperty _cutOffThreshold;
    IntProperty _tmPointFilterSize;
    FloatProperty _tmPointSigma;
    IVec2Property _additionalNodes;
    FloatProperty _tmPointPixelWeightThreshold;
    FloatProperty _lodPixelThreshold;

    Vec2Property _posXThreshold;
    Vec2Property _posYThreshold;
    Vec2Property _posZThreshold;
    Vec2Property _gMagThreshold;
    Vec2Property _bpRpThreshold;
    Vec2Property _distThreshold;

    IntProperty _firstRow;
    IntProperty _lastRow;
    std::vector<std::string> _columnNames;
    OptionProperty _fileReaderOption;
    OptionProperty _renderMode;
    IntProperty _nRenderedStars;
    // LongLongProperty doesn't show up in menu, use FloatProperty instead.
    FloatProperty _cpuRamBudgetProperty;
    FloatProperty _gpuStreamBudgetProperty;
    FloatProperty _maxGpuMemoryPercent;
    FloatProperty _maxCpuMemoryPercent;

    std::unique_ptr<ghoul::opengl::ProgramObject> _program;
    UniformCache(model, view, viewScaling, projection, renderOption, luminosityMultiplier,
        cutOffThreshold, time, colorTexture, nChunksToRender, valuesPerStar,
        maxStarsPerNode, posXThreshold, posYThreshold, posZThreshold, gMagThreshold,
        bpRpThreshold, distThreshold)
        _uniformCache;

    std::unique_ptr<ghoul::opengl::ProgramObject> _programTM;
    UniformCache(renderedTexture, screenSize, filterSize, sigma, pixelWeightThreshold,
        projection) _uniformCacheTM;
    std::unique_ptr<ghoul::opengl::Texture> _fboTexture;

    OctreeManager _octreeManager;
    std::unique_ptr<ghoul::opengl::BufferBinding<
        ghoul::opengl::bufferbinding::Buffer::ShaderStorage>> _ssboIdxBinding;
    std::unique_ptr<ghoul::opengl::BufferBinding<
        ghoul::opengl::bufferbinding::Buffer::ShaderStorage>> _ssboDataBinding;

    bool _dataIsDirty = true;
    bool _buffersAreDirty = true;
    bool _colorTextureIsDirty = true;

    std::vector<int> _accumulatedIndices;
    size_t _nRenderValuesPerStar = 0;
    int _nStarsToRender = 0;
    bool _firstDrawCalls = true;
    long long _cpuRamBudgetInBytes = 0;
    long long _totalDatasetSizeInBytes = 0;
    long long _gpuMemoryBudgetInBytes = 0;
    long long _maxStreamingBudgetInBytes = 0;
    size_t _chunkSize = 0;

    GLuint _vaoEmpty = 0;
    GLuint _ssboIdx = 0;
    GLuint _ssboData = 0;
    GLuint _vaoQuad = 0;
    GLuint _vboQuad = 0;
    GLuint _fbo = 0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GAIA___RENDERABLEGAIASTARS___H__
