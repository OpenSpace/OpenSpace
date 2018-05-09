/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_GAIAMISSION___RENDERABLEGAIASTARS___H__
#define __OPENSPACE_MODULE_GAIAMISSION___RENDERABLEGAIASTARS___H__

#include <openspace/rendering/renderable.h>

#include <modules/gaiamission/rendering/renderoption.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringlistproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/boolproperty.h>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <ghoul/opengl/bufferbinding.h>

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

class OctreeManager;

namespace documentation { struct Documentation; }

class RenderableGaiaStars : public Renderable {
public:
    explicit RenderableGaiaStars(const ghoul::Dictionary& dictionary);
    ~RenderableGaiaStars();

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    const size_t POS_SIZE = 3;
    const size_t COL_SIZE = 2;
    const size_t VEL_SIZE = 3;
    const float MAX_GPU_MEMORY_PERCENT = 0.8f;
    const float MAX_CPU_RAM_PERCENT = 0.5f;

    enum FileReaderOption {
        Fits = 0,
        Speck = 1,
        BinaryRaw = 2,
        BinaryOctree = 3,
        StreamOctree = 4
    };
    
    enum ShaderOption {
        Point_SSBO = 0,
        Point_VBO = 1,
        Billboard_SSBO = 2,
        Billboard_VBO = 3
    };
    
    bool readDataFile();
    int readFitsFile(std::string filePath);
    int readSpeckFile(std::string filePath);
    int readBinaryRawFile(std::string filePath);
    int readBinaryOctreeFile(std::string filePath);
    int readBinaryOctreeStructureFile(std::string folderPath);


    properties::StringProperty _filePath;
    std::unique_ptr<ghoul::filesystem::File> _dataFile;
    bool _dataIsDirty;
    bool _buffersAreDirty;
    bool _shadersAreDirty;

    properties::StringProperty _pointSpreadFunctionTexturePath;
    std::unique_ptr<ghoul::opengl::Texture> _pointSpreadFunctionTexture;
    std::unique_ptr<ghoul::filesystem::File> _pointSpreadFunctionFile;
    bool _pointSpreadFunctionTextureIsDirty;

    properties::StringProperty _colorTexturePath;
    std::unique_ptr<ghoul::opengl::Texture> _colorTexture;
    std::unique_ptr<ghoul::filesystem::File> _colorTextureFile;
    bool _colorTextureIsDirty;

    properties::FloatProperty _luminosityMultiplier;
    properties::FloatProperty _magnitudeBoost;
    properties::FloatProperty _cutOffThreshold;
    properties::FloatProperty _sharpness;
    properties::FloatProperty _billboardSize;
    properties::FloatProperty _closeUpBoostDist;

    properties::IntProperty _firstRow;
    properties::IntProperty _lastRow;
    properties::StringListProperty _columnNamesList;
    std::vector<std::string> _columnNames;
    properties::OptionProperty _fileReaderOption;
    properties::OptionProperty _renderOption;
    properties::OptionProperty _shaderOption;
    properties::IntProperty _nRenderedStars;
    // LongLongProperty doesn't show up in menu, use FloatProperty instead.
    properties::FloatProperty _cpuRamBudgetProperty;
    properties::FloatProperty _ssboStreamBudgetProperty;

    std::unique_ptr<ghoul::opengl::ProgramObject> _program;
    UniformCache(model, view, viewScaling, projection, renderOption, luminosityMultiplier,
        magnitudeBoost, cutOffThreshold, sharpness, billboardSize, closeUpBoostDist, 
        screenSize, psfTexture, time, colorTexture, nChunksToRender, valuesPerStar, 
        maxStarsPerNode) _uniformCache;

    std::unique_ptr<ghoul::opengl::ProgramObject> _programTM;
    UniformCache(renderedTexture, screenSize) _uniformCacheTM;
    std::unique_ptr<ghoul::opengl::Texture> _fboTexture;

    std::shared_ptr<OctreeManager> _octreeManager;
    std::unique_ptr<ghoul::opengl::BufferBinding<
        ghoul::opengl::bufferbinding::Buffer::ShaderStorage>> _ssboIdxBinding;
    std::unique_ptr<ghoul::opengl::BufferBinding<
        ghoul::opengl::bufferbinding::Buffer::ShaderStorage>> _ssboDataBinding;

    std::vector<int> _accumulatedIndices;
    size_t _nRenderValuesPerStar;
    int _nStarsToRender;
    bool _firstDrawCalls;
    glm::dquat _previousCameraRotation;
    bool _initialDataFilesLoaded;
    bool _useVBO;
    long long _cpuRamBudgetInBytes;
    long long _totalDatasetSizeInBytes;
    long long _gpuMemoryBudgetInBytes;
    long long _maxStreamingBudgetInBytes;
    size_t _chunkSize;

    GLuint _vao;
    GLuint _vaoEmpty;
    GLuint _vboPos;
    GLuint _vboCol;
    GLuint _vboVel;
    GLuint _ssboIdx;
    GLuint _ssboData;
    GLuint _vaoQuad;
    GLuint _vboQuad;
    GLuint _fbo;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GAIAMISSION___RENDERABLEGAIASTARS___H__
