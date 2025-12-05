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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCE___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCE___H__

#include <openspace/rendering/renderable.h>

#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/misc/triggerproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/rendering/transferfunction.h>
#include <openspace/util/dynamicfilesequencedownloader.h>
#include <deque>
#include <vector>

namespace openspace {

class RenderableFieldlinesSequence : public Renderable {
public:
    enum class LoadingType {
        StaticLoading,
        DynamicDownloading
    };

    enum class SourceFileType {
        Cdf,
        Json,
        Osfls
    };

    // Used to determine if lines should be colored uniformly or by some data variable
    enum class ColorMethod {
        Uniform,
        ByQuantity
    };

    explicit RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary);
    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;
    void updateDynamicDownloading(double currentTime, double deltaTime);
    void firstUpdate();
    void computeSequenceEndTime();

    static documentation::Documentation Documentation();

    struct File {
        enum class FileStatus {
            Downloaded,
            Loaded
        };
        FileStatus status;
        std::filesystem::path path;
        // Assume timestamp is -1 until FileStatus is = Loaded
        double timestamp = -1.0;
        FieldlinesState state;

        bool operator<(const File& other) const noexcept {
            return timestamp < other.timestamp;
        }
    };

private:
    void setupProperties();
    void setModelDependentConstants();

    int updateActiveIndex(double currentTime);
    void updateVertexPositionBuffer();
    void updateVertexColorBuffer();
    void updateVertexMaskingBuffer();

    void staticallyLoadFiles(const std::optional<std::filesystem::path>& seed,
        const std::optional<std::string>& traceVariable);

    std::deque<File> _files;
    // The function loads the file in the sense that it creates the FieldlineState object
    // in the File object. The function also deletes the oldest file if the loadedFiles
    // queue is full.
    void loadFile(File& file);
    void trackOldest(File& file);

    // Not optional, but initialized with osfls because that is what we expect to be used
    SourceFileType _inputFileType = SourceFileType::Osfls;
    // Static Loading on default if not specified
    LoadingType _loadingType = LoadingType::StaticLoading;
    // path to directory with seed point files
    std::filesystem::path _seedPointDirectory;
    // Which tracing variable to trace. Often 'b' is for magnetic field
    std::string _tracingVariable;
    // Extra variables such as rho, p or t for density, pressure or temperature
    std::vector<std::string> _extraVars;
    float _manualTimeOffset = 0.0;
    // Estimated / calculated end of sequence
    double _sequenceEndTime = 0.0;
    // If there's just one state it should never disappear
    bool _renderForever = true;
    bool _inInterval = false;

    // Data ID that corresponds to what dataset to use if using DynamicDownloading
    int _dataID;
    // Number of files to queue up at a time
    size_t _nFilesToQueue = 10;
    // To keep track of oldest file
    std::deque<File*> _loadedFiles;
    // The max number of files loaded at once
    size_t _maxLoadedFiles = 100;
    std::string _infoURL;
    std::string _dataURL;
    // DynamicFileSequenceDownloader downloads and updates renderable field lines with
    // field lines downloaded from the web.
    std::unique_ptr<DynamicFileSequenceDownloader> _dynamicFileDownloader;

    // In setup it is used to scale JSON coordinates. During runtime it is used to scale
    // domain limits.
    float _scalingFactor = 1.f;
    fls::Model _model = fls::Model::Invalid;
    bool _shouldUpdateMaskingBuffer = false;
    bool _shouldUpdateColorBuffer = false;
    int _activeIndex = -1;
    bool _atLeastOneFileLoaded = false;

    bool _isLoadingStateFromDisk = false;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shaderProgram;
    // Transfer function used to color lines when _pColorMethod is set to BY_QUANTITY
    std::unique_ptr<TransferFunction> _transferFunction;

    // OpenGL Vertex Array Object
    GLuint _vertexArrayObject = 0;
    // OpenGL Vertex Buffer Object containing the extraQuantity values used for coloring
    // the lines
    GLuint _vertexColorBuffer = 0;
    // OpenGL Vertex Buffer Object containing the extraQuantity values used for masking
    // out segments of the lines
    GLuint _vertexMaskingBuffer = 0;
    // OpenGL Vertex Buffer Object containing the vertex positions
    GLuint _vertexPositionBuffer = 0;

    // Group to hold the color properties
    properties::PropertyOwner _colorGroup;
    // Uniform/transfer function/topology?
    properties::OptionProperty _colorMethod;
    // Index of the extra quantity to color lines by
    properties::OptionProperty _colorQuantity;
    // Used to save property for later initialization, because firstUpdate needs to run
    // first, to populate _colorQuantity with options
    int _colorQuantityTemp;
    std::vector<glm::vec2> _colorTableRanges;
    // Color table/transfer function selected min and max range
    properties::Vec2Property _selectedColorRange;
    // Paths to color tables. One for each 'extraQuantity'
    std::vector<std::filesystem::path> _colorTablePaths;
    // Color table/transfer function for "By Quantity" coloring
    properties::StringProperty _colorTablePath;
    // Uniform Field Line Color
    properties::Vec4Property _colorUniform;
    // Whether or not to use additive blending
    properties::BoolProperty _colorABlendEnabled;

    // Whether or not to use Domain limits
    properties::BoolProperty _domainEnabled;
    // Group to hold the Domain properties
    properties::PropertyOwner _domainGroup;
    properties::Vec2Property _domainX;
    properties::Vec2Property _domainY;
    properties::Vec2Property _domainZ;
    properties::Vec2Property _domainR;

    // Toggle flow [ON/OFF]
    properties::BoolProperty _flowEnabled;
    // Group to hold the flow/particle properties
    properties::PropertyOwner _flowGroup;
    // Simulated particles' color
    properties::Vec4Property _flowColor;
    // Size of simulated flow particles
    properties::IntProperty _flowParticleSize;
    // Size of simulated flow particles
    properties::IntProperty _flowParticleSpacing;
    // Toggle flow direction [FORWARDS/BACKWARDS]
    properties::BoolProperty _flowReversed;
    // Speed of simulated flow
    properties::IntProperty _flowSpeed;

    // Whether or not to use masking
    properties::BoolProperty _maskingEnabled;
    // Group to hold the masking properties
    properties::PropertyOwner _maskingGroup;
    std::vector<glm::vec2> _maskingRanges;
    // Selected lower and upper range limits for masking
    properties::Vec2Property _selectedMaskingRange;
    // Index of the extra quantity to use for masking
    properties::OptionProperty _maskingQuantity;
    // used to save property for later initialization
    int _maskingQuantityTemp = 0;

    // Line width for the line rendering part
    properties::FloatProperty _lineWidth;
    // Button which executes a time jump to start of sequence
    properties::TriggerProperty _jumpToStart;
    properties::BoolProperty _saveDownloadsOnShutdown;

    bool _isFirstLoad = true;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCE___H__
