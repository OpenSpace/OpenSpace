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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCENEW___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCENEW___H__

#include <openspace/rendering/renderable.h>

#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <modules/fieldlinessequence/util/dynamicdownloadermanager.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec4property.h>

#include <openspace/rendering/transferfunction.h>

#include <filesystem>
#include <vector>


namespace openspace {

class RenderableFieldlinesSequenceNew : public Renderable {
public:
    RenderableFieldlinesSequenceNew(const ghoul::Dictionary& dictionary);
    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();


private:
    void definePropertyCallbackFunctions();
    void setupProperties();
    void setModelDependentConstants();
    void setupDynamicDownloading(const Parameters& p);
    // True when new state is loaded or user change which quantity used for masking out
    // line segments
    bool shouldUpdateColorBuffer();
    bool shouldUpdateMaskingBuffer();
    void updateVertexColorBuffer();
    void updateVertexMaskingBuffer();


    // remnent from old renderable needed to not break potential old assets from people
    bool _loadAtRuntime = false;
    //0: static loading and static downloading
    //1: dynamic loading but static downloading
    //2: dynamic loading and dynamic downloading
    enum class LoadingType {
        StaticLoading = 0,
        DynamicLoading = 1,
        DynamicDownloading = 2
    };
    LoadingType _loadingType;
    // dataID that corresponds to what dataset to use if using DynamicDownloading
    int _dataID;
    // number of files to queue up at a time
    int _nOfFilesToQueue = 10;
    std::string _baseURL = "";
    std::string _dataURL = "";
    //  DynamicDownloaderManager downloads and updates renderable field lines with
    //  field lines downloaded from the web.
    std::unique_ptr<DynamicDownloaderManager> _dynamicdownloaderManager;

    enum class SourceFileType {
        Cdf = 0,
        Json = 1,
        Osfls = 2
    };
    SourceFileType _inputFileType;

    // Used to determine if lines should be colored UNIFORMLY or by an extraQuantity
    enum class ColorMethod {
        Uniform = 0,
        ByQuantity = 1
    };

    struct File {
        enum class FileStatus {
            Available = 0,
            Downloaded = 1,
            Loaded = 2
        };
        FileStatus status;
        std::filesystem::path path;
        double timestamp;

    };

    // In setup it is used to scale JSON coordinates. During runtime it is used to scale
    // domain limits.
    float _scalingFactor = 1.f;


    std::vector<File> _files;
    size_t activeTriggerTimeIndex = -1;
    bool isInInterval = false;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shaderProgram;
    // Transfer function used to color lines when _pColorMethod is set to BY_QUANTITY
    std::unique_ptr<TransferFunction> _transferFunction;


    ///////////////////////////////////////////////
    //                PROPERTIES                 //
    ///////////////////////////////////////////////

    // Group to hold the color properties
    properties::PropertyOwner _colorGroup;
    // Uniform/transfer function/topology?
    properties::OptionProperty _colorMethod;
    // Index of the extra quantity to color lines by.
    //TODO: Change to options instead of index
    properties::OptionProperty _colorQuantity;
    // Color table/transfer function min and max range
    properties::Vec2Property _colorQuantityMinMax;
    // Uniform Field Line Color
    properties::Vec4Property _colorUniform;
    // Whether or not to use additive blending
    properties::BoolProperty _colorABlendEnabled;

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
    // Lower and upper range limit for allowed values
    properties::Vec2Property _maskingMinMax;
    // Index of the extra quantity to use for masking
    properties::OptionProperty _maskingQuantity;

    // Whether or not to use Domain
    properties::BoolProperty _domainEnabled;
    // Group to hold the Domain properties
    properties::PropertyOwner _domainGroup;
    // Domain Limits along x-axis
    properties::Vec2Property _domainX;
    // Domain Limits along y-axis
    properties::Vec2Property _domainY;
    // Domain Limits along z-axis
    properties::Vec2Property _domainZ;
    // Domain Limits radially
    properties::Vec2Property _domainR;

    // Line width for the line rendering part
    properties::FloatProperty _lineWidth;

    ///////////////other.//////////////////////////

    // Paths to color tables. One for each 'extraQuantity'
    std::vector<std::string> _colorTablePaths;
    // Values represents min & max values represented in the color table
    std::vector<glm::vec2> _colorTableRanges;


    // Values represents min & max limits for valid masking range
    std::vector<glm::vec2> _maskingRanges;

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCENEW___H__
