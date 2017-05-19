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

#include <modules/fieldlinessequence/rendering/renderablefieldlinessequence.h>
#include <modules/fieldlinessequence/util/fieldlinessequencemanager.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>

#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/textureunit.h>

#include <glm/gtc/matrix_transform.hpp>

#include <algorithm>

namespace {
    std::string _loggerCat = "RenderableFieldlinesSequence";
}

namespace {
    const char* keyTracingMethod            = "TracingMethod";
    // TODO CHOOSE METHODS BASED ON INTEGERS INSTEAD?
    const char* keyTracingMethodPreTraced   = "PreTraced";
    const char* keyTracingMethodPreProcess  = "PreProcess";
    const char* keyTracingMethodLiveTrace   = "LiveTrace";

    const char* keyVertexListDirectory      = "VertexListDirectory";
    const char* keyVertexListFileType       = "FileType";
    const char* keyVertexListFileTypeJson   = "Json";
    const char* keyVertexListFileTypeBinary = "Binary";

    const char* keyVolume = "VectorVolume";
    const char* keyFieldlines = "Fieldlines";
    const char* keySeedPoints = "SeedPoints";

    const char* keyVolumeDirectory = "Directory";
    const char* keyVolumeTracingVariable = "TracingVariable";
    const char* keyMaxNumVolumes         = "NumVolumes";

    const char* keyFieldlineMaxTraceSteps = "MaximumTracingSteps";
    const char* keyFieldlineShouldMorph = "Morphing";
    const char* keyFieldlineResamples = "NumResamples";
    const char* keyFieldlineResampleOption = "ResamplingType";
    const char* keyFieldlineQuickMorphDistance = "QuickMorphDistance";

    const char* keySeedPointsFile = "File";

    const float R_E_TO_METER = 6371000.f; // Earth radius
    const float R_S_TO_METER = 695700000.f; // Sun radius
    const float A_U_TO_METER = 149597870700.f; // Astronomical Units
    // const char* keySeedPointsDirectory = "Directory"; // TODO: allow for varying seed points?

    enum colorMethod{UNIFORM, QUANTITY_DEPENDENT, CLASSIFIED};
    // const int colorUniform = 0;
    // const int colorUnitDependent = 1;
    // const int colorClassified = 2;
}


namespace openspace {

RenderableFieldlinesSequence::RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary),
      _isClampingColorValues("isClamping", "Clamp", true),
      _isMorphing("isMorphing", "Morphing", false),
      _show3DLines("show3DLines", "3D Lines", false),
      _showSeedPoints("showSeedPoints", "Show Seed Points", false),
      _useNearestSampling("useNearestSampling", "Nearest Sampling", false),
      _lineWidth("fieldlineWidth", "Fieldline Width", 1.f, 0.f, 10.f),
      _seedPointSize("seedPointSize", "Seed Point Size", 4.0, 0.0, 20.0),
      _fieldlineParticleSize("fieldlineParticleSize", "FL Particle Size", 0, 0, 1000),
      _modulusDivider("fieldlineParticleFrequency", "FL Particle Frequency (reversed)", 100, 1, 10000),
      _timeMultiplier("fieldlineParticleSpeed", "FL Particle Speed", 20, 1, 1000),
      _colorMethod("fieldlineColorMethod", "Color Method", properties::OptionProperty::DisplayType::Radio),
      _colorizingQuantity("fieldlineColorQuantity", "Quantity", properties::OptionProperty::DisplayType::Dropdown),
      _colorGroup("Color"),
      _domainGroup("Domain Limits"),
      _particleGroup("Particles"),
      _seedGroup("Seed Points"),
      _transferFunctionPath("transferFunctionPath", "Transfer Function Path"),
      _transferFunctionMinVal("transferFunctionLimit1", "TF minimum", "0"),
      _transferFunctionMaxVal("transferFunctionLimit2", "TF maximum", "1"),
      _domainLimR("domainLimitsRadial", "Domain Limits Radial"),
      _domainLimX("domainLimitsX", "Domain Limits X"),
      _domainLimY("domainLimitsY", "Domain Limits Y"),
      _domainLimZ("domainLimitsZ", "Domain Limits Z"),
      _fieldlineColor("fieldlineColor", "Fieldline Color", glm::vec4(0.7f,0.f,0.7f,0.75f),
                                                           glm::vec4(0.f),
                                                           glm::vec4(1.f)),
      _fieldlineParticleColor("fieldlineParticleColor", "FL Particle Color",
                                                           glm::vec4(1.f,0.f,1.f,0.5f),
                                                           glm::vec4(0.f),
                                                           glm::vec4(1.f)),
      _uniformSeedPointColor("seedPointColor", "SeedPoint Color",
                                                           glm::vec4(1.f,0.33f,0.f,0.85f),
                                                           glm::vec4(0.f),
                                                           glm::vec4(1.f)) {

    std::string name;
    dictionary.getValue(SceneGraphNode::KeyName, name);

    _loggerCat = "RenderableFieldlines [" + name + "]";

    if (!dictionary.getValue(keyTracingMethod, _tracingMethod)) {
        LERROR("Renderable does not contain a key for '" << keyTracingMethod << "'");
        // deinitialize();
    }

    // Find VectorVolume, SeedPoint and Fieldlines Info from Lua
    if (!dictionary.getValue(keyVolume, _vectorVolumeInfo)) {
        LERROR("Renderable does not contain a key for '" << keyVolume << "'");
    }

    if (!dictionary.getValue(keyFieldlines, _fieldlineInfo)) {
        LERROR("Renderable does not contain a key for '" << keyFieldlines << "'");
    }

    if (!dictionary.getValue(keySeedPoints, _seedPointsInfo)) {
        LERROR("Renderable does not contain a key for '" << keySeedPoints << "'");
    }

    // TODO: REMOVE HARDCODED PATH
    _transferFunctionPath = "/home/ccarlbau/workspace/OpenSpace/data/colortables/uniform_heatmap_bcgyr.txt";
    _transferFunction = std::make_shared<TransferFunction>(_transferFunctionPath);
}

bool RenderableFieldlinesSequence::isReady() const {
    return _program ? true : false;
}

bool RenderableFieldlinesSequence::initialize() {
    int numResamples;
    bool allowSeedPoints = false;

    if (_tracingMethod == keyTracingMethodPreProcess) {
        allowSeedPoints = true;
        // SeedPoints Info. Needs a .txt file containing seed points.
        // Each row should have 3 floats seperated by spaces
        std::string pathToSeedPointFile;
        if (!_seedPointsInfo.getValue(keySeedPointsFile, pathToSeedPointFile)) {
            LERROR(keySeedPoints << " doesn't specify a '" << keySeedPointsFile << "'" <<
                "\n\tRequires a path to a .txt file containing seed point data." <<
                "Each row should have 3 floats seperated by spaces.");
            return false;
        } else {
            if (!FieldlinesSequenceManager::ref().getSeedPointsFromFile(pathToSeedPointFile,
                                                                        _seedPoints)) {
                LERROR("Failed to find seed points in'" << pathToSeedPointFile << "'");
                return false;
            }
        }

        // VectorVolume Info. Needs a folder containing .CDF files
        std::string pathToCdfDirectory;
        if (!_vectorVolumeInfo.getValue(keyVolumeDirectory, pathToCdfDirectory)) {
            LERROR(keyVolume << " doesn't specify a '" << keyVolumeDirectory <<
                    "'\n\tRequires a path to a Directory containing .CDF files. " <<
                    "Files must be of the same model and in sequence!");
            return false;
        }
        // Everything essential is provided
        std::vector<std::string> validCdfFilePaths;
        const std::string fileExt = "cdf";
        // const std::string fileExt = ".cdf";
        if (!FieldlinesSequenceManager::ref().getAllFilePathsOfType(
                pathToCdfDirectory, fileExt, validCdfFilePaths) ||
                validCdfFilePaths.empty() ) {

            LERROR("Failed to get valid "<< fileExt << " file paths from '"
                    << pathToCdfDirectory << "'" );
            return false;
        }

        // Only choose n number of volumes
        float f_maxStates;
        int numMaxStates = 0;
        int numValidPaths = static_cast<int>(validCdfFilePaths.size());

        if (!_vectorVolumeInfo.getValue(keyMaxNumVolumes, f_maxStates)) {
            numMaxStates = numValidPaths;
            LWARNING(keyVolume << " isn't specifying a " <<
                     keyMaxNumVolumes << ". Using all valid paths found.");
        } else {
            numMaxStates = static_cast<int>(f_maxStates);
            if (numMaxStates >= numValidPaths || numMaxStates == 0) {
                numMaxStates = numValidPaths;
            } else {
                validCdfFilePaths.erase(validCdfFilePaths.begin() + numMaxStates,
                                        validCdfFilePaths.end());
            }
        }

        // Specify which quantity to trace
        std::string tracingVariable;
        if (!_vectorVolumeInfo.getValue(keyVolumeTracingVariable, tracingVariable)) {
            tracingVariable = "b"; //default: b = magnetic field.
            LWARNING(keyVolume << " isn't specifying a " <<
                     keyVolumeTracingVariable << ". Using default value: '" <<
                     tracingVariable << "' for magnetic field.");
        }

        int maxSteps = 1000; // Default value
        float f_maxSteps;
        if (!_fieldlineInfo.getValue(keyFieldlineMaxTraceSteps, f_maxSteps)) {
            LWARNING(keyFieldlines << " isn't specifying " << keyFieldlineMaxTraceSteps
                    << ". Using default value: " << maxSteps);
        } else {
            maxSteps = static_cast<int>(f_maxSteps);
        }

        bool userWantsMorphing;
        if (!_fieldlineInfo.getValue(keyFieldlineShouldMorph, userWantsMorphing)) {
            _isMorphing = false; // Default value
            LWARNING(keyFieldlines << " isn't specifying " << keyFieldlineShouldMorph
                    << ". Using default: " << _isMorphing);
        } else {
            _isMorphing = userWantsMorphing;
        }

        int resamplingOption = 4; // Default
        numResamples = 2 * maxSteps + 3; // Default
        if (_isMorphing) {

            float f_resamplingOption;

            if(!_fieldlineInfo.getValue(keyFieldlineResampleOption, f_resamplingOption)) {
                LWARNING(keyFieldlines << " isn't specifying " <<
                         keyFieldlineResampleOption << ". Default is set to " <<
                         resamplingOption);
            } else {
                resamplingOption = static_cast<int>(f_resamplingOption);
            }

            float f_numResamples;

            if(_fieldlineInfo.getValue(keyFieldlineResamples, f_numResamples)) {
                if (resamplingOption == 4) {
                    LWARNING(keyFieldlineResampleOption << " 4 does not allow a custom" <<
                             " value for " << keyFieldlineResamples <<". Using (2 * " <<
                             keyFieldlineMaxTraceSteps <<" + 3) = " << numResamples);
                } else {
                    numResamples = static_cast<int>(f_numResamples);
                }
            } else {
                if (resamplingOption != 4) {
                    LWARNING(keyFieldlines << " isn't specifying " <<
                             keyFieldlineResamples << ". Default is set to (2*" <<
                             keyFieldlineMaxTraceSteps << "+3) = " << numResamples);
                }
            }
        }

        LDEBUG("Found the following valid .cdf files in " << pathToCdfDirectory);
        for (std::string str : validCdfFilePaths) {
            LDEBUG("\t" << str);
        }

        // TODO: This should be specified in LUA .mod file!
        std::vector<std::string> colorizingFloatVars;
        std::vector<std::string> colorizingMagVars;
        colorizingFloatVars.insert(colorizingFloatVars.end(), {
                                                                 "T",
                                                                 // "dp",
                                                                 "rho",
                                                                 // "p",
                                                                 "status"
                                                              });

        colorizingMagVars.insert(colorizingMagVars.end(), {
                                                              "ux", "uy", "uz",
                                                              "jx", "jy", "jz",
                                                              "jr", "jtheta", "jphi",
                                                              // "br", "btheta", "bphi",
                                                              "ur", "utheta", "uphi"
                                                          });

        // TODO this could be done in manager
        _numberOfStates = validCdfFilePaths.size();
        _states.reserve(_numberOfStates);
        _startTimes.reserve(_numberOfStates);

        // TODO specify saveJsonState in LUA;
        std::string folder = "${OPENSPACE_DATA}/scene/fieldlinessequence/json_new/";
        // std::string prefix = "";
        // std::string separator = "-";
        // int indentations = 1;

        for (int i = 0; i < _numberOfStates; ++i) {
           LDEBUG(validCdfFilePaths[i] << " is now being traced.");
           _states.push_back(FieldlinesState(_seedPoints.size()));
           FieldlinesSequenceManager::ref().getFieldlinesState(validCdfFilePaths[i],
                                                               tracingVariable,
                                                               _seedPoints,
                                                               maxSteps,
                                                               _isMorphing,
                                                               numResamples,
                                                               resamplingOption,
                                                               colorizingFloatVars,
                                                               colorizingMagVars,
                                                               _startTimes,
                                                               _states[i]);

           FieldlinesSequenceManager::ref().saveFieldlinesStateAsJson(_states[i],
                                                                      folder,
                                                                      false//,
                                                                      ,"0",true,"_",0
                                                                      // prefix,
                                                                      // true,
                                                                      // separator,
                                                                      // indentations
                                                                      );
        }
    } else if (_tracingMethod == keyTracingMethodPreTraced) {
        allowSeedPoints = false;
        // TODO: DON'T HARDCODE.. GET FROM LUA
        std::string jsonFolder = "${OPENSPACE_DATA}/scene/fieldlinessequence/enlilMarch2015/";
        std::vector<std::string> validJsonFilePaths;
        FieldlinesSequenceManager::ref().getAllFilePathsOfType(jsonFolder,
                                                               "json",
                                                               validJsonFilePaths);
        // std::vector<std::string> validJsonFilePaths{"${OPENSPACE_DATA}/scene/fieldlinessequence/json_new1/"};
        // std::vector<std::string> validJsonFilePaths{"C:/Users/oskar/Develop/workspace/OpenSpace/data/scene/fieldlinessequence/precalculatedjson/fieldline_samples.json "};


        // Only choose n number of volumes
        // int numMaxStates = 20;
        // int numValidPaths = static_cast<int>(validJsonFilePaths.size());
        // validJsonFilePaths.erase(validJsonFilePaths.begin() + numMaxStates,
        //                                 validJsonFilePaths.end());

        LDEBUG("Found the following valid .json files in " << jsonFolder);
        for (std::string str : validJsonFilePaths) {
            LDEBUG("\t" << str);
        }

        LERROR("TODO: allow Morphing for provided vertex lists!");
        _isMorphing = false;
        int resamplingOption = 0;   // TODO: implement morphing
        numResamples = 0;       // TODO: implement morphing
        _numberOfStates = validJsonFilePaths.size();
        _states.reserve(_numberOfStates);
        _startTimes.reserve(_numberOfStates);

        for (int i = 0; i < _numberOfStates; ++i) {
            LDEBUG(validJsonFilePaths[i] << " is now being processed.");
            FieldlinesState tmpState;
            _states.push_back(tmpState);
            FieldlinesSequenceManager::ref().getFieldlinesState(validJsonFilePaths[i],
                                                               _isMorphing,
                                                               numResamples,
                                                               resamplingOption,
                                                               _startTimes,
                                                               _states[i]);
        }

        // TODO specify saveJsonState in LUA;
        // std::string folder = "${OPENSPACE_DATA}/scene/fieldlinessequence/json_new/";
        // std::string prefix = "jsonConv";
        // std::string separator = "-";
        // int indentations = 1;

        // FieldlinesSequenceManager::ref().saveFieldlinesStateAsJson(_states[0],
        //                                                            folder,
        //                                                            false,
        //                                                            prefix,
        //                                                            true,
        //                                                            separator,
        //                                                            indentations);

    } else if (_tracingMethod == keyTracingMethodLiveTrace) {
        LERROR("NOT YET INCORPORATED INTO THIS CLASS! TODO, TODO TODO!");
        allowSeedPoints = true;
        return false;
    } else {
        LERROR(keyTracingMethod << " isn't specifying a valid tracing method.\n\t"
                << "Valid methods are: " << keyTracingMethodPreTraced << ", "
                                         << keyTracingMethodPreProcess << " and "
                                         << keyTracingMethodPreTraced);
        return false;
    }

    bool allowUnitColoring = false;
    int numQuanityColorVariables = 0;
    if (_numberOfStates > 0) {
        // Approximate the end time of last state (and for the sequence as a whole)
        _seqStartTime = _startTimes[0];

        double lastStateStart = _startTimes[_numberOfStates-1];
        if (_numberOfStates > 1) {
            double avgTimeOffset = (lastStateStart - _seqStartTime) /
                                   (static_cast<double>(_numberOfStates) - 1.0);
            _seqEndTime =  lastStateStart + avgTimeOffset;
        } else {
            _isMorphing = false;
            // _seqEndTime = 631108800.f; // January 1st 2020
            _seqEndTime = FLT_MAX; // UNTIL THE END OF DAYS!
        }
        // Add seqEndTime as the last start time
        // to prevent vector from going out of bounds later.
        _startTimes.push_back(_seqEndTime); // =  lastStateStart + avgTimeOffset;

        float rmin, rmax, xmin, xmax, ymin, ymax, zmin, zmax;

        std::string model = _states[0]._modelName;
        if (model == "enlil") {
            _scalingFactor          = A_U_TO_METER;
            _scalingFactorLineWidth = R_S_TO_METER;
            _scalingFactorUnit = "AU";
            // TODO: change these hardcoded limits to something that makes sense
            // or allow user to specify in LUA
            rmin =  0.09f;
            xmin = ymin = zmin = -40.f;
            xmax = ymax = zmax = 40.f;
            rmax = std::max(xmax, std::max(ymax,zmax));

            _isSpherical = true;
        } else if (model == "batsrus") {
            _scalingFactor          = R_E_TO_METER;
            _scalingFactorLineWidth = R_E_TO_METER;
            _scalingFactorUnit = "RE";
            // TODO: change these hardcoded limits to something that makes sense
            // or allow user to specify in LUA
            rmin =  1.0f;
            xmin = -250.f;
            ymin = zmin = -150.f;
            xmax = 50.f;
            ymax = zmax = 150.f;
            rmax = std::max(xmax, std::max(ymax,zmax));

            _isSpherical = false;
        } else {
            LERROR("OpenSpace's RenderableFieldlinesSequence class can only support the "
                << " batsrus and enlil models for the time being! CDF contains the "
                << model << " model.");
            return false;
        }

        _domainLimR.setMinValue(glm::vec2(rmin));
        _domainLimR.setMaxValue(glm::vec2(rmax));
        _domainLimX.setMinValue(glm::vec2(xmin));
        _domainLimX.setMaxValue(glm::vec2(xmax));
        _domainLimY.setMinValue(glm::vec2(ymin));
        _domainLimY.setMaxValue(glm::vec2(ymax));
        _domainLimZ.setMinValue(glm::vec2(zmin));
        _domainLimZ.setMaxValue(glm::vec2(zmax));

        _domainLimR.setValue(glm::vec2(rmin,rmax));
        _domainLimX.setValue(glm::vec2(xmin,xmax));
        _domainLimY.setValue(glm::vec2(ymin,ymax));
        _domainLimZ.setValue(glm::vec2(zmin,zmax));

        numQuanityColorVariables = _states[0]._extraVariables.size();
        if (numQuanityColorVariables > 0) {
            allowUnitColoring = (_states[0]._extraVariables[0].size() > 0) ? true : false;
        }

    } else {
        LERROR("Couldn't create any states!");
        return false;
    }

    if (_isMorphing) {
        LDEBUG("Optimising morphing!");
        float quickMorphDist;
        if(!_fieldlineInfo.getValue(keyFieldlineQuickMorphDistance, quickMorphDist)) {
            quickMorphDist = 20.f * R_E_TO_METER; // 2 times Earth's radius
            LWARNING(keyFieldlines << " isn't specifying " <<
                     keyFieldlineQuickMorphDistance <<
                     ". Default is set to 20 Earth radii.");
        }

        FieldlinesSequenceManager::ref().setQuickMorphBooleans(_states,
                                                               numResamples,
                                                               quickMorphDist);
    }

    // GL_LINE width related constants dependent on hardware..
    // glGetFloatv(GL_LINE_WIDTH, &_maxLineWidthOpenGl);
    // glGetFloatv(GL_ALIASED_LINE_WIDTH_RANGE, &_maxLineWidthOpenGl);
    // glGetFloatv(GL_SMOOTH_LINE_WIDTH_RANGE, &_maxLineWidthOpenGl);
    // glGetFloatv(GL_SMOOTH_LINE_WIDTH_GRANULARITY, &_maxLineWidthOpenGl);

    if (allowSeedPoints) {
        _seedPointProgram = OsEng.renderEngine().buildRenderProgram(
            "FieldlinesSequenceSeeds",
            "${MODULE_FIELDLINESSEQUENCE}/shaders/seedpoint_vs.glsl",
            "${MODULE_FIELDLINESSEQUENCE}/shaders/seedpoint_fs.glsl"
        );

        if (!_seedPointProgram) {
            LERROR("SeedPoint Shader program failed initialization!");
            return false;
        }
    }

    if (_isMorphing) {
        _program = OsEng.renderEngine().buildRenderProgram(
            "FieldlinesSequence",
            "${MODULE_FIELDLINESSEQUENCE}/shaders/fieldline_morph_flow_direction_vs.glsl",
            "${MODULE_FIELDLINESSEQUENCE}/shaders/fieldline_flow_direction_fs.glsl"
        );
    } else {
        _program = OsEng.renderEngine().buildRenderProgram(
            "FieldlinesSequence",
            "${MODULE_FIELDLINESSEQUENCE}/shaders/fieldline_flow_direction_vs.glsl",
            "${MODULE_FIELDLINESSEQUENCE}/shaders/fieldline_flow_direction_fs.glsl"
        );
    }
    _ropeProgram = OsEng.renderEngine().buildRenderProgram(
        "FieldlinesSequenceRope",
        "${MODULE_FIELDLINESSEQUENCE}/shaders/fieldline_rope_flow_direction_vs.glsl",
        "${MODULE_FIELDLINESSEQUENCE}/shaders/fieldline_rope_fs.glsl",
        "${MODULE_FIELDLINESSEQUENCE}/shaders/fieldline_rope_gs.glsl"
    );

    if (!_program) {
        LERROR("Shader program failed initialization!");
        return false;
    }

    _activeProgramPtr = &*_program;

    if (!_ropeProgram) {
        LERROR("Shader program 'ropeProgram' failed initialization!");
    } else {
        addProperty(_show3DLines);
        addProperty(_lineWidth);
        _show3DLines.onChange([this] {
            // TOGGLE ACTIVE SHADER PROGRAM
            _activeProgramPtr = (_activeProgramPtr == _program.get()) ? &*_ropeProgram : &*_program;
        });
    }
    // TODO: IT MAY BE BENEFICIAL IF SOME OF THESE PROPERTIES WERE DEPENDENT ON THE
    // NUMBER OF MAX TRACING STEPS THAT THE USER DEFINED IN LUA!
    // The fieldlineParticleSize and modulusDivider espacially
    addPropertySubOwner(_domainGroup);
    _domainGroup.addProperty(_domainLimR);
    _domainGroup.addProperty(_domainLimX);
    _domainGroup.addProperty(_domainLimY);
    _domainGroup.addProperty(_domainLimZ);

    addPropertySubOwner(_particleGroup);
    _particleGroup.addProperty(_fieldlineParticleColor);
    _particleGroup.addProperty(_fieldlineParticleSize);
    _particleGroup.addProperty(_modulusDivider);
    _particleGroup.addProperty(_timeMultiplier);

    if (allowSeedPoints) {
        addPropertySubOwner(_seedGroup);
        _seedGroup.addProperty(_seedPointSize);
        _seedGroup.addProperty(_showSeedPoints);
        _seedGroup.addProperty(_uniformSeedPointColor);
    }

    if (_isMorphing) {
        addProperty(_isMorphing);
    }

    if (allowUnitColoring) {
        _colorMethod.addOption(colorMethod::UNIFORM, "Uniform Color");
        _colorMethod.addOption(colorMethod::QUANTITY_DEPENDENT, "Quantity Dependent");

        // ASSUMING ALL STATES HAVE THE SAME COLOR VARIABLES
        for (size_t i = 0; i < _states[0]._extraVariableNames.size(); ++i) {
            _colorizingQuantity.addOption(i, _states[0]._extraVariableNames[i]);
        }

        // Set tranferfunction min/max to min/max in the given range
        // TODO: this should probably be determined some other way..
        // TODO: set in LUA when tracing variables are set there! Requires state to store min/max
        for (size_t i = 0; i < _states[0]._extraVariables.size(); i++) {
            std::vector<float>& quantityVec = _states[0]._extraVariables[i];
        // for (std::vector<float>& quantityVec : _states[0]._extraVariables) {
            auto minMaxPos = std::minmax_element(quantityVec.begin(), quantityVec.end());
            float minVal = *minMaxPos.first;
            float maxVal = *minMaxPos.second;
            _transferFunctionLimits.push_back(glm::vec2(minVal, maxVal));
        }

        _transferFunctionMinVal = std::to_string(_transferFunctionLimits[0].x);
        _transferFunctionMaxVal = std::to_string(_transferFunctionLimits[0].y);

        _colorizingQuantity.onChange([this] {
            LDEBUG("CHANGED COLORIZING QUANTITY");
            _updateColorBuffer = true;
            _transferFunctionMinVal = std::to_string(_transferFunctionLimits[_colorizingQuantity].x);
            _transferFunctionMaxVal = std::to_string(_transferFunctionLimits[_colorizingQuantity].y);
        });

        _transferFunctionPath.onChange([this] {
            // TOGGLE ACTIVE SHADER PROGRAM
            _transferFunction->setPath(_transferFunctionPath);
        });

        _transferFunctionMinVal.onChange([this] {
            LDEBUG("CHANGED MIN VALUE");
            // TODO CHECK IF VALID NUMBER!
            // _updateTransferFunctionMin = true;
            _transferFunctionLimits[_colorizingQuantity].x = std::stof(_transferFunctionMinVal);
        });

        _transferFunctionMaxVal.onChange([this] {
            LDEBUG("CHANGED MAX VALUE");
            // TODO CHECK IF VALID NUMBER!
            // _updateTransferFunctionMin = true;
            _transferFunctionLimits[_colorizingQuantity].y = std::stof(_transferFunctionMaxVal);
        });

        // TODO: fix bug related to changing/updating of transfer function
        // TODO: Bug causes texture to get default FilterMode: linear
        _useNearestSampling.onChange([this] {
            LDEBUG("Changed TransferFunction sampling method!");

            ghoul::opengl::Texture& texture = _transferFunction->getTexture();
            // TOGGLE BETWEEN NEAREST AND LINEAR SAMPLING
            texture.setFilter(_useNearestSampling ? texture.FilterMode::Linear :
                                                    texture.FilterMode::Nearest);
        });

        _isClampingColorValues.onChange([this] {
            LDEBUG("Changed whether to Clamp or Discard values outside of TF range!");
        });

        // _colorGroup.setPropertyGroupName(0,"FieldlineColorRelated");
        addPropertySubOwner(_colorGroup);

        _colorGroup.addProperty(_colorMethod);
        _colorGroup.addProperty(_colorizingQuantity);
        _colorGroup.addProperty(_isClampingColorValues);
        _colorGroup.addProperty(_transferFunctionPath);
        _colorGroup.addProperty(_transferFunctionMinVal);
        _colorGroup.addProperty(_transferFunctionMaxVal);
        _colorGroup.addProperty(_fieldlineColor);
        _colorGroup.addProperty(_useNearestSampling);

    } else {
        addProperty(_fieldlineColor);

    }

    return true;
}

bool RenderableFieldlinesSequence::deinitialize() {
    glDeleteVertexArrays(1, &_vertexArrayObject);
    _vertexArrayObject = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    glDeleteBuffers(1, &_vertexColorBuffer);
    _vertexColorBuffer = 0;

    glDeleteBuffers(1, &_morphToPositionBuffer);
    _morphToPositionBuffer = 0;

    glDeleteBuffers(1, &_quickMorphBuffer);
    _quickMorphBuffer = 0;


    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_program) {
        renderEngine.removeRenderProgram(_program);
        _program = nullptr;
    }

    if (_ropeProgram) {
        renderEngine.removeRenderProgram(_ropeProgram);
        _ropeProgram = nullptr;
    }

    if (_seedPointProgram) {
        renderEngine.removeRenderProgram(_seedPointProgram);
        _seedPointProgram = nullptr;
    }

    return true;
}

void RenderableFieldlinesSequence::render(const RenderData& data) {
    // if (_isWithinTimeInterval) {
    if (_shouldRender) {
        _activeProgramPtr->activate();

        glm::dmat4 rotationTransform = glm::dmat4(data.modelTransform.rotation);
        glm::mat4 scaleTransform = glm::mat4(1.0); // TODO remove if no use
        glm::dmat4 modelTransform =
                glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
                rotationTransform *
                glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))) *
                glm::dmat4(scaleTransform);
        glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

        int testTime = static_cast<int>(Time::ref().deltaTime() * OsEng.runTime() * 100) / 5;
        // double testTime = _;

        // Set uniforms for shaders
        // _activeProgramPtr->setUniform("time", testTime * _timeMultiplier);
        _activeProgramPtr->setUniform("timeD", _currentTime * _timeMultiplier);
        _activeProgramPtr->setUniform("flParticleSize", _fieldlineParticleSize);
        _activeProgramPtr->setUniform("modulusDivider", _modulusDivider);
        _activeProgramPtr->setUniform("colorMethod", _colorMethod);
        _activeProgramPtr->setUniform("fieldlineColor", _fieldlineColor);
        _activeProgramPtr->setUniform("fieldlineParticleColor", _fieldlineParticleColor);
        _activeProgramPtr->setUniform("domainLimR", _domainLimR.value() * _scalingFactor);
        _activeProgramPtr->setUniform("domainLimX", _domainLimX.value() * _scalingFactor);
        _activeProgramPtr->setUniform("domainLimY", _domainLimY.value() * _scalingFactor);
        _activeProgramPtr->setUniform("domainLimZ", _domainLimZ.value() * _scalingFactor);
        if (_show3DLines) {
            _activeProgramPtr->setUniform("width", _lineWidth * _scalingFactorLineWidth);
            // _activeProgramPtr->setUniform("camDirection",
            //                             glm::vec3(data.camera.viewDirectionWorldSpace()));
            // _activeProgramPtr->setUniform("modelTransform", modelTransform);
            // _activeProgramPtr->setUniform("viewTransform", data.camera.combinedViewMatrix());
            _activeProgramPtr->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
            _activeProgramPtr->setUniform("projectionTransform", data.camera.sgctInternal.projectionMatrix());
        } else {
            glLineWidth(_lineWidth);
            _activeProgramPtr->setUniform("modelViewProjection",
                    data.camera.sgctInternal.projectionMatrix() * glm::mat4(modelViewTransform));
            if (_isMorphing) { // TODO ALLOW MORPHING AND 3D LINES/ROPES
                _activeProgramPtr->setUniform("state_progression", _stateProgress);
                _activeProgramPtr->setUniform("isMorphing", _isMorphing);
            }
        }

        if (_colorMethod == colorMethod::QUANTITY_DEPENDENT) {
            // TODO MOVE THIS TO UPDATE AND CHECK
            _textureUnit = std::make_unique<ghoul::opengl::TextureUnit>();
            _textureUnit->activate();
            _transferFunction->bind(); // Calls update internally
            _activeProgramPtr->setUniform("transferFunctionLimits", _transferFunctionLimits[_colorizingQuantity]);
            _activeProgramPtr->setUniform("colorMap", _textureUnit->unitNumber());
            _activeProgramPtr->setUniform("isClamping", _isClampingColorValues);
        }

        glDisable(GL_CULL_FACE);

        // _activeProgramPtr->setUniform("classification", _classification);
        // if (!_classification)
        //     _activeProgramPtr->setUniform("fieldLineColor", _fieldlineColor);

        glBindVertexArray(_vertexArrayObject);
        glMultiDrawArrays(
                GL_LINE_STRIP_ADJACENCY,
                &_states[_activeStateIndex]._lineStart[0],
                &_states[_activeStateIndex]._lineCount[0],
                static_cast<GLsizei>(_states[_activeStateIndex]._lineStart.size())
        );

        glBindVertexArray(0);
        _activeProgramPtr->deactivate();

        if (_showSeedPoints) {
            glBindVertexArray(_seedArrayObject);
            _seedPointProgram->activate();
            _seedPointProgram->setUniform("color", _uniformSeedPointColor);
            _seedPointProgram->setUniform("isSpherical", _isSpherical);
            _seedPointProgram->setUniform("scaleFactor", _scalingFactor);
            _seedPointProgram->setUniform("modelViewProjection",
                data.camera.sgctInternal.projectionMatrix() * glm::mat4(modelViewTransform));
            glPointSize(_seedPointSize);
            glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>( _seedPoints.size() ) );

            glBindVertexArray(0);
            _seedPointProgram->deactivate();
        }

        glEnable(GL_CULL_FACE);
    }
}

void RenderableFieldlinesSequence::update(const UpdateData&) {
    if (_activeProgramPtr->isDirty()) {
        _activeProgramPtr->rebuildFromFile();
    }

    if (_showSeedPoints) {
        if (_seedPointProgram->isDirty()) {
            _seedPointProgram->rebuildFromFile();
        }
    }

    // Check if current time in OpenSpace is within sequence interval
    if (isWithinSequenceInterval()) {
        // if NOT in the same state as in the previous update..
        if ( _activeStateIndex < 0 ||
             _currentTime < _startTimes[_activeStateIndex] ||
             // This next line requires/assumes seqEndTime to be last position in _startTimes
             _currentTime >= _startTimes[_activeStateIndex + 1]) {
            _needsUpdate = true;
        } else if (_isMorphing) {
            double stateDuration = _startTimes[_activeStateIndex + 1] -
                                   _startTimes[_activeStateIndex]; // TODO? could be stored
            double stateTimeElapsed = _currentTime - _startTimes[_activeStateIndex];
            _stateProgress = static_cast<float>(stateTimeElapsed / stateDuration);
            // ghoul_assert(_stateProgress >= 0.0f, "_stateProgress is NEGATIVE!!");
        } // else {we're still in same state as previous update (no changes needed)}
    } else {
        // Not in interval => set everything to false
        _activeStateIndex = -1;
        _shouldRender = false;
        _needsUpdate = false;
    }

    if(_needsUpdate) {
        updateActiveStateIndex(); // sets _activeStateIndex
        if (_vertexArrayObject == 0) {
            glGenVertexArrays(1, &_vertexArrayObject);

            if (_seedPoints.size() > 0) {
                glGenVertexArrays(1, &_seedArrayObject);
                glBindVertexArray(_seedArrayObject);
                glGenBuffers(1, &_seedPositionBuffer);

                glBindBuffer(GL_ARRAY_BUFFER, _seedPositionBuffer);
                glBufferData(GL_ARRAY_BUFFER,
                    _seedPoints.size() * sizeof(glm::vec3),
                    &_seedPoints.front(),
                    GL_STATIC_DRAW);
                GLuint seedLocation = 0;
                glEnableVertexAttribArray(seedLocation);
                glVertexAttribPointer(seedLocation, 3, GL_FLOAT, GL_FALSE, 0, 0);
            }
        }
        glBindVertexArray(_vertexArrayObject);

        if (_vertexPositionBuffer == 0) {
            glGenBuffers(1, &_vertexPositionBuffer);
            if (_isMorphing) {
                glGenBuffers(1, &_morphToPositionBuffer);
                glGenBuffers(1, &_quickMorphBuffer);
            }
        }

        if (_vertexColorBuffer == 0) {
            glGenBuffers(1, &_vertexColorBuffer);
        }

        updateVertexPosBuffer();

        if (_isMorphing) {
            updateMorphingBuffers();
        }

        // TODO fix colors -- color classification, etc
        // Set color buffer if state has values for it
        _hasUnitColoring = (_states[_activeStateIndex]._extraVariables.size() > 0) ? true : false;
        if (_hasUnitColoring) {
            _updateColorBuffer = true;
        }

        // UNBIND
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);

        _needsUpdate = false;
        _shouldRender = true;

        if (_isMorphing) {
            double stateDuration = _startTimes[_activeStateIndex + 1] -
                                       _startTimes[_activeStateIndex]; // TODO? could be stored
            double stateTimeElapsed = _currentTime - _startTimes[_activeStateIndex];
            _stateProgress = static_cast<float>(stateTimeElapsed / stateDuration);
        }
    }

    if (_updateColorBuffer) {
        _updateColorBuffer = false;
        updateColorBuffer();
    }
}

bool RenderableFieldlinesSequence::isWithinSequenceInterval() {
    _currentTime = Time::ref().j2000Seconds();
    return (_currentTime >= _seqStartTime) &&
           (_isMorphing ? _currentTime < _startTimes[_numberOfStates-1] // nothing to morph to after last state
                        : _currentTime < _seqEndTime);
}

// Assumes we already know that _currentTime is within the sequence interval
void RenderableFieldlinesSequence::updateActiveStateIndex() {
    auto iter = std::upper_bound(_startTimes.begin(), _startTimes.end(), _currentTime);
    //
    if (iter != _startTimes.end()) {
        if ( iter != _startTimes.begin()) {
            _activeStateIndex = std::distance(_startTimes.begin(), iter) - 1;
        } else {
            _activeStateIndex = 0;
        }
    } else {
        _activeStateIndex = _numberOfStates - 1;
    }
}

void RenderableFieldlinesSequence::updateColorBuffer() {
    glBindVertexArray(_vertexArrayObject);

    glBindBuffer(GL_ARRAY_BUFFER, _vertexColorBuffer);

    auto& quantityVec = _states[_activeStateIndex]._extraVariables[_colorizingQuantity];

    glBufferData(GL_ARRAY_BUFFER, quantityVec.size() * sizeof(float),
            &quantityVec.front(), GL_STATIC_DRAW);

    glEnableVertexAttribArray(_vertAttrColorQuantity);
    glVertexAttribPointer(_vertAttrColorQuantity, 1, GL_FLOAT, GL_FALSE, 0, 0);
}

void RenderableFieldlinesSequence::updateMorphingBuffers() {
    /// TODO SWAP BUFFERING IF MORPHING BUFFER CONTAINS THE POINT OF THE NEW _activeIndex
    glBindBuffer(GL_ARRAY_BUFFER, _morphToPositionBuffer);

    glBufferData(GL_ARRAY_BUFFER,
        _states[_activeStateIndex+1]._vertexPositions.size() * sizeof(glm::vec3),
        &_states[_activeStateIndex+1]._vertexPositions.front(),
        GL_STATIC_DRAW);

    glEnableVertexAttribArray(_vertAttrMorphToPos);
    glVertexAttribPointer(_vertAttrMorphToPos, 3, GL_FLOAT, GL_FALSE, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, _quickMorphBuffer);

    glBufferData(GL_ARRAY_BUFFER,
            _states[_activeStateIndex]._quickMorph.size() * sizeof(GLfloat),
            &_states[_activeStateIndex]._quickMorph.front(),
            GL_STATIC_DRAW);

    glEnableVertexAttribArray(_vertAttrMorphQuick);
    glVertexAttribPointer(_vertAttrMorphQuick, 1, GL_FLOAT, GL_FALSE, 0, 0);
}

void RenderableFieldlinesSequence::updateVertexPosBuffer() {
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    auto& vertexPosVec = _states[_activeStateIndex]._vertexPositions;

    glBufferData(GL_ARRAY_BUFFER, vertexPosVec.size() * sizeof(glm::vec3),
            &vertexPosVec.front(), GL_STATIC_DRAW);

    glEnableVertexAttribArray(_vertAttrVertexPos);
    glVertexAttribPointer(_vertAttrVertexPos, 3, GL_FLOAT, GL_FALSE, 0, 0);
}

} // namespace openspace
