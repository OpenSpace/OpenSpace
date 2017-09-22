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

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>

using std::string;

namespace {
    std::string _loggerCat = "RenderableFieldlinesSequence";

    // ----- KEYS POSSIBLE IN MODFILE. EXPECTED DATA TYPE OF VALUE IN [BRACKETS]  ----- //
    // ---------------------------- MANDATORY MODFILE KEYS ---------------------------- //
    const char* KEY_INPUT_FILE_TYPE         = "InputFileType";   // [STRING]
    const char* KEY_SOURCE_FOLDER           = "SourceFolder";    // [STRING]

    // ---------------------------- OPTIONAL MODFILE KEYS  ---------------------------- //
    const char* KEY_COLOR_TABLE_PATHS       = "ColorTablePaths"; // [STRING ARRAY] Values should be paths to .txt files
    const char* KEY_COLOR_TABLE_RANGES      = "ColorTableRanges";// [VEC2 ARRAY] Values should be paths to .txt files
    const char* KEY_OSLFS_LOAD_AT_RUNTIME   = "LoadAtRuntime";   // [BOOLEAN] If value False => Load in initializing step and store in RAM

    // ------------- POSSIBLE STRING VALUES FOR CORRESPONDING MODFILE KEY ------------- //
    const char* VALUE_INPUT_FILE_TYPE_CDF   = "cdf";
    const char* VALUE_INPUT_FILE_TYPE_JSON  = "json";
    const char* VALUE_INPUT_FILE_TYPE_OSFLS = "osfls";

    static const openspace::properties::Property::PropertyInfo ColorMethodInfo = {
        "colorMethod", "Color Method", "Color lines uniformly or using color tables based on extra quantities like e.g. temperature or particle density."
    };
    static const openspace::properties::Property::PropertyInfo ColorQuantityInfo = {
        "colorQuantity", "Quantity to Color By", "Quantity used to color lines if the \"By Quantity\" color method is selected."
    };
    static const openspace::properties::Property::PropertyInfo ColorQuantityMinInfo = {
        "colorQuantityMin", "ColorTable Min Value", "Value to map to the lowest end of the color table."
    };
    static const openspace::properties::Property::PropertyInfo ColorQuantityMaxInfo = {
        "colorQuantityMax", "ColorTable Max Value", "Value to map to the highest end of the color table."
    };
    static const openspace::properties::Property::PropertyInfo ColorTablePathInfo = {
        "colorTablePath", "Path to Color Table", "Color Table/Transfer Function to use for \"By Quantity\" coloring."
    };
    static const openspace::properties::Property::PropertyInfo ColorUniformInfo = {
        "uniform", "Uniform Line Color", "The uniform color of lines shown when \"Color Method\" is set to \"Uniform\"."
    };
    static const openspace::properties::Property::PropertyInfo FlowColorInfo = {
        "color", "Color", "Color of particles."
    };
    static const openspace::properties::Property::PropertyInfo EnableFlowInfo = {
        "Enable", "ON/OFF",
        "Toggles the rendering of moving particles along the lines. Can e.g. illustrate magnetic flow."
    };
    static const openspace::properties::Property::PropertyInfo ReverseFlowInfo = {
        "reversed", "Reversed Flow", "Toggle to make the flow move in the opposite direction."
    };
    static const openspace::properties::Property::PropertyInfo ParticleSizeInfo = {
        "particleSize", "Particle Size", "Size of the particles."
    };
    static const openspace::properties::Property::PropertyInfo ParticleSpacingInfo = {
        "particleSpacing", "Particle Spacing", "Spacing inbetween particles."
    };
    static const openspace::properties::Property::PropertyInfo FlowSpeedInfo = {
        "speed", "Speed", "Speed of the flow."
    };

    enum ColorMethod { UNIFORM = 0, BY_QUANTITY };
} // namespace

namespace openspace {

RenderableFieldlinesSequence::RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary),
      _colorGroup({ "Color" }),
      _colorMethod(ColorMethodInfo, properties::OptionProperty::DisplayType::Radio),
      _colorQuantity(ColorQuantityInfo, properties::OptionProperty::DisplayType::Dropdown),
      _colorQuantityMin(ColorQuantityMinInfo),
      _colorQuantityMax(ColorQuantityMaxInfo),
      _colorTablePath(ColorTablePathInfo),
      _colorUniform(ColorUniformInfo, glm::vec4(0.75f, 0.5f, 0.0f, 0.5f),
                                      glm::vec4(0.f), glm::vec4(1.f)),
      _flowColor(FlowColorInfo, glm::vec4(0.8f, 0.7f, 0.0f, 0.6f),
                                glm::vec4(0.f), glm::vec4(1.f)),
      _flowEnabled(EnableFlowInfo, true),
      _flowGroup({ "Flow" }),
      _flowParticleSize(ParticleSizeInfo, 5, 0, 500),
      _flowParticleSpacing(ParticleSpacingInfo, 60, 0, 500),
      _flowReversed(ReverseFlowInfo, false),
      _flowSpeed(FlowSpeedInfo, 20, 0, 1000) {

    // Set the default color table, just in case user defined paths are corrupt!
    _transferFunction = std::make_shared<TransferFunction>(absPath(_colorTablePaths[0]));

    if(!extractInfoFromDictionary(dictionary)) {
        _sourceFileType = INVALID;
    }
}

void RenderableFieldlinesSequence::initialize() {
    switch (_sourceFileType) {
        case CDF:
            LERROR("CDF NOT YET IMPLEMENTED!");
            break;
        case JSON:
            LERROR("JSON NOT YET IMPLEMENTED!");
            break;
        case OSFLS:
            if (_isLoadingStatesAtRuntime) {
                extractTriggerTimesFromFileNames();
                FieldlinesState newState;
                bool loadedSuccessfully = newState.loadStateFromOsfls(_sourceFiles[0]);
                if (loadedSuccessfully) {
                    _states.push_back(newState);
                    _nStates = _startTimes.size();
                    _activeStateIndex = 0;
                } else {
                    LERROR("The provided .osfls files seem to be corrupt!");
                    _sourceFileType = INVALID;
                }
            } else {
                // Load states into RAM!
                for (string filePath : _sourceFiles) {
                    FieldlinesState newState;
                    bool loadedSuccessfully = newState.loadStateFromOsfls(filePath);
                    if (loadedSuccessfully) {
                        _states.push_back(newState);
                        _startTimes.push_back(newState.triggerTime());
                        _nStates++;
                    }
                }
            }
            break;
        default:
            break;
    }

    // At this point there's at least one state loaded into memory!
    // No need to store source paths in memory if they are already in RAM!
    if (!_isLoadingStatesAtRuntime) {
        _sourceFiles.clear();
    }

    computeSequenceEndTime();

    // --------------------- HANDLE PROPERTIES --------------------- //
    // Add Property Groups
    addPropertySubOwner(_colorGroup);
    addPropertySubOwner(_flowGroup);

    // Add Properties to the groups
    _colorGroup.addProperty(_colorMethod);
    _colorGroup.addProperty(_colorQuantity);
    _colorGroup.addProperty(_colorQuantityMin);
    _colorGroup.addProperty(_colorQuantityMax);
    _colorGroup.addProperty(_colorTablePath);
    _colorGroup.addProperty(_colorUniform);

    _flowGroup.addProperty(_flowEnabled);
    _flowGroup.addProperty(_flowReversed);
    _flowGroup.addProperty(_flowColor);
    _flowGroup.addProperty(_flowParticleSize);
    _flowGroup.addProperty(_flowParticleSpacing);
    _flowGroup.addProperty(_flowSpeed);

    // Add Options to OptionProperties
    _colorMethod.addOption(ColorMethod::UNIFORM, "Uniform");
    _colorMethod.addOption(ColorMethod::BY_QUANTITY, "By Quantity");

    // Add option for each extra quantity. We assume that there are just as many names to
    // extra quantities as there are extra quantities. We also assume that all states in
    // the given sequence have the same extra quantities!
    const size_t N_EXTRA_QUANTITIES = _states[0].nExtraQuantities();
    auto EXTRA_VARIABLE_NAMES_VEC = _states[0].extraQuantityNames();
    for (size_t i = 0; i < N_EXTRA_QUANTITIES; ++i) {
        _colorQuantity.addOption(i, EXTRA_VARIABLE_NAMES_VEC[i]);
    }
    // Each quantity should have its own color table and color table range, no more, no less
    _colorTablePaths.resize(N_EXTRA_QUANTITIES, _colorTablePaths.back());
    _colorTableRanges.resize(N_EXTRA_QUANTITIES, _colorTableRanges.back());

    // Add Property Callback Functions
    _colorQuantity.onChange([this] {
        LDEBUG("CHANGED COLORING QUANTITY");
        _shouldUpdateColorBuffer = true;
        _colorQuantityMin = std::to_string(_colorTableRanges[_colorQuantity].x);
        _colorQuantityMax = std::to_string(_colorTableRanges[_colorQuantity].y);
        _activeColorTable = &_colorTablePaths[_colorQuantity];
        _colorTablePath = *_activeColorTable;
    });

    _colorTablePath.onChange([this] {
        // TOGGLE ACTIVE SHADER PROGRAM
        _transferFunction->setPath(_colorTablePath);
        *_activeColorTable = _colorTablePath;
    });

    _colorQuantityMin.onChange([this] {
        LDEBUG("CHANGED MIN VALUE");
        // TODO CHECK IF VALID NUMBER!
        // _updateTransferFunctionMin = true;
        _colorTableRanges[_colorQuantity].x = std::stof(_colorQuantityMin);
    });

    _colorQuantityMax.onChange([this] {
        LDEBUG("CHANGED MAX VALUE");
        // TODO CHECK IF VALID NUMBER!
        // _updateTransferFunctionMin = true;
        _colorTableRanges[_colorQuantity].y = std::stof(_colorQuantityMax);
    });

    // Setup shader program
    _shaderProgram = OsEng.renderEngine().buildRenderProgram(
        "FieldlinesSequence",
        "${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_vs.glsl",
        "${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_fs.glsl"
    );

    if (!_shaderProgram) {
        LERROR("Shader program failed initialization!");
        _sourceFileType = INVALID;
    }

    //------------------ Initialize OpenGL VBOs and VAOs-------------------------------//
    glGenVertexArrays(1, &_vertexArrayObject);
    glGenBuffers(1, &_vertexPositionBuffer);
    glGenBuffers(1, &_vertexColorBuffer);
}

void RenderableFieldlinesSequence::deinitialize() {
    glDeleteVertexArrays(1, &_vertexArrayObject);
    _vertexArrayObject = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shaderProgram) {
        renderEngine.removeRenderProgram(_shaderProgram);
        _shaderProgram = nullptr;
    }

    // Stall main thread until thread that's loading states is done!
    while (/*!_newStateIsReady &&*/ _isLoadingStateFromDisk) {
        LWARNING("TRYING TO DESTROY CLASS WHEN A THREAD USING IT IS STILL ACTIVE");
    }
}

bool RenderableFieldlinesSequence::isReady() const {
    return _sourceFileType != INVALID;
}

void RenderableFieldlinesSequence::render(const RenderData& data, RendererTasks&) {
    if (_activeTriggerTimeIndex != -1) {
        _shaderProgram->activate();

        // Calculate Model View MatrixProjection
        const glm::dmat4 ROT_MAT = glm::dmat4(data.modelTransform.rotation);
        // const glm::mat4 SCALE_TRANSFORM = glm::mat4(1.0); // TODO remove if no use
        const glm::dmat4 MODEL_MAT =
                glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
                ROT_MAT *
                glm::dmat4(glm::scale(glm::dmat4(1), glm::dvec3(data.modelTransform.scale)));
        const glm::dmat4 MODEL_VIEW_MAT = data.camera.combinedViewMatrix() * MODEL_MAT;

        _shaderProgram->setUniform("modelViewProjection",
                    data.camera.sgctInternal.projectionMatrix() * glm::mat4(MODEL_VIEW_MAT));


        _shaderProgram->setUniform("colorMethod", _colorMethod);
        _shaderProgram->setUniform("lineColor", _colorUniform);

        if (_colorMethod == ColorMethod::BY_QUANTITY) {
                ghoul::opengl::TextureUnit textureUnit;
                textureUnit.activate();
                _transferFunction->bind(); // Calls update internally
                _shaderProgram->setUniform("colorTable", textureUnit);
                _shaderProgram->setUniform("colorTableRange",
                                              _colorTableRanges[_colorQuantity]);
        }

        // Flow/Particles
        _shaderProgram->setUniform("usingParticles", _flowEnabled);
        _shaderProgram->setUniform("flowColor", _flowColor);
        _shaderProgram->setUniform("particleSize", _flowParticleSize);
        _shaderProgram->setUniform("particleSpeed", _flowSpeed);
        _shaderProgram->setUniform("particleSpacing", _flowParticleSpacing);
        _shaderProgram->setUniform("time", OsEng.runTime() * (_flowReversed ? -1 : 1));

        glBindVertexArray(_vertexArrayObject);
        glMultiDrawArrays(
                GL_LINE_STRIP, //_drawingOutputType,
                _states[_activeStateIndex].lineStart().data(),
                _states[_activeStateIndex].lineCount().data(),
                static_cast<GLsizei>(_states[_activeStateIndex].lineStart().size())
        );

        glBindVertexArray(0);
        _shaderProgram->deactivate();
    }
}

void RenderableFieldlinesSequence::update(const UpdateData& data) {
    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
    }

    // This node shouldn't do anything if its been disabled from the gui!
    if (_enabled) {
        const double CURRENT_TIME = data.time.j2000Seconds();
        // Check if current time in OpenSpace is within sequence interval
        if (isWithinSequenceInterval(CURRENT_TIME)) {
            const int NEXT_IDX = _activeTriggerTimeIndex + 1;
            if (_activeTriggerTimeIndex < 0                                          // true => Previous frame was not within the sequence interval
                || CURRENT_TIME < _startTimes[_activeTriggerTimeIndex]               // true => OpenSpace has stepped back    to a time represented by another state
                || (NEXT_IDX < _nStates && CURRENT_TIME >= _startTimes[NEXT_IDX])) { // true => OpenSpace has stepped forward to a time represented by another state

                updateActiveTriggerTimeIndex(CURRENT_TIME);

                if (_isLoadingStatesAtRuntime) {
                    _mustLoadNewStateFromDisk = true;
                } else {
                    _needsUpdate = true;
                    _activeStateIndex = _activeTriggerTimeIndex;
                }
            } // else {we're still in same state as previous frame (no changes needed)}
        } else {
            // Not in interval => set everything to false
            _activeTriggerTimeIndex   = -1;
            _mustLoadNewStateFromDisk = false;
            _needsUpdate              = false;
        }

        if (_mustLoadNewStateFromDisk) {
            if (!_isLoadingStateFromDisk && !_newStateIsReady) {
                    _isLoadingStateFromDisk    = true;
                    _mustLoadNewStateFromDisk  = false;
                    const std::string FILEPATH = _sourceFiles[_activeTriggerTimeIndex];
                    std::thread readBinaryThread([this, FILEPATH] {
                        this->readNewState(FILEPATH);
                    });
                    readBinaryThread.detach();
            }
        }

        if (_needsUpdate || _newStateIsReady) {
            if (_isLoadingStatesAtRuntime) {
                _states[0] = std::move(_newState);
            }

            updateVertexPositionBuffer();

            if (_states[_activeStateIndex].nExtraQuantities() > 0) {
                _shouldUpdateColorBuffer = true;
            } else {
                _colorMethod = ColorMethod::UNIFORM;
            }

            // Everything is set and ready for rendering!
            _needsUpdate     = false;
            _newStateIsReady = false;
        }

        if (_shouldUpdateColorBuffer) {
            updateVertexColorBuffer();
            _shouldUpdateColorBuffer = false;
        }
    }
}

inline bool RenderableFieldlinesSequence::isWithinSequenceInterval(const double CURRENT_TIME) const {
    return (CURRENT_TIME >= _startTimes[0]) && (CURRENT_TIME < _sequenceEndTime);
}

// Assumes we already know that CURRENT_TIME is within the sequence interval
void RenderableFieldlinesSequence::updateActiveTriggerTimeIndex(const double CURRENT_TIME) {
    auto iter = std::upper_bound(_startTimes.begin(), _startTimes.end(), CURRENT_TIME);
    if (iter != _startTimes.end()) {
        if ( iter != _startTimes.begin()) {
            _activeTriggerTimeIndex = std::distance(_startTimes.begin(), iter) - 1;
        } else {
            _activeTriggerTimeIndex = 0;
        }
    } else {
        _activeTriggerTimeIndex = static_cast<int>(_nStates) - 1;
    }
}

// Reading state from disk. Thread safe!
void RenderableFieldlinesSequence::readNewState(const std::string& FILEPATH) {
    FieldlinesState newState;

    bool isSuccessful = newState.loadStateFromOsfls(FILEPATH);
    _newState = std::move(newState);

    _newStateIsReady        = true;
    _isLoadingStateFromDisk = false;
}

// Unbind buffers and arrays
inline void unbindGL() {
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableFieldlinesSequence::updateVertexPositionBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    const std::vector<glm::vec3>& VERTEX_POS_VEC =
            _states[_activeStateIndex].vertexPositions();

    glBufferData(GL_ARRAY_BUFFER, VERTEX_POS_VEC.size() * sizeof(glm::vec3),
            &VERTEX_POS_VEC.front(), GL_STATIC_DRAW);

    glEnableVertexAttribArray(_VA_POSITION);
    glVertexAttribPointer(_VA_POSITION, 3, GL_FLOAT, GL_FALSE, 0, 0);

    unbindGL();
}

void RenderableFieldlinesSequence::updateVertexColorBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexColorBuffer);

    bool isSuccessful;
    const std::vector<float>& QUANTITY_VEC =
            _states[_activeStateIndex].extraQuantity(_colorQuantity, isSuccessful);

    if (isSuccessful) {
        glBufferData(GL_ARRAY_BUFFER, QUANTITY_VEC.size() * sizeof(float),
                &QUANTITY_VEC.front(), GL_STATIC_DRAW);

        glEnableVertexAttribArray(_VA_COLOR);
        glVertexAttribPointer(_VA_COLOR, 1, GL_FLOAT, GL_FALSE, 0, 0);

        unbindGL();
    }
}

bool RenderableFieldlinesSequence::extractInfoFromDictionary(
        const ghoul::Dictionary& dictionary) {

    string name;
    dictionary.getValue(SceneGraphNode::KeyName, name);
    name += ": ";

    // ------------------- EXTRACT MANDATORY VALUES FROM DICTIONARY ------------------- //
    string inputFileTypeValue;
    if(!dictionary.getValue(KEY_INPUT_FILE_TYPE, inputFileTypeValue)) {
        LERROR(name << "The field " << string(KEY_INPUT_FILE_TYPE) << " is missing!");
        return false;
    } else {
        std::transform(inputFileTypeValue.begin(), inputFileTypeValue.end(),
                       inputFileTypeValue.begin(), ::tolower);
        // Verify that the input type is correct
        if (inputFileTypeValue == VALUE_INPUT_FILE_TYPE_CDF) {
            _sourceFileType = CDF;
        } else if (inputFileTypeValue == VALUE_INPUT_FILE_TYPE_JSON) {
            _sourceFileType = JSON;
        } else if (inputFileTypeValue == VALUE_INPUT_FILE_TYPE_OSFLS) {
            _sourceFileType = OSFLS;
        } else {
            LERROR(name << inputFileTypeValue << " is not a recognised "
                        << KEY_INPUT_FILE_TYPE);
            _sourceFileType = INVALID;
            return false;
        }
    }

    string sourceFolderPath;
    if(!dictionary.getValue(KEY_SOURCE_FOLDER, sourceFolderPath)) {
        LERROR(name << "The field " << string(KEY_SOURCE_FOLDER) << " is missing!");
        return false;
    }

    // Ensure that the source folder exists and then extract
    // the files with the same extension as <inputFileTypeValue>
    ghoul::filesystem::Directory sourceFolder(sourceFolderPath);
    if (FileSys.directoryExists(sourceFolder)) {
        // Extract all file paths from the provided folder (Non-recursively! Sorted!)
        _sourceFiles = sourceFolder.readFiles(ghoul::Boolean::No, ghoul::Boolean::Yes);

        // Remove all files that don't have <inputFileTypeValue> as extension
        _sourceFiles.erase(std::remove_if(_sourceFiles.begin(), _sourceFiles.end(),
            [inputFileTypeValue] (string str) {
                const size_t EXT_LENGTH = inputFileTypeValue.length();
                string sub = str.substr(str.length() - EXT_LENGTH, EXT_LENGTH);
                std::transform(sub.begin(), sub.end(), sub.begin(), ::tolower);
                return sub != inputFileTypeValue;
            }), _sourceFiles.end());
        // Ensure that there are available and valid source files left
        if (_sourceFiles.empty()) {
            LERROR(name << sourceFolderPath << " contains no ." << inputFileTypeValue
                        << " files!");
            return false;
        }
    } else {
        LERROR(name << "FieldlinesSequence" << sourceFolderPath
                    << " is not a valid directory!");
        return false;
    }

    // Extract optional info from modfile
    ghoul::Dictionary colorTablesPathsDictionary;
    if (dictionary.getValue(KEY_COLOR_TABLE_PATHS, colorTablesPathsDictionary)) {
        const size_t N_PROVIDED_PATHS = colorTablesPathsDictionary.size();
        if (N_PROVIDED_PATHS > 0) {
            // Clear the default! It is already specified in the transferFunction
            _colorTablePaths.clear();
            for (size_t i = 1; i <= N_PROVIDED_PATHS; ++i) {
                _colorTablePaths.push_back(
                        colorTablesPathsDictionary.value<std::string>( std::to_string(i) ) );
            }
        }
    }

    ghoul::Dictionary colorTablesRangesDictionary;
    if (dictionary.getValue(KEY_COLOR_TABLE_RANGES, colorTablesRangesDictionary)) {
        const size_t N_PROVIDED_RANGES = colorTablesRangesDictionary.size();
        for (size_t i = 1; i <= N_PROVIDED_RANGES; ++i) {
            _colorTableRanges.push_back(
                    colorTablesRangesDictionary.value<glm::vec2>( std::to_string(i) ) );
        }
    } else {
        _colorTableRanges.push_back(glm::vec2(0, 1));
    }

    // Extract info specific to each inputType
    switch (_sourceFileType) {
        case CDF:
            LERROR(name << "CDF NOT YET IMPLEMENTED!");
            break;
        case JSON:
            LERROR(name << "JSON NOT YET IMPLEMENTED!");
            break;
        case OSFLS: {
                bool shouldLoadInRealtime = false;
                if (dictionary.getValue(KEY_OSLFS_LOAD_AT_RUNTIME, shouldLoadInRealtime)) {
                    _isLoadingStatesAtRuntime = shouldLoadInRealtime;
                } else {
                    LWARNING(name << KEY_OSLFS_LOAD_AT_RUNTIME <<
                        " isn't specified! OSFLS files will be loaded during runtime!");
                }
            } break;
        default:
            break;
    }
}

// Calculate expected end time.
void RenderableFieldlinesSequence::computeSequenceEndTime() {
    if (_nStates > 1) {
        const double LAST_TRIGGER_TIME      = _startTimes[_nStates - 1];
        const double SEQUENCE_DURATION      = LAST_TRIGGER_TIME - _startTimes[0];
        const double AVERAGE_STATE_DURATION = SEQUENCE_DURATION / (static_cast<double>(_nStates) - 1.0);
        _sequenceEndTime = LAST_TRIGGER_TIME + AVERAGE_STATE_DURATION;
    } else {
        // If there's just one state it should never disappear!
        _sequenceEndTime = DBL_MAX;
    }
}

// Extract J2000 time from file names
// Requires files to be named as such: 'YYYY-MM-DDTHH-MM-SS-XXX.osfls'
void RenderableFieldlinesSequence::extractTriggerTimesFromFileNames() {
    const size_t FILENAME_SIZE = 23; // number of  characters in filename (excluding '.osfls')
    const size_t EXT_SIZE      = 6;  // size(".osfls")

    for (const std::string& FILEPATH : _sourceFiles) {
        const size_t STR_LENGTH = FILEPATH.size();
        // Extract the filename from the path (without extension)
        std::string timeString = FILEPATH.substr(STR_LENGTH - FILENAME_SIZE - EXT_SIZE,
                                                FILENAME_SIZE - 1);
        // Ensure the separators are correct
        timeString.replace( 4, 1, "-");
        timeString.replace( 7, 1, "-");
        timeString.replace(13, 1, ":");
        timeString.replace(16, 1, ":");
        timeString.replace(19, 1, ".");
        const double TRIGGER_TIME = Time::convertTime(timeString);
        _startTimes.push_back(TRIGGER_TIME);
    }
}

} // namespace openspace
