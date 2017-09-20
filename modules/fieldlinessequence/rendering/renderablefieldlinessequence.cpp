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

using std::string;

namespace {
    std::string _loggerCat = "RenderableFieldlinesSequence";

    // ----- KEYS POSSIBLE IN MODFILE. EXPECTED DATA TYPE OF VALUE IN [BRACKETS]  ----- //
    // ---------------------------- MANDATORY MODFILE KEYS ---------------------------- //
    const char* KEY_INPUT_FILE_TYPE         = "InputFileType";   // [STRING]
    const char* KEY_SOURCE_FOLDER           = "SourceFolder";    // [STRING]

    // ---------------------------- OPTIONAL MODFILE KEYS  ---------------------------- //
    const char* KEY_OSLFS_LOAD_AT_RUNTIME   = "LoadAtRuntime";   // [BOOLEAN] If value False => Load in initializing step and store in RAM

    // ------------- POSSIBLE STRING VALUES FOR CORRESPONDING MODFILE KEY ------------- //
    const char* VALUE_INPUT_FILE_TYPE_CDF   = "cdf";
    const char* VALUE_INPUT_FILE_TYPE_JSON  = "json";
    const char* VALUE_INPUT_FILE_TYPE_OSFLS = "osfls";
} // namespace

namespace openspace {

RenderableFieldlinesSequence::RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary) {

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
                LERROR("OSFLS LOAD AT RUNTIME NOT YET IMPLEMENTED!");
            } else {
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

    computeSequenceEndTime();

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
}

bool RenderableFieldlinesSequence::isReady() const {
    return true;
}

void RenderableFieldlinesSequence::render(const RenderData& data, RendererTasks&) {
    if (_activeStateIndex != -1) {
        _shaderProgram->activate();

        const glm::dmat4 ROTATION_TRANSFORM = glm::dmat4(data.modelTransform.rotation);
        // const glm::mat4 SCALE_TRANSFORM = glm::mat4(1.0); // TODO remove if no use
        const glm::dmat4 MODEL_TRANSFORM =
                glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
                ROTATION_TRANSFORM *
                glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));
        const glm::dmat4 MODEL_VIEW_TRANSFORM = data.camera.combinedViewMatrix() * MODEL_TRANSFORM;

        _shaderProgram->setUniform("modelViewProjection",
                    data.camera.sgctInternal.projectionMatrix() * glm::mat4(MODEL_VIEW_TRANSFORM));

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
            const int NEXT_IDX = _activeStateIndex + 1;
            if (_activeStateIndex < 0                                                // true => Previous frame was not within the sequence interval
                || CURRENT_TIME < _startTimes[_activeStateIndex]                     // true => OpenSpace has stepped back    to a time represented by another state
                || (NEXT_IDX < _nStates && CURRENT_TIME >= _startTimes[NEXT_IDX])) { // true => OpenSpace has stepped forward to a time represented by another state

                updateActiveStateIndex(CURRENT_TIME);
                _needsUpdate = true;
            } // else {we're still in same state as previous frame (no changes needed)}
        } else {
            // Not in interval => set everything to false
            _activeStateIndex = -1;
            _needsUpdate      = false;
        }

        if (_needsUpdate) {
            glBindVertexArray(_vertexArrayObject);
            glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

            const std::vector<glm::vec3>& VERTEX_POS_VEC =
                    _states[_activeStateIndex].vertexPositions();

            glBufferData(GL_ARRAY_BUFFER, VERTEX_POS_VEC.size() * sizeof(glm::vec3),
                    &VERTEX_POS_VEC.front(), GL_STATIC_DRAW);

            glEnableVertexAttribArray(_vertAttrVertexPos);
            glVertexAttribPointer(_vertAttrVertexPos, 3, GL_FLOAT, GL_FALSE, 0, 0);

            // UNBIND
            glBindBuffer(GL_ARRAY_BUFFER, 0);
            glBindVertexArray(0);

            // Everything is set and ready for rendering!
            _needsUpdate = false;
        }
    }
}

inline bool RenderableFieldlinesSequence::isWithinSequenceInterval(const double CURRENT_TIME) {
    return (CURRENT_TIME >= _startTimes[0]) && (CURRENT_TIME < _sequenceEndTime);
}

// Assumes we already know that CURRENT_TIME is within the sequence interval
void RenderableFieldlinesSequence::updateActiveStateIndex(const double CURRENT_TIME) {
    auto iter = std::upper_bound(_startTimes.begin(), _startTimes.end(), CURRENT_TIME);
    if (iter != _startTimes.end()) {
        if ( iter != _startTimes.begin()) {
            _activeStateIndex = std::distance(_startTimes.begin(), iter) - 1;
        } else {
            _activeStateIndex = 0;
        }
    } else {
        _activeStateIndex = _nStates - 1;
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
                        " isn't specified! Fieldline states will be stored in RAM");
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

} // namespace openspace
