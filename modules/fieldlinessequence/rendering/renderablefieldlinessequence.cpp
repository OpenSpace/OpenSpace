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

#include <ghoul/glm.h>
#include <glm/gtc/matrix_transform.hpp>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/misc/assert.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>

namespace {
    std::string _loggerCat = "RenderableFieldlinesSequence";
}

namespace {
    const char* keyVolume = "VectorVolume";
    const char* keyFieldlines = "Fieldlines";
    const char* keySeedPoints = "SeedPoints";

    const char* keyVolumeDirectory = "Directory";
    const char* keyVolumeTracingVariable = "TracingVariable";

    const char* keyFieldlineMaxTraceSteps = "MaximumTracingSteps";
    const char* keyFieldlineShouldMorph = "Morphing";
    const char* keyFieldlineResamples = "NumResamples";
    const char* keyFieldlineResampleOption = "ResamplingType";
    const char* keyFieldlineQuickMorphDistance = "QuickMorphDistance";

    const char* keySeedPointsFile = "File";

    // const char* keySeedPointsDirectory = "Directory"; // TODO: allow for varying seed points?
}

// const float R_E_TO_METER = 6371000.f; // Earth radius

namespace openspace {

RenderableFieldlinesSequence::RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary),
      _vertexArrayObject(0),
      _vertexPositionBuffer(0),
      _morphToPositionBuffer(0),
      _quickMorphBuffer(0),
      _vertexColorBuffer(0),
      _shouldRender(false),
      _needsUpdate(false),
      _activeStateIndex(-1) {

    std::string name;
    dictionary.getValue(SceneGraphNode::KeyName, name);

    _loggerCat = "RenderableFieldlines [" + name + "]";

    // Find VectorVolume, SeedPoint and Fieldlines Info from Lua
    if (!dictionary.getValue(keyVolume, _vectorVolumeInfo)) {
        LERROR("Renderable does not contain a key for '" << keyVolume << "'");
        // deinitialize();
    }

    if (!dictionary.getValue(keyFieldlines, _fieldlineInfo)) {
        LERROR("Renderable does not contain a key for '" << keyFieldlines << "'");
        // deinitialize();
    }

    if (!dictionary.getValue(keySeedPoints, _seedPointsInfo)) {
        LERROR("Renderable does not contain a key for '" << keySeedPoints << "'");
        // deinitialize();
    }
}

bool RenderableFieldlinesSequence::isReady() const {
    return _program ? true : false;
}

bool RenderableFieldlinesSequence::initialize() {
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
    } else { // Everything essential is provided
        std::vector<std::string> validCdfFilePaths;
        if (!FieldlinesSequenceManager::ref().getCdfFilePaths(pathToCdfDirectory,
                                                              validCdfFilePaths)) {
            LERROR("Failed to get valid .cdf file paths from '"
                    << pathToCdfDirectory << "'" );
            return false;
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

        if (!_fieldlineInfo.getValue(keyFieldlineShouldMorph, _isMorphing)) {
            _isMorphing = false; // Default value
            LWARNING(keyFieldlines << " isn't specifying " << keyFieldlineShouldMorph
                    << ". Using default: " << _isMorphing);
        }

        int resamplingOption = 4; // Default
        int numResamples = 2 * maxSteps + 3; // Default
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

        _numberOfStates = validCdfFilePaths.size();
        _states.reserve(_numberOfStates);
        _startTimes.reserve(_numberOfStates);

        LDEBUG("Found the following valid .cdf files in " << pathToCdfDirectory);

        // TODO this could be done in manager
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
                                                                _startTimes,
                                                                _states[i]);

        }

        // Approximate the end time of last state (and for the sequence as a whole)
        if (_numberOfStates > 0) {
            _seqStartTime = _startTimes[0];
            double lastStateStart = _startTimes[_numberOfStates-1];
            double avgTimeOffset = (lastStateStart - _seqStartTime) /
                                   (static_cast<double>(_numberOfStates) - 1.0);
            _seqEndTime =  lastStateStart + avgTimeOffset;
            // Add seqEndTime as the last start time
            // to prevent vector from going out of bounds later.
            _startTimes.push_back(_seqEndTime); // =  lastStateStart + avgTimeOffset;
        }
        if (_isMorphing) {
            LDEBUG("Optimising morphing!");
            float quickMorphDist;
            if(!_fieldlineInfo.getValue(keyFieldlineQuickMorphDistance, quickMorphDist)) {
                quickMorphDist = 20.f * 6371000.f; // 2 times Earth's radius
                LWARNING(keyFieldlines << " isn't specifying " <<
                         keyFieldlineQuickMorphDistance <<
                         ". Default is set to 20 Earth radii.");
            }

            FieldlinesSequenceManager::ref().setQuickMorphBooleans(_states,
                                                                   numResamples,
                                                                   quickMorphDist);
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

    if (!_program) {
        LERROR("Shader program failed initialization!");
        return false;
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

    return true;
}

void RenderableFieldlinesSequence::render(const RenderData& data) {
    // if (_isWithinTimeInterval) {
    if (_shouldRender) {
        _program->activate();

        glm::dmat4 rotationTransform = glm::dmat4(data.modelTransform.rotation);
        glm::mat4 scaleTransform = glm::mat4(1.0); // TODO remove if no use
        glm::dmat4 modelTransform =
                glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
                rotationTransform *
                glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))) *
                glm::dmat4(scaleTransform);
        glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

        // Set uniforms for shaders
        _program->setUniform("modelViewProjection",
                data.camera.projectionMatrix() * glm::mat4(modelViewTransform));

        int testTime = static_cast<int>(OsEng.runTime() * 100) / 5;
        _program->setUniform("time", testTime);
        if (_isMorphing) {
            _program->setUniform("state_progression", _stateProgress);
        }
        glDisable(GL_CULL_FACE);

        // _program->setUniform("classification", _classification);
        // if (!_classification)
        //     _program->setUniform("fieldLineColor", _fieldlineColor);

        glBindVertexArray(_vertexArrayObject);
        glMultiDrawArrays(
                GL_LINE_STRIP_ADJACENCY,
                &_states[_activeStateIndex]._lineStart[0],
                &_states[_activeStateIndex]._lineCount[0],
                static_cast<GLsizei>(_states[_activeStateIndex]._lineStart.size())
        );

        glBindVertexArray(0);
        glEnable(GL_CULL_FACE);
        _program->deactivate();
    }
}

void RenderableFieldlinesSequence::update(const UpdateData&) {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
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
        }
        glBindVertexArray(_vertexArrayObject);

        if (_vertexPositionBuffer == 0) {
            glGenBuffers(1, &_vertexPositionBuffer);
            if (_isMorphing) {
                glGenBuffers(1, &_morphToPositionBuffer);
                glGenBuffers(1, &_quickMorphBuffer);
            }
        }
        glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

        glBufferData(GL_ARRAY_BUFFER,
            _states[_activeStateIndex]._vertexPositions.size() * sizeof(glm::vec3),
            &_states[_activeStateIndex]._vertexPositions.front(),
            GL_STATIC_DRAW);

        GLuint vertexLocation = 0;
        glEnableVertexAttribArray(vertexLocation);
        glVertexAttribPointer(vertexLocation, 3, GL_FLOAT, GL_FALSE, 0, 0);//sizeof(glm::vec3), reinterpret_cast<void*>(0));

        if (_isMorphing) {
            // glBindBuffer(GL_ARRAY_BUFFER, 0); // is this necessary?
            glBindBuffer(GL_ARRAY_BUFFER, _morphToPositionBuffer);

            glBufferData(GL_ARRAY_BUFFER,
                _states[_activeStateIndex+1]._vertexPositions.size() * sizeof(glm::vec3),
                &_states[_activeStateIndex+1]._vertexPositions.front(),
                GL_STATIC_DRAW);

            GLuint morphToLocation = 1;
            glEnableVertexAttribArray(morphToLocation);
            glVertexAttribPointer(morphToLocation, 3, GL_FLOAT, GL_FALSE, 0, 0);//(void*)(sizeof(glm::vec3)));

            // glBindBuffer(GL_ARRAY_BUFFER, 0); // is this necessary?
            glBindBuffer(GL_ARRAY_BUFFER, _quickMorphBuffer);

            glBufferData(GL_ARRAY_BUFFER,
                    _states[_activeStateIndex]._quickMorph.size() * sizeof(GLfloat),
                    &_states[_activeStateIndex]._quickMorph.front(),
                    GL_STATIC_DRAW);

            GLuint useQuickMorph = 2;
            glEnableVertexAttribArray(useQuickMorph);
            glVertexAttribPointer(useQuickMorph, 1, GL_FLOAT, GL_FALSE, 0, 0);
        }

        // TODO fix colors
        // GLuint colorLocation = 1;
        // glEnableVertexAttribArray(colorLocation);
        // glVertexAttribPointer(colorLocation, 4, GL_FLOAT, GL_FALSE, sizeof(LinePoint), (void*)(sizeof(glm::vec3)));

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

} // namespace openspace
