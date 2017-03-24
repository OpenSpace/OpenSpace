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

#include <ghoul/misc/assert.h>

#include <openspace/engine/openspaceengine.h>
// #include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>

namespace {
    std::string _loggerCat = "RenderableFieldlinesSequence";
}

namespace {
    const char* keyVectorVolume = "VectorVolume";
    const char* keyFieldlines = "Fieldlines";
    const char* keySeedPoints = "SeedPoints";

    const char* keyVectorVolumeDirectory = "Directory";
    const char* keyVectorVolumeTracingVariable = "TracingVariable";

    const char* keySeedPointsFile = "File";

    // const char* keySeedPointsDirectory = "Directory"; // TODO: allow for varying seed points?

    // FROM renderablekameleonvolume
    // const char* KeyDimensions = "Dimensions";
    // const char* KeyStepSize = "StepSize";
    // const char* KeyTransferFunction = "TransferFunction";
    // const char* KeySource = "Source";
    // const char* KeyVariable = "Variable";
    // const char* KeyLowerDomainBound = "LowerDomainBound";
    // const char* KeyUpperDomainBound = "UpperDomainBound";
    // const char* KeyDomainScale = "DomainScale";
    // const char* KeyLowerValueBound = "LowerValueBound";
    // const char* KeyUpperValueBound = "UpperValueBound";
    // const char* KeyClipPlanes = "ClipPlanes";
    // const char* KeyCache = "Cache";
    // const char* KeyGridType = "GridType";
    // const char* ValueSphericalGridType = "Spherical";
}

const float R_E_TO_METER = 6371000.f; // Earth radius

namespace openspace {

RenderableFieldlinesSequence::RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary) {

    std::string name;
    dictionary.getValue(SceneGraphNode::KeyName, name);

    _loggerCat = "RenderableFieldlines [" + name + "]";

    ghoul::Dictionary vectorVolumeInfo;
    ghoul::Dictionary fieldlineInfo;
    ghoul::Dictionary seedPointsInfo;

    // Find VectorVolume, SeedPoint and Fieldlines Info from Lua
    if (!dictionary.getValue(keyVectorVolume, vectorVolumeInfo)) {
        LERROR("Renderable does not contain a key for '" << keyVectorVolume << "'");
        // deinitialize();
    }

    if (!dictionary.getValue(keyFieldlines, fieldlineInfo)) {
        LERROR("Renderable does not contain a key for '" << keyFieldlines << "'");
        // deinitialize();
    }

    if (!dictionary.getValue(keySeedPoints, seedPointsInfo)) {
        LERROR("Renderable does not contain a key for '" << keySeedPoints << "'");
        // deinitialize();
    }

    // SeedPoints Info. Needs a .txt file containing seed points. Each row should have 3 floats seperated by spaces
    std::string pathToSeedPointFile;
    if (!seedPointsInfo.getValue(keySeedPointsFile, pathToSeedPointFile)) {
        LERROR(keySeedPoints << " doesn't specify a '" << keySeedPointsFile << "'" <<
            "\n\tRequires a path to a .txt file containing seed point data. Each row should have 3 floats seperated by spaces.");
    } else {
        if(!FieldlinesSequenceManager::ref().getSeedPointsFromFile(pathToSeedPointFile, _seedPoints)) {
            LERROR("Failed to find seed points in'" << pathToSeedPointFile << "'");
        }
    }

    // VectorVolume Info. Needs a folder containing .CDF files
    std::string pathToCdfDirectory;
    std::string tracingVariable;
    std::vector<std::string> validCdfFilePaths;
    if (!vectorVolumeInfo.getValue(keyVectorVolumeDirectory, pathToCdfDirectory)) {
        LERROR(keyVectorVolume << " doesn't specify a '" << keyVectorVolumeDirectory << "'" <<
            "\n\tRequires a path to a Directory containing .CDF files. Files should be of the same model and in sequence!");
    } else {
        if (!vectorVolumeInfo.getValue(keyVectorVolumeTracingVariable, tracingVariable)) {

        } else {
            tracingVariable = 'b'; //default: b = magnetic field.
        }
        if (!FieldlinesSequenceManager::ref().getCdfFilePaths(pathToCdfDirectory, validCdfFilePaths)) {
            LERROR("Failed to get valid .cdf file paths from '" << pathToCdfDirectory << "'");
        } else {
            int numberOfStates = validCdfFilePaths.size();
            _states.reserve(numberOfStates);
            LDEBUG("Found the following valid .cdf files in " << pathToCdfDirectory );
            for (int i = 0; i < numberOfStates; ++i) {
                LDEBUG(validCdfFilePaths[i] << " is now being traced.");
                _states.push_back(FieldlinesState(_seedPoints.size()));
                FieldlinesSequenceManager::ref().traceFieldlinesState(
                        validCdfFilePaths[i],
                        tracingVariable,
                        _seedPoints,
                        _states[i]);
            }
        }
    }



        // if(!FieldlinesSequenceManager::ref().traceFieldlines(pathToCdfDirectory, _seedPoints, _states)) {
    { //ONLY FOR DEBUG
        // int spSize = _seedPoints.size();
        // for (int i = 0; i < spSize ; ++i) {
        //     LINFO(_seedPoints[i].x << " " << _seedPoints[i].y << " " << _seedPoints[i].z);
        // }
    }
}

bool RenderableFieldlinesSequence::isReady() const { return true; }

bool RenderableFieldlinesSequence::initialize() { 
    _program = OsEng.renderEngine().buildRenderProgram(
        "FieldlinesSequence",
        "${MODULE_FIELDLINESSEQUENCE}/shaders/fieldline_flow_direction_vs.glsl",
        "${MODULE_FIELDLINESSEQUENCE}/shaders/fieldline_flow_direction_fs.glsl"
    );

    if (!_program) {
        return false;
    }

    return true;
}

bool RenderableFieldlinesSequence::deinitialize() { return true; }

void RenderableFieldlinesSequence::render(const RenderData& data) {
    // if (_isWithinTimeInterval) {
        _program->activate();

        glm::dmat4 rotationTransform = glm::dmat4(data.modelTransform.rotation);
        glm::mat4 scaleTransform = glm::scale(R_E_TO_METER);
        glm::dmat4 modelTransform =
            glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
            rotationTransform *
            glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))) *
            glm::dmat4(scaleTransform);
        glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

        // Set uniforms for shaders
        _program->setUniform("modelViewProjection",
                data.camera.projectionMatrix() * glm::mat4(modelViewTransform));

        //_program->setUniform("modelViewProjection", data.camera.viewProjectionMatrix());
        //_program->setUniform("modelTransform", glm::mat4(1.0));
        int testTime = static_cast<int>(OsEng.runTime()*100)/5;
        _program->setUniform("time", testTime);
        //_program->setUniform("cameraViewDir", glm::vec3(data.camera.viewDirectionWorldSpace()));
        glDisable(GL_CULL_FACE);
        //setPscUniforms(*_program, data.camera, data.position);

        // _program->setUniform("classification", _classification);
        // if (!_classification)
        //     _program->setUniform("fieldLineColor", _fieldlineColor);

        glBindVertexArray(_fieldlineVAO);
        glMultiDrawArrays(
            GL_LINE_STRIP_ADJACENCY,
            &_lineStart[0],
            &_lineCount[0],
            static_cast<GLsizei>(_lineStart.size())
        );
        glBindVertexArray(0);
        glEnable(GL_CULL_FACE);
        _program->deactivate();
    // }

}

void RenderableFieldlinesSequence::update(const UpdateData&) {}

} // namespace openspace
