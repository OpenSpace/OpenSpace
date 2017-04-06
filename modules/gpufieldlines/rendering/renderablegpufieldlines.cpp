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

#include <modules/gpufieldlines/rendering/renderablegpufieldlines.h>
#include <modules/gpufieldlines/util/gpufieldlinesmanager.h>

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

// VOLUME
#include <modules/kameleonvolume/kameleonvolumereader.h>

namespace {
    std::string _loggerCat = "GpuRenderableFieldlines";
}

namespace {
    const char* keyVolume = "VectorVolume";
    const char* keyFieldlines = "Fieldlines";
    const char* keySeedPoints = "SeedPoints";

    const char* keyVolumeDirectory = "Directory";
    const char* keyVolumeTracingVariable = "TracingVariable";

    const char* keyFieldlineMaxTraceSteps = "MaximumTracingSteps";

    const char* keySeedPointsFile = "File";

    // const char* keySeedPointsDirectory = "Directory"; // TODO: allow for varying seed points?
}

// const float R_E_TO_METER = 6371000.f; // Earth radius

namespace openspace {

RenderableGpuFieldlines::RenderableGpuFieldlines(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary),
      _vertexArrayObject(0),
      _vertexPositionBuffer(0),
      _vertexColorBuffer(0),
      _shouldRender(false),
      _needsUpdate(false),
      _activeStateIndex(-1) {

    std::string name;
    dictionary.getValue(SceneGraphNode::KeyName, name);

    _loggerCat = "RenderableGpuFieldlines [" + name + "]";

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

bool RenderableGpuFieldlines::isReady() const {
    return _program ? true : false;
}

bool RenderableGpuFieldlines::initialize() {
    // SeedPoints Info. Needs a .txt file containing seed points.
    // Each row should have 3 floats seperated by spaces
    std::string pathToSeedPointFile;
    if (!_seedPointsInfo.getValue(keySeedPointsFile, pathToSeedPointFile)) {
        LERROR(keySeedPoints << " doesn't specify a '" << keySeedPointsFile << "'" <<
            "\n\tRequires a path to a .txt file containing seed point data." <<
            "Each row should have 3 floats seperated by spaces.");
        return false;
    } else {
        if (!GpuFieldlinesManager::ref().getSeedPointsFromFile(pathToSeedPointFile,
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
                "Files must be of the same model and in !");
        return false;
    } else { // Everything essential is provided
        std::vector<std::string> validCdfFilePaths;
        if (!GpuFieldlinesManager::ref().getCdfFilePaths(pathToCdfDirectory,
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

        _numberOfStates = 1;//validCdfFilePaths.size();
        _states.reserve(_numberOfStates);
        _startTimes.reserve(_numberOfStates);

        LDEBUG("Found the following valid .cdf files in " << pathToCdfDirectory);

        // TODO this could be done in manager
        for (int i = 0; i < _numberOfStates; ++i) {
            LDEBUG(validCdfFilePaths[i] << " is now being traced.");

            KameleonVolumeReader kvr(validCdfFilePaths[i]);

            ghoul::Dictionary md = kvr.readMetaData();

            // FOR ENLIL AND BATSRUS: std::vector of length 3, capacity 3
            // {'r', 'theta', 'phi'} or {'x', 'y', 'z'}
            std::vector<std::string> gvn = kvr.gridVariableNames();

            // FOR ENLIL: std::vector of length 13, capacity 16
            // [0] = "r",  [1] = "theta",  [2]  = "phi",    [3] = "rho", [4] = "T",  [5] = "ur",     [6]  = "utheta", [7] = "uphi"
            // [8] = "br", [9] = "btheta", [10] = "bphi",   [11] = "dp", [12] = "bp", [13] = "b1r", [14] = "b1theta", [15] = "b1phi"
            // FOR BATSRUS: std::vector of length 40, capacity 64
            // [0] = "x", [1] = "y", [2] = "z"
            // [3] = "bx", [4] = "by", [5] = "bz", [6] = "b1x", [7] = "b1y", [8] = "b1z", [9] = "ux", [10] = "uy", [11] = "uz", [12] = "jx", [13] = "jy", [14] = "jz", [15] = "rho", [16] = "p", [17] = "e", [18] = "block_amr_levels", [19] = "block_x_min", [20] = "block_x_max", [21] = "block_y_min", [22] = "block_y_max", [23] = "block_z_min", [24] = "block_z_max", [25] = "block_x_center", [26] = "block_y_center", [27] = "block_z_center", [28] = "block_at_amr_level", [29] = "block_parent_id", [30] = "block_child_count", [31] = "block_child_id_1", [32] = "block_child_id_2", [33] = "block_child_id_3", [34] = "block_child_id_4", [35] = "block_child_id_5", [36] = "block_child_id_6", [37] = "block_child_id_7", [38] = "block_child_id_8", [39] = "status"}
            std::vector<std::string> vn = kvr.variableNames();

            // FOR ENLIL: std::vector of length 13, capacity 16
            // [0] = "valid_min", [1] = "valid_max", [2] = "units", [3] = "grid_system", [4] = "mask", [5] = "description", [6] = "is_vector_components", [7] = "position_grid_system", [8] = "data_grid_system", [9] = "actual_min", [10] = "actual_max", [11] = "Original Name", [12] = "long_name"}
            // FOR BATSRUS: std::vector of length 11, capacity 16
            // [0] = "valid_min", [1] = "valid_max", [2] = "units", [3] = "grid_system", [4] = "mask", [5] = "description", [6] = "is_vector_component", [7] = "position_grid_system", [8] = "data_grid_system", [9] = "actual_min", [10] = "actual_max"}
            std::vector<std::string> van = kvr.variableAttributeNames();

            // FOR ENLIL: std::vector of length 55, capacity 64
            // [0] = "README", [1] = "model_type", [2] = "grid_system_count", [3] = "model_name", [4] = "output_type", [5] = "grid_system_1", [6] = "grid_1_type", [7] = "run_type", [8] = "standard_grid_target", [9] = "original_output_file_name", [10] = "run_registration_number", [11] = "terms_of_usage", [12] = "tim_type", [13] = "tim_title", [14] = "tim_program", [15] = "tim_version", [16] = "tim_project", [17] = "tim_code", [18] = "tim_model", [19] = "tim_geometry", [20] = "tim_grid", [21] = "tim_coordinates", [22] = "tim_rotation", [23] = "tim_case", [24] = "tim_cordata", [25] = "tim_observatory", [26] = "tim_corona", [27] = "tim_crpos", [28] = "tim_shift_deg", [29] = "tim_boundary", [30] = "tim_run", [31] = "tim_parameters", [32] = "tim_boundary_old", [33] = "tim_obsdate_mjd", [34] = "tim_obsdate_cal", [35] = "tim_crstart_mjd", [36] = "tim_crstart_cal", [37] = "tim_rundate_mjd", [38] = "tim_rundate_cal", [39] = "tim_rbnd", [40] = "tim_gamma", [41] = "tim_xalpha", [42] = "tim_mevo", [43] = "tim_mfld", [44] = "tim_mslc", [45] = "tim_mtim", [46] = "tim_creation", [47] = "grid_system_1_dimension_1_size", [48] = "grid_system_1_dimension_2_size", [49] = "grid_system_1_dimension_3_size", [50] = "grid_system_1_number_of_dimensions", [51] = "time_physical_time", [52] = "time_physical_time_step", [53] = "time_numerical_time_step", [54] = "Conversion Time"}
            // FOR BATSRUS: std::vector of length 52, capacity 64
            // [0] = "README", [1] = "model_name", [2] = "model_type", [3] = "generation_date", [4] = "original_output_file_name", [5] = "generated_by", [6] = "terms_of_usage", [7] = "grid_system_count", [8] = "grid_system_1_number_of_dimensions", [9] = "grid_system_1_dimension_1_size", [10] = "grid_system_1_dimension_2_size", [11] = "grid_system_1_dimension_3_size", [12] = "grid_system_1", [13] = "output_type", [14] = "standard_grid_target", [15] = "grid_1_type", [16] = "start_time", [17] = "end_time", [18] = "run_type", [19] = "kameleon_version", [20] = "elapsed_time_in_seconds", [21] = "number_of_dimensions", [22] = "special_parameter_g", [23] = "special_parameter_c", [24] = "special_parameter_th", [25] = "special_parameter_P1", [26] = "special_parameter_P2", [27] = "special_parameter_P3", [28] = "special_parameter_R", [29] = "special_parameter_NX", [30] = "special_parameter_NY", [31] = "special_parameter_NZ", [32] = "x_dimension_size", [33] = "y_dimension_size", [34] = "z_dimension_size", [35] = "current_iteration_step", [36] = "global_x_min", [37] = "global_x_max", [38] = "global_y_min", [39] = "global_y_max", [40] = "global_z_min", [41] = "global_z_max", [42] = "max_amr_level", [43] = "number_of_cells", [44] = "number_of_blocks", [45] = "smallest_cell_size", [46] = "r_body", [47] = "r_currents", [48] = "dipole_time", [49] = "dipole_update", [50] = "dipole_tilt", [51] = "dipole_tilt_y"}
            std::vector<std::string> gan = kvr.globalAttributeNames();


            float  xMin = kvr.minValue("x");
            float  xMax = kvr.maxValue("x");
            float  yMin = kvr.minValue("y");
            float  yMax = kvr.maxValue("y");
            float  zMin = kvr.minValue("z");
            float  zMax = kvr.maxValue("z");

            glm::vec3 domainBoundsLower = glm::vec3(xMin,yMin,zMin);
            glm::vec3 domainBoundsUpper = glm::vec3(xMax,yMax,zMax);
            // float bxMin = kvr.minValue("bx");
            // float bxMax = kvr.maxValue("bx");

            auto bxUniformDistr = kvr.readFloatVolume(glm::uvec3(32,32,32), "bx", domainBoundsLower, domainBoundsUpper);
            float* p = bxUniformDistr->data();


            _states.push_back(GpuFieldlinesState(_seedPoints.size()));

        }

        // Approximate the end time of last state (and for the  as a whole)
        if (_numberOfStates > 0) {
            _seqStartTime = _startTimes[0];
            double lastStateStart = _startTimes[_numberOfStates-1];
            // double avgTimeOffset = (lastStateStart - _seqStartTime) /
            //                        (static_cast<double>(_numberOfStates) - 1.0);
            _seqEndTime =  lastStateStart + 99999.9;//avgTimeOffset;
            // Add seqEndTime as the last start time
            // to prevent vector from going out of bounds later.
            _startTimes.push_back(_seqEndTime); // =  lastStateStart + avgTimeOffset;
        }
    }

    _program = OsEng.renderEngine().buildRenderProgram(
        "GpuFieldlines",
        "${MODULE_GPUFIELDLINES}/shaders/gpufieldline_flow_direction_vs.glsl",
        "${MODULE_GPUFIELDLINES}/shaders/gpufieldline_flow_direction_gs.glsl",
        // "${MODULE_GPUFIELDLINES}/shaders/fieldline_morph_flow_direction_vs.glsl",
        "${MODULE_GPUFIELDLINES}/shaders/fieldline_flow_direction_fs.glsl"
    );

    if (!_program) {
        LERROR("Shader program failed initialization!");
        return false;
    }

    return true;
}

bool RenderableGpuFieldlines::deinitialize() {
    glDeleteVertexArrays(1, &_vertexArrayObject);
    _vertexArrayObject = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    glDeleteBuffers(1, &_vertexColorBuffer);
    _vertexColorBuffer = 0;


    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_program) {
        renderEngine.removeRenderProgram(_program);
        _program = nullptr;
    }

    return true;
}

void RenderableGpuFieldlines::render(const RenderData& data) {
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

void RenderableGpuFieldlines::update(const UpdateData&) {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
    }

    // Check if current time in OpenSpace is within  interval
    if (isWithinInterval()) {
        // if NOT in the same state as in the previous update..
        if ( _activeStateIndex < 0 ||
             _currentTime < _startTimes[_activeStateIndex] ||
             // This next line requires/assumes seqEndTime to be last position in _startTimes
             _currentTime >= _startTimes[_activeStateIndex + 1]) {
            _needsUpdate = true;
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
        }
        glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

        glBufferData(GL_ARRAY_BUFFER,
            _states[_activeStateIndex]._vertexPositions.size() * sizeof(glm::vec3),
            &_states[_activeStateIndex]._vertexPositions.front(),
            GL_STATIC_DRAW);

        GLuint vertexLocation = 0;
        glEnableVertexAttribArray(vertexLocation);
        glVertexAttribPointer(vertexLocation, 3, GL_FLOAT, GL_FALSE, 0, 0);//sizeof(glm::vec3), reinterpret_cast<void*>(0));

        // TODO fix colors
        // GLuint colorLocation = 1;
        // glEnableVertexAttribArray(colorLocation);
        // glVertexAttribPointer(colorLocation, 4, GL_FLOAT, GL_FALSE, sizeof(LinePoint), (void*)(sizeof(glm::vec3)));

        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);

        _needsUpdate = false;
        _shouldRender = true;
    }
}

bool RenderableGpuFieldlines::isWithinInterval() {
    _currentTime = Time::ref().j2000Seconds();
    return (_currentTime >= _seqStartTime) && (_currentTime <  _seqEndTime);
}

// Assumes we already know that _currentTime is within the  interval
void RenderableGpuFieldlines::updateActiveStateIndex() {
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
