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

#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <glm/gtc/matrix_transform.hpp>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>

#include <modules/gpufieldlines/util/gpufieldlinesmanager.h>

// VOLUME
#include <modules/kameleonvolume/kameleonvolumereader.h>


namespace {
    std::string _loggerCat = "GpuRenderableFieldlines";
}

namespace {
    const char* keyFieldlines = "Fieldlines";
    const char* keyFieldlineMaxTraceSteps = "MaximumTracingSteps";

    const char* keyVolume = "VectorVolume";
    const char* keyVolumeDirectory = "Directory";
    const char* keyVolumeTracingVariable = "TracingVariable";

    const char* keySeedPoints = "SeedPoints";
    const char* keySeedPointsFile = "File";

    const int integrationSimpleEuler = 0;
    const int integrationRungeKutta4 = 1;

    // const char* keySeedPointsDirectory = "Directory"; // TODO: allow for varying seed points?
}

// const float R_E_TO_METER = 6371000.f; // Earth radius

namespace openspace {

RenderableGpuFieldlines::RenderableGpuFieldlines(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary),
      _vertexArrayObject(0),
      _vertexPositionBuffer(0),
      _vertexColorBuffer(0),
      _gridVAO(0),
      _gridVBO(0),
      _stepSize("stepSize", "Step coefficient", 0.2, 0.0001, 3.0),
      _clippingRadius("clippingRadius", "Clipping Radius", 3.0, 1.0, 5.0),
      // _minLength("minLength", "Min Step Size", 0.0, 0.0, 4.0),
      _integrationMethod("integrationMethod", "Integration Method", properties::OptionProperty::DisplayType::Radio),
      _maximumVertices("numMaxVertices", "Max Number Of Vertices", 1, 1, 10),
      _showGrid("showGrid", "Show Grid", false),
      _isMorphing("isMorphing", "Morphing", true),
      _domainX("domainX", "Domain Limits X-axis"),
      _domainY("domainY", "Domain Limits Y-axis"),
      _domainZ("domainZ", "Domain Limits Z-axis"),
      _uniformFieldlineColor("fieldLineColor", "Fieldline Color",
                             glm::vec4(0.f,1.f,0.f,0.45f),
                             glm::vec4(0.f),
                             glm::vec4(1.f)),
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

    _integrationMethod.addOption(integrationSimpleEuler, "Simple Euler");
    _integrationMethod.addOption(integrationRungeKutta4, "Runge-Kutta 4th Order");

    // GEOMETRY SHADER CAN ONLY OUTPUT A SMALL NUMBER OF VERTICES
    GLint max_vertices, max_components;
    glGetIntegerv(GL_MAX_GEOMETRY_OUTPUT_VERTICES, &max_vertices);
    glGetIntegerv(GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS, &max_components);
    const int NUM_GS_OUTPUT_COMPONENTS_PER_VERTEX = 5; // Output components per vertex from Geometry Shader.. TODO: change this const if GS outputs are changed!
    _maximumVertices.setMaxValue(max_components / NUM_GS_OUTPUT_COMPONENTS_PER_VERTEX);
    _maximumVertices.setValue(max_components / NUM_GS_OUTPUT_COMPONENTS_PER_VERTEX);
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
        // TODO: remove this else scope? not needed!
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

        _numberOfStates = validCdfFilePaths.size();
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
            //      [0] = "r",  [1] = "theta",  [2]  = "phi",    [3] = "rho", [4] = "T",  [5] = "ur",     [6]  = "utheta", [7] = "uphi"
            //      [8] = "br", [9] = "btheta", [10] = "bphi",   [11] = "dp", [12] = "bp", [13] = "b1r", [14] = "b1theta", [15] = "b1phi"
            // FOR BATSRUS: std::vector of length 40, capacity 64
            //      [0] = "x", [1] = "y", [2] = "z"
            //      [3] = "bx", [4] = "by", [5] = "bz", [6] = "b1x", [7] = "b1y", [8] = "b1z", [9] = "ux", [10] = "uy", [11] = "uz", [12] = "jx", [13] = "jy", [14] = "jz", [15] = "rho", [16] = "p", [17] = "e", [18] = "block_amr_levels", [19] = "block_x_min", [20] = "block_x_max", [21] = "block_y_min", [22] = "block_y_max", [23] = "block_z_min", [24] = "block_z_max", [25] = "block_x_center", [26] = "block_y_center", [27] = "block_z_center", [28] = "block_at_amr_level", [29] = "block_parent_id", [30] = "block_child_count", [31] = "block_child_id_1", [32] = "block_child_id_2", [33] = "block_child_id_3", [34] = "block_child_id_4", [35] = "block_child_id_5", [36] = "block_child_id_6", [37] = "block_child_id_7", [38] = "block_child_id_8", [39] = "status"}
            std::vector<std::string> vn = kvr.variableNames();

            // FOR ENLIL: std::vector of length 13, capacity 16
            //      [0] = "valid_min", [1] = "valid_max", [2] = "units", [3] = "grid_system", [4] = "mask", [5] = "description", [6] = "is_vector_components", [7] = "position_grid_system", [8] = "data_grid_system", [9] = "actual_min", [10] = "actual_max", [11] = "Original Name", [12] = "long_name"}
            // FOR BATSRUS: std::vector of length 11, capacity 16
            //      [0] = "valid_min", [1] = "valid_max", [2] = "units", [3] = "grid_system", [4] = "mask", [5] = "description", [6] = "is_vector_component", [7] = "position_grid_system", [8] = "data_grid_system", [9] = "actual_min", [10] = "actual_max"}
            std::vector<std::string> van = kvr.variableAttributeNames();

            // FOR ENLIL: std::vector of length 55, capacity 64
            //      [0] = "README", [1] = "model_type", [2] = "grid_system_count", [3] = "model_name", [4] = "output_type", [5] = "grid_system_1", [6] = "grid_1_type", [7] = "run_type", [8] = "standard_grid_target", [9] = "original_output_file_name", [10] = "run_registration_number", [11] = "terms_of_usage", [12] = "tim_type", [13] = "tim_title", [14] = "tim_program", [15] = "tim_version", [16] = "tim_project", [17] = "tim_code", [18] = "tim_model", [19] = "tim_geometry", [20] = "tim_grid", [21] = "tim_coordinates", [22] = "tim_rotation", [23] = "tim_case", [24] = "tim_cordata", [25] = "tim_observatory", [26] = "tim_corona", [27] = "tim_crpos", [28] = "tim_shift_deg", [29] = "tim_boundary", [30] = "tim_run", [31] = "tim_parameters", [32] = "tim_boundary_old", [33] = "tim_obsdate_mjd", [34] = "tim_obsdate_cal", [35] = "tim_crstart_mjd", [36] = "tim_crstart_cal", [37] = "tim_rundate_mjd", [38] = "tim_rundate_cal", [39] = "tim_rbnd", [40] = "tim_gamma", [41] = "tim_xalpha", [42] = "tim_mevo", [43] = "tim_mfld", [44] = "tim_mslc", [45] = "tim_mtim", [46] = "tim_creation", [47] = "grid_system_1_dimension_1_size", [48] = "grid_system_1_dimension_2_size", [49] = "grid_system_1_dimension_3_size", [50] = "grid_system_1_number_of_dimensions", [51] = "time_physical_time", [52] = "time_physical_time_step", [53] = "time_numerical_time_step", [54] = "Conversion Time"}
            // FOR BATSRUS: std::vector of length 52, capacity 64
            //      [0] = "README", [1] = "model_name", [2] = "model_type", [3] = "generation_date", [4] = "original_output_file_name", [5] = "generated_by", [6] = "terms_of_usage", [7] = "grid_system_count", [8] = "grid_system_1_number_of_dimensions", [9] = "grid_system_1_dimension_1_size", [10] = "grid_system_1_dimension_2_size", [11] = "grid_system_1_dimension_3_size", [12] = "grid_system_1", [13] = "output_type", [14] = "standard_grid_target", [15] = "grid_1_type", [16] = "start_time", [17] = "end_time", [18] = "run_type", [19] = "kameleon_version", [20] = "elapsed_time_in_seconds", [21] = "number_of_dimensions", [22] = "special_parameter_g", [23] = "special_parameter_c", [24] = "special_parameter_th", [25] = "special_parameter_P1", [26] = "special_parameter_P2", [27] = "special_parameter_P3", [28] = "special_parameter_R", [29] = "special_parameter_NX", [30] = "special_parameter_NY", [31] = "special_parameter_NZ", [32] = "x_dimension_size", [33] = "y_dimension_size", [34] = "z_dimension_size", [35] = "current_iteration_step", [36] = "global_x_min", [37] = "global_x_max", [38] = "global_y_min", [39] = "global_y_max", [40] = "global_z_min", [41] = "global_z_max", [42] = "max_amr_level", [43] = "number_of_cells", [44] = "number_of_blocks", [45] = "smallest_cell_size", [46] = "r_body", [47] = "r_currents", [48] = "dipole_time", [49] = "dipole_update", [50] = "dipole_tilt", [51] = "dipole_tilt_y"}
            std::vector<std::string> gan = kvr.globalAttributeNames();

            // Vector volume. Space related (domain)
            float  xMin = kvr.minValue("x");
            float  xMax = kvr.maxValue("x");
            float  yMin = kvr.minValue("y");
            float  yMax = kvr.maxValue("y");
            float  zMin = kvr.minValue("z");
            float  zMax = kvr.maxValue("z");

            if (i == 0) {
                _domainMins = glm::vec3(xMin,yMin,zMin);
                _domainMaxs = glm::vec3(xMax,yMax,zMax);
            } else {
                ghoul_assert(_domainMins == glm::vec3(xMin,yMin,zMin) &&
                             _domainMaxs == glm::vec3(xMax,yMax,zMax),
                             "Spatial domains of CDF files are of different dimensions!");
            }

            // New resampled domain dimensions (voxel grid)
            _dimensions = glm::uvec3(4,4,4);
            // _dimensions = glm::uvec3(128,128,128);

            // Actual max/min values after uniform resampling of volume
            float newBxMin;
            float newBxMax;
            float newByMin;
            float newByMax;
            float newBzMin;
            float newBzMax;

            // Uniform resampling of magnetic components within the domain
            LDEBUG("Creating float volume for variable 'bx'. Dimensions are: " << _dimensions.x << " x " << _dimensions.y << " x " <<_dimensions.z);
            auto bxUniformDistr = kvr.readFloatVolume(_dimensions, "bx", _domainMins, _domainMaxs, newBxMin, newBxMax);

            LDEBUG("Creating float volume for variable 'by'. Dimensions are: " << _dimensions.x << " x " << _dimensions.y << " x " <<_dimensions.z);
            auto byUniformDistr = kvr.readFloatVolume(_dimensions, "by", _domainMins, _domainMaxs, newByMin, newByMax);

            LDEBUG("Creating float volume for variable 'bz'. Dimensions are: " << _dimensions.x << " x " << _dimensions.y << " x " <<_dimensions.z);
            auto bzUniformDistr = kvr.readFloatVolume(_dimensions, "bz", _domainMins, _domainMaxs, newBzMin, newBzMax);

            LDEBUG("Done creating float volumes");

            float* bxVol = bxUniformDistr->data();
            float* byVol = byUniformDistr->data();
            float* bzVol = bzUniformDistr->data();

            LDEBUG("Creating raw volume!");
            _normalizedVolume = std::make_unique<RawVolume<glm::vec3>>(_dimensions);

            glm::vec3* out = _normalizedVolume->data();

            LDEBUG("Normalizing!");
            for (size_t i = 0; i < _normalizedVolume->nCells(); ++i) {
                float ox = bxVol[i];
                float oy = byVol[i];
                float oz = bzVol[i];
                out[i] = glm::vec3(ox,oy,oz);
            }
            LDEBUG("\n\tNormalizing DONE!");

            _volumeTexture.push_back(std::make_unique<ghoul::opengl::Texture>(
                _dimensions,
                ghoul::opengl::Texture::Format::RGB,
                GL_RGBA32F,
                GL_FLOAT,
                ghoul::opengl::Texture::FilterMode::Linear,
                ghoul::opengl::Texture::WrappingMode::ClampToEdge
            ));

            void* data = reinterpret_cast<void*>(_normalizedVolume->data());

            _volumeTexture[i]->setPixelData(data, ghoul::opengl::Texture::TakeOwnership::No);

            // TODO MOVE SOMEWHERE
            _volumeTexture[i]->uploadTexture();

            _startTimes.push_back(GpuFieldlinesManager::ref().getTime(kvr.getKameleon())); // March 15th 2015 00:00:00.000
            // _startTimes.push_back(479649600.0); // March 15th 2015 00:00:00.000

            if (i == 0) {
                // TODO:
                // if (model == "batsrus") {
                    generateUniformCartesian3DGrid();
                // } else if (model == "enlil") {
                    // generateUniformSphericalGrid();
                // }
            }
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
    }

    _program = OsEng.renderEngine().buildRenderProgram(
        "GpuFieldlines",
        "${MODULE_GPUFIELDLINES}/shaders/gpufieldline_flow_direction_vs.glsl",
        "${MODULE_GPUFIELDLINES}/shaders/gpufieldline_flow_direction_fs.glsl",
        "${MODULE_GPUFIELDLINES}/shaders/gpufieldline_flow_direction_gs.glsl"
    );

    if (!_program) {
        LERROR("Shader program failed initialization!");
        return false;
    }

    _gridProgram = OsEng.renderEngine().buildRenderProgram(
        "GpuFieldlinesGrid",
        "${MODULE_GPUFIELDLINES}/shaders/linedraw_vs.glsl",
        "${MODULE_GPUFIELDLINES}/shaders/linedraw_fs.glsl"
    );

    if (!_gridProgram) {
        LERROR("Shader program failed initialization!");
        return false;
    }

    // TODO REMOVE.. ONLY FOR DEBUGGING!!
    // using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    // _program->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    // _program->setIgnoreUniformLocationError(IgnoreError::Yes);

    // ADD PROPERTIES
    addProperty(_stepSize);
    addProperty(_clippingRadius);
    addProperty(_showGrid);
    addProperty(_isMorphing);
    addProperty(_integrationMethod);
    addProperty(_maximumVertices);
    addProperty(_domainX);
    addProperty(_domainY);
    addProperty(_domainZ);
    addProperty(_uniformFieldlineColor);

    _domainX.setMinValue(glm::vec2(_domainMins.x, _domainMins.x));
    _domainX.setMaxValue(glm::vec2(_domainMaxs.x, _domainMaxs.x));
    _domainX.setValue(   glm::vec2(_domainMins.x, _domainMaxs.x));

    _domainY.setMinValue(glm::vec2(_domainMins.y, _domainMins.y));
    _domainY.setMaxValue(glm::vec2(_domainMaxs.y, _domainMaxs.y));
    _domainY.setValue(   glm::vec2(_domainMins.y, _domainMaxs.y));

    _domainZ.setMinValue(glm::vec2(_domainMins.z, _domainMins.z));
    _domainZ.setMaxValue(glm::vec2(_domainMaxs.z, _domainMaxs.z));
    _domainZ.setValue(   glm::vec2(_domainMins.z, _domainMaxs.z));

    _domainX.onChange([this] {
        if (_domainX.value()[0] > _domainX.value()[1]) {
            _domainX.setValue(glm::vec2(_domainX.value()[1],_domainX.value()[1]));
        }
    });
    _domainY.onChange([this] {
        if (_domainY.value()[0] > _domainY.value()[1]) {
            _domainY.setValue(glm::vec2(_domainY.value()[1],_domainY.value()[1]));
        }
    });
    _domainZ.onChange([this] {
        if (_domainZ.value()[0] > _domainZ.value()[1]) {
            _domainZ.setValue(glm::vec2(_domainZ.value()[1],_domainZ.value()[1]));
        }
    });

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
    if (_gridProgram) {
        renderEngine.removeRenderProgram(_gridProgram);
        _gridProgram = nullptr;
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
        _program->setUniform("clippingRadius", _clippingRadius);
        _program->setUniform("integrationMethod", _integrationMethod);
        _program->setUniform("maxVertices", _maximumVertices);
        _program->setUniform("domainMins", _domainMins);
        _program->setUniform("domainDiffs", _domainMaxs - _domainMins);
        _program->setUniform("domainXLimits", _domainX);
        _program->setUniform("domainYLimits", _domainY);
        _program->setUniform("domainZLimits", _domainZ);
        _program->setUniform("color", _uniformFieldlineColor);
        _program->setUniform("isMorphing", _isMorphing);

        // TODO MOVE THIS TO UPDATE AND CHECK
        _textureUnit = std::make_unique<ghoul::opengl::TextureUnit>();
        _textureUnit->activate();
        _volumeTexture[_activeStateIndex]->bind();
        _program->setUniform("volumeTexture", _textureUnit->unitNumber());

        if (_isMorphing) {
            _program->setUniform("state_progression", _stateProgress);
            _textureUnit2 = std::make_unique<ghoul::opengl::TextureUnit>();
            _textureUnit2->activate();
            if (_activeStateIndex < _numberOfStates-1) {
                _volumeTexture[_activeStateIndex+1]->bind();
            }
            _program->setUniform("nextVolumeTexture", _textureUnit2->unitNumber());
        }

        // int testTime = static_cast<int>(OsEng.runTime() * 100) / 5;
        // _program->setUniform("time", testTime);

        glDisable(GL_CULL_FACE);

        // _program->setUniform("classification", _classification);
        // if (!_classification)
        //     _program->setUniform("fieldLineColor", _fieldlineColor);

        glBindVertexArray(_vertexArrayObject);

        // GEOMETRY SHADER ONLY ALLOWS A RATHER SMALL AMOUNT OF COMPONENTS TO BE OUTPUT
        // WE THEREFORE SPLIT UP THE TRACING AND RENDERING OF THE FIELDLINES INTO 2 PARTS
        // THIS ALLOWS US TO GET TWICE AS MUCH
        // Forward tracing
        _program->setUniform("stepSize", _stepSize);
        glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>( _seedPoints.size() ) );

        // Backwards tracing
        _program->setUniform("stepSize", -_stepSize);
        glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>( _seedPoints.size() ) );

        glBindVertexArray(0);
        glEnable(GL_CULL_FACE);
        _program->deactivate();

        if (_showGrid) {
            _gridProgram->activate();

            // TODO:
            // if enlil its supposed to be true.. implement an _isSpherical variable
            _gridProgram->setUniform("isSpherical", false);
            _gridProgram->setUniform("modelViewProjection",
                    data.camera.projectionMatrix() * glm::mat4(modelViewTransform));

            glBindVertexArray(_gridVAO);

            glMultiDrawArrays(
                    GL_LINE_STRIP,
                    &_gridStartPos[0],
                    &_gridLineCount[0],
                    static_cast<GLsizei>(_gridStartPos.size())
            );

            _gridProgram->deactivate();
        }
    }
}

void RenderableGpuFieldlines::update(const UpdateData&) {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
    }

    if (_gridProgram->isDirty()) {
        _gridProgram->rebuildFromFile();
    }

    // Check if current time in OpenSpace is within  interval
    if (isWithinInterval()) {
        // if NOT in the same state as in the previous update..
        if ( _activeStateIndex < 0 ||
             _currentTime < _startTimes[_activeStateIndex] ||
             // This condition assumes seqEndTime to be last position in _startTimes
             _currentTime >= _startTimes[_activeStateIndex + 1]) {
            _needsUpdate = true;
        } else if (_isMorphing) {
            double stateDuration = _startTimes[_activeStateIndex + 1] -
                                   _startTimes[_activeStateIndex]; // TODO? could be stored
            double stateTimeElapsed = _currentTime - _startTimes[_activeStateIndex];
            _stateProgress = static_cast<float>(stateTimeElapsed / stateDuration);
            // ghoul_assert(_stateProgress >= 0.0f, "_stateProgress is NEGATIVE!!");
        }
    } else {
        // Not in interval => set everything to false
        _activeStateIndex = -1;
        _shouldRender = false;
        _needsUpdate = false;
    }

    if(_needsUpdate) { // TODO MOST OF THIS IS JUST IMPORTANT FOR SEED POINT.. which are static/const.. so TODO: fix this .....
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
            _seedPoints.size() * sizeof(glm::vec3),
            &_seedPoints.front(),
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

        { //GRID
            if (_gridVAO == 0) {
                glGenVertexArrays(1, &_gridVAO);
            }
            glBindVertexArray(_gridVAO);

            if (_gridVBO == 0) {
                glGenBuffers(1, &_gridVBO);
            }
            glBindBuffer(GL_ARRAY_BUFFER, _gridVBO);

            glBufferData(GL_ARRAY_BUFFER,
                _gridVertices.size() * sizeof(glm::vec3),
                &_gridVertices.front(),
                GL_STATIC_DRAW);

            GLuint gridVertexPos = 1;
            glEnableVertexAttribArray(gridVertexPos);
            glVertexAttribPointer(gridVertexPos, 3, GL_FLOAT, GL_FALSE, 0, 0);//sizeof(glm::vec3), reinterpret_cast<void*>(0));

            glBindBuffer(GL_ARRAY_BUFFER, 0);
            glBindVertexArray(0);
        }

        _needsUpdate = false;
        _shouldRender = true;
    }
}

bool RenderableGpuFieldlines::isWithinInterval() {
    _currentTime = Time::ref().j2000Seconds();
    return (_currentTime >= _seqStartTime) &&
           (_isMorphing ? _currentTime < _startTimes[_numberOfStates-1] // nothing to morph to after last state
                        : _currentTime < _seqEndTime);
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

// TODO MOVE SOMEWHERE
glm::vec3 rLonLatToCartesian(glm::vec3 p) {
    float r         = p.x;
    float lat_rad   = DEG_TO_RAD   * p.y;
    float lon_rad   = DEG_TO_RAD   * p.z;
    float r_cosLat  = r * cos(lat_rad);
    return glm::vec3(r_cosLat * cos(lon_rad),
                     r_cosLat * sin(lon_rad),
                     r * sin(lat_rad));
}

// FOR DEBUGGING PURPOSES
void RenderableGpuFieldlines::generateUniformSphericalGrid() {

    glm::vec3 deltas = (_domainMaxs - _domainMins) / glm::vec3(
                                                        static_cast<float>(_dimensions.x),
                                                        static_cast<float>(_dimensions.y),
                                                        static_cast<float>(_dimensions.z));

    int segmentResolution = 1;
    int lStart = 0;
    // LINES parallel to x axis (radius, r)
    for (int z = 0; z < _dimensions.z; ++z) {
        for (int y = 0; y < _dimensions.y + 1; ++y) {
            _gridStartPos.push_back(lStart);
            _gridVertices.push_back(rLonLatToCartesian(glm::vec3(_domainMins.x,
                                              _domainMins.y + static_cast<float>(y) * deltas.y,
                                              _domainMins.z + static_cast<float>(z) * deltas.z)));
            _gridVertices.push_back(rLonLatToCartesian(glm::vec3(_domainMaxs.x,
                                              _domainMins.y + static_cast<float>(y) * deltas.y,
                                              _domainMins.z + static_cast<float>(z) * deltas.z)));
            lStart += 2;
            _gridLineCount.push_back(2);
        }
    }

    // RINGS (latitude, theta)
    for (int x = 0; x < _dimensions.x + 1; ++x) {
        for (int z = 0; z < _dimensions.z + 1; ++z) {
            _gridStartPos.push_back(lStart);
            int count = 0;
            for (int y = 0; y < _dimensions.y + 1; ++y) {
                // for (int s = 0; s < segmentResolution + 1; ++s) {

                    _gridVertices.push_back(rLonLatToCartesian(glm::vec3(_domainMins.x + static_cast<float>(x) * deltas.x,
                                                      _domainMins.y + static_cast<float>(y) * deltas.y /*+ s / segmentResolution*/,
                                                      _domainMins.z + static_cast<float>(z) * deltas.z)));
                    // _gridVertices.push_back(glm::vec3(_domainMins.x + static_cast<float>(x) * deltas.x,
                    //                                   _domainMins.y,
                    //                                   _domainMins.z + static_cast<float>(z) * deltas.z));
                    // _gridVertices.push_back(glm::vec3(_domainMins.x + static_cast<float>(x) * deltas.x,
                    //                                   _domainMaxs.y,
                    //                                   _domainMins.z + static_cast<float>(z) * deltas.z));
                    ++lStart;// += 2+segmentResolution;
               ++count;
                // }
            }
            _gridLineCount.push_back(count);
        }
    }

    // // RINGS (longitude, phi)
    for (int x = 0; x < _dimensions.x + 1; ++x) {
        for (int y = 0; y < _dimensions.y + 1; ++y) {
            _gridStartPos.push_back(lStart);
            int count = 0;
            for (int z = 0; z < _dimensions.z + 1; ++z) {
                // for (int s = 0; s < segmentResolution + 1; ++s) {

                _gridVertices.push_back(rLonLatToCartesian(glm::vec3(_domainMins.x + static_cast<float>(x) * deltas.x,
                    _domainMins.y + static_cast<float>(y) * deltas.y,
                    _domainMins.z + static_cast<float>(z) * deltas.z /*+ s / segmentResolution*/)));
                // _gridVertices.push_back(glm::vec3(_domainMins.x + static_cast<float>(x) * deltas.x,
                //                                   _domainMins.y,
                //                                   _domainMins.z + static_cast<float>(z) * deltas.z));
                // _gridVertices.push_back(glm::vec3(_domainMins.x + static_cast<float>(x) * deltas.x,
                //                                   _domainMaxs.y,
                //                                   _domainMins.z + static_cast<float>(z) * deltas.z));
                ++lStart;// += 2+segmentResolution;
                ++count;
                // }
            }
            _gridLineCount.push_back(count);
        }
    }
}

void RenderableGpuFieldlines::generateUniformCartesian3DGrid() {

    glm::vec3 deltas = (_domainMaxs - _domainMins) / glm::vec3(
                                                        static_cast<float>(_dimensions.x),
                                                        static_cast<float>(_dimensions.y),
                                                        static_cast<float>(_dimensions.z));

    int lStart = 0;
    // HORIZONTAL LINES parallel to x axis
    for (int z = 0; z < _dimensions.z + 1; ++z) {
        for (int y = 0; y < _dimensions.y + 1; ++y) {
            _gridStartPos.push_back(lStart);
            _gridVertices.push_back(glm::vec3(_domainMins.x,
                                              _domainMins.y + static_cast<float>(y) * deltas.y,
                                              _domainMins.z + static_cast<float>(z) * deltas.z));
            _gridVertices.push_back(glm::vec3(_domainMaxs.x,
                                              _domainMins.y + static_cast<float>(y) * deltas.y,
                                              _domainMins.z + static_cast<float>(z) * deltas.z));
            lStart += 2;
            _gridLineCount.push_back(2);
        }
    }

    // HORIZONTAL LINES parallel to y axis
    for (int z = 0; z < _dimensions.z + 1; ++z) {
        for (int x = 0; x < _dimensions.x + 1; ++x) {
            _gridStartPos.push_back(lStart);

            _gridVertices.push_back(glm::vec3(_domainMins.x + static_cast<float>(x) * deltas.x,
                                              _domainMins.y,
                                              _domainMins.z + static_cast<float>(z) * deltas.z));
            _gridVertices.push_back(glm::vec3(_domainMins.x + static_cast<float>(x) * deltas.x,
                                              _domainMaxs.y,
                                              _domainMins.z + static_cast<float>(z) * deltas.z));
            lStart += 2;
            _gridLineCount.push_back(2);
        }
    }

    // VERTICAL LINES parallel to z axis
    for (int x = 0; x < _dimensions.x + 1; ++x) {
        for (int y = 0; y < _dimensions.y + 1; ++y) {
            _gridStartPos.push_back(lStart);

            _gridVertices.push_back(glm::vec3(_domainMins.x + static_cast<float>(x) * deltas.x,
                                              _domainMins.y + static_cast<float>(y) * deltas.y,
                                              _domainMins.z));
            _gridVertices.push_back(glm::vec3(_domainMins.x + static_cast<float>(x) * deltas.x,
                                              _domainMins.y + static_cast<float>(y) * deltas.y,
                                              _domainMaxs.z));
            lStart += 2;
            _gridLineCount.push_back(2);
        }
    }
}

} // namespace openspace
