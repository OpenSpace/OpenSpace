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

#include <functional>

#include <ghoul/filesystem/filesystem.h>
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
    const char* keyVolumeRegionOfInterestMins = "RegionOfInterestMins";
    const char* keyVolumeRegionOfInterestMaxs = "RegionOfInterestMaxs";
    const char* keyVoxelGridDimensions = "VoxelGridDimensions";

    const char* keySeedPoints = "SeedPoints";
    const char* keySeedPointsFile = "File";

    const int integrationSimpleEuler = 0;
    const int integrationRungeKutta4 = 1;

    const float R_E_TO_METER = 6371000.f; // Earth radius
    const float A_U_TO_METER = 149597870700.f; // Astronomical Units
    // TODO use GLM instead!
    const float DEG_TO_RAD   = 3.14159265359f / 180.f;
    const float RAD_TO_DEG   = 180.f / 3.14159265359f;

    // const char* keySeedPointsDirectory = "Directory"; // TODO: allow for varying seed points?

    // std::function<void (openspace::RenderableGpuFieldlines*)>  = print_num;
}

namespace openspace {

void RenderableGpuFieldlines::updateSeedPointFile(const ghoul::filesystem::File& file) {
    _needsUpdate = true;
    _shouldRender = false;
    _seedPoints.clear();
    loadSeedPoints(file.path());
}

RenderableGpuFieldlines::RenderableGpuFieldlines(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary),
      _isMorphing("isMorphing", "Morphing", true),
      _showGrid("showGrid", "Show Grid", false),
      _showSeedPoints("showSeedPoints", "Show Seed Points", true),
      _clippingRadius("clippingRadius", "Clipping Radius", 0.1, 0.0, 5.0),
      _seedPointSize("seedPointSize", "Seed Point Size", 4.0, 0.0, 20.0),
      _stepSize("stepSize", "Step Coefficient", 0.2, 0.0001, 1.0),
      _maximumVertices("numMaxVertices", "Max Number Of Vertices", 1, 1, 10),
      _stepMultiplier("stepMultiplier", "Step Multiplier", 1, 1, 1000),
      _vertexSkipping("traceVertexSkipping", "Num vertices skipped (FPS will drop)", 0, 0, 30),
      _integrationMethod("integrationMethod", "Integration Method", properties::OptionProperty::DisplayType::Radio),
      _seedPointSourcePath("sourceFile", "SeedPoint File"),
      _domainX("domainX", "Domain Limits X-axis"),
      _domainY("domainY", "Domain Limits Y-axis"),
      _domainZ("domainZ", "Domain Limits Z-axis"),
      _uniformFieldlineColor("fieldLineColor", "Fieldline Color",
                             glm::vec4(0.f,1.f,0.f,0.45f),
                             glm::vec4(0.f),
                             glm::vec4(1.f)),
      _uniformSeedPointColor("seedPointColor", "SeedPoint Color",
                             glm::vec4(1.f,0.33f,0.f,0.85f),
                             glm::vec4(0.f),
                             glm::vec4(1.f)) {

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

        bool successful = loadSeedPoints(pathToSeedPointFile);

        if (!successful) {
            LERROR("Please enter a valid file path to a file containing seed points!");
            // TODO No need to return false here...
            _seedPointSourcePath = "NOT A VALID PATH: " + absPath(pathToSeedPointFile);
        } else {
            _seedPointSourcePath = absPath(pathToSeedPointFile);
        }

        // TODO THESE NEEDS CLEANING UP
        _seedPointFile = std::make_unique<ghoul::filesystem::File>(
                pathToSeedPointFile,
                ghoul::filesystem::File::RawPath::No,
                [this] (const ghoul::filesystem::File&) {
                    _seedPointsAreDirty = true;
                });

        // TODO THESE NEEDS CLEANING UP
        _seedPointSourcePath.onChange(
                [this] {
                           // _seedPoints.clear();
                           LDEBUG("Seed point file path changed!");
                           auto tmp = std::make_unique<ghoul::filesystem::File>(
                               _seedPointSourcePath,
                               ghoul::filesystem::File::RawPath::No,
                               [this] (const ghoul::filesystem::File&) {
                                   _seedPointsAreDirty = true;
                                });
                           // Swap tmp and _seedPointFile -> old file will be removed!
                           _seedPointFile.swap(tmp);
                           _seedPointsAreDirty = true;
                             // this->loadSeedPoints();
                        });
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

        std::vector<std::string> magVars;

        for (int i = 0; i < _numberOfStates; ++i) {
            LDEBUG("\t" << validCdfFilePaths[i] << " is now being traced.");

            KameleonVolumeReader kvr(validCdfFilePaths[i]);
            std::string modelName = kvr.getKameleon()->getModelName();

            // TODO FIX DOMAIN IF ENLIL
            if (i == 0) {
                _modelName = modelName;

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

                // Variable names of magnetic components.
                // BATSRUS => bx, by    , bz
                // ENLIL   => br, btheta, bphi
                // TODO: Let user specify the 'b' variable in LUA?
                magVars.push_back(tracingVariable + gvn[0]);
                magVars.push_back(tracingVariable + gvn[1]);
                magVars.push_back(tracingVariable + gvn[2]);

                // Vector volume. Space related (domain)
                float xMin = kvr.minValue(gvn[0]);
                float xMax = kvr.maxValue(gvn[0]);
                float yMin = kvr.minValue(gvn[1]);
                float yMax = kvr.maxValue(gvn[1]);
                float zMin = kvr.minValue(gvn[2]);
                float zMax = kvr.maxValue(gvn[2]);

                std::string dimPrefix = "grid_system_1_dimension_";
                _dimensions = glm::uvec3(kvr.getKameleon()->getGlobalAttribute(dimPrefix +
                                                            "1_size").getAttributeInt(),
                                         kvr.getKameleon()->getGlobalAttribute(dimPrefix +
                                                            "2_size").getAttributeInt(),
                                         kvr.getKameleon()->getGlobalAttribute(dimPrefix +
                                                            "3_size").getAttributeInt());

                if (modelName == "enlil") {
                    // For Enlil the z dimension is sequential. 0-2pi
                    _dimensions.z += 1;
                }

                glm::vec3 voxelGridDimensions;
                if (!_vectorVolumeInfo.getValue(keyVoxelGridDimensions,
                                                voxelGridDimensions)) {
                    LWARNING(keyVolume << " isn't specifying " <<
                             keyVoxelGridDimensions << ". Using values from .CDF file: {"
                             << _dimensions.x << ", " << _dimensions.y << ", "
                             << _dimensions.z << " }");
                } else {
                    // Makes no sense to over sample the volume. Set to lowest!
                    if (voxelGridDimensions.x < _dimensions.x) {
                        _dimensions.x = voxelGridDimensions.x;
                    } else {
                        LWARNING(keyVoxelGridDimensions << ".x = "<< voxelGridDimensions.x
                            << " is larger than the grid dimension of the .CDF file ( "
                            << _dimensions.x << " ). Using that value instead!");
                    }

                    if (voxelGridDimensions.y < _dimensions.y) {
                        _dimensions.y = voxelGridDimensions.y;
                    } else {
                        LWARNING(keyVoxelGridDimensions << ".y = "<< voxelGridDimensions.y
                            << " is larger than the grid dimension of the .CDF file ( "
                            << _dimensions.y << " ). Using that value instead!");
                    }

                    if (voxelGridDimensions.z < _dimensions.z) {
                        _dimensions.z = voxelGridDimensions.z;
                    } else {
                        LWARNING(keyVoxelGridDimensions << ".z = "<< voxelGridDimensions.z
                            << " is larger than the grid dimension of the .CDF file ( "
                            << _dimensions.z << " ). Using that value instead!");
                    }
                }

                // TODO: find a upper MAX_NUM_CELLS limit that makes sense!
                const unsigned int MAX_NUM_CELLS = pow(2,24);
                // Just some control to not create a MASSIVE grid!
                if (_dimensions.x * _dimensions.y * _dimensions.z > MAX_NUM_CELLS) {
                    unsigned int defaultDim = 32;
                    LWARNING("Voxel Grid Dimensions: { " << _dimensions.x << ", "
                            << _dimensions.y << ", " << _dimensions.z << " }"
                            << " are too big. Setting dimensions to {" << defaultDim
                            << ", " << defaultDim << ", " << defaultDim <<"}"
                            << "\nConsider specifying custom _dimensions in .mod file!"
                            << " Max limit is " << MAX_NUM_CELLS << " cells in grid!");
                    _dimensions = glm::uvec3(defaultDim);
                }

                LDEBUG("Voxel Grid Dimensions: { " << _dimensions.x << ", "
                                                   << _dimensions.y << ", "
                                                   << _dimensions.z << " }");

                // New resampled domain dimensions (voxel grid)
                // TODO: DIMENSIONS NEED TO BE MORE ACCURATE!!!!!!!
                // TODO: LET USER SPECIFY DIMENSIONS IN LUA.. IF LARGER THEN THE ACTUAL.. SET ACTUAL
                if (modelName == "batsrus") {
                    _isSpherical = false;
                    _clippingRadius = 3.0f;

                } else if (modelName == "enlil") {
                    _isSpherical = true;

                    // TODO: FIX THIS
                    if (_dimensions.x > 1024) {
                        LWARNING("ENLIL GRID DIMENSION 'R' IS TOO LARGE. REDUCING TO 1024");
                        _dimensions.x = 1024;
                    }

                    // CORRECT THE VARIABLE UNITS

                    // ENLIL STORES RADIUS IN METERS
                    // INTERPOLATOR EXPECTS RADIUS IN AU
                    // METERES TO ASTRONOMICAL UNITS
                    xMin /= A_U_TO_METER;
                    xMax /= A_U_TO_METER;

                    // ENLIL STORES THETA IN INTERVAL [0,pi] radians.
                    //          0 at north pole, pi at south pole
                    // INTERPOLATOR EXPECTS [-90,90] degrees
                    //          -90 at south pole and +90 the north pole
                    float temp = yMin;
                    yMin = 90.f - yMax * RAD_TO_DEG;
                    yMax = 90.f - temp * RAD_TO_DEG;

                    // ENLIL STORES PHI IN INTERVAL [0,2pi] radians.
                    // INTERPOLATOR EXPECTS [0,360] degrees
                    zMin = 0.f;
                    zMax = 360.f;
                    // NOTE..!!
                    // The actual min and max of phi are something like 2 and 358 = (360 - 2)
                    // But to be able to use a texture and get proper interpolation we need
                    // it to wrap all the way around! Note also the extra cell added to
                    // _dimensions.z for this reason!

                    _clippingRadius = 0.1f;

                } else { //REMOVE THIS ENTIRE NODE. MODEL IS UN RECOGNIZED!
                    LERROR("CANNOT RECOGNIZE MODEL! ONLY BATSRUS & ENLIL ARE SUPORTED");
                    return false;
                }

                glm::vec3 interestRegionMins;
                if (!_vectorVolumeInfo.getValue(keyVolumeRegionOfInterestMins,
                                                interestRegionMins)) {
                    LWARNING(keyVolume << " isn't specifying "
                            << keyVolumeRegionOfInterestMins
                            << ". Using default values provided by .CDF file!" );
                } else {
                    if (interestRegionMins.x > xMin) {
                        xMin = interestRegionMins.x;
                    } else {
                        LWARNING(keyVolumeRegionOfInterestMins << ".x = "
                            << interestRegionMins.x << " is smaller than the min value ("
                            << xMin << ") given by .CDF file. Using that value instead!");
                    }

                    if (interestRegionMins.y > yMin) {
                        yMin = interestRegionMins.y;
                    } else {
                        LWARNING(keyVolumeRegionOfInterestMins << ".y = "
                            << interestRegionMins.y << " is smaller than the min value ("
                            << yMin << ") given by .CDF file. Using that value instead!");
                    }

                    if (modelName != "enlil") {
                        if (interestRegionMins.z > zMin) {
                            zMin = interestRegionMins.z;
                        } else {
                            LWARNING(keyVolumeRegionOfInterestMins << ".z = "
                                << interestRegionMins.z << " is smaller than the min value ("
                                << xMin << ") given by .CDF file. Using that value instead!");
                        }
                    } else {
                        LWARNING("The ENLIL model requires that the third component (phi)"
                                << " is sequential and in range [0, 360]. "
                                << "No custom values allowed!");
                    }
                }

                glm::vec3 interestRegionMaxs;
                if (!_vectorVolumeInfo.getValue(keyVolumeRegionOfInterestMaxs,
                                                interestRegionMaxs)) {
                    LWARNING(keyVolume << " isn't specifying "
                            << keyVolumeRegionOfInterestMaxs
                            << ". Using default values provided by .CDF file!" );
                } else {
                    if (interestRegionMaxs.x < xMax) {
                        xMax = interestRegionMaxs.x;
                    } else {
                        LWARNING(keyVolumeRegionOfInterestMaxs << ".x = "
                            << interestRegionMaxs.x << " is larger than the max value ("
                            << xMax << ") given by .CDF file. Using that value instead!");
                    }
                    if (interestRegionMaxs.y < yMax) {
                        yMax = interestRegionMaxs.y;
                    } else {
                        LWARNING(keyVolumeRegionOfInterestMaxs << ".y = "
                            << interestRegionMaxs.y << " is larger than the max value ("
                            << yMax << ") given by .CDF file. Using that value instead!");
                    }
                    if (modelName != "enlil") {
                        if (interestRegionMaxs.z < zMax) {
                            zMax = interestRegionMaxs.z;
                        } else {
                            LWARNING(keyVolumeRegionOfInterestMaxs << ".z = "
                                << interestRegionMaxs.z << " is larger than the max value ("
                                << xMin << ") given by .CDF file. Using that value instead!");
                        }
                    } else {
                        LWARNING("The ENLIL model requires that the third component (phi)"
                                << " is sequential and in range [0, 360]. "
                                << "No custom values allowed!");
                    }
                }

                _domainMins = glm::vec3(xMin,yMin,zMin);
                _domainMaxs = glm::vec3(xMax,yMax,zMax);

                LDEBUG("Region of interest: "
                        << gvn[0] << " ∈ [" << xMin << ", " << xMax << "], "
                        << gvn[1] << " ∈ [" << yMin << ", " << yMax << "], "
                        << gvn[2] << " ∈ [" << zMin << ", " << zMax << "]");

                generateUniformVoxelGrid(_domainMins, _domainMaxs,
                                         _dimensions, _isSpherical);

            } else { // i != 0
                // ghoul_assert(_domainMins == glm::vec3(xMin,yMin,zMin) &&
                //              _domainMaxs == glm::vec3(xMax,yMax,zMax),
                //              "Spatial domains of CDF files are of different dimensions!");
                if (modelName != _modelName) {
                    LERROR("Looks like the spcified folder contains .CDF files created"
                            << " from different models! Same model is required!");
                    return false;
                }
            }

            // Uniform resampling of magnetic components within the domain
             LDEBUG("Creating glm::vec3 volume for variables "
                    << magVars[0] << ", " << magVars[1] << " & " << magVars[2]
                    << ". Dimensions are: "
                    << _dimensions.x << " x " << _dimensions.y << " x " <<_dimensions.z);

            _normalizedVolume = kvr.readVec3Volume(_dimensions,
                                                   magVars,
                                                   _domainMins,
                                                   _domainMaxs);

            LDEBUG("Done creating volumes");

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
        }

        // Approximate the end time of last state (and for the sequence as a whole)
        if (_numberOfStates > 0) {
            _seqStartTime = _startTimes[0];
            double lastStateStart = _startTimes[_numberOfStates-1];
            if (_numberOfStates > 1) {
                _isMorphing = true;
                double avgTimeOffset = (lastStateStart - _seqStartTime) /
                                   (static_cast<double>(_numberOfStates) - 1.0);
                _seqEndTime =  lastStateStart + avgTimeOffset;
            } else {
                // THERES ONLY ONE FILE => NO INTERPOLATION CAN BE DONE & NO END TIME AVAILALBE!
                _isMorphing = false;
                // _seqEndTime = 631108800.f; // January 1st 2020
                _seqEndTime = DBL_MAX; // UNTIL THE END OF DAYS!
            }

            // Add seqEndTime as the last start time
            // to prevent vector from going out of bounds later.
            _startTimes.push_back(_seqEndTime);
        }
    }

    _program = OsEng.renderEngine().buildRenderProgram(
        "GpuFieldlines",
        "${MODULE_GPUFIELDLINES}/shaders/gpufieldline_flow_direction_vs.glsl",
        "${MODULE_GPUFIELDLINES}/shaders/gpufieldline_flow_direction_fs.glsl",
        "${MODULE_GPUFIELDLINES}/shaders/gpufieldline_flow_direction_gs.glsl"
    );

    if (!_program) {
        LERROR("Fieldlines Shader program failed initialization!");
        return false;
    }

    _seedPointProgram = OsEng.renderEngine().buildRenderProgram(
        "GpuFieldlinesSeeds",
        "${MODULE_GPUFIELDLINES}/shaders/seedpoint_vs.glsl",
        "${MODULE_GPUFIELDLINES}/shaders/gpufieldline_flow_direction_fs.glsl"
    );

    if (!_seedPointProgram) {
        LERROR("SeedPoint Shader program failed initialization!");
        return false;
    }


    _gridProgram = OsEng.renderEngine().buildRenderProgram(
        "GpuFieldlinesGrid",
        "${MODULE_GPUFIELDLINES}/shaders/linedraw_vs.glsl",
        "${MODULE_GPUFIELDLINES}/shaders/linedraw_fs.glsl"
    );

    if (!_gridProgram) {
        LERROR("Grid Shader program failed initialization!");
        return false;
    }

    // TODO REMOVE.. ONLY FOR DEBUGGING!!
    // using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    // _program->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    // _program->setIgnoreUniformLocationError(IgnoreError::Yes);

    // ADD PROPERTIES
    addProperty(_stepSize);
    addProperty(_showSeedPoints);
    addProperty(_seedPointSize);
    addProperty(_seedPointSourcePath);
    addProperty(_clippingRadius);
    addProperty(_stepMultiplier);
    addProperty(_showGrid);
    addProperty(_isMorphing);
    addProperty(_integrationMethod);
    addProperty(_maximumVertices);
    addProperty(_domainX);
    addProperty(_domainY);
    addProperty(_domainZ);
    addProperty(_uniformFieldlineColor);
    addProperty(_uniformSeedPointColor);
    addProperty(_vertexSkipping);

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
    if (_seedPointProgram) {
        renderEngine.removeRenderProgram(_seedPointProgram);
        _seedPointProgram = nullptr;
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
        glm::mat4 scaleTransform = glm::mat4(1.0); // TODO SET SCALING DEPENDING ON MODEL
        glm::dmat4 modelTransform =
                glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
                rotationTransform *
                glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))) *
                glm::dmat4(scaleTransform);
        glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

        // Set uniforms for shaders
        _program->setUniform("modelViewProjection",
                data.camera.sgctInternal.projectionMatrix() * glm::mat4(modelViewTransform));
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
        _program->setUniform("isSpherical", _isSpherical);
        _program->setUniform("vertexSkipping", _vertexSkipping);

        // TODO MOVE THIS TO UPDATE AND CHECK
        _textureUnit = std::make_unique<ghoul::opengl::TextureUnit>();
        _textureUnit->activate();

        _volumeTexture[_activeStateIndex]->bind();

        // TODO: only if ENLIL => change wrapping mode to repeat for third component
        // if (_isSpherical) {
            // glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_REPEAT);
        // }

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

        glDisable(GL_CULL_FACE);

        // _program->setUniform("classification", _classification);
        // if (!_classification)
        //     _program->setUniform("fieldLineColor", _fieldlineColor);

        glBindVertexArray(_vertexArrayObject);

        // GEOMETRY SHADER ONLY ALLOWS A RATHER SMALL AMOUNT OF COMPONENTS TO BE OUTPUT
        // WE THEREFORE SPLIT UP THE TRACING AND RENDERING OF THE FIELDLINES INTO 2 PARTS
        // THIS ALLOWS US TO GET TWICE AS MUCH
        // Forward tracing
        _program->setUniform("stepSize", _stepSize*_stepMultiplier);
        glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>( _seedPoints.size() ) );

        // Backwards tracing
        _program->setUniform("stepSize", -_stepSize*_stepMultiplier);
        glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>( _seedPoints.size() ) );

        _program->deactivate();
        _seedPointProgram->activate();

        if (_showSeedPoints) {
            _seedPointProgram->setUniform("color", _uniformSeedPointColor);
            _seedPointProgram->setUniform("isSpherical", _isSpherical);
            _seedPointProgram->setUniform("modelViewProjection",
                data.camera.sgctInternal.projectionMatrix() * glm::mat4(modelViewTransform));
            glPointSize(_seedPointSize);
            glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>( _seedPoints.size() ) );
        }

        glBindVertexArray(0);
        glEnable(GL_CULL_FACE);
        _seedPointProgram->deactivate();

        if (_showGrid) {
            _gridProgram->activate();

            // TODO:
            // if enlil its supposed to be true.. implement an _isSpherical variable
            _gridProgram->setUniform("isSpherical", _isSpherical);
            _gridProgram->setUniform("modelViewProjection",
                    data.camera.sgctInternal.projectionMatrix() * glm::mat4(modelViewTransform));

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

    if (_seedPointProgram->isDirty()) {
        _seedPointProgram->rebuildFromFile();
    }

    if (_gridProgram->isDirty()) {
        _gridProgram->rebuildFromFile();
    }

    if (_seedPointsAreDirty) {
        _shouldRender = false;
        _needsUpdate = true;
        _seedPoints.clear();
        bool successful = loadSeedPoints(_seedPointFile->path());
        // if (successful) {
        //     // _seedPoints.clear();
        // }
        _seedPointsAreDirty = false;
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
// TODO MOVE TO SOME UTILITY/HELPER CLASS/FOLDER etc..
// TODO ADD INPUT/OUTPUT VERTEX AND START POS VECTORS
void RenderableGpuFieldlines::generateUniformVoxelGrid(const glm::vec3& domainMins,
                                                       const glm::vec3& domainMaxs,
                                                       const glm::uvec3& dimensions,
                                                       const bool isSpherical) {
    if (isSpherical) {
        generateUniformSphericalGrid(domainMins,domainMaxs,dimensions);
    } else {
        generateUniformCartesianGrid(domainMins,domainMaxs,dimensions);
    }

}

void RenderableGpuFieldlines::generateUniformSphericalGrid(const glm::vec3& domainMins,
                                                           const glm::vec3& domainMaxs,
                                                           const glm::uvec3& dimensions) {

    glm::vec3 deltas = (domainMaxs - domainMins) / glm::vec3(
                                                        static_cast<float>(dimensions.x),
                                                        static_cast<float>(dimensions.y),
                                                        static_cast<float>(dimensions.z));

    // int segmentResolution = 1;
    int lStart = 0;
    // LINES parallel to x axis (radius, r)
    for (unsigned int z = 0; z < dimensions.z; ++z) {
        for (unsigned int y = 0; y < dimensions.y + 1; ++y) {
            _gridStartPos.push_back(lStart);
            _gridVertices.push_back(rLonLatToCartesian(glm::vec3(domainMins.x,
                                              domainMins.y + static_cast<float>(y) * deltas.y,
                                              domainMins.z + static_cast<float>(z) * deltas.z)));
            _gridVertices.push_back(rLonLatToCartesian(glm::vec3(domainMaxs.x,
                                              domainMins.y + static_cast<float>(y) * deltas.y,
                                              domainMins.z + static_cast<float>(z) * deltas.z)));
            lStart += 2;
            _gridLineCount.push_back(2);
        }
    }

    // RINGS (latitude, theta)
    for (unsigned int x = 0; x < dimensions.x + 1; ++x) {
        for (unsigned int z = 0; z < dimensions.z + 1; ++z) {
            _gridStartPos.push_back(lStart);
            int count = 0;
            for (unsigned int y = 0; y < dimensions.y + 1; ++y) {
                // for (int s = 0; s < segmentResolution + 1; ++s) {

                    _gridVertices.push_back(rLonLatToCartesian(glm::vec3(domainMins.x + static_cast<float>(x) * deltas.x,
                                                      domainMins.y + static_cast<float>(y) * deltas.y /*+ s / segmentResolution*/,
                                                      domainMins.z + static_cast<float>(z) * deltas.z)));
                    // _gridVertices.push_back(glm::vec3(domainMins.x + static_cast<float>(x) * deltas.x,
                    //                                   domainMins.y,
                    //                                   domainMins.z + static_cast<float>(z) * deltas.z));
                    // _gridVertices.push_back(glm::vec3(domainMins.x + static_cast<float>(x) * deltas.x,
                    //                                   domainMaxs.y,
                    //                                   domainMins.z + static_cast<float>(z) * deltas.z));
                    ++lStart;// += 2+segmentResolution;
               ++count;
                // }
            }
            _gridLineCount.push_back(count);
        }
    }

    // // RINGS (longitude, phi)
    for (unsigned int x = 0; x < dimensions.x + 1; ++x) {
        for (unsigned int y = 0; y < dimensions.y + 1; ++y) {
            _gridStartPos.push_back(lStart);
            int count = 0;
            for (unsigned int z = 0; z < dimensions.z + 1; ++z) {
                // for (int s = 0; s < segmentResolution + 1; ++s) {

                _gridVertices.push_back(rLonLatToCartesian(glm::vec3(domainMins.x + static_cast<float>(x) * deltas.x,
                    domainMins.y + static_cast<float>(y) * deltas.y,
                    domainMins.z + static_cast<float>(z) * deltas.z /*+ s / segmentResolution*/)));
                // _gridVertices.push_back(glm::vec3(domainMins.x + static_cast<float>(x) * deltas.x,
                //                                   domainMins.y,
                //                                   domainMins.z + static_cast<float>(z) * deltas.z));
                // _gridVertices.push_back(glm::vec3(domainMins.x + static_cast<float>(x) * deltas.x,
                //                                   domainMaxs.y,
                //                                   domainMins.z + static_cast<float>(z) * deltas.z));
                ++lStart;// += 2+segmentResolution;
                ++count;
                // }
            }
            _gridLineCount.push_back(count);
        }
    }
}

void RenderableGpuFieldlines::generateUniformCartesianGrid(const glm::vec3& domainMins,
                                                             const glm::vec3& domainMaxs,
                                                             const glm::uvec3& dimensions) {

    glm::vec3 deltas = (domainMaxs - domainMins) / glm::vec3(
                                                        static_cast<float>(dimensions.x),
                                                        static_cast<float>(dimensions.y),
                                                        static_cast<float>(dimensions.z));

    int lStart = 0;
    // HORIZONTAL LINES parallel to x axis
    for (unsigned int z = 0; z < dimensions.z + 1; ++z) {
        for (unsigned int y = 0; y < dimensions.y + 1; ++y) {
            _gridStartPos.push_back(lStart);
            _gridVertices.push_back(glm::vec3(domainMins.x,
                                              domainMins.y + static_cast<float>(y) * deltas.y,
                                              domainMins.z + static_cast<float>(z) * deltas.z));
            _gridVertices.push_back(glm::vec3(domainMaxs.x,
                                              domainMins.y + static_cast<float>(y) * deltas.y,
                                              domainMins.z + static_cast<float>(z) * deltas.z));
            lStart += 2;
            _gridLineCount.push_back(2);
        }
    }

    // HORIZONTAL LINES parallel to y axis
    for (unsigned int z = 0; z < dimensions.z + 1; ++z) {
        for (unsigned int x = 0; x < dimensions.x + 1; ++x) {
            _gridStartPos.push_back(lStart);

            _gridVertices.push_back(glm::vec3(domainMins.x + static_cast<float>(x) * deltas.x,
                                              domainMins.y,
                                              domainMins.z + static_cast<float>(z) * deltas.z));
            _gridVertices.push_back(glm::vec3(domainMins.x + static_cast<float>(x) * deltas.x,
                                              domainMaxs.y,
                                              domainMins.z + static_cast<float>(z) * deltas.z));
            lStart += 2;
            _gridLineCount.push_back(2);
        }
    }

    // VERTICAL LINES parallel to z axis
    for (unsigned int x = 0; x < dimensions.x + 1; ++x) {
        for (unsigned int y = 0; y < dimensions.y + 1; ++y) {
            _gridStartPos.push_back(lStart);

            _gridVertices.push_back(glm::vec3(domainMins.x + static_cast<float>(x) * deltas.x,
                                              domainMins.y + static_cast<float>(y) * deltas.y,
                                              domainMins.z));
            _gridVertices.push_back(glm::vec3(domainMins.x + static_cast<float>(x) * deltas.x,
                                              domainMins.y + static_cast<float>(y) * deltas.y,
                                              domainMaxs.z));
            lStart += 2;
            _gridLineCount.push_back(2);
        }
    }
}

bool RenderableGpuFieldlines::loadSeedPoints(const std::string& path) {
    if (!GpuFieldlinesManager::ref().getSeedPointsFromFile(path, _seedPoints)) {
        LERROR("Failed to find seed points in'" << path << "'");
        return false;
    }
    // LDEBUG("FOUND SEED POINT FILE! " << _seedPoints.size() << " seed points are provided!");
    return true;
}


} // namespace openspace
