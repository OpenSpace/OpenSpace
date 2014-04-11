/*
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */

#include <openspace/flare/Config.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <openspace/flare/Utils.h>
#include <ghoul/filesystem/filesystem.h>

using namespace osp;

Config::Config() : configFilename_("OopsNotSet") {}

Config::Config(const std::string &_configFilename) 
  : configFilename_(_configFilename),
    winWidth_(512),
    winHeight_(512),
    clearCache_(false),
    localWorkSizeX_(16),
    localWorkSizeY_(16),
    TSPFilename_("notSet"),
    TFFilename_("notSet"),
    raycasterKernelFilename_("notSet"),
    TSPTraversalKernelFilename_("notSet"),
    cubeShaderVertFilename_("notSet"),
    cubeShaderFragFilename_("notSet"),
    quadShaderVertFilename_("notSet"),
    quadShaderFragFilename_("notSet"),
    spatialErrorTolerance_(0.f),
    temporalErrorTolerance_(0.f),
    TSPTraversalStepsize_(0.1f),
    raycasterStepsize_(0.1f),
    raycasterIntensity_(1.f),
    animatorRefreshInterval_(1.f),
    mousePitchFactor_(1.f),
    mouseRollFactor_(1.f),
    zoomFactor_(1.f),
    startPitch_(0.f),
    startRoll_(0.f),
    startYaw_(0.f),
    translateX_(0.f),
    translateY_(0.f),
    translateZ_(0.f),
    calculateError_(1),
    pitchSpeed_(0.f),
    rollSpeed_(0.f),
    yawSpeed_(0.f),
    takeScreenshot_(false) 
{}
    
Config::~Config() {}

Config * Config::New(const std::string &_configFilename) {
  Config *config = new Config(_configFilename);
  if (!config->Read()) {
    ERROR("Could not read config. Returning NULL.");
    delete config;
    return NULL;
  }
  return config;
}

bool Config::Read() {
  
  INFO("\nReading config from " << configFilename_);

  std::ifstream in;
  in.open(configFilename_.c_str(), std::ifstream::in);
  if (!in.is_open()) {
    ERROR("Could not open " << configFilename_);
    return false;
  }

  std::string line;
  while (std::getline(in, line)) {
    // Ignore empty lines and comments
    if (!line.empty() && line.at(0) != '#') {
      // Read variable name
      std::stringstream ss;
      ss.str(line);
      std::string variable;
      ss >> variable;
      // Save value
      if (variable == "tsp_filename") {
        ss >> TSPFilename_;
        TSPFilename_ = absPath(TSPFilename_);
        INFO("TSP file name: " << TSPFilename_);
      } else if (variable == "transferfunction_filename") {
        ss >> TFFilename_;
        TFFilename_ = absPath(TFFilename_);
        INFO("Transfer function file name " << TFFilename_);
      } else if (variable == "spatial_error_tolerance") {
        ss >> spatialErrorTolerance_;
        INFO("Spatial error tolerance: " << spatialErrorTolerance_);
      } else if (variable == "temporal_error_tolerance") {
        ss >> temporalErrorTolerance_;
        INFO("Temporal error tolerance: " << temporalErrorTolerance_);
      } else if (variable == "tsp_traversal_stepsize") {
        ss >> TSPTraversalStepsize_;
        INFO("TSP traversal step size: " << TSPTraversalStepsize_); 
      } else if (variable == "raycaster_stepsize") {
        ss >> raycasterStepsize_; 
        INFO("Ray caster step size: " << raycasterStepsize_);
      } else if (variable == "raycaster_intensity") {
        ss >> raycasterIntensity_;
        INFO("Ray caster intensity: " << raycasterIntensity_);
      } else if (variable == "animator_refresh_interval") {
        ss >> animatorRefreshInterval_;
        INFO("Animator refresh interval: " << animatorRefreshInterval_);
      } else if (variable == "win_width") {
        ss >> winWidth_;
        INFO("Win width: " << winWidth_);
      } else if (variable == "win_height") {
        ss >> winHeight_;
        INFO("Win height: " << winHeight_);
      } else if (variable == "raycaster_kernel_filename") {
        ss >> raycasterKernelFilename_;
        raycasterKernelFilename_ = absPath(raycasterKernelFilename_);
        INFO("Raycaster kernel file name: " << raycasterKernelFilename_);
      } else if (variable == "tsp_traversal_kernel_filename" ) {
        ss >> TSPTraversalKernelFilename_;
        TSPTraversalKernelFilename_ = absPath(TSPTraversalKernelFilename_);
        INFO("TSP traversal kernel file name: " <<TSPTraversalKernelFilename_);
      } else if (variable == "cube_shader_vert_filename") {
        ss >> cubeShaderVertFilename_;
        cubeShaderVertFilename_ = absPath(cubeShaderVertFilename_);
        INFO("Cube vertex shader file name: " << cubeShaderVertFilename_);
      } else if (variable == "cube_shader_frag_filename") {
        ss >> cubeShaderFragFilename_;
        cubeShaderFragFilename_ = absPath(cubeShaderFragFilename_);
        INFO("Cube fragment shader file name: " << cubeShaderFragFilename_);
      } else if (variable == "quad_shader_vert_filename") {
        ss >> quadShaderVertFilename_;
        quadShaderVertFilename_ = absPath(quadShaderVertFilename_);
        INFO("Quad vertex shader file name: " << quadShaderVertFilename_);
       } else if (variable == "quad_shader_frag_filename") {
        ss >> quadShaderFragFilename_;
        quadShaderFragFilename_ = absPath(quadShaderFragFilename_);
        INFO("Cube vertex shader file name: " << quadShaderFragFilename_);
      } else if (variable == "mouse_pitch_factor") {
        ss >> mousePitchFactor_;
        INFO("Mouse pitch factor " << mousePitchFactor_);
      } else if (variable == "mouse_roll_factor") {
        ss >> mouseRollFactor_;
        INFO("Mouse roll factor " << mouseRollFactor_);
      } else if (variable == "zoom_factor") {
        ss >> zoomFactor_;
        INFO("Zoom factor " << zoomFactor_);
      } else if (variable == "start_pitch") {
        ss >> startPitch_;
        INFO("Start pitch: " << startPitch_);
      } else if (variable == "start_roll") {
        ss >> startRoll_;
        INFO("Start roll: " << startRoll_);
      } else if (variable == "start_yaw") {
        ss >> startYaw_;
        INFO("Start yaw: " << startYaw_);
      } else if (variable == "translate_x") {
        ss >> translateX_;
        INFO("Translate X: " << translateX_);
      } else if (variable == "translate_y") {
        ss >> translateY_;
        INFO("Translate Y: " << translateY_);
      } else if (variable == "translate_z") {
        ss >> translateZ_;
        INFO("Translate Z: " << translateZ_);
      } else if (variable == "local_worksize_x") {
        ss >> localWorkSizeX_;
        INFO("Local worksize X: " << localWorkSizeX_);
      } else if (variable == "local_worksize_y") {
        ss >> localWorkSizeY_;
        INFO("Local worksize Y: " << localWorkSizeY_);
      } else if (variable == "texture_division_factor") {
        ss >> textureDivisionFactor_;
        INFO("Texture division factor: " << textureDivisionFactor_);
      } else if (variable == "clear_cache") {
        ss >> clearCache_;
        INFO("Clearing cache: " << clearCache_);
      } else if (variable == "calculate_error") {
        ss >> calculateError_;
        INFO("Calculate error: " << calculateError_);
      } else if (variable == "pitch_speed") {
        ss >> pitchSpeed_;
        INFO("Pitch speed: " << pitchSpeed_);
      } else if (variable == "roll_speed") {
        ss >> rollSpeed_;
        INFO("Roll speed: " << rollSpeed_);
      } else if (variable == "yaw_speed") {
        ss >> yawSpeed_;
        INFO("Yaw speed: " << yawSpeed_);
      } else if (variable == "take_screenshot") {
        ss >> takeScreenshot_;
        INFO("Take screenshot: " << takeScreenshot_);
      } else { 
        ERROR("Variable name " << variable << " unknown");
      } 
    }
  }

  INFO("");

  return true;
}





