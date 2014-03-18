/* 
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 * Simple functionality to read, save and access constants.
 * Reads the specified file as soon as the object is created. 
 *
 */

#ifndef CONFIG_H_
#define CONFIG_H_

#include <string>

namespace osp {

class Config {
public:
  static Config * New(const std::string &_configFilename);
  ~Config();

  // Reads the config file, can be called by external modules
  bool Read();

  int WinWidth() const { return winWidth_; }
  int WinHeight() const { return winHeight_; }
  bool ClearCache() const { return clearCache_; }
  int TextureDivisionFactor() const { return textureDivisionFactor_; }
  unsigned int LocalWorkSizeX() const { return localWorkSizeX_; }
  unsigned int LocalWorkSizeY() const { return localWorkSizeY_; }
  std::string TSPFilename() const { return TSPFilename_; }
  std::string TFFilename() const { return TFFilename_; }
  std::string RaycasterKernelFilename()const{return raycasterKernelFilename_;}
  std::string TSPTraversalKernelFilename() const 
    { return TSPTraversalKernelFilename_; }
  std::string CubeShaderVertFilename() const { return cubeShaderVertFilename_;}
  std::string CubeShaderFragFilename() const { return cubeShaderFragFilename_;}
  std::string QuadShaderVertFilename() const { return quadShaderVertFilename_;}
  std::string QuadShaderFragFilename() const { return quadShaderFragFilename_;}
  float SpatialErrorTolerance() const { return spatialErrorTolerance_; }
  float TemporalErrorTolerance() const { return temporalErrorTolerance_; }
  float TSPTraversalStepsize() const { return TSPTraversalStepsize_; }
  float RaycasterStepsize() const { return raycasterStepsize_; }
  float RaycasterIntensity() const { return raycasterIntensity_; }
  float AnimatorRefreshInterval() const { return animatorRefreshInterval_; }
  float MousePitchFactor() const { return mousePitchFactor_; }
  float MouseRollFactor() const { return mouseRollFactor_; }
  float ZoomFactor() const { return zoomFactor_; }
  float StartPitch() const { return startPitch_; }
  float StartRoll() const { return startRoll_; }
  float StartYaw() const { return startYaw_; }
  float TranslateX() const { return translateX_; }
  float TranslateY() const { return translateY_; }
  float TranslateZ() const { return translateZ_; }
  int CalculateError() const { return calculateError_; }
  float PitchSpeed() const { return pitchSpeed_; }
  float RollSpeed() const { return rollSpeed_; }
  float YawSpeed() const { return yawSpeed_; }
  bool TakeScreenshot() const { return takeScreenshot_; }

private:
  Config();
  Config(const std::string &_configFilename);
  Config(const Config&);

  std::string configFilename_;

  int winWidth_;
  int winHeight_;
  bool clearCache_;
  int textureDivisionFactor_;
  unsigned int localWorkSizeX_;
  unsigned int localWorkSizeY_;
  std::string TSPFilename_;
  std::string TFFilename_;
  std::string raycasterKernelFilename_;
  std::string TSPTraversalKernelFilename_;
  std::string cubeShaderVertFilename_;
  std::string cubeShaderFragFilename_;
  std::string quadShaderVertFilename_;
  std::string quadShaderFragFilename_;
  float spatialErrorTolerance_;
  float temporalErrorTolerance_;
  float TSPTraversalStepsize_;
  float raycasterStepsize_;
  float raycasterIntensity_;
  float animatorRefreshInterval_;
  float mousePitchFactor_;
  float mouseRollFactor_;
  float zoomFactor_;
  float startPitch_;
  float startRoll_;
  float startYaw_;
  float translateX_;
  float translateY_;
  float translateZ_;
  int calculateError_;
  float pitchSpeed_;
  float rollSpeed_;
  float yawSpeed_;
  bool takeScreenshot_;


};

}

#endif


