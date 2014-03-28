#ifndef TRANSFERFUNCTION_H
#define TRANSFERFUNCTION_H

/* 
Author: Victor Sand (victor.sand@gmail.com)
Transfer function class that consists of a number of MappingKeys. When
the mapping keys are added, the transfer function can be constructed by
interpolating between the MappingKey values.
TODO Use Texture1D for implementation when OpenCL 1.2 is supported 
*/

#include <flare/MappingKey.h>
#include <set>
#include <string>
#include <iostream>
#include <ghoul/opengl/texture.h>

namespace osp {

  
class TransferFunction {
public:
  static TransferFunction * New();
  ~TransferFunction();

  struct Color {
    float r;
    float g;
    float b;
    float a;
  };

  enum Interpolation {
    LINEAR = 0,
  };

  // Host side sample
  bool Sample(float &_r, float &_g, float &_b, float &_a, float _i);

  // Take the saved values and construct a texture
  bool ConstructTexture();
  // Read TF from the in file
  bool ReadFile();
  std::string ToString() const;
  
  // Mutators
  void SetInFilename(const std::string &_inFilename);

  // Accessors
  unsigned int Width() const { return width_; }
  ghoul::opengl::Texture * Texture() { return texture_; }
  
  // TODO temp
  float * FloatData() { return floatData_; }

  // Operators
  TransferFunction& operator=(const TransferFunction &_tf);

private:
  TransferFunction();
  TransferFunction(const TransferFunction &_tf);
  float *floatData_;
  
  ghoul::opengl::Texture *texture_;
  unsigned int width_;
  float lower_;
  float upper_;
  bool generatedTexture_;
  std::string inFilename_;
  std::set<MappingKey> mappingKeys_;
  Interpolation interpolation_;

  // Linearly interpolate between two values. Distance
  // is assumed to be normalized.
  inline float Lerp(float _v0, float _v1, float _d) {
    return _v0*(1.0 - _d) + _v1*_d;
  }


};

}

std::ostream & operator<<(std::ostream &os, const osp::TransferFunction &_tf);

#endif
     
