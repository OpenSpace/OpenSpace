#ifndef TEXTURE3D_H_
#define TEXTURE3D_H_

/* 
 * Author: Victor Sand (victor.sand@gmail.com)
 * 
 */

#include <flare/Texture.h>
#include <vector>

namespace osp {

class ShaderProgram;

class Texture3D : public Texture {
public:
  static Texture3D * New(std::vector<unsigned int> _dim);
  virtual bool Init(float *_data = 0);
  bool UpdateSubRegion(unsigned int _xOffset,
                       unsigned int _yOffset,
                       unsigned int _zOffset,
                       unsigned int _xSize,
                       unsigned int _ySize,
                       unsigned int _zSize,
                       float *_data);
private:
  Texture3D(std::vector<unsigned int> _dim);
};

}

#endif

