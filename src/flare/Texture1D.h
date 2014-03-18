#ifndef TEXTURE1D_H
#define TEXTURE1D_H

/*
Author: Victor Sand (victor.sand@gmail.com
Wrapper for OpenGL 1D Texture
*/

#include <flare/Texture.h>
#include <vector>

namespace osp {

class ShaderProgram;

class Texture1D : public Texture {
public:
  static Texture1D * New(std::vector<unsigned int> _dim);
  virtual bool Init(float *_data);
  virtual bool Bind(ShaderProgram *_shaderProgram,
                    std::string _uniformName,
                    unsigned int _texUnit) const;
private:
  Texture1D(std::vector<unsigned int> dim_);
};

}

#endif

