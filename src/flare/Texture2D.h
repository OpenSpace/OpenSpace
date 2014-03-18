#ifndef TEXTURE2D_H_
#define TEXTURE2D_H_

/* 
Author: Victor Sand (victor.sand@gmail.com)
Wrapper for OpenGL 2D Texture
*/

#include <flare/Texture.h>
#include <vector>

namespace osp {

class ShaderProgram;

class Texture2D : public Texture {
public:
  static Texture2D * New(std::vector<unsigned int> _dim);
  virtual bool Init(float *_data = 0);
  virtual bool Bind(ShaderProgram * _shaderProgram,
                    std::string _uniformName,
                    unsigned int _texUnit) const;
private:
  Texture2D(std::vector<unsigned int> _dim);
};

}

#endif
