#ifndef TEXTURE_H
#define TEXTURE_H

/*
Author: Victor Sand (victor.sand@gmail.com)
Wrapper for OpenGL 1D, 2D and 3D textures
*/

#include <vector>
#include <string>
#include <ghoul/opengl/texture.h>


namespace osp {

class ShaderProgram;

class Texture {
public:
  // Get x, y or z dimensions (0, 1, 2 etc) if they exist
  unsigned int Dim(unsigned int _axis) const;
  // Return handle for OpenGL use
  unsigned int Handle() const;
  // Initialize, subclasses have to implement
  virtual bool Init(float * data) = 0;
  // Bind to a ShaderProgram
  virtual bool Bind(ShaderProgram * _shaderProgram,
                    std::string _uniformName,
                    unsigned int _texUnit);
  virtual ~Texture();

protected:
  Texture(std::vector<unsigned int> _dim);
  std::vector<unsigned int> dim_;
  bool initialized_;
  ghoul::opengl::Texture* _texture;
};

}

#endif
