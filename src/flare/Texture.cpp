/*
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */


#include <ghoul/opengl/ghoul_gl.h>
#include <flare/Texture.h>
#include <flare/Utils.h>

using namespace osp;

Texture::Texture(std::vector<unsigned int> _dim) 
  : dim_(_dim),
    initialized_(false), _texture(nullptr) {
}

unsigned int Texture::Dim(unsigned int _axis) const {
  if (_axis > dim_.size()) {
    ERROR("Texture axis too large");
    return 0;
  }
  return dim_[_axis];
}

bool Texture::Bind(ShaderProgram *_shaderProgram, std::string _uniformName,
                   unsigned int _texUnit) {
  WARNING("Call to default implementation of Texture::Bind");
  return true;
}

unsigned int Texture::Handle() const {
  if(_texture) {
	 return *_texture;
  }
  return 0;
}

Texture::~Texture() { 
	if(_texture) {
	  delete _texture;
	}
}


