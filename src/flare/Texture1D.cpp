/*
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */
#include <GL/glew.h>
#include <flare/Texture1D.h>
#include <flare/ShaderProgram.h>
#include <flare/Utils.h>

using namespace osp;

Texture1D::Texture1D(std::vector<unsigned int> _dim) : Texture(_dim) {
	_texture = new ghoul::opengl::Texture(glm::size3_t(_dim[0], 1, 1),
			ghoul::opengl::Texture::Format::RGBA, GL_RGBA, GL_FLOAT);
}

Texture1D * Texture1D::New(std::vector<unsigned int> _dim) {
  if (_dim.size() != 1) {
    ERROR("Texture1D needs dimension vector of size 1, defaulting to width 1");
    _dim = std::vector<unsigned int>(1,1);
  }
  return new Texture1D(_dim);
}

bool Texture1D::Init(float *_data) {
  INFO("Initializing Texture1D");

  if (initialized_) {
    WARNING("Texture1D already initialized, doing nothing");
    return true;
  }
    if(_data != 0)
  _texture->setPixelData(_data);
	_texture->uploadTexture();


  initialized_ = true;
  return CheckGLError("Texture1D::Init()") == GL_NO_ERROR;
}

bool Texture1D::Bind(ShaderProgram * _shaderProgram,
                     std::string _uniformName,
                     unsigned int _texUnit) const {
  // TODO unused
  return false;
}
