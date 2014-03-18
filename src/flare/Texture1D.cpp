/*
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */
#include <GL/glew.h>
#include <flare/Texture1D.h>
#include <flare/ShaderProgram.h>
#include <flare/Utils.h>

using namespace osp;

Texture1D::Texture1D(std::vector<unsigned int> _dim) : Texture(_dim) {}

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

  //glEnable(GL_TEXTURE_1D);
  glGenTextures(1, &handle_);
  glBindTexture(GL_TEXTURE_1D, handle_);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_BASE_LEVEL, 0);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAX_LEVEL, 0);
  glTexImage1D(GL_TEXTURE_1D,
               0,                // level
               GL_RGBA,          // internal format
               dim_[0],          // width,
               0,                // border
               GL_RGBA,          // format
               GL_FLOAT,         // type
               _data);
  glBindTexture(GL_TEXTURE_1D, 0);
  initialized_ = true;
  return CheckGLError("Texture1D::Init()") == GL_NO_ERROR;
}

bool Texture1D::Bind(ShaderProgram * _shaderProgram,
                     std::string _uniformName,
                     unsigned int _texUnit) const {
  // TODO unused
  return false;
}
