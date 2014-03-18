/* 
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */
#include <GL/glew.h>
#include <flare/Texture2D.h>
#include <flare/ShaderProgram.h>
#include <flare/Utils.h>

using namespace osp;

Texture2D::Texture2D(std::vector<unsigned int> _dim) : Texture(_dim) {
}

Texture2D * Texture2D::New(std::vector<unsigned int> _dim) {
  if (_dim.size() != 2) {
    ERROR("Texture2D need a dimension vector of size 2, defaulting to 1x1");
    _dim = std::vector<unsigned int>(2, 1);
  }
  INFO("Creating Texture2D with dimensions: " << _dim[0] << " " << _dim[1]);
  return new Texture2D(_dim);
}

bool Texture2D::Init(float *_data) {
  INFO("Initializing Texture2D");

  if (initialized_) {
    WARNING("Texture2D already initialized, doing nothing");
    return true;
  }

//  glEnable(GL_TEXTURE_2D);
  glGenTextures(1, &handle_);
  glBindTexture(GL_TEXTURE_2D, handle_);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, 0);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);
  glTexImage2D(GL_TEXTURE_2D,           // target
               0,                       // level
               GL_RGBA,                 // internal format
               dim_[0],                 // width
               dim_[1],                 // height
               0,                       // border
               GL_RGBA,                 // format
               GL_FLOAT,                // type
               _data);                  // data

  glBindTexture(GL_TEXTURE_2D, 0);
  initialized_ = true;
  return CheckGLError("Texture2D::Init()") == GL_NO_ERROR;
}

bool Texture2D::Bind(ShaderProgram * _shaderProgram,
                     std::string _uniformName,
                     unsigned int _texUnit) const {
  if (!initialized_) {
    ERROR("Can't bind texture, not initialized");
    return false;
  }

  glGetError();

  glUseProgram(_shaderProgram->Handle());
  glActiveTexture(GL_TEXTURE0 + _texUnit);
  //glEnable(GL_TEXTURE_2D);

  int location = glGetUniformLocation(_shaderProgram->Handle(),
                                      _uniformName.c_str());
  if (location == -1) {
    ERROR("Uniform " << _uniformName << " could not be found");
    glUseProgram(0);
    return false;
  }

  glUniform1i(location, _texUnit);
  glBindTexture(GL_TEXTURE_2D, handle_);
  glUseProgram(0);
  return (CheckGLError("Texture2D::Bind() " + _uniformName) == GL_NO_ERROR);

}





