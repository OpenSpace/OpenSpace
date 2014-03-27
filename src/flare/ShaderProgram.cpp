/* 
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */

// TODO abstraction of shader binder, maybe a templated ShaderBinder class?
// or string values service
// possibly make a common UniformType class to handle matrices, ints, floats
#include <ghoul/opengl/ghoul_gl.h>
#include <flare/Utils.h>
#include <flare/ShaderProgram.h>
#include <flare/Texture2D.h>
#include <algorithm>
#include <fstream>

using namespace osp;

ShaderProgram * ShaderProgram::New() {
  return new ShaderProgram();
}

ShaderProgram::ShaderProgram() 
  : vertexShaderSet_(false),
    fragmentShaderSet_(false),
    programHandle_(0),
    vertexSource_(""),
    fragmentSource_("") {
}


bool ShaderProgram::CreateShader(ShaderType _type, std::string _fileName) {
  // Handle different types
  // Save source filenames to enable reloading shaders
  std::string typeString;
  GLuint type;
  switch (_type) {
    case ShaderProgram::VERTEX:
      type = GL_VERTEX_SHADER;
      typeString = "vertex";
      vertexSource_ = _fileName;
      break;
    case ShaderProgram::FRAGMENT:
      type = GL_FRAGMENT_SHADER;
      typeString = "fragment";
      fragmentSource_ = _fileName;
      break;
    default:
      ERROR("Invalid shader type");
      return false;
  }

  INFO("Creating " << typeString << " shader from source file " << _fileName);

  // Create shader
  unsigned int handle = glCreateShader(type);
  if (glIsShader(handle) == GL_FALSE) {
    ERROR("Failed to create shader");
    CheckGLError("CreateShader()");
    return false;
  }

  // Read shader source
  char *source = ReadTextFile(_fileName);
  const char *constSource = source;
  glShaderSource(handle, 1, &constSource, NULL);
  glCompileShader(handle);
  int shaderCompiled;
  glGetShaderiv(handle, GL_COMPILE_STATUS, &shaderCompiled);

  if (shaderCompiled != GL_TRUE) {
    ERROR("Shader compilation failed");
    CheckGLError("CreateShader()");
    PrintLog(handle);
    return false;
  }

  // Save shader handle
  shaderHandles_.push_back(handle);
  free(source);
  if (_type == ShaderProgram::VERTEX) vertexShaderSet_ = true;
  else if (_type == ShaderProgram::FRAGMENT) fragmentShaderSet_ = true;
  return true;
}

bool ShaderProgram::CreateProgram() {
  INFO("Creating shader program");

  if (!vertexShaderSet_){
    ERROR("Can't create program, vertex shader not set");
    return false;
  }

  if (!fragmentShaderSet_) {
    ERROR("Can't create program, fragment shader not set");
    return false;
  }

  // Create new program
  programHandle_ = glCreateProgram();

  // Attach the shaders
  std::vector<unsigned int>::iterator it;
  for (it=shaderHandles_.begin(); it!=shaderHandles_.end(); it++)
  {
    glAttachShader(programHandle_, *it);
  }

  // Link the program
  INFO("Linking shader program");
  glLinkProgram(programHandle_);
  int programLinked;
  glGetProgramiv(programHandle_, GL_LINK_STATUS, &programLinked);
  if (programLinked != GL_TRUE) {
    ERROR("Program linking failed");
    CheckGLError("CreateProgram()");
    PrintLog(programHandle_);
    return false;
  }

  return true;
}

bool ShaderProgram::PrintLog(unsigned int _handle) const {
  int logLength, maxLength;

  // Handle shaders and programs differently
  if (glIsShader(_handle)) {
    glGetShaderiv(_handle, GL_INFO_LOG_LENGTH, &maxLength);
  } else {
    glGetProgramiv(_handle, GL_INFO_LOG_LENGTH, &maxLength);
  }

  std::vector<char> log(maxLength);
  if (glIsShader(_handle)) {
    glGetShaderInfoLog(_handle, maxLength, &logLength, (GLchar*)&log[0]);
  } else {
    glGetProgramInfoLog(_handle, maxLength, &logLength, (GLchar*)&log[0]);
  }

  if (logLength > 0) {
    DEBUG(&log[0]);
  } else {
    ERROR("Log length is not over zero");
    return false;
  }

  return true;
}

char * ShaderProgram::ReadTextFile(std::string _fileName) const 
{
  FILE * in;
  char * content = NULL;
  in = fopen(_fileName.c_str(), "r");
  if (in != NULL) {
    fseek(in, 0, SEEK_END);
    int count = ftell(in);
    rewind(in);
    content = (char *)malloc(sizeof(char)*(count+1));
    count = fread(content, sizeof(char), count, in);
    content[count] = '\0';
    fclose(in);
  } else {
    ERROR("Could not read file " + _fileName);
  }
  return content;
}

bool ShaderProgram::BindMatrix4f(std::string _uniform, float * _matrix) {
  glUseProgram(programHandle_);
  int location = glGetUniformLocation(programHandle_, _uniform.c_str());
  if (location == -1) {
    ERROR("Uniform " << _uniform << " could not be found");
    glUseProgram(0);
    return false;
  }
  glUniformMatrix4fv(location, 1, GL_FALSE, _matrix);
  glUseProgram(0);
  return CheckGLError("BindMatrix4f()") == GL_NO_ERROR;
}

// TODO see if glUseProgram must be called before glGetUniformLocation
bool ShaderProgram::BindFloat(std::string _uniform, float _value) {
  glUseProgram(programHandle_);
  int location = glGetUniformLocation(programHandle_, _uniform.c_str());
  if (location == -1) {
    ERROR("Uniform " << _uniform << " could not be found");
    glUseProgram(0);
    return false;
  }
  glUniform1f(location, _value);
  glUseProgram(0);
  return CheckGLError("BindFloat()") == GL_NO_ERROR;
}

bool ShaderProgram::BindInt(std::string _uniform, int _value) {
  glUseProgram(programHandle_);
  int location = glGetUniformLocation(programHandle_, _uniform.c_str());
  if (location == -1) {
    ERROR("Uniform " << _uniform << " could not be found");
    glUseProgram(0);
    return false;
  }
  glUniform1i(location, _value);
  glUseProgram(0);
  return CheckGLError("Bindintf()") == GL_NO_ERROR;
}

unsigned int ShaderProgram::GetAttribLocation(std::string _attrib) const {
  return glGetAttribLocation(programHandle_, _attrib.c_str());
}

bool ShaderProgram::Reload() {
  if (!vertexShaderSet_ || !fragmentShaderSet_) {
    ERROR("Can't reload, shaders not set");
    return false;
  }
  CreateShader(ShaderProgram::VERTEX, vertexSource_);
  CreateShader(ShaderProgram::FRAGMENT, fragmentSource_);
  CreateProgram();
  return true;
}

bool ShaderProgram::DeleteShaders() {
  if (!vertexShaderSet_ || !fragmentShaderSet_) {
    ERROR("Shader(s) not set, nothing to delete");
    return false;
  }
  for (unsigned int i=0; i<shaderHandles_.size(); i++) {
    glDetachShader(programHandle_, shaderHandles_[i]);
    glDeleteShader(shaderHandles_[i]);
  }
  shaderHandles_.clear();
  glDeleteProgram(programHandle_);
  return true;
}
