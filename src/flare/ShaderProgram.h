#ifndef SHADERPROGRAM_H_
#define SHADERPROGRAM_H_


/* 
Author: Victor Sand (victor.sand@gmail.com)
Wrapper for GLSL shaders
*/

#include <vector>
#include <string>

namespace osp {

class Texture2D;

class ShaderProgram {
public:
  enum ShaderType {
    VERTEX,
    FRAGMENT
  };
  static ShaderProgram * New();
  // Create shader from source file
  bool CreateShader(ShaderType _type, std::string _fileName);
  // Create and link shader program using shaders in shaderHandles_
  bool CreateProgram();
  // Print the log of any shader
  bool PrintLog (unsigned int _handle) const;
  // Bind a 4x4 float matrix to the shader program
  bool BindMatrix4f(std::string _uniformName, float *_matrix);
  // Bind a float to the shader program
  bool BindFloat(std::string _uniformName, float _value);
  // Bind an integer to the shader program
  bool BindInt(std::string _uniformName, int _value);
  unsigned int Handle() const { return programHandle_; }
  // Get location for named attribute
  unsigned int GetAttribLocation(std::string _attrib) const;
  // Use the last used file name to re-create shader program
  bool Reload();
  // Detach and delete shaders
  bool DeleteShaders();
private:
  ShaderProgram();
  // Read a shader source text file, return char array
  char * ReadTextFile(std::string _fileName) const;
  bool vertexShaderSet_;
  bool fragmentShaderSet_;
  // Handles to shaders created by CreateShader are saved here
  std::vector<unsigned int> shaderHandles_;
  unsigned int programHandle_;
  // Keep track of last used source file names
  std::string vertexSource_;
  std::string fragmentSource_;
};

}

#endif
