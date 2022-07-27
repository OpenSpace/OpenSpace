#include "billboard.h"

#include <glbinding/gl/gl.h>
#include <glm/gtc/type_ptr.hpp>

using namespace gl;

constexpr const char* vertShader= R"(
#version 330 core
layout (location = 0) in vec3 aPos;
uniform mat4 uTransform;
out vec2 pos;

void main() {
  gl_Position = uTransform * vec4(aPos.x, aPos.y, aPos.z, 1.0);
  gl_Position /= gl_Position.w;
  gl_Position.z = 0.0; // always visible
  pos = aPos.xy;
}
)";

constexpr const char* fragShader = R"(
#version 330 core
out vec4 color;
in vec2 pos;
uniform float uStrokeWidth;
uniform float uFragDepth;
uniform vec4 uStrokeColor;
uniform vec4 uFillColor;

void main() {
  float len = length(pos) * 2.0;

  gl_FragDepth = uFragDepth;
  color = uStrokeColor;

  if (len < 1.0 - uStrokeWidth)
    color = uFillColor;
  
  else if (len > 1)
    discard;
}
)";

static const float vertices[] = {
     0.5f,  0.5f, 0.0f,  // top right
     0.5f, -0.5f, 0.0f,  // bottom right
    -0.5f,  0.5f, 0.0f,  // top left 

     0.5f, -0.5f, 0.0f,  // bottom right
    -0.5f, -0.5f, 0.0f,  // bottom left
    -0.5f,  0.5f, 0.0f,  // top left 
};


static GLuint prog = 0, vao, vbo;

void billboardGlInit() {
  if (prog)
    return;
  
  GLuint vs = glCreateShader(GL_VERTEX_SHADER);
  GLuint fs = glCreateShader(GL_FRAGMENT_SHADER);
  glShaderSource(vs, 1, &vertShader, nullptr);
  glCompileShader(vs);
  glShaderSource(fs, 1, &fragShader, nullptr);
  glCompileShader(fs);

  prog = glCreateProgram();
  glAttachShader(prog, vs);
  glAttachShader(prog, fs);
  glLinkProgram(prog);

  glDeleteShader(vs);
  glDeleteShader(fs); 

  glGenVertexArrays(1, &vao);
  glGenBuffers(1, &vbo);
  glBindVertexArray(vao);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);

  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
  
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), nullptr);
  glEnableVertexAttribArray(0);
}

void billboardGlDeinit() {
  if (prog) {
    glDeleteBuffers(1, &vao);
    glDeleteBuffers(1, &vbo);
    glDeleteProgram(prog);
  }
}

void billboardDraw(glm::mat4 const& transform, glm::vec4 const& fill, glm::vec4 const& stroke, float width, float depth) {
  glUseProgram(prog);
  glBindVertexArray(vao);
  glUniformMatrix4fv(glGetUniformLocation(prog, "uTransform"), 1, false, glm::value_ptr(transform));
  glUniform1f(glGetUniformLocation(prog, "uStrokeWidth"), width);
  glUniform1f(glGetUniformLocation(prog, "uFragDepth"), depth);
  glUniform4fv(glGetUniformLocation(prog, "uFillColor"), 1, glm::value_ptr(fill));
  glUniform4fv(glGetUniformLocation(prog, "uStrokeColor"), 1, glm::value_ptr(stroke));
  glDrawArrays(GL_TRIANGLES, 0, 6);
  glBindVertexArray(0);
  glUseProgram(0);
}
