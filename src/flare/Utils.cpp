/*
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */
#include <ghoul/opengl/ghoul_gl.h>
#include <flare/Utils.h>

unsigned int osp::CheckGLError(std::string _location) {
  unsigned int error = glGetError();
  switch (error) {
   case GL_NO_ERROR:
     break;
   case GL_INVALID_ENUM:
     ERROR(_location.c_str() << " GL_INVALID_ENUM");
     break;
   case GL_INVALID_VALUE:
     ERROR(_location.c_str() << " GL_INVALID_VALUE");
     break;
   case GL_INVALID_OPERATION:
     ERROR(_location.c_str() << " GL_INVALID_OPERATION");
     break;
   case GL_STACK_OVERFLOW:
     ERROR(_location.c_str() << " GL_STACK_OVERFLOW");
     break;
   case GL_STACK_UNDERFLOW:
     ERROR(_location.c_str() << " GL_STACK_UNDERFLOW");
     break;
   case GL_OUT_OF_MEMORY:
     ERROR(_location.c_str() << " GL_OUT_OF_MEMORY");
     break;
   case GL_INVALID_FRAMEBUFFER_OPERATION:
     ERROR(_location.c_str() << " GL_INVALID_FRAMEBUFFER_OPERATION");
     break;
   case GL_TABLE_TOO_LARGE:
     ERROR(_location.c_str() << " GL_TABLE_TOO_LARGE");
     break;
  }
  return error;
}
