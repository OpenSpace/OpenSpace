#pragma once

#include "gl.h"

#include <core/md_str.h>

namespace gl {

bool get_shader_compile_error(char* buffer, int max_length, GLuint shader);
bool get_program_link_error(char* buffer, int max_length, GLuint program);
GLuint compile_shader_from_source(str_t source, GLenum shader_type, str_t defines = {}, str_t base_include_dir = {});
GLuint compile_shader_from_file(str_t filename, GLenum shader_type, str_t defines = {});
bool attach_link_detach(GLuint program, const GLuint shaders[], int num_shaders);
bool attach_link_detach_with_transform_feedback(GLuint program, const GLuint shaders[], int num_shaders, const char* varyings[], int num_varyings, GLenum buffer_capture_mode);


bool init_texture_2D(GLuint* texture, int width, int height, GLenum format);
bool init_texture_3D(GLuint* texture, int width, int height, int depth, GLenum format);

bool free_texture(GLuint* texture);

// We assume you set the entire data for the texture
bool set_texture_2D_data(GLuint texture, const void* data, GLenum format);
bool set_texture_3D_data(GLuint texture, const void* data, GLenum format);


}  // namespace gl
