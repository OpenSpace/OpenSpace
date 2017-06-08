/*****************************************************************************************
*                                                                                       *
* GHOUL                                                                                 *
* General Helpful Open Utility Library                                                  *
*                                                                                       *
* Copyright (c) 2012-2017                                                               *
*                                                                                       *
* Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
* software and associated documentation files (the "Software"), to deal in the Software *
* without restriction, including without limitation the rights to use, copy, modify,    *
* merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
* permit persons to whom the Software is furnished to do so, subject to the following   *
* conditions:                                                                           *
*                                                                                       *
* The above copyright notice and this permission notice shall be included in all copies *
* or substantial portions of the Software.                                              *
*                                                                                       *
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
* INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
* PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
* HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
* CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
* OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
****************************************************************************************/

#include <modules/globebrowsing/opengl/texturearray.h>

#include <modules/globebrowsing/globebrowsingmodule.h>

#include <ghoul/misc/assert.h>

#include <ghoul/logging/logmanager.h>

namespace {
	const std::string _loggerCat = "TextureArray";
}

namespace openspace {
namespace globebrowsing {

	TextureArray::TextureArray(glm::uvec3 dimensions, int size, GLint format, 
				GLint internalFormat, GLenum dataType, bool allocateData)
		: _dimensions(std::move(dimensions)),
		_size(size),
		_internalFormat(internalFormat),
		_format(format),
		_dataType(dataType),
		_counter(0) {

		ghoul_assert(_dimensions.x >= 1, "Element of dimensions must be bigger or equal 1");
		ghoul_assert(_dimensions.y >= 1, "Element of dimensions must be bigger or equal 1");
		ghoul_assert(_dimensions.z >= 1, "Element of dimensions must be bigger or equal 1");

		initialize(allocateData);
	}

/*TextureArray::~TextureArray() {
	if (_id) {
		glDeleteTextures(1, &_id);
	}
}*/

void TextureArray::initialize(bool allocateData) {
	determineTextureType();
	generateId();
	if (allocateData) {
		allocateMemory();
	}
}

void TextureArray::setType(GLenum type) {
	ghoul_assert(
		(type == GL_TEXTURE_2D_ARRAY),
		"Type must be GL_TEXTURE_2D_ARRAY"
	);
	_type = type;
}



void TextureArray::determineTextureType() {
	if (_dimensions.y == 1) {
		_type = GL_TEXTURE_1D_ARRAY;
	}
	else {
		_type = GL_TEXTURE_2D_ARRAY;
	}
}

GLuint TextureArray::id() {
	return _id;
}

void TextureArray::allocateMemory() {
	bind();
	glTexStorage3D(_type, 1, _internalFormat, _dimensions.x, _dimensions.y, _size);
}

void TextureArray::generateId() {
	_id = 0;
	glGenTextures(1, &_id);
}

void TextureArray::bind() const {
	glBindTexture(_type, _id);
}

void TextureArray::uploadTexture(const void* pixels) {
	bind();
	switch (_type) {
	case GL_TEXTURE_2D:
		glTexImage2D(
			_type,
			0,
			_internalFormat,
			GLsizei(_dimensions.x),
			GLsizei(_dimensions.y),
			0,
			GLint(_format),
			_dataType,
			pixels
		);
		break;
	case GL_TEXTURE_2D_ARRAY: {
		glTexSubImage3D(
			_type,
			0,
			0,
			0,
			_counter,
			_dimensions.x,
			_dimensions.y,
			_dimensions.z,
			_format,
			_dataType,
			pixels);

		_counter++;
		break;
	}
	default:
		ghoul_assert(false, "Missing case label");
	}
}

}
}
