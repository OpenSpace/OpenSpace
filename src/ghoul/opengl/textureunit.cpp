/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#include <ghoul/opengl/textureunit.h>

#include <ghoul/opengl/texture.h>
#include <ghoul/systemcapabilities/systemcapabilities.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>
#include <utility>

namespace ghoul::opengl {

TextureUnit::TextureUnitError::TextureUnitError(std::string msg)
    : RuntimeError(std::move(msg), "TextureUnit")
{}

bool TextureUnit::_isInitialized = false;
unsigned int TextureUnit::_totalActive = 0;
unsigned int TextureUnit::_maxTexUnits = 0;
std::vector<bool> TextureUnit::_busyUnits = std::vector<bool>();

TextureUnit::TextureUnit() {
    if (!_isInitialized) {
        initialize();
    }
}

TextureUnit::~TextureUnit() {
    unassign();
}

void TextureUnit::bind(GLuint texture) {
    if (!_assigned) {
        assignUnit();
    }

    glBindTextureUnit(_number, texture);
}

void TextureUnit::unassign() {
    if (_assigned) {
        _assigned = false;
        _busyUnits.at(_number) = false;

        ghoul_assert(_totalActive > 0, "No active texture units");
        --_totalActive;
    }
}

TextureUnit::operator GLint() {
    if (!_assigned) {
        assignUnit();
    }
    return _number;
}

void TextureUnit::assignUnit() {
    if (_totalActive >= _maxTexUnits) {
        throw TextureUnitError("No more texture units available");
    }

    _assigned = true;

    for (size_t i = 0; i < _maxTexUnits; i++) {
        if (!_busyUnits[i]) {
            _number = static_cast<GLint>(i);
            _glEnum = GL_TEXTURE0 + _number;
            _busyUnits[i] = true;
            _totalActive++;
            break;
        }
    }
}

void TextureUnit::initialize() {
    if (systemcapabilities::SystemCapabilities::isInitialized()) {
        _maxTexUnits = OpenGLCap.maxTextureUnits();
    }
    else {
        // OpenGL requires at least 16 texture units to exist
        _maxTexUnits = 16;
    }
    _busyUnits = std::vector<bool>(_maxTexUnits, false);
    _isInitialized = true;
}

} // namespace ghoul::opengl
