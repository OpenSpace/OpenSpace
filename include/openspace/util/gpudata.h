/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_CORE___GPUDATA___H__
#define __OPENSPACE_CORE___GPUDATA___H__

#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <string>
#include <memory>

// @TODO:  This class should disappear as it doesn't server much of a purpose and only
//         complicates local reasoning

namespace openspace {

/**
 * Manages a GPU representation of the templated data type T.
 * This class provides a simple interface setting the value of
 * the binded GLSL variable.
 */
template<typename T>
class GPUData {
public:
    /**
     * Updates the uniform location of the uniform variable named <name>
     * in the provided shader program.
     */
    void bind(ghoul::opengl::ProgramObject* program, const std::string& name) {
        _uniformLocation = program->uniformLocation(name);
    }

    /**
     * Sets the value of T to its corresponding GPU value.
     * OBS! Users must ensure bind has been called before using this method
     */
    void setValue(ghoul::opengl::ProgramObject* program, T val) {
        program->setUniform(_uniformLocation, val);
    }

protected:
    GLint _uniformLocation = -1;
};

/**
 * Manages a Texture on the GPU.
 * This class provides a simple interface binding texture to the
 * named uniform.
 */
class GPUTexture {
public:
    /**
     * Updates the uniform location of the uniform variable named <name>
     * in the provided shader program.
     */
    void bind(ghoul::opengl::ProgramObject* program, const std::string& name) {
        _uniformLocation = program->uniformLocation(name);
    }

    /**
     * Sets and assignes a texture unit within the provided shader
     * program.
     * OBS! Users must ensure bind has been called before using this method.
     */
    void setValue(ghoul::opengl::ProgramObject* program, ghoul::opengl::Texture* texture){
        _texUnit = std::make_unique<ghoul::opengl::TextureUnit>();
        _texUnit->activate();
        if (texture) {
            texture->bind();
        }
        program->setUniform(_uniformLocation, *_texUnit);
    }

    void deactivate() {
        _texUnit = nullptr;
    }

private:
    std::unique_ptr<ghoul::opengl::TextureUnit> _texUnit;
    GLint _uniformLocation = -1;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___GPUDATA___H__
