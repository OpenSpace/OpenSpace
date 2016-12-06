/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __GPUDATA_H__
#define __GPUDATA_H__

#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <string>
#include <memory>

namespace openspace {

using namespace ghoul::opengl;


/**
 * Very simple class maintaining a uniform location.
 */
class UniformLocation {
public:

    /**
    * Updates the uniform location of the uniform variable named <name> 
    * in the provided shader program.
    */
    void bind(ProgramObject* program, const std::string& name);
    
protected:
    GLint _uniformLocation = -1;
};

    
/**
 * Manages a GPU representation of the templated data type T.
 * This class provides a simple interface setting the value of 
 * the binded GLSL variable.
 */
template<typename T>
class GPUData : public UniformLocation{
public:
    
    /**
     * Sets the value of T to its corresponding GPU value.
     * OBS! Users must ensure bind has been called before using this method
     */
    void setValue(ProgramObject* program, T val){
        program->setUniform(_uniformLocation, val);
    }

};

/**
 * Manages a Texture on the GPU.
 * This class provides a simple interface binding texture to the 
 * named uniform.
 */
class GPUTexture : public UniformLocation{
public:

    /**
     * Sets and assignes a texture unit within the provided shader 
     * program.
     * OBS! Users must ensure bind has been called before using this method.
     */
    void setValue(ProgramObject* program, std::shared_ptr<Texture> texture){
        _texUnit = std::make_unique<TextureUnit>();
        _texUnit->activate();
        texture->bind();
        program->setUniform(_uniformLocation, *_texUnit);
    }

    void deactivate(){
        _texUnit = nullptr;
    }

private:

    std::unique_ptr<TextureUnit> _texUnit;

};

} // namespace openspace

#endif // __GPUDATA_H__
