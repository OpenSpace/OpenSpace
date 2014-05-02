/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#ifndef VOLUMERAYCASTERBOX_H_
#define VOLUMERAYCASTERBOX_H_

#include <ghoul/opengl/texture.h>

// forward declare private objects
namespace sgct_utils {
    class SGCTBox;
}
namespace ghoul {
    namespace opengl {
        class FramebufferObject;
        class ProgramObject;
    }
}

namespace openspace {

class VolumeRaycasterBox {
public:
	VolumeRaycasterBox();
	~VolumeRaycasterBox();
    bool initialize();
	void render(const glm::mat4& MVP);
    
	ghoul::opengl::Texture* backFace();
	ghoul::opengl::Texture* frontFace();
    glm::size2_t dimensions();

private:
	ghoul::opengl::FramebufferObject* _fbo;
	ghoul::opengl::Texture *_backTexture, *_frontTexture;
	ghoul::opengl::ProgramObject *_boxProgram;
	sgct_utils::SGCTBox* _boundingBox;
	GLint _MVPLocation;
    
    glm::size2_t _dimensions;
};

} // namespace openspace
#endif // VOLUMERAYCASTERBOX_H_
