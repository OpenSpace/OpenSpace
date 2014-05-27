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

#include <openspace/abuffer/abufferSingleLinked.h>

#include <sgct.h>

#define MAX_LAYERS 10

namespace openspace {

ABufferSingleLinked::ABufferSingleLinked() {
	int x1, xSize, y1, ySize;
    sgct::Engine::instance()->getActiveWindowPtr()->getCurrentViewportPixelCoords(x1, y1, xSize, ySize);
    width = xSize;
    height = ySize;
    totalPixels = width * height;
	maxFragments = totalPixels * MAX_LAYERS;
}

ABufferSingleLinked::~ABufferSingleLinked() {

}

bool ABufferSingleLinked::initialize() {
	data = new ghoul::opengl::Texture(glm::size3_t(width, height, 1), ghoul::opengl::Texture::Format::RED, GL_RED, GL_R32UI);
	return true;
}

void ABufferSingleLinked::clear() {

}

void ABufferSingleLinked::preRender() {

}

void ABufferSingleLinked::postRender() {

}

} // openspace