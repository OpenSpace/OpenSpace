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
#include <modules/base/rendering/screenspaceimage.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem>

namespace {
	const std::string _loggerCat = "ScreenSpaceImage";
}

namespace openspace {
ScreenSpaceImage::ScreenSpaceImage(std::string texturePath)
	:ScreenSpaceRenderable()
	,_texturePath("texturePath", "Texture path", texturePath)

{
	_id = id();
	setName("ScreenSpaceImage" + std::to_string(_id));
	
	registerProperties();

	addProperty(_texturePath);
	OsEng.gui()._screenSpaceProperty.registerProperty(&_texturePath);	
	_texturePath.onChange([this](){ loadTexture(); });
}

ScreenSpaceImage::~ScreenSpaceImage(){}

bool ScreenSpaceImage::initialize(){
	_originalViewportSize = OsEng.windowWrapper().currentWindowResolution();

	createPlane();
	createShaders();
	loadTexture();

	return isReady();
}

bool ScreenSpaceImage::deinitialize(){
	unregisterProperties();

	glDeleteVertexArrays(1, &_quad);
	_quad = 0;

	glDeleteBuffers(1, &_vertexPositionBuffer);
	_vertexPositionBuffer = 0;

	_texturePath = "";
	_texture = nullptr;

	 RenderEngine& renderEngine = OsEng.renderEngine();
	if (_shader) {
		renderEngine.removeRenderProgram(_shader);
		_shader = nullptr;
	}

	return true;
}

void ScreenSpaceImage::render(){
	glm::mat4 rotation = rotationMatrix();
	glm::mat4 translation = translationMatrix();
	glm::mat4 scale = scaleMatrix();
	glm::mat4 modelTransform = rotation*translation*scale;

	draw(modelTransform);
}

void ScreenSpaceImage::update(){
	// if(_toDelete)
	// 	OsEng.renderEngine().unregisterScreenSpaceRenderable(name());
}


bool ScreenSpaceImage::isReady() const{
	bool ready = true;
	if (!_shader)
		ready &= false;
	if(!_texture)
		ready &= false;
	return ready;
}

void ScreenSpaceImage::loadTexture() {
	if (_texturePath.value() != "") {
        std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath.value()));
		if (texture) {
			LDEBUG("Loaded texture from '" << absPath(_texturePath) << "'");
			texture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

            _texture = std::move(texture);
		}
	}
}

int ScreenSpaceImage::id(){
	static int id = 0;
	return id++;
}
}