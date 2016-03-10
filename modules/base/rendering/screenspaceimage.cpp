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
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/openspaceengine.h>
#include <ghoul/opengl/textureunit.h>
#include <modules/onscreengui/include/gui.h>

namespace openspace {
	ScreenSpaceImage::ScreenSpaceImage()
		:ScreenSpaceRenderable()
	{
		setName("ScreenSpaceImage");
	}

	ScreenSpaceImage::~ScreenSpaceImage(){}


void ScreenSpaceImage::render(){
	glm::mat4 transform = glm::translate(glm::mat4(1.f), _position.value());
	transform = glm::scale(transform, glm::vec3(_scale.value()));

    // transform.translate(position.value());
    _shader->activate();

    _shader->setUniform("ModelTransform",transform);

	ghoul::opengl::TextureUnit unit;
	unit.activate();
	_texture->bind();
	_shader->setUniform("texture1", unit);

	glBindVertexArray(_quad);
	glDrawArrays(GL_TRIANGLES, 0, 6);

	_shader->deactivate();
}

bool ScreenSpaceImage::initialize(){
	OsEng.gui()._property.registerProperty(&_enabled);
	OsEng.gui()._property.registerProperty(&_flatScreen);

	glGenVertexArrays(1, &_quad); // generate array
	glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
	createPlane();

	_texturePath = "${OPENSPACE_DATA}/test.png";

	if(_shader == nullptr) {
    	RenderEngine& renderEngine = OsEng.renderEngine();
        _shader = renderEngine.buildRenderProgram("ScreenSpaceProgram",
            "${MODULE_BASE}/shaders/screnspace_vs.glsl",
            "${MODULE_BASE}/shaders/screnspace_fs.glsl"
            );
        if (!_shader)
            return false;
	}

	loadTexture();

	return isReady();
}
bool ScreenSpaceImage::deinitialize(){
	return true;
}
void ScreenSpaceImage::update(){
		
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
	if (_texturePath != "") {
        std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath));
		if (texture) {
			// LDEBUG("Loaded texture from '" << absPath(_texturePath) << "'");
			// std::cout<< std::endl << std::endl << "Loaded texture from '" << absPath(_texturePath) << "'" <<std::endl << std::endl;
			texture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

            _texture = std::move(texture);

            // delete _textureFile;
            // _textureFile = new ghoul::filesystem::File(_texturePath);
            // _textureFile->setCallback([&](const ghoul::filesystem::File&) { _textureIsDirty = true; });
		}
	}
}
}