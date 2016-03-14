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
#include <glm/gtx/polar_coordinates.hpp>

namespace openspace {
ScreenSpaceImage::ScreenSpaceImage(std::string texturePath)
		:ScreenSpaceRenderable(texturePath)
	{
		_id = id();
		setName("ScreenSpaceImage" + std::to_string(_id));
		OsEng.gui()._property.registerProperty(&_enabled);
		OsEng.gui()._property.registerProperty(&_flatScreen);
		OsEng.gui()._property.registerProperty(&_position);
		OsEng.gui()._property.registerProperty(&_scale);
		OsEng.gui()._property.registerProperty(&_texturePath);

		_texturePath.onChange([this](){ loadTexture(); });
	}

ScreenSpaceImage::~ScreenSpaceImage(){}


void ScreenSpaceImage::render(Camera* camera){

	GLfloat m_viewport[4];
	glGetFloatv(GL_VIEWPORT, m_viewport);

	//to scale the plane
	float textureRatio =  (float(_texture->height())/float(_texture->width()));

	//to keep the texture ratio after viewport is distorted.
	float scalingRatioX = m_viewport[2] / _originalViewportSize[0];
	float scalingRatioY = m_viewport[3] / _originalViewportSize[1];

	glm::vec3 position = _position.value();
	float occlusionDepth = position.z;
	if(!_isFlatScreen){
		occlusionDepth = position.x;
		position.x = _planeDepth;
		position = toEuclidean(position);
	} else {
		position.z = _planeDepth;
	}

	glm::mat4 modelTransform = glm::translate(glm::mat4(1.f), position);
	modelTransform = glm::scale(modelTransform, glm::vec3(_scale.value()*scalingRatioY, _scale.value()*textureRatio*scalingRatioX, 1));

	glEnable(GL_DEPTH_TEST);
    _shader->activate();
    _shader->setUniform("OcclusionDepth", occlusionDepth);
    _shader->setUniform("ModelTransform",modelTransform);
    _shader->setUniform("ViewProjectionMatrix", camera->viewProjectionMatrix());
	ghoul::opengl::TextureUnit unit;
	unit.activate();
	_texture->bind();
	_shader->setUniform("texture1", unit);

	glBindVertexArray(_quad);
	glDrawArrays(GL_TRIANGLES, 0, 6);

	_shader->deactivate();
}

bool ScreenSpaceImage::initialize(){
	glGenVertexArrays(1, &_quad); // generate array
	glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
	createPlane();

	if(_shader == nullptr) {
    	RenderEngine& renderEngine = OsEng.renderEngine();
        _shader = renderEngine.buildRenderProgram("ScreenSpaceProgram",
            "${MODULE_BASE}/shaders/screnspace_vs.glsl",
            "${MODULE_BASE}/shaders/screnspace_fs.glsl"
            );
        if (!_shader)
            return false;
	}
	GLfloat m_viewport[4];
	glGetFloatv(GL_VIEWPORT, m_viewport);
	_originalViewportSize = glm::vec2(m_viewport[2], m_viewport[3]);
	loadTexture();

	return isReady();
}
bool ScreenSpaceImage::deinitialize(){
	return true;
}
void ScreenSpaceImage::update(){

	if(_flatScreen.value() != _isFlatScreen){
		_isFlatScreen = _flatScreen.value();
		glm::vec3 pos = _position.value();

		if(!_isFlatScreen){
			_position.set(toPolar(pos));
		} else {
			_position.set(toEuclidean(pos));
		}

	}
}

glm::vec3 ScreenSpaceImage::toEuclidean(glm::vec3 polar){
	float x = polar[0]*cos(polar[1])*sin(polar[2]);
	float y = polar[0]*sin(polar[1])*sin(polar[2]);
	float z = polar[0]*cos(polar[2]);
	return glm::vec3(x, y, z);
}

glm::vec3 ScreenSpaceImage::toPolar(glm::vec3 euclidean){
	float r = sqrt(pow(euclidean[0], 2) + pow(euclidean[1], 2) + pow(euclidean[2], 2));
	float theta= atan2(euclidean[1],euclidean[0]);
	float phi = acos(euclidean[2]/r);
	return glm::vec3(r, theta, phi);
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
			// LDEBUG("Loaded texture from '" << absPath(_texturePath) << "'");
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

int ScreenSpaceImage::id(){
		static int id = 0;
		return id++;
}
}