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
		OsEng.gui()._property.registerProperty(&_euclideanPosition);
		OsEng.gui()._property.registerProperty(&_sphericalPosition);
		OsEng.gui()._property.registerProperty(&_depth);
		OsEng.gui()._property.registerProperty(&_scale);
		OsEng.gui()._property.registerProperty(&_texturePath);

		_texturePath.onChange([this](){ loadTexture(); });

		if(_useEuclideanCoordinates){
			_euclideanPosition.onChange([this](){
				_sphericalPosition.set(toSpherical(_euclideanPosition.value()));
			});
			_sphericalPosition.onChange([this](){});
		}else{
			_euclideanPosition.onChange([this](){
				_sphericalPosition.set(toSpherical(_euclideanPosition.value()));
			});
		}
		// _flatScreen.onChange([this](){ _useEuclideanCoordinates = _flatScreen.value(); });
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

	float occlusionDepth = 1-_depth.value();

	glm::mat4 modelTransform;
	if(!_useEuclideanCoordinates){
		glm::vec2 position = _sphericalPosition.value();
		float phi = position.y - M_PI/2.0;
		glm::mat4 rotation = glm::rotate(glm::mat4(1.0f),  position.x, glm::vec3(0.0f, 1.0f, 0.0f));
		rotation = glm::rotate(rotation, phi , glm::vec3(1.0f, 0.0f, 0.0f));
		glm::mat4 translate = glm::translate(glm::mat4(1.0f), glm::vec3(0.0f, 0.0f, _planeDepth));

		modelTransform = rotation * translate;
	} else {
		glm::vec2 position = _euclideanPosition.value();
		modelTransform = glm::translate(glm::mat4(1.f), glm::vec3(position, _planeDepth));
	}

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
	if(_flatScreen.value() != _useEuclideanCoordinates){
		_useEuclideanCoordinates = _flatScreen.value();

		if(_useEuclideanCoordinates){
			_euclideanPosition.set(toEuclidean(_sphericalPosition.value(), _radius));
			_euclideanPosition.onChange([this](){
				_sphericalPosition.set(toSpherical(_euclideanPosition.value()));
			});
			_sphericalPosition.onChange([this](){});
		} else {
			_sphericalPosition.set(toSpherical(_euclideanPosition.value()));
			_sphericalPosition.onChange([this](){
				_euclideanPosition.set(toEuclidean(_sphericalPosition.value(), _radius));
			});
			_euclideanPosition.onChange([this](){});
		}

	}
}

glm::vec2 ScreenSpaceImage::toEuclidean(glm::vec2 polar, float r){
	float x = r*sin(polar[0])*sin(polar[1]);
	float y = r*cos(polar[1]);
	float z = _planeDepth;
	return glm::vec2(x, y);
}

glm::vec2 ScreenSpaceImage::toSpherical(glm::vec2 euclidean){	
	_radius  = 		-sqrt(pow(euclidean[0],2)+pow(euclidean[1],2)+pow(_planeDepth,2));
	float theta	= 	atan2(-_planeDepth,euclidean[0])-M_PI/2.0;
	float phi = 	acos(euclidean[1]/_radius);

	while(phi>=M_PI){
		phi -= M_PI;
	}

	if(!_radius){
		phi = 0;
	}

	while(theta <= -M_PI){
		theta += 2.0*M_PI;
	}

	while(theta >= M_PI) {
		theta -= 2.0*M_PI;
	}

	// std::cout << euclidean[2] << " " << r 
	return glm::vec2(theta, phi);
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