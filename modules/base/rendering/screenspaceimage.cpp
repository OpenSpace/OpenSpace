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
#include <openspace/util/camera.h>
#include <ghoul/logging/logmanager.h>

namespace openspace {
ScreenSpaceImage::ScreenSpaceImage(std::string texturePath)
		:ScreenSpaceRenderable()
		,_texturePath("texturePath", "Texture path", texturePath)
{
	addProperty(_texturePath);
		:ScreenSpaceRenderable(texturePath)
		, _vertexPath("${MODULE_BASE}/shaders/screnspace_vs.glsl")
		, _fragmentPath("${MODULE_BASE}/shaders/screnspace_fs.glsl")
		OsEng.gui()._property.registerProperty(&_useFlatScreen);

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
}

ScreenSpaceImage::~ScreenSpaceImage(){}


void ScreenSpaceImage::render(){
	glm::vec2 resolution = OsEng.windowWrapper().currentWindowResolution();
	//to scale the plane
	float textureRatio =  (float(_texture->height())/float(_texture->width()));

	//to keep the texture ratio after viewport is distorted.
	float scalingRatioX = resolution[0] / _originalViewportSize[0];
	float scalingRatioY = resolution[1] / _originalViewportSize[1];

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
	float occlusionDepth = 1-_depth.value();

	glEnable(GL_DEPTH_TEST);
    _shader->activate();
    _shader->setUniform("OcclusionDepth", occlusionDepth);
    _shader->setUniform("ModelTransform",modelTransform);
    _shader->setUniform("ViewProjectionMatrix", OsEng.renderEngine().camera()->viewProjectionMatrix());
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

        ghoul::Dictionary dict = ghoul::Dictionary();

        dict.setValue("rendererData", _rendererData);
        dict.setValue("fragmentPath", _fragmentPath);
		_shader = ghoul::opengl::ProgramObject::Build("ScreenSpaceProgram",
			_vertexPath,
			"${SHADERS}/render.frag",
			dict
			);

        if (!_shader)
            return false;
	}

	_originalViewportSize = OsEng.windowWrapper().currentWindowResolution();

	loadTexture();

	// Setting spherical/euclidean onchange handler
	_useFlatScreen.onChange([this](){
		useEuclideanCoordinates(_useFlatScreen.value());
	});
	return isReady();
}

bool ScreenSpaceImage::deinitialize(){
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


void ScreenSpaceImage::update(){
	
}

void ScreenSpaceImage::useEuclideanCoordinates(bool b){
	_useEuclideanCoordinates = b;
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
		}
	}
}

int ScreenSpaceImage::id(){
		static int id = 0;
		return id++;
}
}