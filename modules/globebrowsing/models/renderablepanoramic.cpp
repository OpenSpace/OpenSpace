/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2017                                                               *
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


#include <modules/globebrowsing/models/renderablepanoramic.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>
#include <modules\globebrowsing\geometry\spheregeometry.h>

#include <openspace/scene/scenegraphnode.h>

#include <openspace/util/powerscaledsphere.h>
#include <glm/gtx/quaternion.hpp>
#include <glm/gtx/euler_angles.hpp>

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
	std::string _loggerCat = "RenderablePanoramic";
};

namespace openspace {
namespace globebrowsing {

RenderablePanoramic::RenderablePanoramic(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _texturePath("texture", "Texture")
	, _segments("segments", "Segments", 8, 4, 100) 
{

	std::string texturePath;
	if (dictionary.getValue("Texture", texturePath)) {
		_texturePath = absPath(texturePath);
	} 

	float size;
	if (dictionary.getValue("Size", size)) {
		_size = size;
	}

	if (dictionary.hasKeyAndValue<glm::vec2>("Segments")) {
		dictionary.getValue("Segments", _segments);
	}
	_texturePath.onChange(std::bind(&RenderablePanoramic::loadTexture, this));

}

bool RenderablePanoramic::isReady() const {
	return (_shader != nullptr) && (_sphere != nullptr) && (_texture != nullptr);
}
	
bool RenderablePanoramic::initialize() {
	//_sphere = new PowerScaledSphere(_size.value(), _segments);

	_sphere = new SphereGeometry(_size, _segments);
	_sphere->initialize();

	if (!_shader) {
		_shader = OsEng.renderEngine().buildRenderProgram("RenderablePanoramic", "${MODULE_GLOBEBROWSING}/shaders/renderablepanoramic_vs.glsl", "${MODULE_GLOBEBROWSING}/shaders/renderablepanoramic_fs.glsl");
		if (!_shader) return false;
	}

	loadTexture();

	return isReady();
}

bool RenderablePanoramic::deinitialize() {
	delete _sphere;
	_sphere = nullptr;

	_texture = nullptr;
	
	if (_shader) {
		OsEng.renderEngine().removeRenderProgram(_shader);
		_shader = nullptr;
	}
		
	return true;
}

void RenderablePanoramic::render(const RenderData& data) {

	if (!_shader) return;

	glm::dmat4 transform = glm::dmat4(1.0);

	_shader->activate();
	// Rotate sphere up to be the same as camera up
	glm::mat4 upRotation = glm::mat4(1.0);
	upRotation = glm::rotate(upRotation, static_cast<float>(M_PI_2), glm::vec3(0, 0, 1));

	// Quick fix for rotation around static 'up' for now
	glm::quat cameraQuat = data.camera.rotationQuaternion();
	double ysqr = cameraQuat.y * cameraQuat.y;
	double t3 = +2.0 * (cameraQuat.w * cameraQuat.z + cameraQuat.x * cameraQuat.y);
	double t4 = +1.0 - 2.0 * (ysqr + cameraQuat.z * cameraQuat.z);
	double yaw = std::atan2(t3, t4);

	glm::dmat4 unitMat = glm::dmat4(1.0);
	glm::dmat4 xRot = glm::rotate(unitMat, yaw, glm::dvec3(1,0,0));

	glm::dmat4 modelTransform = translate(transform, data.modelTransform.translation) *
		glm::dmat4(upRotation) * xRot;
	
	glm::vec2 res = OsEng.renderEngine().renderingResolution();
	GLfloat aspectRatio = static_cast<GLfloat>(res.x / res.y);	
	glm::mat4 projection = glm::perspective(glm::radians(90.0f), aspectRatio, 0.1f, 10000.0f);
	
	// If change FoV in single.xml or similar config file
	// _shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	// else
	_shader->setUniform("ViewProjection", projection * glm::mat4(data.camera.viewMatrix()));
	_shader->setUniform("ModelTransform", glm::mat4(modelTransform));

	ghoul::opengl::TextureUnit unit;
	unit.activate();

	_texture->bind();
	_shader->setUniform("texture1", unit);

	glEnable(GL_CULL_FACE);
	glCullFace(GL_BACK);
	
	_sphere->render();
	_shader->deactivate();
}

void RenderablePanoramic::update(const UpdateData& data) {
	
}

void RenderablePanoramic::loadTexture() {
	if (_texturePath.value() != "") {
		std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(_texturePath);
		if (texture) {
			LDEBUG("Loaded texture from '" << absPath(_texturePath) << "'");
			texture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			// TODO: AnisotropicMipMap crashes on ATI cards ---abock
			//texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
			texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

			_texture = std::move(texture);
		}
	}
}
}
}
