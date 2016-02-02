/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2015                                                               *
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

// open space includes
#include <modules/newhorizons/rendering/renderablemodelprojection.h>
#include <openspace/util/constants.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include "imgui.h"

#define _USE_MATH_DEFINES
#include <math.h>
#include <thread>

namespace {
	const std::string _loggerCat = "RenderableModelProjection";
	const std::string keySource = "Rotation.Source";
	const std::string keyDestination = "Rotation.Destination";
	const std::string keyBody = "Body";
	const std::string keyGeometry = "Geometry";

	const std::string keyTextureColor = "Textures.Color";
	const std::string keyTextureProject = "Textures.Project";
	const std::string keyTextureDefault = "Textures.Default";

	const std::string keySequenceDir = "Projection.Sequence";
	const std::string keySequenceType = "Projection.SequenceType";
	const std::string keyProjObserver = "Projection.Observer";
	const std::string keyProjTarget = "Projection.Target";
	const std::string keyProjAberration = "Projection.Aberration";

	const std::string keyInstrument = "Instrument.Name";
	const std::string keyInstrumentFovy = "Instrument.Fovy";
	const std::string keyInstrumentAspect = "Instrument.Aspect";
	const std::string keyInstrumentNear = "Instrument.Near";
	const std::string keyInstrumentFar = "Instrument.Far";

	const std::string keyTranslation = "DataInputTranslation";
	const std::string sequenceTypeImage = "image-sequence";

}

namespace openspace {

RenderableModelProjection::RenderableModelProjection(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _colorTexturePath("colorTexture", "Color Texture")
	, _projectionTexturePath("projectionTexture", "RGB Texture")
	, _rotationX("rotationX", "RotationX", 0, 0, 360)
	, _rotationY("rotationY", "RotationY", 0, 0, 360)
	, _rotationZ("rotationZ", "RotationZ", 0, 0, 360)
	, _programObject(nullptr)
	, _fboProgramObject(nullptr)
	, _texture(nullptr)
	, _geometry(nullptr)
	, _textureOriginal(nullptr)
	, _textureProj(nullptr)
	, _textureWhiteSquare(nullptr)
	, _alpha(1.f)
	, _performShading("performShading", "Perform Shading", true)
	, _performProjection("performProjection", "Perform Projections", true)
	, _frameCount(0)
	, _programIsDirty(false)
{
	std::string name;
	bool success = dictionary.getValue(constants::scenegraphnode::keyName, name);
	ghoul_assert(success, "Name was not passed to RenderableModelProjection");

	ghoul::Dictionary geometryDictionary;
	success = dictionary.getValue(keyGeometry, geometryDictionary);
	if (success) {
		geometryDictionary.setValue(constants::scenegraphnode::keyName, name);
		_geometry = modelgeometry::ModelGeometry::createFromDictionary(geometryDictionary);
	}

	std::string texturePath = "";
	success = dictionary.getValue(keyTextureColor, texturePath);
	if (success)
		_colorTexturePath = absPath(texturePath);
		
	success = dictionary.getValue(keyTextureProject, texturePath);
	if (success)
		_projectionTexturePath = absPath(texturePath);

	success = dictionary.getValue(keyTextureDefault, texturePath);
	if (success)
		_defaultProjImage = absPath(texturePath);

	addPropertySubOwner(_geometry);

	addProperty(_colorTexturePath);
	addProperty(_projectionTexturePath);
	_colorTexturePath.onChange(std::bind(&RenderableModelProjection::loadTexture, this));
	_projectionTexturePath.onChange(std::bind(&RenderableModelProjection::loadProjectionTexture, this));

	dictionary.getValue(keySource, _source);
	dictionary.getValue(keyDestination, _destination);
	dictionary.getValue(keyBody, _target);
	if (_target != "")
		setBody(_target);

	bool completeSuccess = true;
	completeSuccess &= dictionary.getValue(keyInstrument, _instrumentID);
	completeSuccess &= dictionary.getValue(keyProjObserver, _projectorID);
	completeSuccess &= dictionary.getValue(keyProjTarget, _projecteeID);
	completeSuccess &= dictionary.getValue(keyInstrumentFovy, _fovy);
	completeSuccess &= dictionary.getValue(keyInstrumentAspect, _aspectRatio);
	completeSuccess &= dictionary.getValue(keyInstrumentNear, _nearPlane);
	completeSuccess &= dictionary.getValue(keyInstrumentFar, _farPlane);
	ghoul_assert(completeSuccess, "All neccessary attributes not found in modfile");
		
	completeSuccess = dictionary.getValue(keyProjAberration, _aberration);
	if (!completeSuccess)
		_aberration = "NONE";

	openspace::SpiceManager::ref().addFrame(_target, _source);
	setBoundingSphere(pss(1.f, 9.f));

	addProperty(_performShading);
	addProperty(_performProjection);
	addProperty(_rotationX);
	addProperty(_rotationY);
	addProperty(_rotationZ);

	SequenceParser* parser;

	bool foundSequence = dictionary.getValue(keySequenceDir, _sequenceSource);
	if (foundSequence) {
		_sequenceSource = absPath(_sequenceSource);

		foundSequence = dictionary.getValue(keySequenceType, _sequenceType);
		//Important: client must define translation-list in mod file IFF playbook
		if (dictionary.hasKey(keyTranslation)) {
			ghoul::Dictionary translationDictionary;
			//get translation dictionary
			dictionary.getValue(keyTranslation, translationDictionary);
			if (_sequenceType == sequenceTypeImage) {
				parser = new LabelParser(name, _sequenceSource, translationDictionary);
				openspace::ImageSequencer2::ref().runSequenceParser(parser);

			}
		}
		else {
			LWARNING("No translation provided, please make sure all spice calls match playbook!");
		}
	}

}

bool RenderableModelProjection::isReady() const {
	bool ready = true;
	ready &= (_programObject != nullptr);
	ready &= (_texture != nullptr);
	return ready;
}

bool RenderableModelProjection::initialize() {
	bool completeSuccess = true;
		
	if (_programObject == nullptr) {
        RenderEngine* renderEngine = OsEng.renderEngine();
        _programObject = renderEngine->buildRenderProgram("ModelShader",
            "${MODULES}/newhorizons/shaders/modelShader_vs.glsl",
            "${MODULES}/newhorizons/shaders/modelShader_fs.glsl");

		if (!_programObject)
			return false;
	}
	_programObject->setProgramObjectCallback([&](ghoul::opengl::ProgramObject*) { this->_programIsDirty = true; } );

	if (_fboProgramObject == nullptr) {
		_fboProgramObject = ghoul::opengl::ProgramObject::Build("ProjectionPass",
			"${MODULES}/newhorizons/shaders/projectionPass_vs.glsl",
			"${MODULES}/newhorizons/shaders/projectionPass_fs.glsl");
		if (!_fboProgramObject)
			return false;
	}
	_fboProgramObject->setProgramObjectCallback([&](ghoul::opengl::ProgramObject*) { this->_programIsDirty = true; } );

	loadTexture();
	loadProjectionTexture();

	completeSuccess &= (_texture != nullptr);
	completeSuccess &= (_textureOriginal != nullptr);
	completeSuccess &= (_textureProj != nullptr);
	completeSuccess &= (_textureWhiteSquare != nullptr);

	completeSuccess &= _geometry->initialize(this);
	completeSuccess &= !_source.empty();
	completeSuccess &= !_destination.empty();
		

	bool gotverts = _geometry->getVertices(&_geometryVertecies) && _geometry->getIndices(&_geometryIndeces);
	if (!gotverts)
		LWARNING("Lack of vertex data from geometry for image projection");

	completeSuccess &= auxiliaryRendertarget();

	return completeSuccess;
}

bool RenderableModelProjection::auxiliaryRendertarget() {
	bool completeSuccess = true;
	// set FBO to texture to project to
	glGenFramebuffers(1, &_fboID);
	glBindFramebuffer(GL_FRAMEBUFFER, _fboID);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, *_texture, 0);
	// check FBO status
	GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
	if (status != GL_FRAMEBUFFER_COMPLETE)
		completeSuccess &= false;
	// switch back to window-system-provided framebuffer
	glBindFramebuffer(GL_FRAMEBUFFER, 0);

	int vertexSize = sizeof(modelgeometry::ModelGeometry::Vertex);

	glGenVertexArrays(1, &_vaoID);
	glGenBuffers(1, &_vbo);
	glGenBuffers(1, &_ibo);

	glBindVertexArray(_vaoID);
	glBindBuffer(GL_ARRAY_BUFFER, _vbo);
	glBufferData(GL_ARRAY_BUFFER, _geometryVertecies.size() * vertexSize, &_geometryVertecies[0], GL_STATIC_DRAW);

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glEnableVertexAttribArray(2);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, vertexSize,
		reinterpret_cast<const GLvoid*>(offsetof(modelgeometry::ModelGeometry::Vertex, location)));
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, vertexSize,
		reinterpret_cast<const GLvoid*>(offsetof(modelgeometry::ModelGeometry::Vertex, tex)));
	glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, vertexSize,
		reinterpret_cast<const GLvoid*>(offsetof(modelgeometry::ModelGeometry::Vertex, normal)));
			
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _ibo);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, _geometryIndeces.size() * sizeof(int), &_geometryIndeces[0], GL_STATIC_DRAW);

	glBindVertexArray(0);


	return completeSuccess;
}

bool RenderableModelProjection::deinitialize() {
	if (_geometry) {
		_geometry->deinitialize();
		delete _geometry;
	}

	if (_texture)
		delete _texture;
	if (_textureProj) 
		delete _textureProj;
	if (_textureOriginal)
		delete _textureOriginal;
	if (_textureWhiteSquare)
		delete _textureWhiteSquare;

	_geometry = nullptr;
	_texture = nullptr;
	_textureProj = nullptr;
	_textureOriginal = nullptr;
	_textureWhiteSquare = nullptr;

	glDeleteBuffers(1, &_vbo);

    RenderEngine* renderEngine = OsEng.renderEngine();
    if (_programObject) {
        renderEngine->removeRenderProgram(_programObject);
        _programObject = nullptr;
    }

	return true;
}

void RenderableModelProjection::render(const RenderData& data) {
	if (!_programObject) return;
	if (!_textureProj) return;
	_programObject->activate();
	_frameCount++;

	_camScaling = data.camera.scaling();
	_up = data.camera.lookUpVector();

	if (_capture && _performProjection)
		project();

	attitudeParameters(_time);
	_imageTimes.clear();
		
	double time = openspace::Time::ref().currentTime();
	bool targetPositionCoverage = openspace::SpiceManager::ref().hasSpkCoverage(_target, time);
	if (!targetPositionCoverage) {
		int frame = _frameCount % 180;

		float fadingFactor = static_cast<float>(sin((frame * M_PI) / 180));
		_alpha = 0.5f + fadingFactor * 0.5f;
	}
	else
		_alpha = 1.0f;
		
	_programObject->setUniform("ProjectorMatrix", _projectorMatrix);
	_programObject->setUniform("boresight", _boresight);
	_programObject->setUniform("_performShading", _performShading);
	_programObject->setUniform("sun_pos", _sunPosition.vec3());
	_viewProjection = data.camera.viewProjectionMatrix();
	_programObject->setUniform("ViewProjection", _viewProjection);
	_programObject->setUniform("ModelTransform", _transform);
	setPscUniforms(_programObject, &data.camera, data.position);
	
	textureBind();
	_geometry->render();
		
	// disable shader
	_programObject->deactivate();
}

void RenderableModelProjection::update(const UpdateData& data) {
	if (_programIsDirty) {
		_programObject->rebuildFromFile();
		_fboProgramObject->rebuildFromFile();
		_programIsDirty = false;
	}
		
	_time = data.time;

	if (openspace::ImageSequencer2::ref().isReady() && _performProjection) {
		openspace::ImageSequencer2::ref().updateSequencer(_time);
		_capture = openspace::ImageSequencer2::ref().getImagePaths(_imageTimes, _projecteeID, _instrumentID);
	}
		
	// set spice-orientation in accordance to timestamp
	if (!_source.empty())
		openspace::SpiceManager::ref().getPositionTransformMatrix(_source, _destination, _time, _stateMatrix);

	double  lt;
	openspace::SpiceManager::ref().getTargetPosition("SUN", _target, "GALACTIC", "NONE", _time, _sunPosition, lt);
}

void RenderableModelProjection::imageProjectGPU() {
		
	// keep handle to the current bound FBO
	GLint defaultFBO;
	glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);
	
	GLint m_viewport[4];
	glGetIntegerv(GL_VIEWPORT, m_viewport);	
	glBindFramebuffer(GL_FRAMEBUFFER, _fboID);
	// set blend eq
	glEnable(GL_BLEND);
	glBlendEquationSeparate(GL_FUNC_ADD, GL_FUNC_ADD);
	glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ZERO, GL_ZERO);

	glViewport(0, 0, static_cast<GLsizei>(_texture->width()), static_cast<GLsizei>(_texture->height()));
	_fboProgramObject->activate();

	ghoul::opengl::TextureUnit unitFboProject;
	unitFboProject.activate();
	_textureProj->bind();
	_fboProgramObject->setUniform("projectTexture", unitFboProject);

	ghoul::opengl::TextureUnit unitFboCurrent;
	unitFboCurrent.activate();
	_texture->bind();
	_fboProgramObject->setUniform("currentTexture", unitFboCurrent);
	_fboProgramObject->setUniform("ProjectorMatrix", _projectorMatrix);
	_fboProgramObject->setUniform("ModelTransform", _transform);
	_fboProgramObject->setUniform("_scaling", _camScaling);
	_fboProgramObject->setUniform("boresight", _boresight);

	glBindVertexArray(_vaoID);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _ibo);
	glDrawElements(GL_TRIANGLES, static_cast<GLsizei>(_geometryIndeces.size()), GL_UNSIGNED_INT, 0);
	glBindVertexArray(0);
	
	_fboProgramObject->deactivate();
	glDisable(GL_BLEND);
	//bind back to default
	glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
	glViewport(m_viewport[0], m_viewport[1],
		m_viewport[2], m_viewport[3]);
			
}

void RenderableModelProjection::attitudeParameters(double time) {
	openspace::SpiceManager::ref().getPositionTransformMatrix(_source, _destination, time, _stateMatrix);
	openspace::SpiceManager::ref().getPositionTransformMatrix(_instrumentID, _destination, time, _instrumentMatrix);

	_transform = glm::mat4(1);

	glm::mat4 rotPropX = glm::rotate(_transform, static_cast<float>(_rotationX), glm::vec3(1, 0, 0));
	glm::mat4 rotPropY = glm::rotate(_transform, static_cast<float>(_rotationY), glm::vec3(0, 1, 0));
	glm::mat4 rotPropZ = glm::rotate(_transform, static_cast<float>(_rotationZ), glm::vec3(0, 0, 1));
		
	for (int i = 0; i < 3; i++) {
		for (int j = 0; j < 3; j++) {
			_transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
		}
	}
	_transform = _transform * rotPropX * rotPropY * rotPropZ;

	std::string shape, instrument;
	std::vector<glm::dvec3> bounds;
	glm::dvec3 boresight;
	bool found = openspace::SpiceManager::ref().getFieldOfView(_instrumentID, shape, instrument, boresight, bounds);
	if (!found)
		return;

	double lightTime;
	psc position;                                //observer      target
	found = SpiceManager::ref().getTargetPosition(_projectorID, _projecteeID, _destination, _aberration, time, position, lightTime);
 
	position[3] += (3 + _camScaling[1]);
	glm::vec3 cpos = position.vec3();

	_projectorMatrix = computeProjectorMatrix(cpos, boresight, _up);
}

glm::mat4 RenderableModelProjection::computeProjectorMatrix(const glm::vec3 loc, glm::dvec3 aim, const glm::vec3 up) {
	//rotate boresight into correct alignment
	_boresight = _instrumentMatrix*aim;
	glm::vec3 uptmp(_instrumentMatrix*glm::dvec3(up));

	// create view matrix
	glm::vec3 e3 = glm::normalize(_boresight);
	glm::vec3 e1 = glm::normalize(glm::cross(uptmp, e3));
	glm::vec3 e2 = glm::normalize(glm::cross(e3, e1));
	glm::mat4 projViewMatrix = glm::mat4(e1.x, e2.x, e3.x, 0.f,
											e1.y, e2.y, e3.y, 0.f,
											e1.z, e2.z, e3.z, 0.f,
											-glm::dot(e1, loc), -glm::dot(e2, loc), -glm::dot(e3, loc), 1.f);
		
	// create perspective projection matrix
	glm::mat4 projProjectionMatrix = glm::perspective(_fovy, _aspectRatio, _nearPlane, _farPlane);
	// bias matrix
	glm::mat4 projNormalizationMatrix = glm::mat4(0.5f, 0, 0, 0,
													0, 0.5f, 0, 0,
													0, 0, 0.5f, 0,
													0.5f, 0.5f, 0.5f, 1);
	return projNormalizationMatrix*projProjectionMatrix*projViewMatrix;
}


void RenderableModelProjection::textureBind() {
	ghoul::opengl::TextureUnit unit[2];
	unit[0].activate();
	_texture->bind();
	_programObject->setUniform("currentTexture", unit[0]);
	unit[1].activate();
	_textureWhiteSquare->bind();
	_programObject->setUniform("projectedTexture", unit[1]);
}

void RenderableModelProjection::project() {
	for (auto img : _imageTimes) {
		std::thread t1(&RenderableModelProjection::attitudeParameters, this, img.startTime);
		t1.join();
		_projectionTexturePath = img.path;
		imageProjectGPU(); //fbopass
	}
	_capture = false;
}

void RenderableModelProjection::loadTexture() {
	delete _texture;
	_texture = nullptr;
	if (_colorTexturePath.value() != "") {
		_texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath));
		if (_texture) {
			LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
			_texture->uploadTexture();
			_texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
		}
	}
	delete _textureOriginal;
	_textureOriginal = nullptr;
	if (_colorTexturePath.value() != "") {
		_textureOriginal = ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath));
		if (_textureOriginal) {
			LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
			_textureOriginal->uploadTexture();
			_textureOriginal->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
		}
	}
	delete _textureWhiteSquare;
	_textureWhiteSquare = nullptr;
	if (_defaultProjImage != "") {
		_textureWhiteSquare = ghoul::io::TextureReader::ref().loadTexture(absPath(_defaultProjImage));
		if (_textureWhiteSquare) {
			_textureWhiteSquare->uploadTexture();
			_textureWhiteSquare->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
		}
	}
}

void RenderableModelProjection::loadProjectionTexture() {
	delete _textureProj;
	_textureProj = nullptr;
	if (_projectionTexturePath.value() != "") {
		_textureProj = ghoul::io::TextureReader::ref().loadTexture(absPath(_projectionTexturePath));
		if (_textureProj) {
			_textureProj->uploadTexture();
			_textureProj->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
			_textureProj->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToBorder);
		}
	}

}

}  // namespace openspace
