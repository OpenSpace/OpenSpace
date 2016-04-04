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

// open space includes
#include <modules/newhorizons/rendering/renderableplanetprojection.h>
#include <modules/newhorizons/rendering/planetgeometryprojection.h>

#include <openspace/engine/configurationmanager.h>

#include <ghoul/io/texture/texturereader.h>
//#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/scene/scenegraphnode.h>

#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>

#include <openspace/util/factorymanager.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/configurationmanager.h>
#include <openspace/rendering/renderengine.h>
#include <iomanip> 
#include <string>
#include <thread>      

#define _USE_MATH_DEFINES
#include <math.h>


namespace {
	const std::string _loggerCat = "RenderablePlanetProjection";
	const std::string keyProjObserver         = "Projection.Observer";
	const std::string keyProjTarget           = "Projection.Target";
	const std::string keyProjAberration       = "Projection.Aberration";
	const std::string keyInstrument           = "Instrument.Name";
	const std::string keyInstrumentFovy       = "Instrument.Fovy";
	const std::string keyInstrumentAspect     = "Instrument.Aspect";
	const std::string keyInstrumentNear       = "Instrument.Near";
	const std::string keyInstrumentFar        = "Instrument.Far";
	const std::string keySequenceDir          = "Projection.Sequence";
    const std::string keySequenceType         = "Projection.SequenceType";
    const std::string keyPotentialTargets     = "PotentialTargets";
	const std::string keyTranslation          = "DataInputTranslation";



	const std::string keyFrame = "Frame";
	const std::string keyGeometry = "Geometry";
	const std::string keyShading = "PerformShading";
	const std::string keyBody = "Body";
	const std::string _mainFrame = "GALACTIC";
    const std::string sequenceTypeImage = "image-sequence";
    const std::string sequenceTypePlaybook = "playbook";
	const std::string sequenceTypeHybrid = "hybrid";

}

namespace openspace {

//#define ORIGINAL_SEQUENCER

RenderablePlanetProjection::RenderablePlanetProjection(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
	, _colorTexturePath("planetTexture", "RGB Texture")
	, _projectionTexturePath("projectionTexture", "RGB Texture")
    , _rotation("rotation", "Rotation", 0, 0, 360)
	, _fadeProjection("fadeProjections", "Image Fading Factor", 0.f, 0.f, 1.f)
    , _performProjection("performProjection", "Perform Projections", true)
	, _clearAllProjections("clearAllProjections", "Clear Projections", false)
    , _programObject(nullptr)
	, _fboProgramObject(nullptr)
    , _texture(nullptr)
	, _textureOriginal(nullptr)
    , _textureProj(nullptr)
	, _textureWhiteSquare(nullptr)
    , _geometry(nullptr)
	, _capture(false)
	, _clearingImage(absPath("${OPENSPACE_DATA}/scene/common/textures/clear.png"))
{
	std::string name;
	bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
	ghoul_assert(success, "");

	_defaultProjImage = absPath("textures/defaultProj.png");

    ghoul::Dictionary geometryDictionary;
    success = dictionary.getValue(
		keyGeometry, geometryDictionary);
	if (success) {
		geometryDictionary.setValue(SceneGraphNode::KeyName, name);
		_geometry = planetgeometryprojection::PlanetGeometryProjection::createFromDictionary(geometryDictionary);
	}

	dictionary.getValue(keyFrame, _frame);
	dictionary.getValue(keyBody, _target);
	if (_target != "")
		setBody(_target);

    bool b1 = dictionary.getValue(keyInstrument, _instrumentID);
    bool b2 = dictionary.getValue(keyProjObserver, _projectorID);
    bool b3 = dictionary.getValue(keyProjTarget, _projecteeID);
    std::string a = "NONE";
    bool b4 = dictionary.getValue(keyProjAberration, a);
    _aberration = SpiceManager::AberrationCorrection(a);
    bool b5 = dictionary.getValue(keyInstrumentFovy, _fovy);        
    bool b6 = dictionary.getValue(keyInstrumentAspect, _aspectRatio); 
    bool b7 = dictionary.getValue(keyInstrumentNear, _nearPlane);
    bool b8 = dictionary.getValue(keyInstrumentFar, _farPlane);

    ghoul_assert(b1, "");
    ghoul_assert(b2, "");
    ghoul_assert(b3, "");
    ghoul_assert(b4, "");
    ghoul_assert(b5, "");
    ghoul_assert(b6, "");
    ghoul_assert(b7, "");
    ghoul_assert(b8, "");

    // @TODO copy-n-paste from renderablefov ---abock
    ghoul::Dictionary potentialTargets;
    success = dictionary.getValue(keyPotentialTargets, potentialTargets);
    ghoul_assert(success, "");

    _potentialTargets.resize(potentialTargets.size());
    for (int i = 0; i < potentialTargets.size(); ++i) {
        std::string target;
        potentialTargets.getValue(std::to_string(i + 1), target);
        _potentialTargets[i] = target;
    }

    // TODO: textures need to be replaced by a good system similar to the geometry as soon
    // as the requirements are fixed (ab)
    std::string texturePath = "";
	success = dictionary.getValue("Textures.Color", texturePath);
	if (success){
		_colorTexturePath = absPath(texturePath); 
	}
	success = dictionary.getValue("Textures.Project", texturePath);
	if (success){
		_projectionTexturePath = absPath(texturePath);
	}
	addPropertySubOwner(_geometry);
	addProperty(_rotation);
	addProperty(_fadeProjection);
	addProperty(_performProjection);
	addProperty(_clearAllProjections);


	addProperty(_colorTexturePath);
	_colorTexturePath.onChange(std::bind(&RenderablePlanetProjection::loadTexture, this));
	addProperty(_projectionTexturePath);
	_projectionTexturePath.onChange(std::bind(&RenderablePlanetProjection::loadProjectionTexture, this));

	SequenceParser* parser;

   // std::string sequenceSource;
	bool _foundSequence = dictionary.getValue(keySequenceDir, _sequenceSource);
	if (_foundSequence) {
        _sequenceSource = absPath(_sequenceSource);

		_foundSequence = dictionary.getValue(keySequenceType, _sequenceType);
		//Important: client must define translation-list in mod file IFF playbook
		if (dictionary.hasKey(keyTranslation)){
			ghoul::Dictionary translationDictionary;
			//get translation dictionary
			dictionary.getValue(keyTranslation, translationDictionary);

			if (_sequenceType == sequenceTypePlaybook) {
				parser = new HongKangParser(name,
                                            _sequenceSource,
											_projectorID,
											translationDictionary,
											_potentialTargets);
				openspace::ImageSequencer2::ref().runSequenceParser(parser);
			}
			else if (_sequenceType == sequenceTypeImage) {
				parser = new LabelParser(name,
                                         _sequenceSource,
                                         translationDictionary);
				openspace::ImageSequencer2::ref().runSequenceParser(parser);
			}
			else if (_sequenceType == sequenceTypeHybrid) {
				//first read labels
				parser = new LabelParser(name,
                                         _sequenceSource,
                                         translationDictionary);
				openspace::ImageSequencer2::ref().runSequenceParser(parser);

				std::string _eventFile;
				bool foundEventFile = dictionary.getValue("Projection.EventFile", _eventFile);
				if (foundEventFile){
					//then read playbook
					_eventFile = absPath(_eventFile);
					parser = new HongKangParser(name,
                                                _eventFile,
						                        _projectorID,
						                        translationDictionary,
						                        _potentialTargets);
					openspace::ImageSequencer2::ref().runSequenceParser(parser);
				}
				else{
					LWARNING("No eventfile has been provided, please check modfiles");
				}
			}
		}
		else{
			LWARNING("No playbook translation provided, please make sure all spice calls match playbook!");
		}
    }
}

RenderablePlanetProjection::~RenderablePlanetProjection() {
    deinitialize();
}

bool RenderablePlanetProjection::initialize() {
    bool completeSuccess = true;
    if (_programObject == nullptr) {
        // projection program

        RenderEngine& renderEngine = OsEng.renderEngine();
        _programObject = renderEngine.buildRenderProgram("projectiveProgram",
            "${MODULES}/newhorizons/shaders/projectiveTexture_vs.glsl",
            "${MODULES}/newhorizons/shaders/projectiveTexture_fs.glsl");

        if (!_programObject)
            return false;
    }

    _fboProgramObject = ghoul::opengl::ProgramObject::Build("fboPassProgram",
                                      "${SHADERS}/fboPass_vs.glsl",
                                      "${SHADERS}/fboPass_fs.glsl");
    
    loadTexture();
	loadProjectionTexture();
    completeSuccess &= (_texture != nullptr);
	completeSuccess &= (_textureOriginal != nullptr);
	completeSuccess &= (_textureProj != nullptr);
	completeSuccess &= (_textureWhiteSquare != nullptr);

    completeSuccess &= _geometry->initialize(this);

    if (completeSuccess)
	    completeSuccess &= auxiliaryRendertarget();

    return completeSuccess;
}

bool RenderablePlanetProjection::auxiliaryRendertarget(){
	bool completeSuccess = true;
	if (!_texture) return false;

	GLint defaultFBO;
	glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);

	// setup FBO
	glGenFramebuffers(1, &_fboID);
	glBindFramebuffer(GL_FRAMEBUFFER, _fboID);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, *_texture, 0);
	// check FBO status
	GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
	if (status != GL_FRAMEBUFFER_COMPLETE)
		completeSuccess &= false;
	// switch back to window-system-provided framebuffer
	glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);

	// SCREEN-QUAD 
	const GLfloat size = 1.0f;
	const GLfloat w = 1.0f;
	const GLfloat vertex_data[] = {
		-size, -size, 0.0f, w, 0, 1,
	 	 size,  size, 0.0f, w, 1, 0,
		-size,  size, 0.0f, w, 0, 0,
		-size, -size, 0.0f, w, 0, 1,
		 size, -size, 0.0f, w, 1, 1,
		 size,  size, 0.0f, w, 1, 0,
	};

	glGenVertexArrays(1, &_quad);                         // generate array
	glBindVertexArray(_quad);                             // bind array
	glGenBuffers(1, &_vertexPositionBuffer);              // generate buffer
	glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
	glEnableVertexAttribArray(3);
	glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
	glEnableVertexAttribArray(4);
	glVertexAttribPointer(4, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));


	return completeSuccess;
}

bool RenderablePlanetProjection::deinitialize(){
    _texture = nullptr;
	_textureProj = nullptr;
	_textureOriginal = nullptr;
	_textureWhiteSquare = nullptr;
	delete _geometry;
	_geometry = nullptr;

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_programObject) {
        renderEngine.removeRenderProgram(_programObject);
        _programObject = nullptr;
    }

    return true;
}
bool RenderablePlanetProjection::isReady() const {
	return _geometry && _programObject && _texture && _textureWhiteSquare;
}

void RenderablePlanetProjection::imageProjectGPU(){

    glDisable(GL_DEPTH_TEST);

	// keep handle to the current bound FBO
	GLint defaultFBO;
	glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);

	GLint m_viewport[4];
	glGetIntegerv(GL_VIEWPORT, m_viewport);
	//counter = 0;
	glBindFramebuffer(GL_FRAMEBUFFER, _fboID);
	// set blend eq
	glEnable(GL_BLEND);
	glBlendEquationSeparate(GL_FUNC_ADD, GL_FUNC_ADD);
	glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ZERO, GL_ZERO);

	glViewport(0, 0, static_cast<GLsizei>(_texture->width()), static_cast<GLsizei>(_texture->height()));
	_fboProgramObject->activate();

	ghoul::opengl::TextureUnit unitFbo;
	unitFbo.activate();
	_textureProj->bind();
	_fboProgramObject->setUniform("texture1"       , unitFbo);
		
	ghoul::opengl::TextureUnit unitFbo2;
	unitFbo2.activate();
	_textureOriginal->bind();
	_fboProgramObject->setUniform("texture2", unitFbo2);
	_fboProgramObject->setUniform("projectionFading", _fadeProjection);

	_fboProgramObject->setUniform("ProjectorMatrix", _projectorMatrix);
	_fboProgramObject->setUniform("ModelTransform" , _transform);
	_fboProgramObject->setUniform("_scaling"       , _camScaling);
	_fboProgramObject->setUniform("boresight"      , _boresight);

	if (_geometry->hasProperty("radius")){ 
        ghoul::any r = _geometry->property("radius")->get();
		if (glm::vec4* radius = ghoul::any_cast<glm::vec4>(&r)){
			_fboProgramObject->setUniform("radius", radius);
		}
	}else{
		LERROR("Geometry object needs to provide radius");
	}
	if (_geometry->hasProperty("segments")){
        ghoul::any s = _geometry->property("segments")->get();
		if (int* segments = ghoul::any_cast<int>(&s)){
			_fboProgramObject->setAttribute("segments", segments[0]);
		}
	}else{
		LERROR("Geometry object needs to provide segment count");
	}

	glBindVertexArray(_quad);
	glDrawArrays(GL_TRIANGLES, 0, 6);
	_fboProgramObject->deactivate();
	//glDisable(GL_BLEND);

	//bind back to default
	glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
	glViewport(m_viewport[0], m_viewport[1],
			    m_viewport[2], m_viewport[3]);

	glEnable(GL_DEPTH_TEST);
}

glm::mat4 RenderablePlanetProjection::computeProjectorMatrix(const glm::vec3 loc, glm::dvec3 aim, const glm::vec3 up){
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
	glm::mat4 projProjectionMatrix = glm::perspective(glm::radians(_fovy), _aspectRatio, _nearPlane, _farPlane);
	// bias matrix
	glm::mat4 projNormalizationMatrix = glm::mat4(0.5f, 0, 0, 0,
		                                          0, 0.5f, 0, 0,
		                                          0, 0, 0.5f, 0,
		                                          0.5f, 0.5f, 0.5f, 1);
	return projNormalizationMatrix*projProjectionMatrix*projViewMatrix;
}

void RenderablePlanetProjection::attitudeParameters(double time){
	// precomputations for shader
    _stateMatrix = SpiceManager::ref().positionTransformMatrix(_frame, _mainFrame, time);
    _instrumentMatrix = SpiceManager::ref().positionTransformMatrix(_instrumentID, _mainFrame, time);

	_transform = glm::mat4(1);
	//90 deg rotation w.r.t spice req. 
	glm::mat4 rot = glm::rotate(_transform, static_cast<float>(M_PI_2), glm::vec3(1, 0, 0));
	glm::mat4 roty = glm::rotate(_transform, static_cast<float>(M_PI_2), glm::vec3(0, -1, 0));
	glm::mat4 rotProp = glm::rotate(_transform, static_cast<float>(glm::radians(static_cast<float>(_rotation))), glm::vec3(0, 1, 0));

	for (int i = 0; i < 3; i++){
		for (int j = 0; j < 3; j++){
			_transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
		}
	}
	_transform = _transform * rot * roty * rotProp;

	glm::dvec3 bs;
    try {
        SpiceManager::FieldOfViewResult res = SpiceManager::ref().fieldOfView(_instrumentID);
        bs = std::move(res.boresightVector);
    }
    catch (const SpiceManager::SpiceException& e) {
        LERRORC(e.component, e.what());
        return;
    }

    glm::dvec3 p = SpiceManager::ref().targetPosition(_projectorID, _projecteeID, _mainFrame, _aberration, time, lightTime);
    psc position = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
   
	//change to KM and add psc camera scaling. 
	position[3] += (3 + _camScaling[1]);
    //position[3] += 3;
	glm::vec3 cpos = position.vec3();

	_projectorMatrix = computeProjectorMatrix(cpos, bs, _up);
}


void RenderablePlanetProjection::textureBind() {
	ghoul::opengl::TextureUnit unit[2];
	unit[0].activate();
	_texture->bind();
	_programObject->setUniform("texture1", unit[0]);
	unit[1].activate();
	_textureWhiteSquare->bind();
	_programObject->setUniform("texture2", unit[1]);
}

void RenderablePlanetProjection::project(){
	// If high dt -> results in GPU queue overflow
	// switching to using a simple queue to distribute
	// images 1 image / frame -> projections appear slower
	// but less viewable lagg for the sim overall.

	// Comment out if not using queue and prefer old method -------------
	// + in update() function
	//if (!imageQueue.empty()){
	//	Image& img = imageQueue.front();
	//	RenderablePlanetProjection::attitudeParameters(img.startTime);
	//	// if image has new path - ie actual image, NOT placeholder
	//	if (_projectionTexturePath.value() != img.path){
	//		// rebind and upload 
	//		_projectionTexturePath = img.path; 
	//	}
	//	imageProjectGPU(); // fbopass
	//	imageQueue.pop();
	//}
	// ------------------------------------------------------------------

	//---- Old method --- // 
	// @mm
	for (auto const &img : _imageTimes){
			RenderablePlanetProjection::attitudeParameters(img.startTime);
			if (_projectionTexturePath.value() != img.path){
				_projectionTexturePath = img.path; // path to current images
			}
			imageProjectGPU(); // fbopass
	}
	_capture = false;
}

void RenderablePlanetProjection::clearAllProjections(){
	float tmp = _fadeProjection;
	_fadeProjection = 1.f;
	_projectionTexturePath = _clearingImage;
	imageProjectGPU();
	_fadeProjection = tmp;
	_clearAllProjections = false;
}


void RenderablePlanetProjection::render(const RenderData& data){
	if (!_programObject) return;
	if (!_textureProj) return;
	
	if (_clearAllProjections) clearAllProjections();

	_camScaling = data.camera.scaling();
	_up = data.camera.lookUpVector();

	if (_capture && _performProjection)
		project();
    attitudeParameters(_time);
	_imageTimes.clear();

	double  lt;
    glm::dvec3 p =
    openspace::SpiceManager::ref().targetPosition("SUN", _projecteeID, "GALACTIC", {}, _time, lt);
    psc sun_pos = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);

	// Main renderpass
	_programObject->activate();
    // setup the data to the shader
	_programObject->setUniform("sun_pos", sun_pos.vec3());
	_programObject->setUniform("ProjectorMatrix", _projectorMatrix);
	_programObject->setUniform("ViewProjection" ,  data.camera.viewProjectionMatrix());
	_programObject->setUniform("ModelTransform" , _transform);
	_programObject->setUniform("boresight"    , _boresight);
	setPscUniforms(*_programObject.get(), data.camera, data.position);
	
	textureBind();
	
    // render geometry
    _geometry->render();
    // disable shader
    _programObject->deactivate();
}

void RenderablePlanetProjection::update(const UpdateData& data){
	if (_time >= Time::ref().currentTime()) {
        // if jump back in time -> empty queue.  
        imageQueue = std::queue<Image>();
	}

	_time = Time::ref().currentTime();
	_capture = false;

	if (openspace::ImageSequencer2::ref().isReady() && _performProjection){
		openspace::ImageSequencer2::ref().updateSequencer(_time);
		_capture = openspace::ImageSequencer2::ref().getImagePaths(_imageTimes, _projecteeID, _instrumentID);
    }

    if (_fboProgramObject && _fboProgramObject->isDirty()) {
        _fboProgramObject->rebuildFromFile();
    }

	// remove these lines if not using queue ------------------------
	// @mm
	//_capture = true;
	//for (auto img : _imageTimes){
	//	imageQueue.push(img);
	//}
	//_imageTimes.clear();
	// --------------------------------------------------------------

    if (_programObject->isDirty())
        _programObject->rebuildFromFile();
}

void RenderablePlanetProjection::loadProjectionTexture() {
	_textureProj = nullptr;
	if (_colorTexturePath.value() != "") {
        _textureProj = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_projectionTexturePath)));
		if (_textureProj) {
			_textureProj->uploadTexture(); 
            // TODO: AnisotropicMipMap crashes on ATI cards ---abock
            //_textureProj->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            _textureProj->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
			_textureProj->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToBorder);
		}
	}
}

void RenderablePlanetProjection::loadTexture() {
    _texture = nullptr;
    if (_colorTexturePath.value() != "") {
        _texture = std::move(ghoul::io::TextureReader::ref().loadTexture(_colorTexturePath));
        if (_texture) {
			_texture->uploadTexture();
			_texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
        }
    }
	_textureOriginal = nullptr;
	if (_colorTexturePath.value() != "") {
        _textureOriginal = std::move(ghoul::io::TextureReader::ref().loadTexture(_colorTexturePath));
		if (_textureOriginal) {
			_textureOriginal->uploadTexture();
			_textureOriginal->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
		}
	}
	_textureWhiteSquare = nullptr;
	if (_colorTexturePath.value() != "") {
        _textureWhiteSquare = std::move(ghoul::io::TextureReader::ref().loadTexture(_defaultProjImage));
		if (_textureWhiteSquare) {
			_textureWhiteSquare->uploadTexture();
			_textureWhiteSquare->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
		}
	}
}
}  // namespace openspace
