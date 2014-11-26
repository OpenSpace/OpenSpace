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

// open space includes
#include <openspace/rendering/planets/RenderablePlanetProjection.h>
#include <openspace/util/constants.h>
#include <openspace/rendering/planets/planetgeometryprojection.h>

#include <ghoul/opengl/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>

#include <openspace/engine/openspaceengine.h>
#include <sgct.h>
#include <iomanip> 



namespace {
const std::string _loggerCat = "RenderablePlanetProjection";
}

namespace openspace {

RenderablePlanetProjection::RenderablePlanetProjection(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
	, _colorTexturePath("colorTexture", "Color Texture")
	, _projectionTexturePath("colorTexture", "Color Texture")
    , _programObject(nullptr)
	, _writeToTextureProgramObject(nullptr)
    , _texture(nullptr)
	, _textureProj(nullptr)
    , _geometry(nullptr)
{
	std::string name;
	bool success = dictionary.getValue(constants::scenegraphnode::keyName, name);
	assert(success);

    std::string path;
    success = dictionary.getValue(constants::scenegraph::keyPathModule, path);
	assert(success);

    ghoul::Dictionary geometryDictionary;
    success = dictionary.getValue(
		constants::renderableplanet::keyGeometry, geometryDictionary);
	if (success) {
		geometryDictionary.setValue(constants::scenegraphnode::keyName, name);
        geometryDictionary.setValue(constants::scenegraph::keyPathModule, path);
		_geometry = planetgeometryprojection::PlanetGeometryProjection::createFromDictionary(geometryDictionary);
	}

	dictionary.getValue(constants::renderableplanet::keyFrame, _target);

    // TODO: textures need to be replaced by a good system similar to the geometry as soon
    // as the requirements are fixed (ab)
    std::string texturePath = "";
	success = dictionary.getValue("Textures.Color", texturePath);
	if (success){
		_colorTexturePath = path + "/" + texturePath; 
	}
	success = dictionary.getValue("Textures.Project", texturePath);
	if (success){
		_projectionTexturePath = path + "/" + texturePath;
	}
	addPropertySubOwner(_geometry);

	addProperty(_colorTexturePath);
	_colorTexturePath.onChange(std::bind(&RenderablePlanetProjection::loadTexture, this));
	addProperty(_projectionTexturePath);
	_projectionTexturePath.onChange(std::bind(&RenderablePlanetProjection::loadTexture, this));
}

RenderablePlanetProjection::~RenderablePlanetProjection(){
    deinitialize();
}

bool RenderablePlanetProjection::initialize(){
    bool completeSuccess = true;
    if (_programObject == nullptr)
        completeSuccess
              &= OsEng.ref().configurationManager().getValue("projectiveProgram", _programObject);
	if (_writeToTextureProgramObject == nullptr)
		completeSuccess
		&= OsEng.ref().configurationManager().getValue("writeToTextureProgram", _writeToTextureProgramObject);

    loadTexture();
    completeSuccess &= (_texture != nullptr);
	completeSuccess &= (_textureProj != nullptr);

    completeSuccess &= _geometry->initialize(this);

    return completeSuccess;
}

bool RenderablePlanetProjection::deinitialize(){
    _geometry->deinitialize();
    delete _geometry;
    _geometry = nullptr;
    delete _texture;
    _texture = nullptr;
	delete _textureProj;
	_textureProj = nullptr;
    return true;
}

void RenderablePlanetProjection::render(const RenderData& data)
{
	if (!_programObject) return;
	if (!_textureProj) return;

    // activate shader
    _programObject->activate();

    // scale the planet to appropriate size since the planet is a unit sphere
    glm::mat4 transform = glm::mat4(1);
	
	//earth needs to be rotated for that to work.
	glm::mat4 rot = glm::rotate(transform, 90.f, glm::vec3(1, 0, 0));
		
	for (int i = 0; i < 3; i++){
		for (int j = 0; j < 3; j++){
			transform[i][j] = _stateMatrix[i][j];
		}
	}
	transform = transform* rot;
	if (_target == "IAU_JUPITER"){ // tmp scale of jupiterx = 0.935126
		transform *= glm::scale(glm::mat4(1), glm::vec3(1, 0.935126, 1));
	}

	// PROJECTIVE TEXTURING----------------------------------------------------------
	// get fov
	std::string shape, instrument;
	std::vector<glm::dvec3> bounds;
	glm::dvec3 boresight;
	bool found = openspace::SpiceManager::ref().getFieldOfView("NH_LORRI", shape, instrument, boresight, bounds);
	if (!found) LERROR("Could not locate instrument");

	psc position;
	double lightTime = 0.0;
	SpiceManager::ref().getTargetPosition("NEW HORIZONS", "JUPITER BARYCENTER","GALACTIC", "NONE", _time, position, lightTime);
	position[3] += 3;
	glm::vec3 nh_pos = position.vec3();

	//get up-vecto
	//rotate boresight into correct alignment
	glm::vec3 bsight(_instrumentMatrix*boresight); // lookat must be vec3 
	glm::vec3 uptmp(_instrumentMatrix*glm::dvec3(data.camera.lookUpVector()));

	//create view matrix
	glm::vec3 e3 = glm::normalize(bsight);
	glm::vec3 e1 = glm::normalize(glm::cross(uptmp, e3));
	glm::vec3 e2 = glm::normalize(glm::cross(e3, e1));
	
	glm::mat4 projViewMatrix = glm::mat4(     e1.x,                  e2.x,                  e3.x,        0.f,
								              e1.y,                  e2.y,                  e3.y,        0.f,
								              e1.z,                  e2.z,                  e3.z,        0.f, 
								    -glm::dot(e1, nh_pos), -glm::dot(e2, nh_pos), -glm::dot(e3, nh_pos), 1.f); 
	//create perspective projection matrix
	glm::mat4 projProjectionMatrix = glm::perspective(0.2907f, 1.f, 0.2f, 1000000.0f);
	//bias matrix
	glm::mat4 projNormalizationMatrix = glm::mat4(0.5f, 0   , 0   , 0,
												  0   , 0.5f, 0   , 0,
												  0   , 0   , 0.5f, 0,
		                                          0.5f, 0.5f, 0.5f, 1 );

	glm::mat4 m = projNormalizationMatrix*projProjectionMatrix*projViewMatrix;
    // setup the data to the shader
	_programObject->setUniform("ProjectorMatrix", m);
	_programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_programObject->setUniform("ModelTransform", transform);
	_programObject->setAttribute("boresight", bsight);
	setPscUniforms(_programObject, &data.camera, data.position);
	
    // Bind texture
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _programObject->setUniform("texture1", unit); // jupiter

	ghoul::opengl::TextureUnit unit2;
	unit2.activate();
	_textureProj->bind();
	_programObject->setUniform("texture2", unit2); // proj

    // render
    _geometry->render();

    // disable shader
    _programObject->deactivate();

	/*
	fbo.activate();
	//glViewport(0, 0, 1024, 1024);
	_writeToTextureProgramObject->activate();
	GLfloat vertices[] = { -1, -1, 0,   // bottom left corner
						   -1,  1, 0,   // top left corner
						    1,  1, 0,   // top right corner
						    1, -1, 0 }; // bottom right corner
	 
	GLubyte indices[] = { 0, 1, 2,      // first triangle (bottom left - top left - top right)
		                0, 2, 3 };      // second triangle (bottom left - top right - bottom right)

	glVertexPointer(3, GL_FLOAT, 0, vertices);
	glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_BYTE, indices);

	_writeToTextureProgramObject->deactivate();
	fbo.deactivate();
	*/
}

void RenderablePlanetProjection::update(const UpdateData& data){
	// set spice-orientation in accordance to timestamp
	_time = data.time;
	openspace::SpiceManager::ref().getPositionTransformMatrix(_target, "GALACTIC", data.time, _stateMatrix);
	openspace::SpiceManager::ref().getPositionTransformMatrix("NH_LORRI", "GALACTIC", data.time, _instrumentMatrix);

}

void RenderablePlanetProjection::loadTexture()
{
    delete _texture;
    _texture = nullptr;
    if (_colorTexturePath.value() != "") {
        _texture = ghoul::opengl::loadTexture(absPath(_colorTexturePath));
        if (_texture) {
            LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
			_texture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			_texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
			fbo.activate();
			fbo.attachTexture(_texture, GL_COLOR_ATTACHMENT0, 0, 0);
			fbo.deactivate();
        }
    }


	delete _textureProj;
	_textureProj = nullptr;
	if (_colorTexturePath.value() != "") {
		_textureProj = ghoul::opengl::loadTexture(absPath(_projectionTexturePath));
		if (_textureProj) {
			LDEBUG("Loaded texture from '" << absPath(_projectionTexturePath) << "'");
			_textureProj->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			_textureProj->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
			_textureProj->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToBorder);

		}
	}
}

}  // namespace openspace
/*
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,GL_LINEAR);
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,GL_LINEAR);
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,GL_CLAMP_TO_BORDER);
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,GL_CLAMP_TO_BORDER);
*/