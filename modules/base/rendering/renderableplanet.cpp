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
#include <modules/base/rendering/renderableplanet.h>

#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <modules/base/rendering/planetgeometry.h>
#include <openspace/util/constants.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    const std::string _loggerCat = "RenderablePlanet";

    const std::string keyFrame = "Frame";
    const std::string keyGeometry = "Geometry";
    const std::string keyShading = "PerformShading";

const std::string keyBody = "Body";
}

namespace openspace {

RenderablePlanet::RenderablePlanet(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
	, _colorTexturePath("colorTexture", "Color Texture")
    , _programObject(nullptr)
    , _texture(nullptr)
	, _nightTexture(nullptr)
    , _geometry(nullptr)
    , _performShading("performShading", "Perform Shading", true)
	, _rotation("rotation", "Rotation", 0, 0, 360)
	, _alpha(1.f)
    , _nightTexturePath("")
    , _hasNightTexture(false)
{
	std::string name;
	bool success = dictionary.getValue(constants::scenegraphnode::keyName, name);
	ghoul_assert(success,
            "RenderablePlanet need the '" <<constants::scenegraphnode::keyName<<"' be specified");

    //std::string path;
    //success = dictionary.getValue(constants::scenegraph::keyPathModule, path);
    //ghoul_assert(success,
    //        "RenderablePlanet need the '"<<constants::scenegraph::keyPathModule<<"' be specified");

    ghoul::Dictionary geometryDictionary;
    success = dictionary.getValue(keyGeometry, geometryDictionary);
	if (success) {
		geometryDictionary.setValue(constants::scenegraphnode::keyName, name);
        //geometryDictionary.setValue(constants::scenegraph::keyPathModule, path);
        _geometry = planetgeometry::PlanetGeometry::createFromDictionary(geometryDictionary);
	}

	dictionary.getValue(keyFrame, _frame);
	dictionary.getValue(keyBody, _target);
	if (_target != "")
		setBody(_target);

    // TODO: textures need to be replaced by a good system similar to the geometry as soon
    // as the requirements are fixed (ab)
    std::string texturePath = "";
	success = dictionary.getValue("Textures.Color", texturePath);
	if (success)
        _colorTexturePath = absPath(texturePath);

	std::string nightTexturePath = "";
	dictionary.getValue("Textures.Night", nightTexturePath);
	
	if (nightTexturePath != ""){
		_hasNightTexture = true;
		_nightTexturePath = absPath(nightTexturePath);
	}

	addPropertySubOwner(_geometry);

	addProperty(_colorTexturePath);
    _colorTexturePath.onChange(std::bind(&RenderablePlanet::loadTexture, this));

    if (dictionary.hasKeyAndValue<bool>(keyShading)) {
        bool shading;
        dictionary.getValue(keyShading, shading);
        _performShading = shading;
    }

    addProperty(_performShading);
	// Mainly for debugging purposes @AA
	addProperty(_rotation);
}

RenderablePlanet::~RenderablePlanet() {
}

bool RenderablePlanet::initialize() {
    if (_programObject == nullptr && _hasNightTexture)
		OsEng.ref().configurationManager()->getValue("nightTextureProgram", _programObject);
	else if (_programObject == nullptr)
		OsEng.ref().configurationManager()->getValue("pscShader", _programObject);

    loadTexture();
    _geometry->initialize(this);

    return isReady();
}

bool RenderablePlanet::deinitialize() {
    if(_geometry) {
        _geometry->deinitialize();
        delete _geometry;
    }
    if (_texture)
        delete _texture;
	if (_nightTexture)
		delete _nightTexture;

    _geometry = nullptr;
    _texture = nullptr;
	_nightTexture = nullptr;
    return true;
}

bool RenderablePlanet::isReady() const {
    bool ready = true;
    ready &= (_programObject != nullptr);
    ready &= (_texture != nullptr);
    ready &= (_geometry != nullptr);
	return ready;
}

void RenderablePlanet::render(const RenderData& data)
{
    // activate shader
    _programObject->activate();

    // scale the planet to appropriate size since the planet is a unit sphere
    glm::mat4 transform = glm::mat4(1);
	
	//earth needs to be rotated for that to work.
	glm::mat4 rot = glm::rotate(transform, 90.f, glm::vec3(1, 0, 0));
	glm::mat4 roty = glm::rotate(transform, 90.f, glm::vec3(0, -1, 0));
	glm::mat4 rotProp = glm::rotate(transform, static_cast<float>(_rotation), glm::vec3(0, 1, 0));

	for (int i = 0; i < 3; i++){
		for (int j = 0; j < 3; j++){
			transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
		}
	}
	transform = transform * rot * roty * rotProp;
	
	//glm::mat4 modelview = data.camera.viewMatrix()*data.camera.modelMatrix();
	//glm::vec3 camSpaceEye = (-(modelview*data.position.vec4())).xyz;

	
	double  lt;
    glm::dvec3 p =
    SpiceManager::ref().targetPosition("SUN", _target, "GALACTIC", {}, _time, lt);
    psc sun_pos = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);

    // setup the data to the shader
//	_programObject->setUniform("camdir", camSpaceEye);
	_programObject->setUniform("transparency", _alpha);
	_programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_programObject->setUniform("ModelTransform", transform);
	setPscUniforms(_programObject, &data.camera, data.position);
	
    _programObject->setUniform("_performShading", _performShading);

    // Bind texture
	ghoul::opengl::TextureUnit dayUnit;
	dayUnit.activate();
    _texture->bind();
	_programObject->setUniform("texture1", dayUnit);

	// Bind possible night texture
	if (_hasNightTexture) {
		ghoul::opengl::TextureUnit nightUnit;
		nightUnit.activate();
		_nightTexture->bind();
		_programObject->setUniform("nightTex", nightUnit);
	}
    // render
    _geometry->render();

    // disable shader
    _programObject->deactivate();
}

void RenderablePlanet::update(const UpdateData& data){
	// set spice-orientation in accordance to timestamp
    _stateMatrix = SpiceManager::ref().positionTransformMatrix(_frame, "GALACTIC", data.time);
	_time = data.time;
}

void RenderablePlanet::loadTexture() {
    delete _texture;
    _texture = nullptr;
	if (_colorTexturePath.value() != "") {
        _texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath));
        if (_texture) {
            LDEBUG("Loaded texture from '" << _colorTexturePath << "'");
			_texture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
            // TODO: AnisotropicMipMap crashes on ATI cards ---abock
            //_texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            _texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
        }
    }
	if (_hasNightTexture) {
		delete _nightTexture;
		_nightTexture = nullptr;
		if (_nightTexturePath != "") {
			_nightTexture = ghoul::io::TextureReader::ref().loadTexture(absPath(_nightTexturePath));
			if (_nightTexture) {
				LDEBUG("Loaded texture from '" << _nightTexturePath << "'");
				_nightTexture->uploadTexture();
                _nightTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
				//_nightTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
			}
		}
	}
}

}  // namespace openspace
