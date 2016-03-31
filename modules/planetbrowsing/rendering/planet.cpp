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
#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <modules/planetbrowsing/rendering/planet.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
	const std::string _loggerCat = "Planet";

	const std::string keyFrame = "Frame";
	const std::string keyGeometry = "Geometry";
	const std::string keyShading = "PerformShading";

	const std::string keyBody = "Body";
}

namespace openspace {

	Planet::Planet(const ghoul::Dictionary& dictionary)
		: Renderable(dictionary)
		, _colorTexturePath("colorTexture", "Color Texture")
		, _programObject(nullptr)
		, _texture(nullptr)
		, _nightTexture(nullptr)
		, _performShading("performShading", "Perform Shading", true)
		, _rotation("rotation", "Rotation", 0, 0, 360)
		, _alpha(1.f)
		, _nightTexturePath("")
		, _hasNightTexture(false)
	{
		std::string name;
		bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
		ghoul_assert(success,
			"RenderablePlanet need the '" << SceneGraphNode::KeyName << "' be specified");

		//std::string path;
		//success = dictionary.getValue(constants::scenegraph::keyPathModule, path);
		//ghoul_assert(success,
		//        "RenderablePlanet need the '"<<constants::scenegraph::keyPathModule<<"' be specified");

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

		if (nightTexturePath != "") {
			_hasNightTexture = true;
			_nightTexturePath = absPath(nightTexturePath);
		}

		addProperty(_colorTexturePath);
		_colorTexturePath.onChange(std::bind(&Planet::loadTexture, this));

		if (dictionary.hasKeyAndValue<bool>(keyShading)) {
			bool shading;
			dictionary.getValue(keyShading, shading);
			_performShading = shading;
		}

		addProperty(_performShading);
		// Mainly for debugging purposes @AA
		addProperty(_rotation);


		// Create a simple triangle for testing the geometry
		std::vector<unsigned int> triangleElements;
		std::vector<glm::vec4> trianglePositions;

		trianglePositions.push_back(glm::vec4(0, 0, 0, 1));
		trianglePositions.push_back(glm::vec4(10000000, 0, 0, 1));
		trianglePositions.push_back(glm::vec4(10000000, 10000000, 0, 1));

		triangleElements.push_back(0);
		triangleElements.push_back(1);
		triangleElements.push_back(2);

		_testGeometry = std::unique_ptr<Geometry>(new Geometry(
			triangleElements,
			Geometry::Positions::Yes,
			Geometry::Textures::No,
			Geometry::Normals::No));

		_testGeometry->setPositionData(trianglePositions);
		_testGeometry->initialize();

	}

	Planet::~Planet() {
	}

	bool Planet::initialize() {
		RenderEngine& renderEngine = OsEng.renderEngine();
		if (_programObject == nullptr && _hasNightTexture) {
			// Night texture program
			_programObject = renderEngine.buildRenderProgram(
				"simpleTextureProgram",
				"${MODULE_PLANETBROWSING}/shaders/simple_vs.glsl",
				"${MODULE_PLANETBROWSING}/shaders/simple_fs.glsl");
			if (!_programObject) return false;
		}
		else if (_programObject == nullptr) {
			// pscstandard
			_programObject = renderEngine.buildRenderProgram(
				"pscstandard",
				"${SHADERS}/pscstandard_vs.glsl",
				"${SHADERS}/pscstandard_fs.glsl");
			if (!_programObject) return false;

		}
		using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
		_programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);

		loadTexture();

		return isReady();
	}

	bool Planet::deinitialize() {
		RenderEngine& renderEngine = OsEng.renderEngine();
		if (_programObject) {
			renderEngine.removeRenderProgram(_programObject);
			_programObject = nullptr;
		}

		_texture = nullptr;
		_nightTexture = nullptr;
		return true;
	}

	bool Planet::isReady() const {
		bool ready = true;
		ready &= (_programObject != nullptr);
		ready &= (_texture != nullptr);
		return ready;
	}

	void Planet::render(const RenderData& data)
	{
		// activate shader
		_programObject->activate();

		// scale the planet to appropriate size since the planet is a unit sphere
		glm::mat4 transform = glm::mat4(1);

		//earth needs to be rotated for that to work.
		glm::mat4 rot = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(1, 0, 0));
		glm::mat4 roty = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(0, -1, 0));
		glm::mat4 rotProp = glm::rotate(transform, glm::radians(static_cast<float>(_rotation)), glm::vec3(0, 1, 0));

		for (int i = 0; i < 3; i++) {
			for (int j = 0; j < 3; j++) {
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
		setPscUniforms(_programObject.get(), &data.camera, data.position);

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

		glEnable(GL_CULL_FACE);
		glCullFace(GL_BACK);

		// render
		_testGeometry->render();

		// disable shader
		_programObject->deactivate();
	}

	void Planet::update(const UpdateData& data) {
		// set spice-orientation in accordance to timestamp
		_stateMatrix = SpiceManager::ref().positionTransformMatrix(_frame, "GALACTIC", data.time);
		_time = data.time;
	}

	void Planet::loadTexture() {
		_texture = nullptr;
		if (_colorTexturePath.value() != "") {
			_texture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath)));
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
			_nightTexture = nullptr;
			if (_nightTexturePath != "") {
				_nightTexture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_nightTexturePath)));
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
