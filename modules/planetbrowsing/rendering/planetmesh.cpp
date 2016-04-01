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

#include <modules/planetbrowsing/rendering/planetmesh.h>

// open space includes
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

// ghoul includes
#include <ghoul/misc/assert.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
	const std::string _loggerCat = "PlanetMesh";

	const std::string keyFrame = "Frame";
	const std::string keyGeometry = "Geometry";
	const std::string keyShading = "PerformShading";

	const std::string keyBody = "Body";
}

namespace openspace {

	PlanetMesh::PlanetMesh(const ghoul::Dictionary& dictionary)
		: DistanceSwitch()
		, _programObject(nullptr)
		, _grid(10, 10, Geometry::Positions::Yes, Geometry::TextureCoordinates::No, Geometry::Normals::No)
		, _rotation("rotation", "Rotation", 0, 0, 360)
	{
		std::string name;
		bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
		ghoul_assert(success,
			"PlanetMesh need the '" << SceneGraphNode::KeyName << "' be specified");
		setName(name);

		dictionary.getValue(keyFrame, _frame);
		dictionary.getValue(keyBody, _target);
		if (_target != "")
			setBody(_target);

		// Mainly for debugging purposes @AA
		addProperty(_rotation);

	}

	PlanetMesh::~PlanetMesh() {
	}

	bool PlanetMesh::initialize() {

		RenderEngine& renderEngine = OsEng.renderEngine();
		if (_programObject == nullptr) {
			// Night texture program
			_programObject = renderEngine.buildRenderProgram(
				"simpleTextureProgram",
				"${MODULE_PLANETBROWSING}/shaders/simple_vs.glsl",
				"${MODULE_PLANETBROWSING}/shaders/simple_fs.glsl");
			if (!_programObject) return false;
		}

		using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
		_programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);

		return isReady();
	}

	bool PlanetMesh::deinitialize() {

		RenderEngine& renderEngine = OsEng.renderEngine();
		if (_programObject) {
			renderEngine.removeRenderProgram(_programObject);
			_programObject = nullptr;
		}

		return true;
	}

	bool PlanetMesh::isReady() const {
		bool ready = true;
		ready &= (_programObject != nullptr);
		return ready;
	}

	void PlanetMesh::render(const RenderData& data)
	{
		// activate shader
		_programObject->activate();

		// scale the planet to appropriate size since the planet is a unit sphere. Ehm no?
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

		// setup the data to the shader
		//	_programObject->setUniform("camdir", camSpaceEye);
		_programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
		_programObject->setUniform("ModelTransform", transform);
		setPscUniforms(_programObject.get(), &data.camera, data.position);

		glEnable(GL_CULL_FACE);
		glCullFace(GL_BACK);

		// render
		// _testGeometry->drawUsingActiveProgram();
		_grid.drawUsingActiveProgram();

		// disable shader
		_programObject->deactivate();
	}

	void PlanetMesh::update(const UpdateData& data) {
		// set spice-orientation in accordance to timestamp
		_stateMatrix = SpiceManager::ref().positionTransformMatrix(_frame, "GALACTIC", data.time);
		_time = data.time;
	}

}  // namespace openspace
