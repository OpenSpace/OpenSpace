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

#include <modules/globebrowsing/globes/globemesh.h>

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
	const std::string _loggerCat = "GlobeMesh";
}

namespace openspace {
	GlobeMesh::GlobeMesh()
		: _programObject(nullptr)
		, _grid(10, 10, TriangleSoup::Positions::Yes, TriangleSoup::TextureCoordinates::No, TriangleSoup::Normals::No)
	{
		
	}

	GlobeMesh::~GlobeMesh() {
	}

	bool GlobeMesh::initialize() {

		RenderEngine& renderEngine = OsEng.renderEngine();
		if (_programObject == nullptr) {
			_programObject = renderEngine.buildRenderProgram(
				"simpleTextureProgram",
				"${MODULE_GLOBEBROWSING}/shaders/simple_vs.glsl",
				"${MODULE_GLOBEBROWSING}/shaders/simple_fs.glsl");
			if (!_programObject) return false;
		}

		using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
		_programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);

		return isReady();
	}

	bool GlobeMesh::deinitialize() {

		RenderEngine& renderEngine = OsEng.renderEngine();
		if (_programObject) {
			renderEngine.removeRenderProgram(_programObject);
			_programObject = nullptr;
		}

		return true;
	}

	bool GlobeMesh::isReady() const {
		bool ready = true;
		ready &= (_programObject != nullptr);
		return ready;
	}

	void GlobeMesh::render(const RenderData& data)
	{
		// activate shader
		_programObject->activate();

		// scale the planet to appropriate size since the planet is a unit sphere. Ehm no?
		glm::mat4 transform = glm::mat4(1);

		// setup the data to the shader
		//	_programObject->setUniform("camdir", camSpaceEye);
		_programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
		_programObject->setUniform("ModelTransform", transform);
		setPscUniforms(*_programObject.get(), data.camera, data.position);

		glDisable(GL_CULL_FACE);
		//glCullFace(GL_BACK);

		// render
		_grid.geometry().drawUsingActiveProgram();

		// disable shader
		_programObject->deactivate();
	}

	void GlobeMesh::update(const UpdateData& data) {
	}

}  // namespace openspace
