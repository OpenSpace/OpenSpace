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

#include <modules/globebrowsing/rendering/patchrenderer.h>

// open space includes
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>

// ghoul includes
#include <ghoul/misc/assert.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
	const std::string _loggerCat = "LatLonPatch";

	const std::string keyFrame = "Frame";
	const std::string keyGeometry = "Geometry";
	const std::string keyShading = "PerformShading";

	const std::string keyBody = "Body";
}

namespace openspace {

	//////////////////////////////////////////////////////////////////////////////////////
	//							PATCH RENDERER											//
	//////////////////////////////////////////////////////////////////////////////////////
	PatchRenderer::PatchRenderer(shared_ptr<Geometry> geometry)
		: _geometry(geometry)
	{

	}

	PatchRenderer::~PatchRenderer() {
		if (_programObject) {
			RenderEngine& renderEngine = OsEng.renderEngine();
			renderEngine.removeRenderProgram(_programObject);
			_programObject = nullptr;
		}
	}

	void PatchRenderer::renderPatch(const LatLonPatch& patch, const RenderData& data, double radius) {
		// nothing to do
	}


	//////////////////////////////////////////////////////////////////////////////////////
	//								LATLON PATCH RENDERER								//
	//////////////////////////////////////////////////////////////////////////////////////
	LatLonPatchRenderer::LatLonPatchRenderer(shared_ptr<Geometry> geometry) 
		: PatchRenderer(geometry)
	{
		_programObject = OsEng.renderEngine().buildRenderProgram(
			"LatLonSphereMappingProgram",
			"${MODULE_GLOBEBROWSING}/shaders/latlonpatch_spheremapping_vs.glsl",
			"${MODULE_GLOBEBROWSING}/shaders/simple_fs.glsl");
		ghoul_assert(_programObject != nullptr, "Failed to initialize programObject!");

		using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
		_programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
	}



	void LatLonPatchRenderer::renderPatch(
		const LatLonPatch& patch, const RenderData& data, double radius) 
	{
		// activate shader
		_programObject->activate();
		using namespace glm;


		// Get camera transform matrix
		// TODO : Should only need to fetch the camera transform and use directly
		// but this is not currently possible in the camera class.
		vec3 cameraPosition = data.camera.position().vec3();
		mat4 viewTransform = inverse(translate(mat4(1.0), cameraPosition));
		viewTransform = mat4(data.camera.viewRotationMatrix()) * viewTransform;

		// TODO : Model transform should be fetched as a matrix directly.
		mat4 modelTransform = translate(mat4(1), data.position.vec3());
		mat4 modelViewProjectionTransform = data.camera.projectionMatrix()
			* viewTransform * modelTransform;

		LatLon swCorner = patch.southWestCorner();

		_programObject->setUniform("modelViewProjectionTransform", modelViewProjectionTransform);
		_programObject->setUniform("minLatLon", vec2(swCorner.lat, swCorner.lon));
		_programObject->setUniform("latLonScalingFactor", 2.0f * vec2(patch.halfSize.lat, patch.halfSize.lon));
		_programObject->setUniform("globeRadius", float(radius));

		glEnable(GL_DEPTH_TEST);
		glEnable(GL_CULL_FACE);
		glCullFace(GL_BACK);

		// render
		_geometry->drawUsingActiveProgram();

		// disable shader
		_programObject->deactivate();
	}


}  // namespace openspace
