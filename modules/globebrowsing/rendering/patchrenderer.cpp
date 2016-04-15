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

#include <modules/globebrowsing/rendering/clipmapgeometry.h>

// open space includes
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>

// ghoul includes
#include <ghoul/misc/assert.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>


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

	void PatchRenderer::setFrustrumCuller(std::shared_ptr<FrustrumCuller> fc) {
		_frustrumCuller = fc;
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


		

		// TODO : Model transform should be fetched as a matrix directly.
		mat4 modelTransform = translate(mat4(1), data.position.vec3());
		mat4 viewTransform = data.camera.combinedViewMatrix();
		mat4 modelViewProjectionTransform = data.camera.projectionMatrix()
			* viewTransform * modelTransform;


		// View frustrum culling
		if (_frustrumCuller != nullptr) {
			//LatLon center = patch.center;
			//vec3 centerPoint = radius * center.asUnitCartesian();
			if (!_frustrumCuller->isVisible(patch, radius, modelViewProjectionTransform)) {
				// dont render the patch
				return;
			}
		}

		// Get the textures that should be used for rendering
	 	glm::ivec3 tileIndex = tileSet.getTileIndex(patch);
		LatLonPatch tilePatch = tileSet.getTilePositionAndScale(tileIndex);
		std::shared_ptr<ghoul::opengl::Texture> tile00 = tileSet.getTile(tileIndex);

		// Bind and use the texture
		ghoul::opengl::TextureUnit texUnit;
		texUnit.activate();
		tile00->bind();
		_programObject->setUniform("textureSampler", texUnit);
		
		LatLon swCorner = patch.southWestCorner();
		_programObject->setUniform("modelViewProjectionTransform", modelViewProjectionTransform);
		_programObject->setUniform("minLatLon", vec2(swCorner.toLonLatVec2()));
		_programObject->setUniform("lonLatScalingFactor", 2.0f * vec2(patch.halfSize.toLonLatVec2()));
		_programObject->setUniform("globeRadius", float(radius));

		glEnable(GL_DEPTH_TEST);
		glEnable(GL_CULL_FACE);
		glCullFace(GL_BACK);

		// render
		_geometry->drawUsingActiveProgram();

		// disable shader
		_programObject->deactivate();
	}
	
	//////////////////////////////////////////////////////////////////////////////////////
	//								CLIPMAP PATCH RENDERER								//
	//////////////////////////////////////////////////////////////////////////////////////
	ClipMapPatchRenderer::ClipMapPatchRenderer()
		: PatchRenderer(shared_ptr<ClipMapGeometry>(new ClipMapGeometry(32)))
	{
		_programObject = OsEng.renderEngine().buildRenderProgram(
			"LatLonSphereMappingProgram",
			"${MODULE_GLOBEBROWSING}/shaders/clipmappatch_spheremapping_vs.glsl",
			"${MODULE_GLOBEBROWSING}/shaders/simple_fs.glsl");
		ghoul_assert(_programObject != nullptr, "Failed to initialize programObject!");

		using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
		_programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
	}

	void ClipMapPatchRenderer::renderPatch(
		const LatLonPatch& patch, const RenderData& data, double radius)
	{
		// activate shader
		_programObject->activate();
		using namespace glm;

		mat4 viewTransform = data.camera.combinedViewMatrix();

		// TODO : Model transform should be fetched as a matrix directly.
		mat4 modelTransform = translate(mat4(1), data.position.vec3());

		// Snap patch position
		int segmentsPerPatch = 32;
		LatLon stepSize = LatLon(
			patch.halfSize.lat * 2 / segmentsPerPatch,
			patch.halfSize.lon * 2 / segmentsPerPatch);
		ivec2 patchesToCoverGlobe = ivec2(
			M_PI / (patch.halfSize.lat * 2) + 0.5,
			M_PI * 2 / (patch.halfSize.lon * 2) + 0.5);
		ivec2 intSnapCoord = ivec2(
			patch.center.lat / (M_PI * 2) * segmentsPerPatch * patchesToCoverGlobe.y,
			patch.center.lon / (M_PI) * segmentsPerPatch * patchesToCoverGlobe.x);
		LatLon swCorner = LatLon(
			stepSize.lat * intSnapCoord.x - patch.halfSize.lat,
			stepSize.lon * intSnapCoord.y - patch.halfSize.lon);

		ivec2 contraction = ivec2(intSnapCoord.y % 2, intSnapCoord.x % 2);

		//LDEBUG("patch.center = [ " << patch.center.lat << " , " << patch.center.lon << " ]");
		//LDEBUG("intSnapCoord = [ " << intSnapCoord.x << " , " << intSnapCoord.y << " ]");
		//LDEBUG("contraction = [ " << contraction.x << " , " << contraction.y << " ]");

		_programObject->setUniform("modelViewProjectionTransform", data.camera.projectionMatrix() * viewTransform *  modelTransform);
		_programObject->setUniform("minLatLon", vec2(swCorner.toLonLatVec2()));
		_programObject->setUniform("lonLatScalingFactor", 2.0f * vec2(patch.halfSize.toLonLatVec2()));
		_programObject->setUniform("globeRadius", float(radius));
		_programObject->setUniform("contraction", contraction);

		glEnable(GL_DEPTH_TEST);
		glEnable(GL_CULL_FACE);
		glCullFace(GL_BACK);

		// render
		_geometry->drawUsingActiveProgram();

		// disable shader
		_programObject->deactivate();
	}

}  // namespace openspace
