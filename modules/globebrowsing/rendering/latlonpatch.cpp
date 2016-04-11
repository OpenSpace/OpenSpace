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

#include <modules/globebrowsing/rendering/latlonpatch.h>

// globe browsing includes
#include <modules/globebrowsing/util/converter.h>

// open space includes
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
// ghoul includes
#include <ghoul/misc/assert.h>

#define _USE_MATH_DEFINES
#include <math.h>
#include <glm/gtc/matrix_transform.hpp>

namespace {
	const std::string _loggerCat = "LatLonPatch";

	const std::string keyFrame = "Frame";
	const std::string keyGeometry = "Geometry";
	const std::string keyShading = "PerformShading";

	const std::string keyBody = "Body";
}

namespace openspace {
	LatLonPatch::LatLonPatch(
		unsigned int xRes,
		unsigned int yRes,
		double posLat,
		double posLon,
		double sizeLat,
		double sizeLon,
		double globeRadius)
	: _grid(
		xRes,
		yRes,
		Geometry::Positions::No,
		Geometry::TextureCoordinates::Yes, // Texture coords are used to derive postions
		Geometry::Normals::No)
	, _programObject(nullptr)
	, _posLatLon(posLat, posLon)
	, _sizeLatLon(sizeLat, sizeLon)
	, _globeRadius(globeRadius)
	{

	}

	LatLonPatch::~LatLonPatch() {

	}

	bool LatLonPatch::initialize() {
		RenderEngine& renderEngine = OsEng.renderEngine();
		if (_programObject == nullptr) {
			_programObject = renderEngine.buildRenderProgram(
				"LatLonSphereMappingProgram",
				"${MODULE_GLOBEBROWSING}/shaders/latlonpatch_spheremapping_vs.glsl",
				"${MODULE_GLOBEBROWSING}/shaders/simple_fs.glsl");
			if (!_programObject) return false;
		}

		using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
		_programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);

		return isReady();
	}

	bool LatLonPatch::deinitialize() {
		RenderEngine& renderEngine = OsEng.renderEngine();
		if (_programObject) {
			renderEngine.removeRenderProgram(_programObject);
			_programObject = nullptr;
		}

		return true;
	}

	bool LatLonPatch::isReady() const {
		bool ready = true;
		ready &= (_programObject != nullptr);
		return ready;
	}

	void LatLonPatch::render(const RenderData& data) {
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

		_programObject->setUniform("modelViewProjectionTransform", data.camera.projectionMatrix() * viewTransform *  modelTransform);		
		_programObject->setUniform("minLatLon", vec2(_posLatLon - _sizeLatLon));
		_programObject->setUniform("latLonScalingFactor", 2.0f * vec2(_sizeLatLon));
		_programObject->setUniform("globeRadius", float(_globeRadius));

		glEnable(GL_DEPTH_TEST);
		glEnable(GL_CULL_FACE);
		glCullFace(GL_BACK);

		// render
		_grid.drawUsingActiveProgram();

		// disable shader
		_programObject->deactivate();
	}

	void LatLonPatch::update(const UpdateData& data) {

	}

	void LatLonPatch::setPositionLatLon(glm::dvec2 posLatLon) {
		_posLatLon = posLatLon;
	}

	glm::dvec3 LatLonPatch::calculateCornerPointBottomLeft() {
		return converter::latLonToCartesian(
			_posLatLon.x - _sizeLatLon.x,
			_posLatLon.y - _sizeLatLon.y,
			_globeRadius);
	}
	glm::dvec3 LatLonPatch::calculateCornerPointBottomRight() {
		return converter::latLonToCartesian(
			_posLatLon.x - _sizeLatLon.x,
			_posLatLon.y + _sizeLatLon.y,
			_globeRadius);
	}
	glm::dvec3 LatLonPatch::calculateCornerPointTopLeft() {
		return converter::latLonToCartesian(
			_posLatLon.x + _sizeLatLon.x,
			_posLatLon.y - _sizeLatLon.y,
			_globeRadius);
	}
	glm::dvec3 LatLonPatch::calculateCornerPointTopRight() {
		return converter::latLonToCartesian(
			_posLatLon.x + _sizeLatLon.x,
			_posLatLon.y + _sizeLatLon.y,
			_globeRadius);
	}

	glm::dvec3 LatLonPatch::calculateCenterPoint(
		glm::dvec3 p0,
		glm::dvec3 n0,
		glm::dvec3 p2,
		glm::dvec3 n2) {
		// Solution derived from plane geometry
		glm::dvec2 u = glm::dvec2(0, glm::dot(p2, n2) - glm::dot(p0, n2));
		double cosNormalAngle = glm::dot(n0, n2);
		glm::dmat2 A = glm::dmat2({ 1, cosNormalAngle, cosNormalAngle, 1 });
		glm::dvec2 stParam = glm::inverse(A) * u;
		glm::dvec3 p1 = p0 + stParam.s * n0 + stParam.t * n2;
		return p1;
	}

	void LatLonPatch::setSizeLatLon(glm::dvec2 sizeLatLon) {
		_sizeLatLon = sizeLatLon;
	}

	void LatLonPatch::setGlobeRadius(double globeRadius) {
		_globeRadius = globeRadius;
	}

}  // namespace openspace
