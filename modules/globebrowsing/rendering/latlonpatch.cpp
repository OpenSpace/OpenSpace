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
		using namespace glm;

		// activate shader
		_programObject->activate();

		// TODO : Not sure if double precision will be needed for all these calculations
		// Using doubles in case for now.

		// Create control points and normals
		// 
		// These are the physical positions of the control points for the patch:
		//
		// y  p[6] p[7] p[8]     p02 p12 p22
		// ^  p[3] p[4] p[5] <=> p01 p11 p21
		// |  p[0] p[1] p[2]     p00 p10 p20
		// |
		//  -----> x
		dvec3 p[9];// p00, p01, p02, p10, p11, p12, p20, p21, p22;
		dvec3 n00, n01, n02, n10, n11, n12, n20, n21, n22;
		dvec3 nHorizontal0, nHorizontal1, nHorizontal2;
		float interpolationWeight;

		// Calculate positions of corner control points 
		p[0] = calculateCornerPointBottomLeft();	// p00
		p[2] = calculateCornerPointBottomRight();	// p20
		p[6] = calculateCornerPointTopLeft();		// p02
		p[8] = calculateCornerPointTopRight();		// p22

		// Calculate the horizontal normals
		// Horizontal normals are the same for constant latitude
		nHorizontal0 = normalize(converter::latLonToCartesian(
			0,
			_posLatLon.y - _sizeLatLon.y,
			_globeRadius));
		nHorizontal1 = normalize(converter::latLonToCartesian(
			0,
			_posLatLon.y,
			_globeRadius));
		nHorizontal2 = normalize(converter::latLonToCartesian(
			0,
			_posLatLon.y + _sizeLatLon.y,
			_globeRadius));

		// Get position of center control points
		p[3] = calculateCenterPoint(p[0], normalize(p[0]), p[6], normalize(p[6]));	// p01
		p[5] = calculateCenterPoint(p[2], normalize(p[2]), p[8], normalize(p[8]));	// p21
		p[1] = calculateCenterPoint(p[0], nHorizontal0, p[2], nHorizontal2);		// p10
		p[7] = calculateCenterPoint(p[6], nHorizontal0, p[8], nHorizontal2);		// p12
		p[4] = calculateCenterPoint(p[3], nHorizontal0, p[5], nHorizontal2);		// p11

		// Calculate one weight to send to GPU for interpolation in longitude range.
		// Actually there are three weights but they all have the same value.
		// By weighting the center control point with this value, a circle segment is
		// defined by the NURBS curve
		interpolationWeight = dot(nHorizontal0, nHorizontal1);

		// TODO : Transformation to world space from model space should also consider
		// rotations. Now it only uses translatation for simplicity. Should be done
		// With a matrix transform
		dvec3 modelPosition = data.position.dvec3();
		for (size_t i = 0; i < 9; i++) {
			p[i] += modelPosition;
		}

		// Get camera transform matrix
		// TODO : Should only need to fetch the camera transform and use directly
		// but this is not currently possible in the camera class.
		dvec3 cameraPosition = data.camera.position().dvec3();
		dmat4 viewTransform = inverse(translate(dmat4(1.0), cameraPosition));
		viewTransform = dmat4(data.camera.viewRotationMatrix()) * viewTransform;

		// Transform control points to camera space
		for (size_t i = 0; i < 9; i++) {
			p[i] = dvec3(viewTransform * dvec4(p[i], 1.0));
		}

		// Transform normals to camera space
		n00 = dvec3(viewTransform * dvec4(n00, 0.0));
		n10 = dvec3(viewTransform * dvec4(n10, 0.0));
		n20 = dvec3(viewTransform * dvec4(n20, 0.0));
		n01 = dvec3(viewTransform * dvec4(n01, 0.0));
		n11 = dvec3(viewTransform * dvec4(n11, 0.0));
		n21 = dvec3(viewTransform * dvec4(n21, 0.0));
		n02 = dvec3(viewTransform * dvec4(n02, 0.0));
		n12 = dvec3(viewTransform * dvec4(n12, 0.0));
		n22 = dvec3(viewTransform * dvec4(n22, 0.0));

		// Send control points to GPU to be used in shader
		// Transform to float values
		vec3 pFloat[9];
		for (size_t i = 0; i < 9; i++) {
			pFloat[i] = vec3(p[i]);
		}
		_programObject->setUniform("p", &pFloat[0], 9);
		_programObject->setUniform("interpolationWeight", interpolationWeight);
		_programObject->setUniform("Projection", data.camera.projectionMatrix());
		 
		// Render triangles (use texture coordinates to interpolate to new positions)
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
