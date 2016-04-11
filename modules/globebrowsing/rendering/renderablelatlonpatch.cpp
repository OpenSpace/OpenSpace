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

#include <modules/globebrowsing/rendering/renderablelatlonpatch.h>

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
	RenderableLatLonPatch::RenderableLatLonPatch(
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
			Geometry::TextureCoordinates::Yes,
			Geometry::Normals::No)
		, _programObject(nullptr)
		, _patch(LatLonPatch(LatLon(posLat, posLon), LatLon(sizeLat, sizeLon)))
		, _globeRadius(globeRadius)
	{

	}

	RenderableLatLonPatch::~RenderableLatLonPatch() {

	}


	bool RenderableLatLonPatch::initialize() {
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

	bool RenderableLatLonPatch::deinitialize() {
		RenderEngine& renderEngine = OsEng.renderEngine();
		if (_programObject) {
			renderEngine.removeRenderProgram(_programObject);
			_programObject = nullptr;
		}

		return true;
	}

	bool RenderableLatLonPatch::isReady() const {
		bool ready = true;
		ready &= (_programObject != nullptr);
		return ready;
	}

	void RenderableLatLonPatch::render(const RenderData& data) {

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

		LatLon swCorner = _patch.southWestCorner();
		
		_programObject->setUniform("modelViewProjectionTransform", data.camera.projectionMatrix() * viewTransform *  modelTransform);		
		_programObject->setUniform("minLatLon", vec2(swCorner.lat, swCorner.lon));
		_programObject->setUniform("latLonScalingFactor", 2.0f * vec2(_patch.halfSize.lat, _patch.halfSize.lon));
		_programObject->setUniform("globeRadius", float(_globeRadius));

		glEnable(GL_DEPTH_TEST);
		glEnable(GL_CULL_FACE);
		glCullFace(GL_BACK);

		// render
		_grid.drawUsingActiveProgram();

		// disable shader
		_programObject->deactivate();

		#if 0

		// activate shader
		_programObject->activate();

		// TODO : Not sure if double precision will be needed for all these calculations
		// Using doubles in case but might slow things down.
		// TODO : Need to get the radius of the globe
		double r = 6.3e6;

		// Create control points and normals(double)
		glm::dvec3 p00, p01, p02, p10, p11, p12, p20, p21, p22;
		glm::dvec3 n00, n01, n02, n10, n11, n12, n20, n21, n22;
		glm::dvec3 nHorizontal0, nHorizontal1, nHorizontal2;
		float w10, w11, w12;

		// Calculate positions of corner control points
		p00 = calculateCornerPointBottomLeft();
		p20 = calculateCornerPointBottomRight();
		p02 = calculateCornerPointTopLeft();
		p22 = calculateCornerPointTopRight();

		// Calculate the horizontal normals
		nHorizontal0 = glm::normalize(converter::latLonToCartesian(
			0,
			_patch.getCenter().lon - _patch.getHalfSize().lon,
			r));
		nHorizontal1 = glm::normalize(converter::latLonToCartesian(
			0,
			_patch.getCenter().lon,
			r));
		nHorizontal2 = glm::normalize(converter::latLonToCartesian(
			0,
			_patch.getCenter().lon + _patch.getHalfSize().lon,
			r));

		// Get position of center control points
		p01 = calculateCenterPoint(p00, glm::normalize(p00), p02, glm::normalize(p02));
		p21 = calculateCenterPoint(p20, glm::normalize(p20), p22, glm::normalize(p22));
		p10 = calculateCenterPoint(p00, nHorizontal0, p20, nHorizontal2);
		p12 = calculateCenterPoint(p02, nHorizontal0, p22, nHorizontal2);
		p11 = calculateCenterPoint(p01, nHorizontal0, p21, nHorizontal2);

		// Calculate normals from control point positions
		n00 = normalize(p00);
		n01 = normalize(p01);
		n02 = normalize(p02);
		n10 = normalize(p10);
		n11 = normalize(p11);
		n12 = normalize(p12);
		n20 = normalize(p20);
		n21 = normalize(p21);
		n22 = normalize(p22);

		// Calculate three weights to send to GPU for interpolation
		w10 = glm::dot(nHorizontal0, nHorizontal1);
		w11 = w10;
		w12 = w10;

		// TODO : Transformation to world space from model space should also consider
		// rotations. Now it only uses translatation for simplicity. Should be done
		// With a matrix transform
		// TODO : Normals should also be transformed
		glm::dvec3 position = data.position.dvec3();
		p00 += position;
		p10 += position;
		p20 += position;
		p01 += position;
		p11 += position;
		p21 += position;
		p02 += position;
		p12 += position;
		p22 += position;

		// Calculate global position of camera
		// TODO : Should only need to fetch the camera transform and use directly
		// Now the viewTransform is wrong due to the constant lookUpVector
		glm::dvec3 camPos = data.camera.position().dvec3();
		glm::dvec3 camDir = glm::normalize(position - camPos);
		glm::dvec3 camUp = glm::dvec3(0,1,0);// data.camera.lookUpVector();

		// Get camera transform matrix (double precision)
		glm::dmat4 viewTransform = glm::inverse(glm::translate(glm::dmat4(1.0), camPos));

		//glm::dmat4 viewTransform = glm::lookAt(camPos, camPos + camDir, camUp);
		viewTransform = glm::dmat4(data.camera.viewRotationMatrix())*viewTransform;

		// Transform control points to camera space
		p00 = glm::dvec3(viewTransform * glm::dvec4(p00, 1.0));
		p10 = glm::dvec3(viewTransform * glm::dvec4(p10, 1.0));
		p20 = glm::dvec3(viewTransform * glm::dvec4(p20, 1.0));
		p01 = glm::dvec3(viewTransform * glm::dvec4(p01, 1.0));
		p11 = glm::dvec3(viewTransform * glm::dvec4(p11, 1.0));
		p21 = glm::dvec3(viewTransform * glm::dvec4(p21, 1.0));
		p02 = glm::dvec3(viewTransform * glm::dvec4(p02, 1.0));
		p12 = glm::dvec3(viewTransform * glm::dvec4(p12, 1.0));
		p22 = glm::dvec3(viewTransform * glm::dvec4(p22, 1.0));

		// Transform normals to camera space
		n00 = glm::dvec3(viewTransform * glm::dvec4(n00, 0.0));
		n10 = glm::dvec3(viewTransform * glm::dvec4(n10, 0.0));
		n20 = glm::dvec3(viewTransform * glm::dvec4(n20, 0.0));
		n01 = glm::dvec3(viewTransform * glm::dvec4(n01, 0.0));
		n11 = glm::dvec3(viewTransform * glm::dvec4(n11, 0.0));
		n21 = glm::dvec3(viewTransform * glm::dvec4(n21, 0.0));
		n02 = glm::dvec3(viewTransform * glm::dvec4(n02, 0.0));
		n12 = glm::dvec3(viewTransform * glm::dvec4(n12, 0.0));
		n22 = glm::dvec3(viewTransform * glm::dvec4(n22, 0.0));

		// Send control points and normals to GPU to be used in shader
		_programObject->setUniform("p00", glm::vec3(p00));
		_programObject->setUniform("p10", glm::vec3(p10));
		_programObject->setUniform("p20", glm::vec3(p20));
		_programObject->setUniform("p01", glm::vec3(p01));
		_programObject->setUniform("p11", glm::vec3(p11));
		_programObject->setUniform("p21", glm::vec3(p21));
		_programObject->setUniform("p02", glm::vec3(p02));
		_programObject->setUniform("p12", glm::vec3(p12));
		_programObject->setUniform("p22", glm::vec3(p22));

		_programObject->setUniform("w10", w10);
		_programObject->setUniform("w11", w11);
		_programObject->setUniform("w12", w12);

		/*
		_programObject->setUniform("n00", glm::vec3(n00));
		_programObject->setUniform("n10", glm::vec3(n10));
		_programObject->setUniform("n20", glm::vec3(n20));
		_programObject->setUniform("n01", glm::vec3(n01));
		_programObject->setUniform("n11", glm::vec3(n11));
		_programObject->setUniform("n21", glm::vec3(n21));
		_programObject->setUniform("n02", glm::vec3(n02));
		_programObject->setUniform("n12", glm::vec3(n12));
		_programObject->setUniform("n22", glm::vec3(n22));
		*/
		_programObject->setUniform("Projection", data.camera.projectionMatrix());
		 
		// Render triangles (use texture coordinates to interpolate to new positions)
		glEnable(GL_DEPTH_TEST);
		glEnable(GL_CULL_FACE);
		glCullFace(GL_BACK);



		// render
		_grid.drawUsingActiveProgram();

		// disable shader
		_programObject->deactivate();

		#endif // DEBUG
	}

	void RenderableLatLonPatch::update(const UpdateData& data) {

	}


	glm::dvec3 RenderableLatLonPatch::calculateCornerPointBottomLeft() {
		return _globeRadius * _patch.southEastCorner().asUnitCartesian();
	}
	glm::dvec3 RenderableLatLonPatch::calculateCornerPointBottomRight() {
		return _globeRadius  * _patch.southEastCorner().asUnitCartesian();
	}
	glm::dvec3 RenderableLatLonPatch::calculateCornerPointTopLeft() {
		return _globeRadius  * _patch.northWestCorner().asUnitCartesian();
	}
	glm::dvec3 RenderableLatLonPatch::calculateCornerPointTopRight() {
		return _globeRadius  * _patch.northEastCorner().asUnitCartesian();
	}

	glm::dvec3 RenderableLatLonPatch::calculateCenterPoint(
		glm::dvec3 p0,
		glm::dvec3 n0,
		glm::dvec3 p2,
		glm::dvec3 n2) {
		// Solution derived
		glm::dvec2 u = glm::dvec2(0, glm::dot(p2, n2) - glm::dot(p0, n2));
		double cosNormalAngle = glm::dot(n0, n2);
		glm::dmat2 A = glm::dmat2({ 1, cosNormalAngle, cosNormalAngle, 1 });
		glm::dvec2 stParam = glm::inverse(A) * u;
		glm::dvec3 p1 = p0 + stParam.s * n0 + stParam.t * n2;
		return p1;
	}

	void RenderableLatLonPatch::setPatch(LatLonPatch patch) {
		_patch = patch;
	}

	LatLonPatch RenderableLatLonPatch::getPatch() {
		return _patch;
	}

}  // namespace openspace
