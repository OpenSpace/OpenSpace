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
		double sizeLon)
	: _grid(
		xRes,
		yRes,
		Geometry::Positions::No,
		Geometry::TextureCoordinates::Yes,
		Geometry::Normals::No)
	, _programObject(nullptr)
	, _posLatLon(posLat, posLon)
	, _sizeLatLon(sizeLat, sizeLon)
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

		// TODO : Not sure if double precision will be needed for all these calculations
		// Using doubles in case but might slow things down.
		// TODO : Need to get the radius of the globe
		double r = 6.3e6;

		// Create control points (double)
		glm::dvec3 p00, p01, p10, p11;

		// Calculate global positions of control points
		p00 = glm::dvec3(converter::latLonToCartesian(
			_posLatLon.x - _sizeLatLon.x,
			_posLatLon.y - _sizeLatLon.y,
			r));
		p10 = glm::dvec3(converter::latLonToCartesian(
			_posLatLon.x + _sizeLatLon.x,
			_posLatLon.y - _sizeLatLon.y,
			r));
		p01 = glm::dvec3(converter::latLonToCartesian(
			_posLatLon.x - _sizeLatLon.x,
			_posLatLon.y + _sizeLatLon.y,
			r));
		p11 = glm::dvec3(converter::latLonToCartesian(
			_posLatLon.x + _sizeLatLon.x,
			_posLatLon.y + _sizeLatLon.y,
			r));

		// TODO : Transformation to world space from model space should also consider
		// rotations. Now it only uses translatation for simplicity. Should be done
		// With a matrix transform
		glm::dvec3 position = data.position.dvec3();
		p00 += position;
		p10 += position;
		p01 += position;
		p11 += position;
		
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
		p01 = glm::dvec3(viewTransform * glm::dvec4(p01, 1.0));
		p11 = glm::dvec3(viewTransform * glm::dvec4(p11, 1.0));

		// Send control points to GPU to be used in shader
		// TODO : Will need more control points or possibly normals to be able to
		// do better than linear interpolation
		_programObject->setUniform("p00", glm::vec3(p00));
		_programObject->setUniform("p10", glm::vec3(p10));
		_programObject->setUniform("p01", glm::vec3(p01));
		_programObject->setUniform("p11", glm::vec3(p11));

		_programObject->setUniform("Projection", data.camera.projectionMatrix());

		// Render triangles (use texture coordinates to interpolate to new positions)
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

	void LatLonPatch::setSizeLatLon(glm::dvec2 sizeLatLon) {
		_sizeLatLon = sizeLatLon;
	}


}  // namespace openspace
