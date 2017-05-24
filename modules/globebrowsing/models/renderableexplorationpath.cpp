/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/globebrowsing/models/renderableexplorationpath.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/properties/scalar/boolproperty.h>

#include <fstream>
#include "ogr_geometry.h"
#include "ogrsf_frmts.h"
#include <gdal_priv.h>

namespace {
	static const std::string _loggerCat = "RenderableExplorationPath";
}

namespace openspace {
namespace globebrowsing {

RenderableExplorationPath::RenderableExplorationPath()
	: _pathShader(nullptr)
	, _siteShader(nullptr)
	, _fading(1.0)
	, _vertexBufferID(0)
	, _vaioID(0)
	, _hasLoopedOnce(false)
	, _isCloseEnough(false)
	, _cameraToPointDistance(0.0)
	, _isEnabled(properties::BoolProperty("enabled", "enabled", false))
{
}

RenderableExplorationPath::~RenderableExplorationPath() {}

bool RenderableExplorationPath::initialize(RenderableGlobe* globe, std::vector<glm::dvec2> coordinates) {

	_globe = globe;
	_latLonCoordinates = coordinates;

	// Shaders for the path (GL_LINES)
	if (_pathShader == nullptr) {
		_pathShader = OsEng.renderEngine().buildRenderProgram(
			"RoverPath",
			"${MODULE_GLOBEBROWSING}/shaders/roverpath_vs.glsl",
			"${MODULE_GLOBEBROWSING}/shaders/roverpath_fs.glsl"
		);
		if (!_pathShader) {
			return false;
		}
	}

	// Shaders for the sites (GL_POINTS)
	if (_siteShader == nullptr) {
		_siteShader = OsEng.renderEngine().buildRenderProgram(
			"RoverPath",
			"${MODULE_GLOBEBROWSING}/shaders/roversite_vs.glsl",
			"${MODULE_GLOBEBROWSING}/shaders/roversite_fs.glsl"
		);
		if (!_siteShader) {
			return false;
		}
	}

	calculatePathModelCoordinates();
	if (_latLonCoordinates.size() == 0) return false;

	// Initialize and upload to graphics card
	glGenVertexArrays(1, &_vaioID);
	ghoul_assert(_vaioID != 0, "Could not generate vertex arrays");

	glGenBuffers(1, &_vertexBufferID);
	ghoul_assert(_vertexBufferID != 0, "Could not create vertex buffer");

	glBindVertexArray(_vaioID);
	glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferID);
	glBufferData(GL_ARRAY_BUFFER,
		_stationPointsModelCoordinates.size() * sizeof(_stationPointsModelCoordinates[0]),
		&_stationPointsModelCoordinates[0],
		GL_DYNAMIC_DRAW);

	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(_stationPointsModelCoordinates[0]), 0);

	glBindVertexArray(0); 

	return true;
}

bool RenderableExplorationPath::deinitialize() {
	RenderEngine& renderEngine = OsEng.renderEngine();
	if (_pathShader) {
		renderEngine.removeRenderProgram(_pathShader);
		_pathShader = nullptr;
	}
	if (_siteShader) {
		renderEngine.removeRenderProgram(_siteShader);
		_siteShader = nullptr;
	}

	glDeleteVertexArrays(1, &_vaioID);
	glDeleteBuffers(1, &_vertexBufferID);

	return false;
}

bool RenderableExplorationPath::isReady() const {
	bool ready = true;
	ready &= (_pathShader != nullptr);
	return true;
}

void RenderableExplorationPath::render(const RenderData& data) {

	// Camera position in model space
	glm::dvec3 camPos = data.camera.positionVec3();
	glm::dmat4 inverseModelTransform = _globe->inverseModelTransform();
	glm::dvec3 cameraPositionModelSpace =
		glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));

	// Takes the coordinates for the station in the middle of the array
	// and calculates the position on the ellipsoid
	int vectorSize = _stationPointsModelCoordinates.size();
	glm::dvec3 positionOnEllipsoid = _stationPointsModelCoordinates[int(vectorSize / 2)];

	// Temporary solution to trigger the calculation of new positions of the stations when the camera is 
	// less than 5000 meters from the "middle' station. Should possibly be moved do a distanceswitch.
	double heightToSurface = _globe->getHeight(_stationPointsModelCoordinates[int(vectorSize / 2)]);
	glm::dvec3 directionFromSurfaceToPointModelSpace = _globe->ellipsoid().
		geodeticSurfaceNormal(_globe->ellipsoid().cartesianToGeodetic2(_stationPointsModelCoordinates[int(vectorSize / 2)]));
	glm::dvec3 tempPos = glm::dvec3(_stationPointsModelCoordinates[int(vectorSize / 2)]) + heightToSurface * directionFromSurfaceToPointModelSpace;

	// The distance from the camera to the position on the ellipsoid
	_cameraToPointDistance = glm::length(cameraPositionModelSpace - tempPos);

	if (_cameraToPointDistance < 10000.0) {
		_isCloseEnough = true;

		// Only show the path when camera is close enough. Especially GL_POINTS look bad
		// when camera is far form the globe. Will have to improve this.
		/*if (distance < 16737 && _fading < 1.f)
			_fading += 0.01f;
		else if (distance >= 16737 && _fading > 0.f)
			_fading -= 0.01f;*/

		// Model transform and view transform needs to be in double precision
		glm::dmat4 globeModelTransform = _globe->modelTransform();
		glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * globeModelTransform;

		_pathShader->activate();

		// Passing model view transform as double to maintain precision for vertices.
		// Otherwise the path i "jumping".
		_pathShader->setUniform("modelViewTransform", modelViewTransform);
		_pathShader->setUniform("projectionTransform", data.camera.projectionMatrix());
		_pathShader->setUniform("fading", _fading);
		_pathShader->setUniform("color", glm::vec3(1.0, 1.0, 1.0));

		glBindVertexArray(_vaioID);
		glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferID);
		glLineWidth(1.0f);
		glDrawArrays(GL_LINE_STRIP, 0, _stationPointsModelCoordinates.size());
		glBindVertexArray(0);

		_pathShader->deactivate();
		
		/*_siteShader->activate();

		_siteShader->setUniform("modelViewTransform", modelViewTransform);
		_siteShader->setUniform("projectionTransform", data.camera.projectionMatrix());
		_siteShader->setUniform("fading", _fading);
		_siteShader->setUniform("color", glm::vec3(1.0, 1.0, 1.0));

		glBindVertexArray(_vaioID);
		glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferID);
		glPointSize(20.0f);
		glDrawArrays(GL_POINTS, 0, _stationPointsModelCoordinates.size());
		glBindVertexArray(0);

		_siteShader->deactivate();*/
	}
}

void RenderableExplorationPath::update(const UpdateData& data) {

	
	if(_isCloseEnough == true && _hasLoopedOnce == false) {
		// Clear old coordinates values.
		_stationPointsModelCoordinates.clear();
		for (auto i : _latLonCoordinates) {

			globebrowsing::Geodetic2 geoTemp = globebrowsing::Geodetic2{ i.x, i.y } / 180 * glm::pi<double>();
			glm::dvec3 positionModelSpaceTemp = _globe->ellipsoid().cartesianSurfacePosition(geoTemp);
			double heightToSurface = _globe->getHeight(positionModelSpaceTemp);

			globebrowsing::Geodetic3 geo3 = globebrowsing::Geodetic3{ geoTemp, heightToSurface + 1.0 };
			glm::dvec3 tempPos2 = _globe->ellipsoid().cartesianPosition(geo3);
			_stationPointsModelCoordinates.push_back(glm::dvec4(tempPos2, 1.0));
		}
		
		_hasLoopedOnce = true;

		// Buffer new data
		glBindVertexArray(_vaioID);
		glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferID);

		glBufferData(GL_ARRAY_BUFFER, _stationPointsModelCoordinates.size() * sizeof(_stationPointsModelCoordinates[0]),
			NULL, GL_STREAM_DRAW);
		glBufferSubData(GL_ARRAY_BUFFER, 0,
			_stationPointsModelCoordinates.size() * sizeof(_stationPointsModelCoordinates[0]),
			&_stationPointsModelCoordinates[0]);

		glEnableVertexAttribArray(0);
		glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, 0);

		glBindVertexArray(0);
	}	
}

void RenderableExplorationPath::calculatePathModelCoordinates() {
	globebrowsing::Geodetic2 geo;
	glm::dvec3 positionModelSpace;

	for (auto position : _latLonCoordinates) {
		geo = globebrowsing::Geodetic2{ position.x, position.y } / 180 * glm::pi<double>();
		positionModelSpace = _globe->ellipsoid().cartesianSurfacePosition(geo);
		_stationPointsModelCoordinates.push_back(glm::dvec4(positionModelSpace, 1.0f));
	}
}

} // namespace globebrowsing
} // namespace openspace
