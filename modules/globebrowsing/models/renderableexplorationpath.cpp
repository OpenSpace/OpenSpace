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
#include <openspace/scene/scenegraphnode.h>
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

RenderableExplorationPath::RenderableExplorationPath(const RenderableSite& owner, std::vector<glm::dvec2> coordinates)
	: _owner(owner)
	, _coordinates(coordinates)
	, _pathShader(nullptr)
	, _siteShader(nullptr)
	, _globe(nullptr)
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

bool RenderableExplorationPath::initialize() {
	
	// Getting the parent renderable to calculate rover model coordinates to world coordinates
	auto parent = OsEng.renderEngine().scene()->sceneGraphNode(_owner.owner()->name())->parent();
	_globe = (globebrowsing::RenderableGlobe *)parent->renderable();

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
	if (_coordinates.size() == 0) return false;

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

bool RenderableExplorationPath::isReady() const
{
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
	//LERROR(_cameraToPointDistance);

	if (_cameraToPointDistance < 10000.0) {
		_isCloseEnough = true;

		// Only show the path when camera is close enough. Especially GL_POINTS look bad
		// when camera is far form the globe. Will have to improve this.
		/*if (distance < 16737 && _fading < 1.f)
			_fading += 0.01f;
		else if (distance >= 16737 && _fading > 0.f)
			_fading -= 0.01f;*/

		// Model transform and view transform needs to be in double precision
		glm::dmat4 modelTransform = _globe->modelTransform();
		glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

		_pathShader->activate();

		_pathShader->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
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

		_siteShader->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
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

	double heightToSurfaceCheck = _globe->getHeight(glm::dvec3(_stationPoints[50].stationPosition));

	if(_isCloseEnough == true && _hasLoopedOnce == false) {

		if(heightToSurfaceCheck < 0.0 || heightToSurfaceCheck > 6.0) {
			glm::dvec3 tempPos;

			for (int i = 0; i < _stationPoints.size(); i++) {
				// TODO: Add padding to the height to surface

				// Gets the height of the station point to the surface of the active heightlayers(s)
				double heightToSurface = _globe->getHeight(glm::dvec3(_stationPoints[i].stationPosition));

				// Small precission issue with heightToSurface which makes the loop run multiple times
				if( heightToSurface > _stationPoints[i].previousStationHeight + 0.5 ||
						heightToSurface < _stationPoints[i].previousStationHeight - 0.5) {

					// The direction in which the point is to be moved
					glm::dvec3 directionFromSurfaceToPointModelSpace = _globe->ellipsoid().
						geodeticSurfaceNormal(_globe->ellipsoid().cartesianToGeodetic2(_stationPoints[i].stationPosition));
					tempPos = glm::dvec3(0.0, 0.0, 0.0);
					tempPos = _stationPoints[i].stationPosition;
					//tempPos += directionFromSurfaceToPointModelSpace * heightToSurface;

					// There is probably a nicer way to do this
					/*if (heightToSurface > 0.0 && _stationPoints[i].previousStationHeight == 0.0)
						tempPos += directionFromSurfaceToPointModelSpace * heightToSurface;
					else if (heightToSurface == 0.0 && _stationPoints[i].previousStationHeight > 0.0)
						tempPos += directionFromSurfaceToPointModelSpace * 	(-_stationPoints[i].previousStationHeight);
					else if (heightToSurface < 0.0 && _stationPoints[i].previousStationHeight == 0.0)
						tempPos += directionFromSurfaceToPointModelSpace * heightToSurface;
					else if (heightToSurface == 0.0 && _stationPoints[i].previousStationHeight < 0.0)
						tempPos += directionFromSurfaceToPointModelSpace * 	(-_stationPoints[i].previousStationHeight);
					else if (heightToSurface < 0.0 && _stationPoints[i].previousStationHeight > 0.0)
						tempPos += directionFromSurfaceToPointModelSpace * 	(-_stationPoints[i].previousStationHeight + heightToSurface);
					else if (heightToSurface > 0.0 && _stationPoints[i].previousStationHeight < 0.0)
						tempPos += directionFromSurfaceToPointModelSpace * 	(-_stationPoints[i].previousStationHeight + heightToSurface);*/
					//if (_stationPoints[i].previousStationHeight == 0.0)
						//LERROR("Previous station Height = 0");

					if(heightToSurface < 0.0) {
						tempPos += directionFromSurfaceToPointModelSpace * ( heightToSurface + 5.0 - _stationPoints[i].previousStationHeight);
					}

					_stationPoints[i].previousStationHeight = heightToSurface;
					_stationPoints[i].stationPosition = glm::dvec4(tempPos, 1.0);
				}
			}

			// Clears and pushes the new position values into the vector used in the vertex buffer
			// Really unecessary to use multiple vectors....
			_stationPointsModelCoordinates.clear();
			for (int i = 0; i < _stationPoints.size(); i++) {
				_stationPointsModelCoordinates.push_back(glm::vec4(_stationPoints[i].stationPosition));
			}

		}
		_hasLoopedOnce = true;
	}

	// Buffer new data
	glBindVertexArray(_vaioID);
	glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferID);
	glBufferData(GL_ARRAY_BUFFER,
		_stationPointsModelCoordinates.size() * sizeof(_stationPointsModelCoordinates[0]),
		&_stationPointsModelCoordinates[0],
		GL_DYNAMIC_DRAW);

	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(_stationPointsModelCoordinates[0]), 0);

	glBindVertexArray(0);
}

void RenderableExplorationPath::calculatePathModelCoordinates() {
	globebrowsing::Geodetic2 geo;
	glm::dvec3 positionModelSpace;
	StationInformation k;

	for (auto position : _coordinates) {
		geo = globebrowsing::Geodetic2{ position.x, position.y } / 180 * glm::pi<double>();
		positionModelSpace = _globe->ellipsoid().cartesianSurfacePosition(geo);
		_stationPointsModelCoordinates.push_back(glm::dvec4(positionModelSpace, 1.0f));

		k.previousStationHeight = 0.0;
		k.stationPosition = glm::dvec4(positionModelSpace, 1.0);
		_stationPoints.push_back(k);
	}
}

} // namespace globebrowsing
} // namespace openspace
