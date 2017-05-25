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
	, _fading(0.0)
	, _vertexBufferID(0)
	, _vaioID(0)
	, _isEnabled(properties::BoolProperty("enabled", "enabled", false))
{}

RenderableExplorationPath::~RenderableExplorationPath() {}

bool RenderableExplorationPath::initialize(RenderableGlobe* globe, const std::vector<Geodetic2> geodetics) {
	_globe = globe;
	_geodetics = geodetics;

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
	if (_geodetics.size() == 0) return false;

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
		GL_STATIC_DRAW);

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
		// Only show the path when camera is close enough
		if (_currentLevel > 0 && _fading < 1.f)
			_fading += 0.01f;
		else if (_currentLevel == 0 && _fading > 0.f)
			_fading -= 0.01f;

		// Model transform and view transform needs to be in double precision
		glm::dmat4 globeModelTransform = _globe->modelTransform();
		glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * globeModelTransform;

		_pathShader->activate();

		// Passing model view transform as double to maintain precision for vertices.
		// Otherwise the path will be twitching.
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

void RenderableExplorationPath::update(const UpdateData& data) {
}

void RenderableExplorationPath::setLevel(const int level) {
	_currentLevel = level;
	if (level > lastLevel) {
		recalculateCartesianPathCoordinates();
		lastLevel = level;
	}
}

void RenderableExplorationPath::calculatePathModelCoordinates() {
	for (auto geodetic : _geodetics) {
		glm::dvec3 positionModelSpace = _globe->ellipsoid().cartesianSurfacePosition(geodetic);
		_stationPointsModelCoordinates.push_back(glm::dvec4(positionModelSpace, 1.0));
	}
}

void RenderableExplorationPath::recalculateCartesianPathCoordinates() {
	_stationPointsModelCoordinates.clear();
	for (auto geodetic : _geodetics) {
		glm::dvec3 positionModelSpaceTemp = _globe->ellipsoid().cartesianSurfacePosition(geodetic);
		double heightToSurface = _globe->getHeight(positionModelSpaceTemp);

		globebrowsing::Geodetic3 geo3 = globebrowsing::Geodetic3{ geodetic, heightToSurface + 0.5 };
		glm::dvec3 tempPos2 = _globe->ellipsoid().cartesianPosition(geo3);
		_stationPointsModelCoordinates.push_back(glm::dvec4(tempPos2, 1.0));
	}

	// Buffer new data
	glBindVertexArray(_vaioID);
	glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferID);

	glBufferData(GL_ARRAY_BUFFER, _stationPointsModelCoordinates.size() * sizeof(_stationPointsModelCoordinates[0]),
		NULL, GL_STATIC_DRAW);
	glBufferSubData(GL_ARRAY_BUFFER, 0,
		_stationPointsModelCoordinates.size() * sizeof(_stationPointsModelCoordinates[0]),
		&_stationPointsModelCoordinates[0]);

	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, 0);

	glBindVertexArray(0);
}

} // namespace globebrowsing
} // namespace openspace
