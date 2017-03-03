#include "renderableexplorationpath.h"
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

#include <fstream>
#include "ogr_geometry.h"
#include "ogrsf_frmts.h"
#include <gdal_priv.h>

namespace {
	static const std::string _loggerCat = "RenderableExplorationPath";
}
namespace openspace {
RenderableExplorationPath::RenderableExplorationPath(const ghoul::Dictionary& dictionary) 
	: Renderable(dictionary)
	, _pathShader(nullptr)
	, _siteShader(nullptr)
	, _globe(nullptr)
	, _fading(0.0)
	, _vertexBufferID(0)
	, _vaioID(0)
	, _isEnabled(properties::BoolProperty("enabled", "enabled", false))
{
	if (!dictionary.getValue("Filepath", _filePath)) {
		throw std::runtime_error(std::string("Must define key Filepath"));
	}

	std::ifstream in(_filePath.c_str());

	if (!in.is_open()) {
		throw ghoul::FileNotFoundError(_filePath);
	}

	std::string json(std::istreambuf_iterator<char>(in), (std::istreambuf_iterator<char>()));
	_isReady = extractCoordinates();
	dictionary.getValue("Enabled", _isEnabled);
}

bool RenderableExplorationPath::extractCoordinates() {
	GDALDataset *poDS;
	poDS = (GDALDataset*)GDALOpenEx(_filePath.c_str(), GDAL_OF_VECTOR, NULL, NULL, NULL);
	if (poDS == NULL) {
		LERROR("Could not open file");
	}

	OGRLayer *poLayer = poDS->GetLayerByName("test");

	_coordMap = std::map<std::string, glm::vec2>();

	OGRFeature *poFeature;
	poLayer->ResetReading();

	while ((poFeature = poLayer->GetNextFeature()) != NULL) {
		// Extract coordinates from OGR
		std::string site = poFeature->GetFieldAsString("site");

		if (_coordMap.find(site) != _coordMap.end()) {
			//Coordinates already found for this site
			continue;
		}

		OGRGeometry *poGeometry;
		poGeometry = poFeature->GetGeometryRef();
		if (poGeometry != NULL && wkbFlatten(poGeometry->getGeometryType()) == wkbPoint) {
			OGRPoint *poPoint = (OGRPoint*)poGeometry;
			_coordMap.insert(std::make_pair(site, glm::vec2(poPoint->getX(), poPoint->getY())));
		}
		OGRFeature::DestroyFeature(poFeature);
	}
	GDALClose(poDS);

	return (_coordMap.size() != 0);
}

bool RenderableExplorationPath::initialize() {

	// Getting the parent renderable to calculate rover model coordinates to world coordinates
	auto parent = OsEng.renderEngine().scene()->sceneGraphNode(this->owner()->name())->parent();
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
	if (_filePath.empty() || _coordMap.size() == 0) return false;

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
	return _isReady;
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

	// The distance from the camera to the position on the ellipsoid
	double distance = glm::length(cameraPositionModelSpace - positionOnEllipsoid);

	// Only show the path when camera is close enough. Especially GL_POINTS look bad
	// when camera is far form the globe. Will have to improve this.
	if (distance < 16737 && _fading < 1.f)
		_fading += 0.01f;
	else if (distance >= 16737 && _fading > 0.f)
		_fading -= 0.01f;

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

	_siteShader->activate();

	_siteShader->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
	_siteShader->setUniform("projectionTransform", data.camera.projectionMatrix());
	_siteShader->setUniform("fading", _fading);
	_siteShader->setUniform("color", glm::vec3(1.0, 1.0, 1.0));

	glBindVertexArray(_vaioID);
	glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferID);
	glPointSize(20.0f);
	glDrawArrays(GL_POINTS, 0, _stationPointsModelCoordinates.size());
	glBindVertexArray(0);

	_siteShader->deactivate();
}

void RenderableExplorationPath::update(const UpdateData& data) {

}

void RenderableExplorationPath::calculatePathModelCoordinates() {
	globebrowsing::Geodetic2 geo;
	glm::dvec3 positionModelSpace;
	for (auto i : _coordMap) {
		// The map has longitude first and lattitude after, need to switch
		geo = globebrowsing::Geodetic2{ i.second.y, i.second.x } / 180 * glm::pi<double>();
		positionModelSpace = _globe->ellipsoid().cartesianSurfacePosition(geo);
		_stationPointsModelCoordinates.push_back(glm::vec4(positionModelSpace, 1.0f));
	}
}

} // namespace openspace
