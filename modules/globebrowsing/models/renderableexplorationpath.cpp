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


namespace {
	static const std::string _loggerCat = "RenderableExplorationPath";
}
namespace openspace {
RenderableExplorationPath::RenderableExplorationPath(const ghoul::Dictionary& dictionary) 
	: Renderable(dictionary)
	, _roverPath(nullptr)
	, _shader(nullptr)
	, _globe(nullptr)
	, _isEnabled(properties::BoolProperty("enabled", "enabled", false))
{
	bool isEnabled;
	if (dictionary.getValue("Enabled", isEnabled)) _isEnabled = isEnabled;

	std::string filePath;
	if (dictionary.getValue("FilePath", filePath)) _filePath = filePath;
}

bool RenderableExplorationPath::initialize() {

	// Getting the parent renderable to be able to calculate world coordinates
	auto parent = OsEng.renderEngine().scene()->sceneGraphNode(this->owner()->name())->parent();
	_globe = (globebrowsing::RenderableGlobe *)parent->renderable();

	if (_shader == nullptr) {
		_shader = OsEng.renderEngine().buildRenderProgram(
			"RoverPath",
			"${MODULE_GLOBEBROWSING}/shaders/roverpath_vs.glsl",
			"${MODULE_GLOBEBROWSING}/shaders/roverpath_fs.glsl"
		);
		if (!_shader)
			return false;
	}

	//Temp points
	_stationPoints.push_back(glm::dvec2(-4.7, 137.4));
	_stationPoints.push_back(glm::dvec2(-4.71, 137.6));

	calculatePathModelCoordinates();

	_roverPath = new RoverPath(_stationPoints);

	return true;
}

bool RenderableExplorationPath::deinitialize() {
	delete _roverPath;
	_roverPath = nullptr;

	RenderEngine& renderEngine = OsEng.renderEngine();
	if (_shader) {
		renderEngine.removeRenderProgram(_shader);
		_shader = nullptr;
	}

	return false;
}


bool RenderableExplorationPath::isReady() const {
	return _isReady;
}

void RenderableExplorationPath::render(const RenderData& data) {
	//_roverPath->render();
}

void RenderableExplorationPath::update(const UpdateData& data) {
	calculatePathWorldCoordinates();
}

void RenderableExplorationPath::calculatePathModelCoordinates() {
	globebrowsing::Geodetic2 geo;
	glm::dvec3 positionModelSpace;

	for (auto i : _stationPoints) {
		geo = globebrowsing::Geodetic2{ i.x, i.y } / 180 * glm::pi<double>();
		positionModelSpace = _globe->ellipsoid().cartesianSurfacePosition(geo);
		_stationPointsWorldCoordinates.push_back(positionModelSpace);
	}
}

void RenderableExplorationPath::calculatePathWorldCoordinates() {
	glm::dmat4 modelTransform = _globe->modelTransform();
	for (auto i : _stationPointsWorldCoordinates) {
		i = modelTransform * glm::dvec4(i, 1.0);
		LERROR("VertexCoords: " << i[0] << " " << i[1] << " " << i[2]);
	}
}

}
