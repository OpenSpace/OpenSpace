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
	, _isEnabled(properties::BoolProperty("enabled", "enabled", false))
{
	if (!dictionary.getValue("Filepath", _filepath)) {
		throw std::runtime_error(std::string("Must define key Filepath"));
	}

	std::ifstream in(_filepath.c_str());

	if (!in.is_open()) {
		throw ghoul::FileNotFoundError(_filepath);
	}

	std::string json(std::istreambuf_iterator<char>(in), (std::istreambuf_iterator<char>()));
	_isReady = extractCoordinates();
	dictionary.getValue("Enabled", _isEnabled);
}

bool RenderableExplorationPath::extractCoordinates() {
	GDALDataset *poDS;
	poDS = (GDALDataset*)GDALOpenEx(_filepath.c_str(), GDAL_OF_VECTOR, NULL, NULL, NULL);
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
	
	if (_filepath.empty() || _coordMap.size() == 0) return false;

	return true;
}

bool RenderableExplorationPath::deinitialize() {
	
	return false;
}


bool RenderableExplorationPath::isReady() const {
	return _isReady;
}

void RenderableExplorationPath::render(const RenderData& data) {
}

void RenderableExplorationPath::update(const UpdateData& data) {

}

}
