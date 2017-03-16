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
#include <modules/globebrowsing/models/renderablesite.h>

#include <fstream>
#include "ogr_geometry.h"
#include "ogrsf_frmts.h"
#include <gdal_priv.h>

#include <ghoul/logging/logmanager.h>

namespace {
	static const std::string _loggerCat = "RenderableSite";
}

namespace openspace {

using namespace properties;

namespace globebrowsing {

RenderableSite::RenderableSite(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _pathShader(nullptr)
	, _globe(nullptr)
	, _generalProperties({
		BoolProperty("enabled", "enabled", true)
	})
{
	setName("RenderableSite");

	if (!dictionary.getValue("Filepath", _filePath)) {
		throw std::runtime_error(std::string("Must define key Filepath"));
	}

	std::ifstream in(_filePath.c_str());

	if (!in.is_open()) {
		throw ghoul::FileNotFoundError(_filePath);
	}

	std::string json(std::istreambuf_iterator<char>(in), (std::istreambuf_iterator<char>()));
	_isReady = extractCoordinates();

	if (_isReady) {
		_renderableExplorationPath = std::make_shared<RenderableExplorationPath>(*this, _pathCoordinates);
	}
}
	
bool RenderableSite::initialize() {
	_renderableExplorationPath->initialize();

	return true;
}

bool RenderableSite::deinitialize() {
	return false;
}

bool RenderableSite::isReady() const {
	return true;
}

void RenderableSite::render(const RenderData& data) {

}

void RenderableSite::update(const UpdateData & data) {

}

bool RenderableSite::extractCoordinates() {
	GDALDataset *poDS;
	poDS = (GDALDataset*)GDALOpenEx(_filePath.c_str(), GDAL_OF_VECTOR, NULL, NULL, NULL);
	if (poDS == NULL) {
		LERROR("Could not open file");
	}

	OGRLayer *poLayer = poDS->GetLayerByName("rover_locations");

	_coordMap = std::map<int, SiteInformation>();

	OGRFeature *poFeature;
	poLayer->ResetReading();

	while ((poFeature = poLayer->GetNextFeature()) != NULL) {

		// Extract coordinates from OGR
		int site = poFeature->GetFieldAsInteger("site");
		int sol = poFeature->GetFieldAsInteger("sol");
		double lat = poFeature->GetFieldAsDouble("plcl");
		double lon = poFeature->GetFieldAsDouble("longitude");

		/*// Site allready exists
		if (_coordMap.find(site) != _coordMap.end()) {
			bool allreadyExists = false;
			std::vector<glm::dvec2> tempVec = _coordMap.at(site).lonlatCoordinates;

			// Check if the latitude and longitude allready exists in the vector
			for (auto i : tempVec) {
				if (i.x == lat && i.y == lon) {
					allreadyExists = true;
					break;
				}
			}

			// Only add new coordinates to prevent duplicates
			if (allreadyExists == false)
				_coordMap.at(site).lonlatCoordinates.push_back(glm::dvec2(lat, lon));
		}
		// Create a new site
		else {
			// Temp variables
			SiteInformation tempSiteInformation;
			std::vector<glm::dvec2> tempVec;

			// Push back the new coordinates
			tempVec.push_back(glm::dvec2(lat, lon));

			// Save variables in the tmep struct
			tempSiteInformation.sol = sol;
			tempSiteInformation.lonlatCoordinates = tempVec;

			_coordMap.insert(std::make_pair(site, tempSiteInformation));

		}*/
		if(lat != 0 && lon != 0)
			_pathCoordinates.push_back(glm::dvec2(lat, lon));

		OGRFeature::DestroyFeature(poFeature);
	}
	GDALClose(poDS);
	
	return (_pathCoordinates.size() != 0);
	//return (_coordMap.size() != 0);
}
}
} // namespace openspace