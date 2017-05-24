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

#include <modules/globebrowsing/tile/loadjob/surfacemodelloadjob.h>
#include <modules/base/rendering/multimodelgeometry.h>
#include <ghoul/io/texture/texturereader.h>

namespace {
	const std::string _loggerCat = "SurfaceModelLoadJob";
	const char* keyPathToTexture = "PathToTexture";
	const char* keyGeometryFile = "GeometryFile";
	const char* keyType = "Type";
}

namespace openspace {
namespace globebrowsing {

void SurfaceModelLoadJob::execute() {
	std::string levelString = std::to_string(_level);
	std::string pathToGeometryFolder = _subsite->pathToGeometryFolder + "level" + levelString + "//" + "site" + _subsite->site +
		"//" + "drive" + _subsite->drive + "//";
	std::string pathToTextureFolder = _subsite->pathToTextureFolder;
	std::string roverSurfaceModelGeometry = "AsyncMultiModelGeometry";

	_subsiteModels->site = _subsite->site;
	_subsiteModels->drive = _subsite->drive;
	_subsiteModels->subsiteCoordinate = Geodetic2{ _subsite->lat, _subsite->lon };
	_subsiteModels->siteCoordinate = Geodetic2{ _subsite->siteLat, _subsite->siteLon };
	_subsiteModels->level = _level;

	for (auto fileName : _subsite->fileNames) {
		// Set up a dictionary to load the model
		ghoul::Dictionary dictionary;
		ghoul::Dictionary dictionary2;
		std::string pathToGeometry = pathToGeometryFolder + "//" + fileName + ".obj";

		dictionary.setValue(keyGeometryFile, pathToGeometry);
		dictionary.setValue(keyType, roverSurfaceModelGeometry);

		// Create modelgeometry from dictionary
		Model model;
		model.geometry = std::make_shared<modelgeometry::AsyncMultiModelGeometry>(dictionary);

		// Load the corresponding texture;
		std::string tempFileName = fileName;
		tempFileName[13] = 'R';
		tempFileName[14] = 'A';
		tempFileName[15] = 'S';

		std::string textureFormat = SurfaceModelLoadJob::textureFormat(_subsite->site);
		std::string pathToTexture = pathToTextureFolder + "site" + _subsite->site +
			"//" + "drive" + _subsite->drive + "//" + tempFileName + textureFormat;
		model.texture = std::move(ghoul::io::TextureReader::ref().loadTexture(pathToTexture));

		_subsiteModels->models.push_back(std::make_shared<Model>(model));
	}
}

std::shared_ptr<SubsiteModels> SurfaceModelLoadJob::product() const {
	return _subsiteModels;
}

std::string SurfaceModelLoadJob::textureFormat(const std::string site) {
	int siteNumber = std::stoi(site);
	std::string textureFormat;
	if (siteNumber <= 21)
		textureFormat = ".jpg";
	else if (siteNumber > 21)
		textureFormat = ".png";

	return textureFormat;
}

} // globebrowsing
} // openspace