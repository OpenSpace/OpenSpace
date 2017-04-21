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

#include <modules/globebrowsing/models/renderableroversurface.h>
#include <ghoul/logging/logmanager.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/engine/openspaceengine.h>
#include <modules/globebrowsing/chunk/chunknode.h>

#include <fstream>
#include <gdal_priv.h>
#include "ogrsf_frmts.h"

namespace {
	const std::string _loggerCat		= "RenderableRoverSurface";
	const char* keyRoverLocationPath	= "RoverLocationPath";
	const char* keyTextFilePath			= "TextFilePath";
	const char* keyModelPath			= "ModelPath";
	const char* keyTexturePath			= "TexturePath";
	const char* keyGeometryFile			= "GeometryFile";
	const char* keyRenderable			= "Renderable";
	const char* keyType					= "Type";
	const char* keyMultiModelGeometry	= "MultiModelGeometry";
	const char* keyName = "Name";
}

namespace openspace {

using namespace properties;

namespace globebrowsing {

RenderableRoverSurface::RenderableRoverSurface(const ghoul::Dictionary & dictionary)
	: Renderable(dictionary),
	_generalProperties({
		BoolProperty("enabled", "enabled", false)
	})
{
	if (!dictionary.getValue(keyRoverLocationPath, _roverLocationPath))
		throw ghoul::RuntimeError(std::string(keyRoverLocationPath) + " must be specified!");

	if (!dictionary.getValue(keyTextFilePath, _textFilePath))
		throw ghoul::RuntimeError(std::string(keyTextFilePath) + " must be specified!");

	if (!dictionary.getValue(keyModelPath, _modelPath))
		throw ghoul::RuntimeError(std::string(keyModelPath) + " must be specified!");

	if (!dictionary.getValue(keyTexturePath, _texturePath))
		throw ghoul::RuntimeError(std::string(keyTexturePath) + " must be specified!");

	std::string name;
	bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
	ghoul_assert(success, "Name was not passed to RenderableSite");

	_multiModelGeometry = "MultiModelGeometry";

	// Extract all coordinates from the GDAL dataset
	extractCoordinates();

	// Save the abspath because it changes when leaving the constructor.
	_absModelPath = absPath(_modelPath);

	_cachingModelProvider = std::make_shared<CachingSurfaceModelProvider>();
}

bool RenderableRoverSurface::initialize() {
	std::string name = owner()->name();
	auto parent = OsEng.renderEngine().scene()->sceneGraphNode(name)->parent();

	_globe = (globebrowsing::RenderableGlobe*)parent->renderable();
	_chunkedLodGlobe = _globe->chunkedLodGlobe();

	return true;
}

bool RenderableRoverSurface::deinitialize() {
	return false;
}

bool RenderableRoverSurface::isReady() const {
	return true;
}

void RenderableRoverSurface::render(const RenderData & data) {
}

void RenderableRoverSurface::update(const UpdateData & data) {
	_cachingModelProvider->update();
	//_pathChunks.clear();

	// Loop through all subsites.
	for (auto i : _subSites) {

		// Check which chunk each subsite corresponds to. Also extract the chunk hashkey to use it for
		// caching when loading models.
		Geodetic2 geodetic2 = Geodetic2{ i.lat, i.lon } / 180 * glm::pi<double>();
		Chunk chunk = _chunkedLodGlobe->findChunkNode(geodetic2).getChunk();
		double chunkLevel = chunk.surfacePatch().maximumTileLevel();
		uint64_t hashKey = chunk.tileIndex().hashKey();
		//auto search = _pathChunks.find(hashKey);

		// Only load models if the camera is so close to the surface so that there will be no further chunking.
		if(chunkLevel == 18) { 
			if(chunk.isVisible()) {

				// Load file names.
				std::string txtFilePath = "site" + i.site + "/" + "drive" + i.drive + "/" + "filenames.txt";
				std::vector<std::string> fileNames = extractFileNames(txtFilePath);

				// Loop through all file names and ask the caching model provider to return all available models \
				// for that chunk.
				for (auto fileName : fileNames) {
					ghoul::Dictionary modelDictionary;
					std::string pathToGeometry = _modelPath + "site" + i.site + "/" + "drive" + i.drive + "/" + fileName + ".obj";
					modelDictionary.setValue(keyGeometryFile, pathToGeometry);
					modelDictionary.setValue(keyType, _multiModelGeometry);
					modelDictionary.setValue(keyName, fileName);

					Model model;
					model.fileName = fileName;
					model.tileHashKey = hashKey;
					std::shared_ptr<Model> theModel = std::make_shared<Model>(model);

					_cachingModelProvider->getModel(modelDictionary, theModel);
				}
			}
		}

		/*glm::dvec3 tempPos = glm::dvec3(1.0, 1.0, 1.0);
		if (search != _pathChunks.end()) {
			search->second.push_back(tempPos);
		}
		else {
			std::vector<glm::dvec3> tempVector;
			tempVector.push_back(tempPos);
			_pathChunks.insert(std::make_pair(hashKey, tempVector));
		}*/

	}
}

std::vector<glm::dvec3> RenderableRoverSurface::pathChunkVector(const TileIndex& tileIndex) const {
	std::vector<glm::dvec3> pathVector = _pathChunks.find(tileIndex.hashKey())->second;

	return pathVector;
}

void RenderableRoverSurface::extractCoordinates() {
	std::fstream in(_roverLocationPath.c_str());

	if (!in.is_open())
		throw ghoul::FileNotFoundError(_roverLocationPath);

	GDALDataset *poDS;
	poDS = (GDALDataset*)GDALOpenEx(_roverLocationPath.c_str(), GDAL_OF_VECTOR, NULL, NULL, NULL);
	if (poDS == NULL) {
		LERROR("Could not open .shp file");
	}

	OGRLayer *poLayer = poDS->GetLayerByName("rover_locations");

	OGRFeature *poFeature;
	poLayer->ResetReading();

	while ((poFeature = poLayer->GetNextFeature()) != NULL) {

		// Extract coordinates from OGR
		std::string frame = poFeature->GetFieldAsString("frame");
		std::string site = poFeature->GetFieldAsString("site");
		std::string drive = poFeature->GetFieldAsString("drive");
		double lat = poFeature->GetFieldAsDouble("plcl");
		double lon = poFeature->GetFieldAsDouble("longitude");

		// Saves all coordinates for rendering the path and only site coordinates for rendering sites.
		// GetFieldAsDouble resturns zero (0) if field is empty.
		if (lat != 0 && lon != 0) {
			SubSite subSite;
			subSite.site = site;
			subSite.drive = drive;
			subSite.lat = lat;
			subSite.lon = lon;

			_coordinates.push_back(glm::fvec2(lat, lon));
			_subSites.push_back(subSite);
		}
		OGRFeature::DestroyFeature(poFeature);
	}
	GDALClose(poDS);
}

std::vector<std::string> RenderableRoverSurface::extractFileNames(const std::string filePath) {

	std::string path = _absModelPath + filePath;
	std::ifstream myfile(path);
	
	std::vector<std::string> fileNames;
	if (myfile.is_open()) {
		while (std::getline(myfile, _textFilePath)) {
			fileNames.push_back(_textFilePath);
		}
		myfile.close();
	}

	return fileNames;
}

} // namespace globebrowsing
} // namepsace openspace


