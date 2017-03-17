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
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/filesystem/filesystem.h>
#include <modules/base/rendering/modelgeometry.h>

#include <fstream>
#include "ogr_geometry.h"
#include "ogrsf_frmts.h"
#include <gdal_priv.h>
#include <iostream>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/textureunit.h>

namespace {
	static const std::string _loggerCat = "RenderableSite";
}

namespace openspace {

using namespace properties;

namespace globebrowsing {
	
RenderableSite::RenderableSite(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _textureTxtPath("textureTxtpath", "Texture txt Path")
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

	// Get absolute path to txt file containing list of all textures
	std::string name;
	bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
	ghoul_assert(success, "Name was not passed to RenderableSite");

	std::string textureTxtPath = "";
	success = dictionary.getValue("TerrainTextures.Filepath", textureTxtPath);

	if (success) {
		_textureTxtPath = absPath(textureTxtPath);
		// Get the file texture file names
		_textureFileNames = loadTexturePaths(_textureTxtPath);
	}		
	
	if (_isReady) {
		_renderableExplorationPath = std::make_shared<RenderableExplorationPath>(*this, _pathCoordinates);
	}
	
	ghoul::Dictionary modelDic;

	if (dictionary.getValue("TerrainModel", modelDic)) {
		int i = 0;
		std::string modelFilepath;
		modelDic.getValue("Filepath", modelFilepath);
		modelDic.setValue("GeometryFile", modelFilepath + "/NLB_486003207RAS_F0481570NCAM00353M1.obj");
		
		modelgeometry::ModelGeometry* m = modelgeometry::ModelGeometry::createFromDictionary(modelDic);
		
		_models.push_back(Models());

		std::string k;
		modelDic.getValue("TexturePath", k);
		LINFO("k : " << k);
		_models.at(i)._texturePath = absPath(k + "/NLB_486003207RAS_F0481570NCAM00353M1.png");
		_models.at(i)._model = m;
	}
}
	
bool RenderableSite::initialize() {
	_renderableExplorationPath->initialize();


	for (auto it = _models.begin(); it != _models.end(); ++it) {
		(*it)._model->initialize(this);
	}

	loadTexture();

	return true;
}

bool RenderableSite::deinitialize() {
	_renderableExplorationPath->deinitialize();
	
	return true;
}

bool RenderableSite::isReady() const {
	return true;
}

void RenderableSite::render(const RenderData& data) {
	_renderableExplorationPath->render(data);

}

void RenderableSite::update(const UpdateData & data) {
	_renderableExplorationPath->update(data);

}

std::vector<std::string> RenderableSite::loadTexturePaths(std::string absoluteFilePath)
{
	std::string fileName;
	std::ifstream myfile(absoluteFilePath);
	std::vector<std::string> fileNameVector;

	if (myfile.is_open()) {
		while (std::getline(myfile, fileName)) {
			LERROR(fileName);
			fileNameVector.push_back(fileName);
		}
		myfile.close();
	}
	else
		LERROR("Could not open file");

	return fileNameVector;
}

bool RenderableSite::extractCoordinates() {
	GDALDataset *poDS;
	poDS = (GDALDataset*)GDALOpenEx(_filePath.c_str(), GDAL_OF_VECTOR, NULL, NULL, NULL);
	if (poDS == NULL) {
		LERROR("Could not open file");
	}

	OGRLayer *poLayer = poDS->GetLayerByName("rover_locations");

	//_coordMap = std::map<int, SiteInformation>();

	OGRFeature *poFeature;
	poLayer->ResetReading();

	while ((poFeature = poLayer->GetNextFeature()) != NULL) {

		// Extract coordinates from OGR
		std::string frame = poFeature->GetFieldAsString("frame");
		int site = poFeature->GetFieldAsInteger("site");
		int sol = poFeature->GetFieldAsInteger("sol");
		double lat = poFeature->GetFieldAsDouble("plcl");
		double lon = poFeature->GetFieldAsDouble("longitude");
		//LERROR(frame);

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

		// Saves all coordinates for rendering the path and only site coordinates for rendering sites.
		if(lat != 0 && lon != 0) {
			_pathCoordinates.push_back(glm::dvec2(lat, lon));

			if (frame == "SITE") {
				_siteCoordinates.push_back(glm::dvec2(lat, lon));
			}
		}
		OGRFeature::DestroyFeature(poFeature);
	}
	GDALClose(poDS);
	
	return (_pathCoordinates.size() != 0);
	//return (_coordMap.size() != 0);
}

void RenderableSite::loadTexture() {
	for (auto it = _models.begin(); it != _models.end(); ++it) {
		(*it)._texture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath((*it)._texturePath)));

		if ((*it)._texture) {
			LDEBUG("Loaded texture from: " << absPath((*it)._texturePath));
			(*it)._texture->uploadTexture();

			(*it)._texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
		}
	}
}

} // namespace globebrowsing
} // namespace openspace
