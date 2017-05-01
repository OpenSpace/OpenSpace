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

#include <ghoul/io/texture/texturereader.h>

#include <fstream>
#include <gdal_priv.h>
#include "ogrsf_frmts.h"
#include <glm/gtx/quaternion.hpp>

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
	const char* keyName					= "Name";
	const char* keyPathToTexture		= "PathToTexture";
}

namespace openspace {

using namespace properties;

namespace globebrowsing {

RenderableRoverSurface::RenderableRoverSurface(const ghoul::Dictionary & dictionary)
	: Renderable(dictionary),
	_generalProperties({
		BoolProperty("enabled", "enabled", false)
	})
	, _debugModelRotation("modelrotation", "Model Rotation", glm::vec3(0.0f), glm::vec3(0.0f), glm::vec3(360.0f))
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

	addProperty(_debugModelRotation);

	// Save the abspath because it changes when leaving the constructor.
	_absModelPath = absPath(_modelPath);
	_absTexturePath = absPath(_texturePath);
	_cachingModelProvider = std::make_shared<CachingSurfaceModelProvider>(this);
}

bool RenderableRoverSurface::initialize() {
	std::string name = owner()->name();
	auto parent = OsEng.renderEngine().scene()->sceneGraphNode(name)->parent();

	_globe = (globebrowsing::RenderableGlobe*)parent->renderable();
	_chunkedLodGlobe = _globe->chunkedLodGlobe();

	RenderEngine& renderEngine = OsEng.renderEngine();
	_programObject = renderEngine.buildRenderProgram("RenderableRoverSurface",
		"${MODULE_BASE}/shaders/model_vs.glsl",
		"${MODULE_BASE}/shaders/model_fs.glsl");

	return true;
}

bool RenderableRoverSurface::deinitialize() {
	return false;
}

bool RenderableRoverSurface::isReady() const {
	return true;
}

void RenderableRoverSurface::render(const RenderData & data) {
	_programObject->activate();
	for (auto model : _models) {
		glm::dmat4 globeTransform = _globe->modelTransform();
		
		glm::dvec3 positionWorldSpace = globeTransform * glm::dvec4(model->cartesianPosition, 1.0);

		// debug rotation controlled from GUI
		glm::mat4 unitMat4(1);
		glm::vec3 debugEulerRot = glm::radians(_debugModelRotation.value());

		//debugEulerRot.x = glm::radians(146.f);
		//debugEulerRot.y = glm::radians(341.f);
		//debugEulerRot.z = glm::radians(79.f);

		glm::mat4 rotX = glm::rotate(unitMat4, debugEulerRot.x, glm::vec3(1, 0, 0));
		glm::mat4 rotY = glm::rotate(unitMat4, debugEulerRot.y, glm::vec3(0, 1, 0));
		glm::mat4 rotZ = glm::rotate(unitMat4, debugEulerRot.z, glm::vec3(0, 0, 1));

		glm::dmat4 debugModelRotation = rotX * rotY * rotZ;

		// Rotation to make model up become normal of position on ellipsoid
		glm::dvec3 surfaceNormal = _globe->ellipsoid().geodeticSurfaceNormal(model->siteCoordinate);

		surfaceNormal = glm::normalize(surfaceNormal);
		float cosTheta = dot(glm::dvec3(0, 0, 1), surfaceNormal);
		glm::dvec3 rotationAxis;

		rotationAxis = cross(glm::dvec3(0, 0, 1), surfaceNormal);

		float s = sqrt((1 + cosTheta) * 2);
		float invs = 1 / s;

		glm::dquat rotationMatrix = glm::dquat(s * 0.5f, rotationAxis.x * invs, rotationAxis.y * invs, rotationAxis.z * invs);

		glm::dvec3 xAxis = _globe->ellipsoid().geodeticSurfaceNorthPoleTangent(positionWorldSpace);

		if (xAxis.x == 0 && xAxis.y == 0 && xAxis.z == 0) {
			LERROR("PLANE AND LINE HAS SAME");
		}
		
		glm::dvec4 test = glm::rotate(rotationMatrix, glm::dvec4(0, -1, 0, 1));
		
		glm::dvec3 testa = glm::dvec3(test.x, test.y, test.z);

		float cosTheta2 = dot(testa, xAxis);
		glm::dvec3 rotationAxis2;

		rotationAxis2 = cross(testa, xAxis);

		float s2 = sqrt((1 + cosTheta2) * 2);
		float invs2 = 1 / s2;

		glm::quat rotationMatrix2 = glm::quat(s2 * 0.5f, rotationAxis2.x * invs2, rotationAxis2.y * invs2, rotationAxis2.z * invs2);

		glm::dmat4 modelTransform =
			glm::translate(glm::dmat4(1.0), positionWorldSpace) *
			glm::dmat4(data.modelTransform.rotation) *
			glm::dmat4(glm::toMat4(rotationMatrix2)) *
			glm::dmat4(glm::toMat4(rotationMatrix)) *
			debugModelRotation;

		//glDisable(GL_CULL_FACE);

		glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;
		glm::vec3 directionToSun = glm::normalize(_sunPos - positionWorldSpace);
		glm::vec3 directionToSunViewSpace = glm::mat3(data.camera.combinedViewMatrix()) * directionToSun;

		_programObject->setUniform("transparency", 1.0f);
		_programObject->setUniform("directionToSunViewSpace", directionToSunViewSpace);
		_programObject->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
		_programObject->setUniform("projectionTransform", data.camera.projectionMatrix());
		_programObject->setUniform("performShading", false);
		_programObject->setUniform("fading", 1.0f);

		model->geometry->setUniforms(*_programObject);

		ghoul::opengl::TextureUnit unit;
		unit.activate();
		model->texture->bind();
		_programObject->setUniform("texture1", unit);
		model->geometry->render();		
	}
	_programObject->deactivate();
}

void RenderableRoverSurface::update(const UpdateData & data) {
	_cachingModelProvider->update(this);
	//_pathChunks.clear();

	_models.clear();
	_sunPos = OsEng.renderEngine().scene()->sceneGraphNode("Sun")->worldPosition();
	// Loop through all subsites.

	globebrowsing::Geodetic2 temp;
	for (auto i : _subSites) {
		if (i.frame == "SITE") {
			temp = globebrowsing::Geodetic2{ i.lat, i.lon } / 180 * glm::pi<double>();
		}
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

					std::string tempFileName = fileName;
					tempFileName[13] = 'R';
					tempFileName[14] = 'A';
					tempFileName[15] = 'S';

					std::string pathToTexture = _absTexturePath + "site" + i.site + "\\" + "drive" + i.drive +
						"\\" + tempFileName + ".png";

					modelDictionary.setValue(keyGeometryFile, pathToGeometry);
					modelDictionary.setValue(keyType, _multiModelGeometry);
					modelDictionary.setValue(keyName, fileName);
					modelDictionary.setValue(keyPathToTexture, pathToTexture);

					Model model;
					model.fileName = fileName;
					model.tileHashKey = hashKey;
					model.lat = i.lat;
					model.lon = i.lon;
					model.siteCoordinate = temp;
					std::shared_ptr<Model> theModel = std::make_shared<Model>(std::move(model));

					_models = _cachingModelProvider->getModels(modelDictionary, theModel);

					calculateSurfacePosition();
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
			std::string type = "site";
			SubSite subSite;
			subSite.site = convertString(site, type);
			type = "drive";
			subSite.drive = convertString(drive, type);
			subSite.lat = lat;
			subSite.lon = lon;
			subSite.frame = frame;

			_coordinates.push_back(glm::fvec2(lat, lon));
			_subSites.push_back(subSite);
		}
		OGRFeature::DestroyFeature(poFeature);
	}
	GDALClose(poDS);
}

void RenderableRoverSurface::calculateSurfacePosition() {
	for (auto i : _models) {
		globebrowsing::Geodetic2 geoTemp = globebrowsing::Geodetic2{ i->lat, i->lon } / 180 * glm::pi<double>();
		glm::dvec3 positionModelSpaceTemp = _globe->ellipsoid().cartesianSurfacePosition(geoTemp);
		double heightToSurface = _globe->getHeight(positionModelSpaceTemp);

		globebrowsing::Geodetic3 geo3 = globebrowsing::Geodetic3{ geoTemp, heightToSurface + 2.0 };
		i->cartesianPosition  = _globe->ellipsoid().cartesianPosition(geo3);
	}
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

std::string RenderableRoverSurface::convertString(std::string sitenr, std::string type) {
	int k = std::stoi(sitenr);
	
	std::string temp;
	if (type == "site") {
		if (k < 10) {
			temp = "00" + std::to_string(k);
		}
		else if (k < 100) {
			temp = "0" + std::to_string(k);
		}
	} 
	else if (type == "drive") {
		if (k < 10) {
			temp = "000" + std::to_string(k);
		}
		else if (k < 100) {
			temp = "00" + std::to_string(k);
		}
		else if (k < 1000){
			temp = "0" + std::to_string(k);
		}
		else {
			temp = std::to_string(k);
		}
	}
	return temp;
}

} // namespace globebrowsing
} // namepsace openspace


