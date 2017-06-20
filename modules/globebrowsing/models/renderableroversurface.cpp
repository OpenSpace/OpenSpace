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
#include <modules/globebrowsing/models/modelprovider.h>
#include <modules/globebrowsing/models/sitemanager.h>
#include <openspace/scene/scene.h>

#include <ghoul/io/texture/texturereader.h>
#include <glm/gtx/quaternion.hpp>

#include <fstream>
#include <gdal_priv.h>
#include "ogrsf_frmts.h"
#include <glm/gtx/quaternion.hpp>

namespace {
	const std::string _loggerCat		= "RenderableRoverSurface";
	const char* keyRoverLocationPath	= "RoverLocationPath";
	const char* keyModelPath			= "ModelPath";
	const char* keyTexturePath			= "TexturePath";
	const char* keyName					= "Name";
	const char* keyAbsPathToTextures	= "AbsPathToTextures";
	const char* keyAbsPathToModels		= "AbsPathToModels";
	const std::string marsRoverModels	= "MarsRoverModels";
}

namespace openspace {

using namespace properties;

namespace globebrowsing {
RenderableRoverSurface::RenderableRoverSurface(const ghoul::Dictionary & dictionary)
	: Renderable(dictionary)
	, _generalProperties({
			BoolProperty("enable", "Enabled", true),
			BoolProperty("enablePath", "Enable path", true),
			BoolProperty("lockSubsite", "Lock subsite", false),
			BoolProperty("useMastCam", "Show mastcam coloring", false),
			BoolProperty("enableDepth", "Enable depth", true),
			FloatProperty("heightProp", "Site height", 0.7f, 0.0f, 3.0f),
			IntProperty("maxLod", "Max LOD", 3, 1, 3)
	})
	, _debugModelRotation("modelrotation", "Model Rotation", glm::vec3(0.0f), glm::vec3(0.0f), glm::vec3(360.0f))
	, _modelSwitch()
	, _prevLevel(3)
	, _isFirst(true)
	, _isFirstLow(true)
	, _isFirstHigh(true)
	, _renderableSitePropertyOwner("LayersOfThis")
{
	if (!dictionary.getValue(keyRoverLocationPath, _roverLocationPath))
		throw ghoul::RuntimeError(std::string(keyRoverLocationPath) + " must be specified!");

	if (!dictionary.getValue(keyModelPath, _modelPath))
		throw ghoul::RuntimeError(std::string(keyModelPath) + " must be specified!");

	if (!dictionary.getValue(keyTexturePath, _texturePath))
		throw ghoul::RuntimeError(std::string(keyTexturePath) + " must be specified!");

	std::string name;
	bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
	ghoul_assert(success, "Name was not passed to RenderableSite");

	// Save abs path to the models and texture folders because the abs path is changed when leaving constructor.
	_absModelPath = absPath(_modelPath);
	_absTexturePath = absPath(_texturePath);

	// Extract all subsites that has models
	ghoul::Dictionary tempDictionary;
	tempDictionary.setValue(keyRoverLocationPath, _roverLocationPath);
	tempDictionary.setValue(keyAbsPathToTextures, _absTexturePath);
	tempDictionary.setValue(keyAbsPathToModels, _absModelPath);
	_subsitesWithModels = RoverPathFileReader::extractSubsitesWithModels(tempDictionary);

	// Extract all subsites
	ghoul::Dictionary tempDictionary2;
	tempDictionary2.setValue(keyRoverLocationPath, _roverLocationPath);
	_subsites = RoverPathFileReader::extractAllSubsites(tempDictionary2);

	addProperty(_generalProperties.enablePath);
	addProperty(_generalProperties.lockSubsite);
	addProperty(_generalProperties.useMastCam);
	addProperty(_generalProperties.enableDepth);
	addProperty(_generalProperties.heightProp);
	addProperty(_generalProperties.maxLod);
	addProperty(_debugModelRotation);
	
	_siteManager = std::make_shared<SiteManager>(marsRoverModels, _subsitesWithModels);
	addPropertySubOwner(_siteManager.get());
	
	_cachingModelProvider = std::make_shared<CachingSurfaceModelProvider>(this);
	_renderableExplorationPath = std::make_shared<RenderableExplorationPath>();
}

bool RenderableRoverSurface::initialize() {
	std::vector<Geodetic2> allCoordinates;
	std::vector<Geodetic2> coordinatesWithModel;

	for (auto subsite : _subsites) {
		allCoordinates.push_back(subsite->geodetic);
	}
	for (auto subsite : _subsitesWithModels) {
		coordinatesWithModel.push_back(subsite->geodetic);
	}
	
	std::string ownerName = owner()->name();
	_parent = OsEng.renderEngine().scene()->sceneGraphNode(ownerName)->parent();

	_globe = (globebrowsing::RenderableGlobe*)_parent->renderable();

	_renderableExplorationPath->initialize(_globe, allCoordinates, coordinatesWithModel);

	_chunkedLodGlobe = _globe->chunkedLodGlobe();

	_modelSwitch.initialize(_globe);

	_chunkedLodGlobe->addSites(_subsitesWithModels);

	RenderEngine& renderEngine = OsEng.renderEngine();
	_programObject = renderEngine.buildRenderProgram("RenderableRoverSurface",
		"${MODULE_GLOBEBROWSING}/shaders/fullsubsite_vs.glsl",
		"${MODULE_GLOBEBROWSING}/shaders/fullsubsite_fs.glsl");

	return true;
}

bool RenderableRoverSurface::deinitialize() {
	return false;
}

bool RenderableRoverSurface::isReady() const {
	return true;
}

void RenderableRoverSurface::render(const RenderData& data) {
	std::vector<std::vector<std::shared_ptr<Subsite>>> subSitesVector = _chunkedLodGlobe->subsites();

	if (subSitesVector.size() < 1) {
		return;
	}

	std::vector<std::shared_ptr<Subsite>> ss;
	ghoul::Dictionary modelDic;
	std::unique_ptr<ModelProvider> _modelProvider;
	int level;
	
	switch (_modelSwitch.getLevel(data)) {
		case LodModelSwitch::Mode::Low :	
			//Low
			if (_isFirstLow) LERROR("GOIING LOW");
			_isFirstLow = false;
			_isFirstHigh = true;
			_isFirst = true;
			modelDic.setValue("Type", "MultiModelProvider");
			_modelProvider = std::move(ModelProvider::createFromDictionary(modelDic));
			ss = _modelProvider->calculate(subSitesVector, data, _parent);
			level = 2;
			break;
		case LodModelSwitch::Mode::Close :
			//Close
			if (_isFirst) LERROR("GOING CLOSE");
			_isFirst = false;
			_isFirstLow = true;
			_isFirstHigh = true;
			
			modelDic.setValue("Type", "SingleModelProvider");
			_modelProvider = std::move(ModelProvider::createFromDictionary(modelDic));
			ss = _modelProvider->calculate(subSitesVector, data, _parent);
			level = 3;
			break;
		case LodModelSwitch::Mode::High :
			if (_isFirstHigh) LERROR("GOING HIGH");
			_isFirstHigh = false;
			_isFirstLow = true;
			_isFirst = true;
			//High up
			level = 1;
			break;
		case LodModelSwitch::Mode::Far :
			//Far away
			// Only used to decide if renderableexplorationpath should be visible or not atm.
			level = 0;
			break;
	}

	int lodCheck = level;
	if (_generalProperties.maxLod.value() < lodCheck)
		lodCheck = _generalProperties.maxLod.value();

	lockSubsite(ss);

	std::vector<std::shared_ptr<SubsiteModels>> vectorOfsubsiteModels;
	if(_generalProperties.lockSubsite.value())
		vectorOfsubsiteModels = _cachingModelProvider->getModels(_prevSubsites, lodCheck);
	else
		vectorOfsubsiteModels = _cachingModelProvider->getModels(ss, lodCheck);
	
	vectorOfsubsiteModels = calculateSurfacePosition(vectorOfsubsiteModels);

	_programObject->activate();
	for (auto subsiteModels : vectorOfsubsiteModels) {
		glm::dmat4 globeTransform = _globe->modelTransform();

		glm::dvec3 positionWorldSpace = globeTransform * glm::dvec4(subsiteModels->cartesianPosition, 1.0);
		glm::dvec3 positionWorldSpace2 = glm::dvec4(subsiteModels->cartesianPosition, 1.0);

		// debug rotation controlled from GUI
		glm::mat4 unitMat4(1);
		glm::vec3 debugEulerRot = glm::radians(_debugModelRotation.value());

		glm::mat4 rotX = glm::rotate(unitMat4, debugEulerRot.x, glm::vec3(1, 0, 0));
		glm::mat4 rotY = glm::rotate(unitMat4, debugEulerRot.y, glm::vec3(0, 1, 0));
		glm::mat4 rotZ = glm::rotate(unitMat4, debugEulerRot.z, glm::vec3(0, 0, 1));

		glm::dmat4 debugModelRotation = rotX * rotY * rotZ;

		// Rotation to make model up become normal of position on ellipsoid
		glm::dvec3 surfaceNormal = _globe->ellipsoid().geodeticSurfaceNormal(subsiteModels->siteGeodetic);

		surfaceNormal = glm::normalize(surfaceNormal);
		double cosTheta = dot(glm::dvec3(0.0, 0.0, 1.0), surfaceNormal);
		glm::dvec3 rotationAxis;

		rotationAxis = cross(glm::dvec3(0.0, 0.0, 1.0), surfaceNormal);

		double s = sqrt((1 + cosTheta) * 2);
		double invs = 1 / s;

		glm::dquat rotationMatrix = glm::dquat(s * 0.5f, rotationAxis.x * invs, rotationAxis.y * invs, rotationAxis.z * invs);

		glm::dvec3 xAxis = _globe->ellipsoid().geodeticSurfaceNorthPoleTangent(positionWorldSpace2);

		if (xAxis.x == 0 && xAxis.y == 0 && xAxis.z == 0) {
			LERROR("PLANE AND LINE HAS SAME");
		}

		glm::dvec4 test = glm::rotate(rotationMatrix, glm::dvec4(0, -1, 0, 1));

		glm::dvec3 testa = glm::dvec3(test.x, test.y, test.z);

		double  cosTheta2 = dot(testa, xAxis);
		glm::dvec3 rotationAxis2;

		rotationAxis2 = cross(testa, xAxis);

		double s2 = sqrt((1 + cosTheta2) * 2);
		double invs2 = 1 / s2;

		glm::quat rotationMatrix2 = glm::quat(s2 * 0.5, rotationAxis2.x * invs2, rotationAxis2.y * invs2, rotationAxis2.z * invs2);

		glm::dmat4 modelTransform =
			glm::translate(glm::dmat4(1.0), positionWorldSpace) *
			glm::dmat4(data.modelTransform.rotation) *
			glm::dmat4(glm::toMat4(rotationMatrix2)) *
			glm::dmat4(glm::toMat4(rotationMatrix)) *
			debugModelRotation;

		glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

		std::vector<glm::fvec3> cameraInfoCenter;
		std::vector<glm::fvec3> cameraInfoAxis;
		std::vector<glm::fvec3> cameraInfoHorizontal;
		std::vector<glm::fvec3> cameraInfoVector;

		for (auto cameraInfo : subsiteModels->cameraInfoVector) {
			ImgReader::PointCloudInfo mInfoTemp = cameraInfo;
			cameraInfoCenter.push_back(mInfoTemp._cameraCenter);
			cameraInfoAxis.push_back(mInfoTemp._cameraAxis);
			cameraInfoHorizontal.push_back(mInfoTemp._cameraHorizontal);
			cameraInfoVector.push_back(mInfoTemp._cameraVector);
		}

		const GLint locationCenter = _programObject->uniformLocation("camerasCenters");
		const GLint locationAxis = _programObject->uniformLocation("camerasAxes");
		const GLint locationHorizontal = _programObject->uniformLocation("camerasHorizontals");
		const GLint locationVector = _programObject->uniformLocation("camerasVectors");

		
		glUniform3fv(locationCenter, cameraInfoCenter.size(), reinterpret_cast<GLfloat *>(cameraInfoCenter.data()));
		glUniform3fv(locationAxis, cameraInfoAxis.size(), reinterpret_cast<GLfloat *>(cameraInfoAxis.data()));
		glUniform3fv(locationHorizontal, cameraInfoHorizontal.size(), reinterpret_cast<GLfloat *>(cameraInfoHorizontal.data()));
		glUniform3fv(locationVector, cameraInfoVector.size(), reinterpret_cast<GLfloat *>(cameraInfoVector.data()));

		_programObject->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
		_programObject->setUniform("projectionTransform", data.camera.projectionMatrix());
		_programObject->setUniform("useMastCamColor", _generalProperties.useMastCam.value());
		_programObject->setUniform("alpha", subsiteModels->alpha());

		std::vector<glm::fvec4> cameraColoredInfoCenter;
		std::vector<glm::fvec4> cameraColoredInfoAxis;
		std::vector<glm::fvec4> cameraColoredInfoHorizontal;
		std::vector<glm::fvec4> cameraColoredInfoVector;

		for (auto coloredCameraInfo : subsiteModels->coloredCameraInfoVector) {
			ImgReader::PointCloudInfo mInfoTemp = coloredCameraInfo;

			cameraColoredInfoCenter.push_back(subsiteModels->rotationMatrix * glm::vec4(mInfoTemp._cameraCenter, 1.0f));
			cameraColoredInfoAxis.push_back(subsiteModels->rotationMatrix * glm::vec4(mInfoTemp._cameraAxis, 1.0f));
			cameraColoredInfoHorizontal.push_back(subsiteModels->rotationMatrix * glm::vec4(mInfoTemp._cameraHorizontal, 1.0f));
			cameraColoredInfoVector.push_back(subsiteModels->rotationMatrix * glm::vec4(mInfoTemp._cameraVector, 1.0f));
				
		}
		
		const GLint locationColoredCenter = _programObject->uniformLocation("camerasColoredCenters");
		const GLint locationColoredAxis = _programObject->uniformLocation("camerasColoredAxes");
		const GLint locationColoredHorizontal = _programObject->uniformLocation("camerasColoredHorizontals");
		const GLint locationColoredVector = _programObject->uniformLocation("camerasColoredVectors");
		
		glUniform4fv(locationColoredCenter, cameraColoredInfoCenter.size(), reinterpret_cast<GLfloat *>(cameraColoredInfoCenter.data()));
		glUniform4fv(locationColoredAxis, cameraColoredInfoAxis.size(), reinterpret_cast<GLfloat *>(cameraColoredInfoAxis.data()));
		glUniform4fv(locationColoredHorizontal, cameraColoredInfoHorizontal.size(), reinterpret_cast<GLfloat *>(cameraColoredInfoHorizontal.data()));
		glUniform4fv(locationColoredVector, cameraColoredInfoVector.size(), reinterpret_cast<GLfloat *>(cameraColoredInfoVector.data()));

		_programObject->setUniform("size", static_cast<int>(cameraInfoCenter.size()));
		_programObject->setUniform("colorSize", static_cast<int>(cameraColoredInfoCenter.size()));
		if (subsiteModels->textures.size() > 0) {
			glActiveTexture(GL_TEXTURE0);
			subsiteModels->textureArray->bind();
			const GLint locationRoverTerrainTextures = _programObject->uniformLocation("roverTerrainTextures");
			glUniform1i(locationRoverTerrainTextures, 0);
		}
		
		if (subsiteModels->coloredTextures.size() > 0) {
			_programObject->setUniform("coloredTextureDimensions", subsiteModels->coloredTextures.at(0)->dimensions());

			glActiveTexture(GL_TEXTURE1);
			subsiteModels->coloredTextureArray->bind();
			const GLint locationRoverTerrainTextures2 = _programObject->uniformLocation("roverTerrainColoredTextures");
			glUniform1i(locationRoverTerrainTextures2, 1);
		}
		
		glEnable(GL_BLEND);
		if (!_generalProperties.enableDepth.value()) {
			glDisable(GL_DEPTH_TEST);
		}
		glDisable(GL_CULL_FACE);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		
		subsiteModels->model->render();
		glEnable(GL_CULL_FACE);
	}
	_programObject->deactivate();

	if (_generalProperties.enablePath.value()) {
		_renderableExplorationPath->setLevel(level);
		_renderableExplorationPath->render(data);
	}
	_prevLevel = level;
}

void RenderableRoverSurface::update(const UpdateData& data) {
	if (_generalProperties.enablePath.value()) {
		_renderableExplorationPath->update(data);
	}
	_cachingModelProvider->update();
}

std::vector<std::shared_ptr<SubsiteModels>> RenderableRoverSurface::calculateSurfacePosition(std::vector<std::shared_ptr<SubsiteModels>> vector) {
	for (auto subsiteModels : vector) {
		glm::dvec3 positionModelSpaceTemp = _globe->ellipsoid().cartesianSurfacePosition(subsiteModels->geodetic);
		double heightToSurface = _globe->getHeight(positionModelSpaceTemp);

		globebrowsing::Geodetic3 geo3 = globebrowsing::Geodetic3{ subsiteModels->geodetic , heightToSurface + _generalProperties.heightProp.value() };
		subsiteModels->cartesianPosition = _globe->ellipsoid().cartesianPosition(geo3);
	}
	return vector;
}

void RenderableRoverSurface::lockSubsite(std::vector<std::shared_ptr<Subsite>> subsites) {
	if (_generalProperties.lockSubsite.value() && _pressedOnce == false) {
		_prevSubsites = subsites;
		_pressedOnce = true;
	}
	else if (!_generalProperties.lockSubsite.value() && _pressedOnce == true) {
		_pressedOnce = false;
	}
}

} // namespace globebrowsing
} // namepsace openspace


