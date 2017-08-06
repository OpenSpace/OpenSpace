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
#include <openspace/engine/openspaceengine.h>
#include <modules/globebrowsing/tasks/imgreader.h>
#include <openspace/scene/scene.h>
//#include <GL/GL.h>
#include <fstream>
#include "ogr_geometry.h"
#include "ogrsf_frmts.h"
#include <gdal_priv.h>
#include <iostream>
#include <glm/gtx/quaternion.hpp>
#include <glm/gtx/vector_angle.hpp>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/textureunit.h>

template<typename Out>
static void split(const std::string &s, char delim, Out result) {
	std::stringstream ss;
	ss.str(s);
	std::string item;
	while (std::getline(ss, item, delim)) {
		*(result++) = item;
	}
}

static std::vector<std::string> split(const std::string &s, char delim) {
	std::vector<std::string> elems;
	split(s, delim, std::back_inserter(elems));
	return elems;
}

namespace {
	static const std::string _loggerCat = "RenderableSite";
}

namespace openspace {

	using namespace properties;

	namespace globebrowsing {
		RenderableSite::RenderableSite(const ghoul::Dictionary& dictionary)
			: Renderable(dictionary)
			, _textureTxtPath("textureTxtpath", "Texture txt Path")
			, _debugModelRotation("modelrotation", "Model Rotation", glm::vec3(0.0f), glm::vec3(0.0f), glm::vec3(360.0f))
			, _debugModelScale(properties::FloatProperty("modelscale", "Model Scale", 1.f, 0.5f, 100.f))
			, _debugModelCullface(properties::BoolProperty("modelcullface", "Model Cullfacing", false))
			, _debugModelMastCamColor(properties::BoolProperty("modelmastcam", "Mastcam coloring", false))
			, _debugUseUVCoord(properties::BoolProperty("useuvcoord", "Model UV coord texturing", false))
			, _debugUseMultipleTextures(properties::BoolProperty("oneModelMultipleTex", "Multiple Textures", false))
			, _hasLoopedOnce(false)
			, _isCloseEnough(false)
			, _recalculateHeight(properties::BoolProperty("recaulateHeight", "Recalculate Height", false))
			, _prevDebugModelMastCamColor(false)
			, _cameraToPointDistance(1000000000.0)
			, _generalProperties({
			BoolProperty("enabled", "enabled", false)
		})
			, _renderableSitePropertyOwner("Models")
		{

			// Get absolute path to txt file containing list of all textures
			std::string name;
			bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
			ghoul_assert(success, "Name was not passed to RenderableSite");

			setName(name);

			/// Site 6
			//_tempLonLat = glm::dvec2(-4.589484879, 137.449129568);
			// Site 23
			//_tempLonLat2 = glm::dvec2(-4.622434756, 137.413125724);
			// Site 47
			// _tempLonLat2 = glm::dvec2(-4.666798644, 137.371330572);
			// Site 48
			//_tempLonLatSite = glm::dvec2(-4.668255201, 137.371271221);
			// Site 49
			//_tempLonLat2 = glm::dvec2(-4.668262624, 137.370031147);
			// Site 54
			_tempLonLatSite = glm::dvec2(-4.678375489, 137.359359292);

			// Landingsite
			//_tempLonLat2 = glm::dvec2(-4.589466996, 137.441632997);
			// Site 23, drive 890
			//_tempLonLat = glm::dvec2(-4.623463408, 137.411840723);
			// Site 47, drive 1452
			// _tempLonLat = glm::dvec2(-4.667842681, 137.371359673);
			// Site 48, drive 1570
			//_tempLonLat = glm::dvec2(-4.668293194, 137.370020299);
			// Site 48, drive 2200
			//_tempLonLat = glm::dvec2(-4.668047895, 137.369953424);
			// Site 54, drive 2280
			_tempLonLat = glm::dvec2(-4.678641386, 137.357708149);

			if (name == "RoverSite") { //01360 
				_tempLonLatSite = glm::dvec2(-4.678375489, 137.359359292);
				_tempLonLat = glm::dvec2(-4.678641386, 137.357708149);
			}
			if (name == "RoverSiteINV" || name == "RoverSiteQUAT" || name == "RoverSiteNONE") { //00997
																								//_tempLonLatSite = glm::dvec2(-4.685180197, 137.355671327);
				_tempLonLatSite = glm::dvec2(-4.668255201, 137.371271221);
				_tempLonLat = glm::dvec2(-4.668293194, 137.370020299);
			}
			else if (name == "RoverSite3") { //00283
				_tempLonLatSite = glm::dvec2(-4.589484879, 137.449129568);
				_tempLonLat = glm::dvec2(-4.589471311, 137.449128359);
			}
			else if (name == "RoverSite981") {
				_tempLonLatSite = glm::dvec2(-4.666798644, 137.371330572);
				_tempLonLat = glm::dvec2(-4.667842681, 137.371359673);
			}

			if (name == "RoverSite1044") {
				_tempLonLatSite = glm::dvec2(-4.668255201, 137.371271221);
				_tempLonLat = glm::dvec2(-4.668047895, 137.369953424);
			}

			if (!dictionary.getValue("Filepath", _filePath)) {
				throw std::runtime_error(std::string("Must define key Filepath"));
			}

			std::ifstream in(_filePath.c_str());

			if (!in.is_open()) {
				throw ghoul::FileNotFoundError(_filePath);
			}

			std::string json(std::istreambuf_iterator<char>(in), (std::istreambuf_iterator<char>()));
			_isReady = extractCoordinates();

			addProperty(_debugModelRotation);
			addProperty(_debugModelScale);
			addProperty(_debugModelCullface);
			addProperty(_debugModelMastCamColor);
			addProperty(_recalculateHeight);
			addProperty(_debugUseUVCoord);

			std::string textureTxtPath = "";
			success = dictionary.getValue("TerrainTextures.Txtpath", textureTxtPath);

			if (success) {
				_textureTxtPath = absPath(textureTxtPath);
				_textureTxtPath = "D:/models/level1/site048/drive1570/filenames.txt";
				// Get the file texture file names
				_fileNames = loadTexturePaths(_textureTxtPath);
			}

			if (_isReady) {
				//_renderableExplorationPath = std::make_shared<RenderableExplorationPath>(*this, _pathLatlonCoordinates);
			}

			ghoul::Dictionary modelDic;

			std::vector<std::string> _textureFileNames;
			const clock_t begin_time = clock();

			if (dictionary.getValue("TerrainModel", modelDic)) {
				int count = 0;
				for (auto i : _fileNames) {
					if (count == 1) break;
					std::string modelFilepath;
					modelDic.getValue("ModelPath", modelFilepath);
					std::string extension = ".IMG";

					extension = ".obj";
					std::string tempfilename = "OBJ";
					modelDic.setValue("GeometryFile", modelFilepath + "/" + tempfilename + extension);

					std::unique_ptr<modelgeometry::ModelGeometry> m = modelgeometry::ModelGeometry::createFromDictionary(modelDic);

					_models.push_back(Models());
					std::string k;
					modelDic.getValue("TexturePath", k);

					std::size_t pos = _fileNames[count].find("XYR");

					//LERROR("CREATING MODEL " << _textureFileNames.at(count));
					//@TODO this must be refactored
					//_models.at(count)._texturePath = absPath(k + "/" + "NLB_486005346RAS_F0481570NCAM07813M1" + ".png");
					std::string texture_extension = ".png";
					if (name == "RoverSite3") {
						texture_extension = ".jpg";
					}

					for (int j = 0; j < _fileNames.size(); ++j) {
						_textureFileNames.push_back(_fileNames[j].substr(0, pos) + "RAS" + _fileNames[j].substr(pos + 3));
						_models.at(count)._texturePaths.push_back(absPath(k + "/" + _textureFileNames.at(j) + texture_extension));
					}

					//_textureFileNames.push_back(_fileNames[count    ].substr(0, pos) + "RAS" + _fileNames[count    ].substr(pos + 3));
					//_textureFileNames.push_back(_fileNames[count + 1].substr(0, pos) + "RAS" + _fileNames[count + 1].substr(pos + 3));
					//_textureFileNames.push_back(_fileNames[count + 2].substr(0, pos) + "RAS" + _fileNames[count + 2].substr(pos + 3));
					//_textureFileNames.push_back(_fileNames[count + 3].substr(0, pos) + "RAS" + _fileNames[count + 3].substr(pos + 3));
					//_textureFileNames.push_back(_fileNames[count + 4].substr(0, pos) + "RAS" + _fileNames[count + 4].substr(pos + 3));
					//_textureFileNames.push_back(_fileNames[count + 5].substr(0, pos) + "RAS" + _fileNames[count + 5].substr(pos + 3));

					
					//_models.at(count)._texturePaths.push_back(absPath(k + "/" + _textureFileNames.at(count + 0) + texture_extension));
					//_models.at(count)._texturePaths.push_back(absPath(k + "/" + _textureFileNames.at(count + 1) + texture_extension));
					//_models.at(count)._texturePaths.push_back(absPath(k + "/" + _textureFileNames.at(count + 2) + texture_extension));
					//_models.at(count)._texturePaths.push_back(absPath(k + "/" + _textureFileNames.at(count + 3) + texture_extension));
					//_models.at(count)._texturePaths.push_back(absPath(k + "/" + _textureFileNames.at(count + 4) + texture_extension));
					//_models.at(count)._texturePaths.push_back(absPath(k + "/" + _textureFileNames.at(count + 5) + texture_extension));

					//_models.at(count)._texturePath = absPath(k + "/" + _textureFileNames.at(count) + texture_extension);
					//_models.at(count)._texturePath2 = absPath(k + "/" + _textureFileNames.at(count + 1) + texture_extension);
					//_models.at(count)._texturePath3 = absPath(k + "/" + _textureFileNames.at(count + 2) + texture_extension);
					//_models.at(count)._texturePath2 = absPath(k + "/0997ML0044110000405009E01_DRCL.JPG");
					//_textureFileNames.at(count)


					/*if (FileSys.fileExists(_models.at(count)._texturePath)) {
						LERROR("FILE EXISTS");
					}
					else {
						LERROR("COULD NOT FIND IT";);
					}*/

					_models.at(count)._model = std::move(m);
					_models.at(count)._programObject = nullptr;
					//_models.at(count)._textures.at(0) = nullptr;
					//_models.at(count)._textures.at(1) = nullptr;
					//_models.at(count)._textures.at(2) = nullptr;

					count++;
				}
				LINFO("FINISHED MODEL CREATION IN : " << float(clock() - begin_time) / CLOCKS_PER_SEC);
			}
			std::string testare = "ouups";
			properties::BoolProperty temp2 = properties::BoolProperty(testare, testare, true);
			_renderableSitePropertyOwner.addProperty(temp2);
			_renderableSitePropertyOwner.
				addPropertySubOwner(_renderableSitePropertyOwner);
			addProperty(_debugUseMultipleTextures);
		}

		bool RenderableSite::initialize() {


			//TODO: Check if _renderableExplorationPath has been created before calling initialize.
			//_renderableExplorationPath->initialize(_globe, );

			for (auto it = _models.begin(); it != _models.end(); ++it) {
				(*it)._model->initialize(this);
				if ((*it)._programObject == nullptr) {
					RenderEngine& renderEngine = OsEng.renderEngine();
					(*it)._programObject = renderEngine.buildRenderProgram("RenderableSite",
						"${MODULE_GLOBEBROWSING}/shaders/fullsubsite_vs.glsl", "${MODULE_GLOBEBROWSING}/shaders/fullsubsite_fs.glsl");

					if (!(*it)._programObject) return false;
				}
			}

			loadTexture();

			bool completeSuccess = true;
			for (auto it = _models.begin(); it != _models.end(); ++it) {
				//for (size_t i = 0; i < (*it)._textures)
				//if ((*it)._texture == nullptr) {
				//	completeSuccess = false;
				//}
			}

			std::string name = owner()->name();
			auto parent = OsEng.renderEngine().scene()->sceneGraphNode(name)->parent();
			_globe = (globebrowsing::RenderableGlobe *)parent->renderable();

			calculateSiteWorldCoordinates();

			for (auto it = _models.begin(); it != _models.end(); ++it) {

				GLuint texture = 0;

				GLsizei width = 2;
				GLsizei height = 2;
				GLsizei layerCount = (*it)._textures.size();
				GLsizei mipLevelCount = 1;

				glGenTextures(1, &texture);
				glBindTexture(GL_TEXTURE_2D_ARRAY, texture);
				//Allocate the storage.
				glTexStorage3D(GL_TEXTURE_2D_ARRAY, mipLevelCount, GL_RGBA8, 1024, 1024, layerCount);

				//Upload pixel data.
				//The first 0 refers to the mipmap level (level 0, since there's only 1)
				//The following 2 zeroes refers to the x and y offsets in case you only want to specify a subrectangle.
				//The final 0 refers to the layer index offset (we start from index 0 and have 2 levels).
				//Altogether you can specify a 3D box subset of the overall texture, but only one mip level at a time.
				for (int i = 0; i < layerCount; ++i) {
					glTexSubImage3D(GL_TEXTURE_2D_ARRAY, 0, 0, 0, i, 1024, 1024, 1, GL_RGBA, GL_UNSIGNED_BYTE, (*it)._textures.at(i)->pixelData());
				}
			}

			glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
			glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
			glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
			glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

			return completeSuccess;
		}

		bool RenderableSite::deinitialize() {
			//TODO: Check if _renderableExplorationPath has been created before calling deinitialize.
			//_renderableExplorationPath->deinitialize();

			for (auto it = _models.begin(); it != _models.end(); ++it) {

				if ((*it)._programObject) {
					OsEng.renderEngine().removeRenderProgram((*it)._programObject);
					(*it)._programObject = nullptr;
				}
			}
			return true;
		}

		bool RenderableSite::isReady() const {
			bool completeSuccess = true;
			for (auto it = _models.begin(); it != _models.end(); ++it) {
				//if ((*it)._texture == nullptr) {
				//	completeSuccess = false;
				//}
			}
			return completeSuccess;
		}

		void RenderableSite::render(const RenderData& data) {
			//TODO: Check if _renderableExplorationPath has been created before calling render.
			//_renderableExplorationPath->render(data);

			// Camera position in model space
			glm::dvec3 camPos = data.camera.positionVec3();
			glm::dmat4 inverseModelTransform = _globe->inverseModelTransform();
			glm::dvec3 cameraPositionModelSpace =
				glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));

			// Takes the coordinates for the station in the middle of the array
			// and calculates the position on the ellipsoid
			glm::dvec3 positionOnEllipsoid = glm::dvec3(_sitesModelCoordinates[48].stationPosition);

			// Temporary solution to trigger the calculation of new positions of the stations when the camera is 
			// less than 5000 meters from the "middle' station. Should possibly be moved do a distanceswitch.
			double heightToSurface = _globe->getHeight(positionOnEllipsoid);
			glm::dvec3 directionFromSurfaceToPointModelSpace = _globe->ellipsoid().
				geodeticSurfaceNormal(_globe->ellipsoid().cartesianToGeodetic2(positionOnEllipsoid));
			glm::dvec3 tempPos = glm::dvec3(positionOnEllipsoid) + heightToSurface * directionFromSurfaceToPointModelSpace;

			// The distance from the camera to the position on the ellipsoid
			_cameraToPointDistance = glm::length(cameraPositionModelSpace - tempPos);

			if (_cameraToPointDistance < 700.0 || _recalculateHeight) {
				_isCloseEnough = true;
				_recalculateHeight = !_recalculateHeight;
				_hasLoopedOnce = false;
			}

			glm::dmat4 globeModelTransform = _globe->modelTransform();
			int tempCount = 0;

			for (auto it = _models.begin(); it != _models.end(); ++it) {
				(*it)._programObject->activate();
				auto k = OsEng.renderEngine().scene()->sceneGraphNode("Mars");

				globebrowsing::RenderableGlobe* globe = (globebrowsing::RenderableGlobe *)k->renderable();

				//TODO: Change to dynamic coordinates
				globebrowsing::Geodetic2 geo = globebrowsing::Geodetic2{ _tempLonLat.x, _tempLonLat.y } / 180 * glm::pi<double>();
				glm::dvec3 positionModelSpace1 = globe->ellipsoid().cartesianSurfacePosition(geo);

				glm::dmat4 globeModelTransform = globe->modelTransform();
				glm::dvec3 positionWorldSpace2 = _sitesModelCoordinates[48].stationPosition;//glm::dvec4(positionModelSpace1, 1.0);
				glm::dvec3 positionWorldSpace = globeModelTransform * _sitesModelCoordinates[48].stationPosition;//glm::dvec4(positionModelSpace1, 1.0);

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

				float scale = _debugModelScale.value();
				globebrowsing::Geodetic2 geoSite = globebrowsing::Geodetic2{ _tempLonLatSite.x, _tempLonLatSite.y } / 180 * glm::pi<double>();
				globebrowsing::Geodetic2 equator = globebrowsing::Geodetic2{ 0.0, 0.0 } / 180 * glm::pi<double>();

				// Rotation to make model up become normal of position on ellipsoid
				glm::dvec3 surfaceNormal = _globe->ellipsoid().geodeticSurfaceNormal(geoSite);

				surfaceNormal = glm::normalize(surfaceNormal);
				float cosTheta = dot(glm::dvec3(0, 0, 1), surfaceNormal);
				glm::dvec3 rotationAxis;

				rotationAxis = cross(glm::dvec3(0, 0, 1), surfaceNormal);

				float s = sqrt((1 + cosTheta) * 2);
				float invs = 1 / s;

				glm::dquat rotationMatrix = glm::dquat(s * 0.5f, rotationAxis.x * invs, rotationAxis.y * invs, rotationAxis.z * invs);

				glm::dvec3 xAxis = _globe->ellipsoid().geodeticSurfaceNorthPoleTangent(positionWorldSpace2);

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
					glm::translate(glm::dmat4(1.0), positionWorldSpace) * // Translation
					glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
					glm::dmat4(glm::scale(glm::dmat4(1.f), glm::dvec3(scale, scale, scale)))*
					glm::dmat4(glm::toMat4(rotationMatrix2)) *
					glm::dmat4(glm::toMat4(rotationMatrix)) * //debug model rotation controlled from GUI
					debugModelRotation;
				//rotationMatrixCombined;
				glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

				//positionModelSpace1  originally data.modelTransform.translation
				glm::vec3 directionToSun = glm::normalize(_sunPos - positionWorldSpace);
				glm::vec3 directionToSunViewSpace = glm::mat3(data.camera.combinedViewMatrix()) * directionToSun;

				if (_prevDebugModelMastCamColor != _debugModelMastCamColor) {
					_prevDebugModelMastCamColor = _debugModelMastCamColor;
				}

				//(*it)._programObject->setUniform("transparency", 1.0f);
				//(*it)._programObject->setUniform("directionToSunViewSpace", directionToSunViewSpace);
				(*it)._programObject->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
				(*it)._programObject->setUniform("projectionTransform", data.camera.projectionMatrix());
				//(*it)._programObject->setUniform("performShading", false);
				//(*it)._programObject->setUniform("useMastCamColor", _debugModelMastCamColor);
				//(*it)._programObject->setUniform("useUVCoord", _debugUseUVCoord);
				//(*it)._programObject->setUniform("useMultipleTextures", _debugUseMultipleTextures);
				//(*it)._programObject->setUniform("fading", 1.0f);
				//
				//(*it)._programObject->setUniform("cameraCenter", glm::vec4(-0.240074, 0.851183, 2.06588, 1));
				//(*it)._programObject->setUniform("cameraAxis", glm::vec4(0.732752, 0.617675, -0.285543, 1));
				//(*it)._programObject->setUniform("cameraHorizontal", glm::vec4(1127.11, -642.278, -272.272, 1));
				//(*it)._programObject->setUniform("cameraVector", glm::vec4(16.512, 188.038, -1310.39, 1));

				//NLB_486003207XYR_F0481570NCAM00353M1

				ImgReader::PointCloudInfo mInfo = _cameraInfoVector.at(tempCount);
				//Hardcoded textures
				//(*it)._programObject->setUniform("cameraCenter", glm::vec4(mInfo._cameraCenter, 1));
				//(*it)._programObject->setUniform("cameraAxis", glm::vec4(mInfo._cameraAxis, 1));
				//(*it)._programObject->setUniform("cameraHorizontal", glm::vec4(mInfo._cameraHorizontal, 1));
				//(*it)._programObject->setUniform("cameraVector", glm::vec4(mInfo._cameraVector, 1));


				glm::mat4 totRot;
				totRot[0][0] = -0.632475;
				totRot[0][1] = 0.767837;
				totRot[0][2] = 0.10198;
				totRot[0][3] = 0;

				totRot[1][0] = 0.771819;
				totRot[1][1] = 0.613632;
				totRot[1][2] = 0.16658;
				totRot[1][3] = 0;

				totRot[2][0] = 0.0653283;
				totRot[2][1] = 0.184068;
				totRot[2][2] = -0.98074;
				totRot[2][3] = 0;

				totRot[3][0] = 0;
				totRot[3][1] = 0;
				totRot[3][2] = 0;
				totRot[3][3] = 1;

				//MeshGeneration(Error) -0.238914, 0.712723, 2.1259
				//MeshGeneration(Error) -0.80878, 0.00265542, -0.588104
				//MeshGeneration(Error) -2212.65, 13406.7, 1958.37
				//MeshGeneration(Error)  7399.61, 2886.77, -11193.6

				//Origin offset vector SMALL mast
				//(0.804422, 0.559539, -1.906076)
				//Origin offset vector BIG mast
				//(0.804372,0.559478,-1.90608)

				//Origin rotation quaternion SMALL mast
				//(0.8483046, -0.1269429, -0.2399421, -0.4546346)
				//Origin rotation quaternion BIG mast
				//(0.878211, -0.0871541, -0.434419, -0.180084)


				glm::quat qSmall = glm::quat(0.8483046, -0.1269429, -0.2399421, -0.4546346);
				glm::quat qBig = glm::quat(0.878211, -0.0871541, -0.434419, -0.180084);

				glm::quat qDiff = glm::inverse(qBig) * qSmall;

				glm::mat4 rotationDiff = glm::toMat4(qDiff);

				glm::vec3 transSmall = glm::vec3(0.804422, 0.559539, -1.906076);
				glm::vec3 transBig = glm::vec3(0.804372, 0.559478, -1.90608);

				glm::vec3 transDiff = transSmall - transBig;

				glm::mat4 translationDiff = glm::translate(glm::mat4(1.0), transDiff);

				//CORRESPONDING SMALL TEXTURE WITH COLOR
				//I BELIEVE THAT THESE VALUES COME FROM THE LBL FILE OF THE IMAGE
				//(*it)._programObject->setUniform("cameraCenter2",rotationDiff * glm::vec4(-0.238914, 0.712723, 2.1259, 1));
				//(*it)._programObject->setUniform("cameraAxis2", rotationDiff * glm::vec4(-0.80878, 0.00265542, -0.588104, 1));
				//(*it)._programObject->setUniform("cameraHorizontal2", rotationDiff * glm::vec4(-2212.65, 13406.7, 1958.37, 1));
				//(*it)._programObject->setUniform("cameraVector2", rotationDiff * glm::vec4(7399.61, 2886.77, -11193.6, 1));

				ImgReader::PointCloudInfo mInfo2 = _cameraInfoVector.at(tempCount + 1);
				//Hardcoded textures
				//(*it)._programObject->setUniform("cameraCenter2", glm::vec4(mInfo2._cameraCenter, 1));
				//(*it)._programObject->setUniform("cameraAxis2", glm::vec4(mInfo2._cameraAxis, 1));
				//(*it)._programObject->setUniform("cameraHorizontal2", glm::vec4(mInfo2._cameraHorizontal, 1));
				//(*it)._programObject->setUniform("cameraVector2", glm::vec4(mInfo2._cameraVector, 1));

				ImgReader::PointCloudInfo mInfo3 = _cameraInfoVector.at(tempCount + 2);
				//Hardcoded textures
				//(*it)._programObject->setUniform("cameraCenter3", glm::vec4(mInfo3._cameraCenter, 1));
				//(*it)._programObject->setUniform("cameraAxis3", glm::vec4(mInfo3._cameraAxis, 1));
				//(*it)._programObject->setUniform("cameraHorizontal3", glm::vec4(mInfo3._cameraHorizontal, 1));
				//(*it)._programObject->setUniform("cameraVector3", glm::vec4(mInfo3._cameraVector, 1));

				std::vector<glm::fvec3> cameraInfoCenter;
				std::vector<glm::fvec3> cameraInfoAxis;
				std::vector<glm::fvec3> cameraInfoHorizontal;
				std::vector<glm::fvec3> cameraInfoVector;

				for (size_t i = 0; i < _cameraInfoVector.size(); ++i) {
					ImgReader::PointCloudInfo mInfoTemp = _cameraInfoVector.at(i);
					cameraInfoCenter.push_back(mInfoTemp._cameraCenter);
					cameraInfoAxis.push_back(mInfoTemp._cameraAxis);
					cameraInfoHorizontal.push_back(mInfoTemp._cameraHorizontal);
					cameraInfoVector.push_back(mInfoTemp._cameraVector);
				}

				//(*it)._programObject->setUniform("camerasCenters", cameraInfoCenter);
				//(*it)._programObject->setUniform("camerasAxes", cameraInfoAxis);
				//(*it)._programObject->setUniform("camerasHorizontals", cameraInfoHorizontal);
				//(*it)._programObject->setUniform("camerasVectors", cameraInfoVector);

				const GLint locationCenter = (*it)._programObject->uniformLocation("camerasCenters");
				const GLint locationAxis = (*it)._programObject->uniformLocation("camerasAxes");
				const GLint locationHorizontal = (*it)._programObject->uniformLocation("camerasHorizontals");
				const GLint locationVector = (*it)._programObject->uniformLocation("camerasVectors");

				glUniform3fv(locationCenter, cameraInfoCenter.size(), reinterpret_cast<GLfloat *>(cameraInfoCenter.data()));
				glUniform3fv(locationAxis, cameraInfoAxis.size(), reinterpret_cast<GLfloat *>(cameraInfoAxis.data()));
				glUniform3fv(locationHorizontal, cameraInfoHorizontal.size(), reinterpret_cast<GLfloat *>(cameraInfoHorizontal.data()));
				glUniform3fv(locationVector, cameraInfoVector.size(), reinterpret_cast<GLfloat *>(cameraInfoVector.data()));
				
				(*it)._programObject->setUniform("size", static_cast<int>(cameraInfoCenter.size()));

				if (!_debugModelCullface) {
					glDisable(GL_CULL_FACE);
				}

				glEnable(GL_BLEND);
				glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

				(*it)._model->setUniforms(*(*it)._programObject);

				ghoul::opengl::TextureUnit unit;

				unit.activate();
				//(*it)._textures.at(0)->bind();
				//(*it)._programObject->setUniform("texture1", unit);

				ghoul::opengl::TextureUnit unit2;

				unit2.activate();
				//(*it)._textures.at(1)->bind();
				//(*it)._programObject->setUniform("texture", unit2);

				ghoul::opengl::TextureUnit unit3;

				unit3.activate();
				//(*it)._textures.at(2)->bind();
				//(*it)._programObject->setUniform("texture3", unit3);

				(*it)._model->render();

				(*it)._programObject->deactivate();
				tempCount++;
			}
		}

		void RenderableSite::update(const UpdateData & data) {
			//TODO: Check if _renderableExplorationPath has been created before calling update.
			//_renderableExplorationPath->update(data);

			if (_isCloseEnough == true && _hasLoopedOnce == false) {
				LERROR("IS CLOSE ENOUGH");
				globebrowsing::Geodetic2 geoTemp = globebrowsing::Geodetic2{ _tempLonLat.x, _tempLonLat.y } / 180 * glm::pi<double>();
				glm::dvec3 positionModelSpaceTemp = _globe->ellipsoid().cartesianSurfacePosition(geoTemp);
				double heightToSurfaceCheck = _globe->getHeight(positionModelSpaceTemp);

				globebrowsing::Geodetic3 geo3 = globebrowsing::Geodetic3{ geoTemp, heightToSurfaceCheck + 1.0 };
				glm::dvec3 tempPos2 = _globe->ellipsoid().cartesianPosition(geo3);

				_sitesModelCoordinates[48].stationPosition = glm::dvec4(tempPos2, 1.0);

				_hasLoopedOnce = true;
			}
		}

		void RenderableSite::calculateSiteWorldCoordinates() {
			globebrowsing::Geodetic2 geo;
			glm::dvec3 positionModelSpace;
			SiteInformation k;

			for (auto position : _siteLatlonCoordinates) {
				geo = globebrowsing::Geodetic2{ _tempLonLat.x, _tempLonLat.y } / 180 * glm::pi<double>();
				positionModelSpace = _globe->ellipsoid().cartesianSurfacePosition(geo);

				k.previousStationHeight = 0;
				k.stationPosition = glm::dvec4(positionModelSpace, 1.0);
				_sitesModelCoordinates.push_back(k);
			}
		}

		std::vector<std::string> RenderableSite::loadTexturePaths(std::string absoluteFilePath)
		{
			std::string fileName;
			std::ifstream myfile(absoluteFilePath);
			std::vector<std::string> fileNameVector;

			if (myfile.is_open()) {
				while (std::getline(myfile, fileName)) {
					fileNameVector.push_back(fileName);
					ImgReader::PointCloudInfo mInfo;

					std::string coordinates;
					std::getline(myfile, coordinates);
					std::vector<std::string> temp;
					temp = split(coordinates, ',');

					mInfo._cameraCenter.x = std::stof(temp.at(0));
					mInfo._cameraCenter.y = std::stof(temp.at(1));
					mInfo._cameraCenter.z = std::stof(temp.at(2));

					std::getline(myfile, coordinates);
					temp = split(coordinates, ',');

					mInfo._cameraAxis.x = std::stof(temp.at(0));
					mInfo._cameraAxis.y = std::stof(temp.at(1));
					mInfo._cameraAxis.z = std::stof(temp.at(2));

					std::getline(myfile, coordinates);
					temp = split(coordinates, ',');

					mInfo._cameraHorizontal.x = std::stof(temp.at(0));
					mInfo._cameraHorizontal.y = std::stof(temp.at(1));
					mInfo._cameraHorizontal.z = std::stof(temp.at(2));

					std::getline(myfile, coordinates);
					temp = split(coordinates, ',');

					mInfo._cameraVector.x = std::stof(temp.at(0));
					mInfo._cameraVector.y = std::stof(temp.at(1));
					mInfo._cameraVector.z = std::stof(temp.at(2));

					_cameraInfoVector.push_back(mInfo);
				}
				myfile.close();
			}
			else
				LERROR("Could not open .txt file");
			return fileNameVector;
		}

		bool RenderableSite::extractCoordinates() {
			GDALDataset *poDS;
			poDS = (GDALDataset*)GDALOpenEx(_filePath.c_str(), GDAL_OF_VECTOR, NULL, NULL, NULL);
			if (poDS == NULL) {
				LERROR("Could not open .shp file");
			}

			OGRLayer *poLayer = poDS->GetLayerByName("rover_locations");

			OGRFeature *poFeature;
			poLayer->ResetReading();

			while ((poFeature = poLayer->GetNextFeature()) != NULL) {

				// Extract coordinates from OGR
				std::string frame = poFeature->GetFieldAsString("frame");
				int site = poFeature->GetFieldAsInteger("site");
				int sol = poFeature->GetFieldAsInteger("sol");
				double lat = poFeature->GetFieldAsDouble("plcl");
				double lon = poFeature->GetFieldAsDouble("longitude");

				// Saves all coordinates for rendering the path and only site coordinates for rendering sites.
				// GetFieldAsDouble resturns zero (0) if field is empty.
				if (lat != 0 && lon != 0) {
					_pathLatlonCoordinates.push_back(glm::dvec2(lat, lon));

					if (frame == "SITE") {
						_siteLatlonCoordinates.insert(std::make_pair(site, glm::dvec2(lat, lon)));
					}
				}
				OGRFeature::DestroyFeature(poFeature);
			}
			GDALClose(poDS);

			return (_pathLatlonCoordinates.size() != 0);
		}

		void RenderableSite::loadTexture() {
			for (auto it = _models.begin(); it != _models.end(); ++it) {

				for (int i = 0; i < (*it)._texturePaths.size(); ++i) {
					(*it)._textures.emplace_back(std::move(ghoul::io::TextureReader::ref().loadTexture(absPath((*it)._texturePaths.at(i)))));
				
					if ((*it)._textures.at(i)) {
						//(*it)._textures.at(i)->uploadTexture();

						(*it)._textures.at(i)->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToBorder);
						(*it)._textures.at(i)->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
					}
				}

				//(*it)._texture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath((*it)._texturePath)));
				//(*it)._texture2 = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath((*it)._texturePath2)));
				//(*it)._texture3 = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath((*it)._texturePath3)));

				/*if ((*it)._texture) {
					(*it)._texture->uploadTexture();

					(*it)._texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToBorder);
					(*it)._texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

					(*it)._texture2->uploadTexture();

					(*it)._texture2->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToBorder);
					(*it)._texture2->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

					(*it)._texture3->uploadTexture();

					(*it)._texture3->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToBorder);
					(*it)._texture3->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
				}*/
			}
		}
	} // namespace globebrowsing
} // namespace openspace
