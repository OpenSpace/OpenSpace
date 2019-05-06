/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2018                                                               *
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

#include <modules/roverterrainrenderer/renderable/roverterrain.h>
#include <modules/roverterrainrenderer/model/modelprovider.h>

#include <openspace/scene/scenegraphnode.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/engine/globals.h>

#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodetic3.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <glm/gtx/quaternion.hpp>

namespace {
    const char* _loggerCat = "RoverTerrain";
    static const openspace::properties::Property::PropertyInfo Enable = {
        "isEnable",
        "Enabled",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo EnablePath = {
        "enablePath",
        "Enable path",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo LockSubsite = {
        "lockSubsite",
        "Lock subsite",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo UseMastCam = {
        "useMastCam",
        "Show mastcam coloring",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo EnableDepth = {
        "enableDepth",
        "Enable depth",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo EnableCulling = {
        "enableCulling",
        "Enable culling",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo HeightProp = {
        "heightProp",
        "Site height",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo MaxLod = {
        "maxLod",
        "Max LOD",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo Modelrotation = {
        "modelrotation",
        "Model Rotation",
        "" // @TODO Missing documentation
    };

}

namespace openspace {
RoverTerrain::RoverTerrain(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _generalProperties({
        properties::BoolProperty(Enable, true),
        properties::BoolProperty(EnablePath, true),
        properties::BoolProperty(LockSubsite, false),
        properties::BoolProperty(UseMastCam, false),
        properties::BoolProperty(EnableDepth, true),
        properties::BoolProperty(EnableCulling, true),
        properties::FloatProperty(HeightProp, 0.7f, 0.0f, 3.0f),
        properties::IntProperty(MaxLod, 3, 1, 3)
    })
    , _debugModelRotation(Modelrotation, glm::vec3(0.0f), glm::vec3(0.0f), glm::vec3(360.0f))
    , _modelSwitch()
    , _prevLevel(3)
    , _isFirst(true)
    , _isFirstLow(true)
    , _isFirstHigh(true)
{
    dictionary.getValue("ModelPath", _modelPath);
    dictionary.getValue("TexturePath", _texturePath);
    dictionary.getValue("RoverLocationPath", _roverLocationPath);

    //ghoul_assert(_modelPath && _texturePath && _roverLocationPath, "Missing either model or texture path");
    
    std::replace(_modelPath.begin(), _modelPath.end(), '\\', '/');
    std::replace(_texturePath.begin(), _texturePath.end(), '\\', '/');
    std::replace(_roverLocationPath.begin(), _roverLocationPath.end(), '\\', '/');

    // Extract all subsites that has models
    ghoul::Dictionary subsiteWithModelDictionary;
    subsiteWithModelDictionary.setValue("RoverLocationPath", _roverLocationPath);
    subsiteWithModelDictionary.setValue("AbsPathToTextures", _texturePath);
    subsiteWithModelDictionary.setValue("AbsPathToModels", _modelPath);
    _subsitesWithModels = RoverPathFileReader::extractSubsitesWithModels(subsiteWithModelDictionary);

    // Extract all subsites
    ghoul::Dictionary subsiteDictionary;
    subsiteDictionary.setValue("RoverLocationPath", _roverLocationPath);
    _subsites = RoverPathFileReader::extractAllSubsites(subsiteDictionary);

    addProperty(_generalProperties.enablePath);
    addProperty(_generalProperties.lockSubsite);
    addProperty(_generalProperties.useMastCam);
    addProperty(_generalProperties.enableDepth);
    addProperty(_generalProperties.enableCulling);
    addProperty(_generalProperties.heightProp);
    addProperty(_generalProperties.maxLod);
    addProperty(_debugModelRotation);

    _siteManager = std::make_shared<SiteManager>("MarsRoverModels", _subsitesWithModels);
    addPropertySubOwner(_siteManager.get());

    _cachingModelProvider = std::make_shared<CachingSurfaceModelProvider>(this);
    _renderableExplorationPath = std::make_shared<RenderableExplorationPath>();
}

bool RoverTerrain::isReady() const {
    return true;
}

void RoverTerrain::initializeGL() {
    std::vector<Geodetic2> allCoordinates;
    std::vector<Geodetic2> coordinatesWithModel;

    for (auto subsite : _subsites) {
        allCoordinates.push_back(subsite->geodetic);
    }
    for (auto subsite : _subsitesWithModels) {
        coordinatesWithModel.push_back(subsite->geodetic);
    }


    _programObject = global::renderEngine.buildRenderProgram("RoverTerrain",
        absPath("${MODULE_ROVERTERRAINRENDERER}/shaders/fullsubsite_vs.glsl"),
        absPath("${MODULE_ROVERTERRAINRENDERER}/shaders/fullsubsite_fs.glsl")
    );
    
    // std::string ownerName = owner()->name();

    _parent = global::renderEngine.scene()->sceneGraphNode("Mars");

    _globe = (globebrowsing::RenderableGlobe*)_parent->renderable();

    _renderableExplorationPath->initialize(_globe, allCoordinates, coordinatesWithModel);

    _chunkedLodGlobe = _globe->chunkedLodGlobe();

    _modelSwitch.initialize(_globe);
}

void RoverTerrain::deinitialize() {
}

void RoverTerrain::initialize() {

    // std::string ownerName = owner()->name();

    _parent = global::renderEngine.scene()->sceneGraphNode("Mars");

    _globe = (globebrowsing::RenderableGlobe*)_parent->renderable();
    
    _chunkedLodGlobe = _globe->chunkedLodGlobe();

    _modelSwitch.initialize(_globe);

    _chunkedLodGlobe->addSites(_subsitesWithModels);
}

void RoverTerrain::deinitializeGL() {
}

void RoverTerrain::render(const RenderData& data, RendererTasks& rendererTask) {
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    std::vector<std::vector<std::shared_ptr<Subsite>>> subSitesVector = _chunkedLodGlobe->subsites();
    
    if (subSitesVector.size() < 1) {
        return;
    }

    std::vector<std::shared_ptr<Subsite>> ss;
    ghoul::Dictionary modelDic;
    std::unique_ptr<ModelProvider> _modelProvider;
    int level;

    switch (_modelSwitch.getLevel(data)) {
    case LodModelSwitch::Mode::Low:
        //Low
        _isFirstLow = false;
        _isFirstHigh = true;
        _isFirst = true;
        modelDic.setValue("Type", "MultiModelProvider");
        _modelProvider = std::move(ModelProvider::createFromDictionary(modelDic));
        ss = _modelProvider->calculate(subSitesVector, data, _parent);
        level = 2;
        break;
    case LodModelSwitch::Mode::Close:
        //Close
        _isFirst = false;
        _isFirstLow = true;
        _isFirstHigh = true;

        modelDic.setValue("Type", "SingleModelProvider");
        _modelProvider = std::move(ModelProvider::createFromDictionary(modelDic));
        ss = _modelProvider->calculate(subSitesVector, data, _parent);
        level = 3;
        break;
    case LodModelSwitch::Mode::High:
        _isFirstHigh = false;
        _isFirstLow = true;
        _isFirst = true;
        //High up
        level = 1;
        break;
    case LodModelSwitch::Mode::Far:
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
    if (_generalProperties.lockSubsite.value())
        vectorOfsubsiteModels = _cachingModelProvider->getModels(_prevSubsites, lodCheck);
    else if (ss.size() > 0)
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
            PointCloudInfo mInfoTemp = cameraInfo;
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
            PointCloudInfo mInfoTemp = coloredCameraInfo;

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

        if (!_generalProperties.enableDepth.value()) {
            glDisable(GL_DEPTH_TEST);
        }

        if (!_generalProperties.enableCulling.value()) {
            glDisable(GL_CULL_FACE);
        }

        subsiteModels->model->render();

        glEnable(GL_DEPTH_TEST);
        glEnable(GL_CULL_FACE);
    }

    _programObject->deactivate();

    if (_generalProperties.enablePath.value()) {
        _renderableExplorationPath->setLevel(level);
        _renderableExplorationPath->render(data);
    }

    _prevLevel = level;
}

void RoverTerrain::update(const UpdateData& data) {
    if (_generalProperties.enablePath.value()) {
        _renderableExplorationPath->update(data);
    }
    _cachingModelProvider->update();
}

std::vector<std::shared_ptr<SubsiteModels>> RoverTerrain::calculateSurfacePosition(std::vector<std::shared_ptr<SubsiteModels>> vector) {
    for (auto subsiteModels : vector) {
        glm::dvec3 positionModelSpaceTemp = _globe->ellipsoid().cartesianSurfacePosition(subsiteModels->geodetic);
        double heightToSurface = _globe->getHeight(positionModelSpaceTemp);

        globebrowsing::Geodetic3 geo3 = globebrowsing::Geodetic3{ subsiteModels->geodetic , heightToSurface + _generalProperties.heightProp.value() };
        subsiteModels->cartesianPosition = _globe->ellipsoid().cartesianPosition(geo3);
    }
    return vector;
}


void RoverTerrain::lockSubsite(std::vector<std::shared_ptr<Subsite>> subsites) {
    if (_generalProperties.lockSubsite.value() && _pressedOnce == false) {
        _prevSubsites = subsites;
        _pressedOnce = true;
    }
    else if (!_generalProperties.lockSubsite.value() && _pressedOnce == true) {
        _pressedOnce = false;
    }
}

} // namespace openspace
