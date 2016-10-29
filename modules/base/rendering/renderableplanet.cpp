/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

// open space includes
#include <modules/base/rendering/renderableplanet.h>

#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <modules/base/rendering/planetgeometry.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <memory>
#include <fstream>

#define _USE_MATH_DEFINES
#include <math.h>


namespace {
    const std::string _loggerCat = "RenderablePlanet";

    const std::string keyFrame                         = "Frame";
    const std::string keyGeometry                      = "Geometry";
    const std::string keyRadius                        = "Radius";
    const std::string keyShading                       = "PerformShading";
    const std::string keyShadowGroup                   = "Shadow_Group";
    const std::string keyShadowSource                  = "Source";
    const std::string keyShadowCaster                  = "Caster";
    const std::string keyAtmosphere                    = "Atmosphere";
    const std::string keyAtmosphereRadius              = "AtmoshereRadius";
    const std::string keyPlanetRadius                  = "PlanetRadius";
    const std::string keyAverageGroundReflectance      = "PlanetAverageGroundReflectance";
    const std::string keyRayleigh                      = "Rayleigh";
    const std::string keyRayleighHeightScale           = "H_R";
    const std::string keyMie                           = "Mie";
    const std::string keyMieHeightScale                = "H_M";
    const std::string keyMiePhaseConstant              = "G";
    const std::string keyBody                          = "Body";
}

namespace openspace {

RenderablePlanet::RenderablePlanet(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorTexturePath("colorTexture", "Color Texture")
    , _nightTexturePath("nightTexture", "Night Texture")
    , _heightMapTexturePath("heightMap", "Heightmap Texture")
    , _cloudsTexturePath("clouds", "Clouds Texture")
    , _reflectanceTexturePath("reflectance", "Reflectance Texture")
    , _heightExaggeration("heightExaggeration", "Height Exaggeration", 1.f, 0.f, 10.f)
    , _programObject(nullptr)
    , _transmittanceProgramObject(nullptr)
    , _irradianceProgramObject(nullptr)
    , _irradianceSupTermsProgramObject(nullptr)
    , _inScatteringProgramObject(nullptr)
    , _inScatteringSupTermsProgramObject(nullptr)
    , _deltaEProgramObject(nullptr)
    , _deltaSProgramObject(nullptr)
    , _deltaSSupTermsProgramObject(nullptr)
    , _deltaJProgramObject(nullptr)
    , _atmosphereProgramObject(nullptr)
    , _texture(nullptr)
    , _nightTexture(nullptr)
    , _reflectanceTexture(nullptr)
    , _heightMapTexture(nullptr)
    , _cloudsTexture(nullptr)
    , _geometry(nullptr)
    , _performShading("performShading", "Perform Shading", true)
    , _rotation("rotation", "Rotation", 0, 0, 360)
    , _alpha(1.f)
    , _planetRadius(0.f)
    , _transmittanceTableTexture(0)
    , _irradianceTableTexture(0)
    , _inScatteringTableTexture(0)
    , _deltaETableTexture(0)
    , _deltaSRayleighTableTexture(0)
    , _deltaSMieTableTexture(0)
    , _deltaJTableTexture(0)
    , _dummyTexture(0)
    , _dummy3DTexture(0)
    , _atmosphereTexture(0)
    , _atmosphereFBO(0)
    , _atmosphereRenderVAO(0)
    , _atmosphereRenderVBO(0)
    , _atmosphereCalculated(false)
    , _atmosphereEnabled(false)
    , _atmosphereRadius(0.f)
    , _atmospherePlanetRadius(0.f)
    , _planetAverageGroundReflectance(0.f)
    , _rayleighHeightScale(0.f)
    , _mieHeightScale(0.f)
    , _miePhaseConstant(0.f)
    , _mieExtinctionCoeff(glm::vec3(0.f))
    , _rayleighScatteringCoeff(glm::vec3(0.f))
    , _mieScatteringCoeff(glm::vec3(0.f))
    , _hasNightTexture(false)
    , _hasHeightTexture(false)
    , _hasReflectanceTexture(false)
    , _hasCloudsTexture(false)
    , _shadowEnabled(false)
{
    std::string name;
    bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
    ghoul_assert(success,
            "RenderablePlanet need the '" << SceneGraphNode::KeyName<<"' be specified");

    //std::string path;
    //success = dictionary.getValue(constants::scenegraph::keyPathModule, path);
    //ghoul_assert(success,
    //        "RenderablePlanet need the '"<<constants::scenegraph::keyPathModule<<"' be specified");

    ghoul::Dictionary geometryDictionary;
    success = dictionary.getValue(keyGeometry, geometryDictionary);
    if (success) {
        geometryDictionary.setValue(SceneGraphNode::KeyName, name);
        //geometryDictionary.setValue(constants::scenegraph::keyPathModule, path);
        _geometry = planetgeometry::PlanetGeometry::createFromDictionary(geometryDictionary);

        glm::vec2 planetRadiusVec;
        success = geometryDictionary.getValue(keyRadius, planetRadiusVec);
        if (success)
            _planetRadius = planetRadiusVec[0] * glm::pow(10, planetRadiusVec[1]);
        else
            LWARNING("No Radius value expecified for " << name << " planet.");
    }

    dictionary.getValue(keyFrame, _frame);
    dictionary.getValue(keyBody, _target);


    // TODO: textures need to be replaced by a good system similar to the geometry as soon
    // as the requirements are fixed (ab)
    std::string texturePath = "";
    success = dictionary.getValue("Textures.Color", texturePath);
    if (success)
        _colorTexturePath = absPath(texturePath);

    std::string nightTexturePath = "";
    dictionary.getValue("Textures.Night", nightTexturePath);
    if (nightTexturePath != ""){
        _hasNightTexture = true;
        _nightTexturePath = absPath(nightTexturePath);
    }

    std::string reflectanceTexturePath = "";
    dictionary.getValue("Textures.Reflectance", reflectanceTexturePath);

    if (reflectanceTexturePath != "") {
        _hasReflectanceTexture = true;
        _reflectanceTexturePath = absPath(reflectanceTexturePath);
    }

    std::string heightMapTexturePath = "";
    dictionary.getValue("Textures.Height", heightMapTexturePath);
    if (heightMapTexturePath != "") {
        _hasHeightTexture = true;
        _heightMapTexturePath = absPath(heightMapTexturePath);
    }

    std::string cloudsTexturePath = "";
    dictionary.getValue("Textures.Clouds", cloudsTexturePath);
    if (cloudsTexturePath != "") {
        _hasCloudsTexture = true;
        _cloudsTexturePath = absPath(cloudsTexturePath);
    }

    addPropertySubOwner(_geometry);

    addProperty(_colorTexturePath);
    _colorTexturePath.onChange(std::bind(&RenderablePlanet::loadTexture, this));

    addProperty(_nightTexturePath);
    _nightTexturePath.onChange(std::bind(&RenderablePlanet::loadTexture, this));

    addProperty(_heightMapTexturePath);
    _heightMapTexturePath.onChange(std::bind(&RenderablePlanet::loadTexture, this));

    addProperty(_reflectanceTexturePath);
    _reflectanceTexturePath.onChange(std::bind(&RenderablePlanet::loadTexture, this));

    addProperty(_cloudsTexturePath);
    _cloudsTexturePath.onChange(std::bind(&RenderablePlanet::loadTexture, this));

    addProperty(_heightExaggeration);

    if (dictionary.hasKeyAndValue<bool>(keyShading)) {
        bool shading;
        dictionary.getValue(keyShading, shading);
        _performShading = shading;
    }

    addProperty(_performShading);
    // Mainly for debugging purposes @AA
    addProperty(_rotation);


    // Shadow data:
    ghoul::Dictionary shadowDictionary;
    success = dictionary.getValue(keyShadowGroup, shadowDictionary);
    bool disableShadows = false;
    if (success) {
        std::vector< std::pair<std::string, float > > sourceArray;
        unsigned int sourceCounter = 1;
        while (success) {
            std::string sourceName;
            std::stringstream ss;
            ss << keyShadowSource << sourceCounter << ".Name";
            success = shadowDictionary.getValue(ss.str(), sourceName);
            if (success) {
                glm::vec2 sourceRadius;
                ss.str(std::string());
                ss << keyShadowSource << sourceCounter << ".Radius";
                success = shadowDictionary.getValue(ss.str(), sourceRadius);
                if (success) {
                    sourceArray.push_back(std::pair< std::string, float>(
                        sourceName, sourceRadius[0] * pow(10.f, sourceRadius[1])));
                }
                else {
                    LWARNING("No Radius value expecified for Shadow Source Name " 
                        << sourceName << " from " << name 
                        << " planet.\nDisabling shadows for this planet.");
                    disableShadows = true;
                    break;
                }
            }
            sourceCounter++;
        }

        if (!disableShadows && !sourceArray.empty()) {
            success = true;
            std::vector< std::pair<std::string, float > > casterArray;
            unsigned int casterCounter = 1;
            while (success) {
                std::string casterName;
                std::stringstream ss;
                ss << keyShadowCaster << casterCounter << ".Name";
                success = shadowDictionary.getValue(ss.str(), casterName);
                if (success) {
                    glm::vec2 casterRadius;
                    ss.str(std::string());
                    ss << keyShadowCaster << casterCounter << ".Radius";
                    success = shadowDictionary.getValue(ss.str(), casterRadius);
                    if (success) {
                        casterArray.push_back(std::pair< std::string, float>(
                            casterName, casterRadius[0] * pow(10.f, casterRadius[1])));
                    }
                    else {
                        LWARNING("No Radius value expecified for Shadow Caster Name "
                            << casterName << " from " << name
                            << " planet.\nDisabling shadows for this planet.");
                        disableShadows = true;
                        break;
                    }
                }

                casterCounter++;
            }

            if (!disableShadows && (!sourceArray.empty() && !casterArray.empty())) {
                for (const auto & source : sourceArray)
                    for (const auto & caster : casterArray) {
                        ShadowConf sc;
                        sc.source = source;
                        sc.caster = caster;
                        _shadowConfArray.push_back(sc);
                    }
                _shadowEnabled = true;
            }
        }
    }

    // Atmosphere data:
    bool errorReadingAtmosphereData = false;
    ghoul::Dictionary atmosphereDictionary;
    success = dictionary.getValue(keyAtmosphere, atmosphereDictionary);
    if (success) {
        if (!atmosphereDictionary.getValue(keyAtmosphereRadius, _atmosphereRadius)) {
            errorReadingAtmosphereData = true;
            LWARNING("No Atmosphere Radius value expecified for Atmosphere Effects of "
                << name << " planet.\nDisabling atmosphere effects for this planet.");
        }

        if (!atmosphereDictionary.getValue(keyPlanetRadius, _atmospherePlanetRadius)) {
            errorReadingAtmosphereData = true;
            LWARNING("No Planet Radius value expecified for Atmosphere Effects of "
                << name << " planet.\nDisabling atmosphere effects for this planet.");
        }

        if (!atmosphereDictionary.getValue(keyAverageGroundReflectance, _planetAverageGroundReflectance)) {
            errorReadingAtmosphereData = true;
            LWARNING("No Average Atmosphere Ground Reflectance value expecified for Atmosphere Effects of "
                << name << " planet.\nDisabling atmosphere effects for this planet.");
        }

        ghoul::Dictionary rayleighDictionary;
        success = atmosphereDictionary.getValue(keyRayleigh, rayleighDictionary);

        if (success) {
            // Not using right now.
            glm::vec3 rayleighWavelengths;
            success = rayleighDictionary.getValue("Coefficients.Wavelengths", rayleighWavelengths);
            
            if (!rayleighDictionary.getValue("Coefficients.Scattering", _rayleighScatteringCoeff)) {
                errorReadingAtmosphereData = true;
                LWARNING("No Rayleigh Scattering parameters expecified for Atmosphere Effects of "
                    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }

            if (!rayleighDictionary.getValue(keyRayleighHeightScale, _rayleighHeightScale)) {
                errorReadingAtmosphereData = true;
                LWARNING("No Rayleigh Height Scale value expecified for Atmosphere Effects of "
                    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }
        } 
        else {
            errorReadingAtmosphereData = true;
            LWARNING("No Rayleigh parameters expecified for Atmosphere Effects of "
                << name << " planet.\nDisabling atmosphere effects for this planet.");
        }

        ghoul::Dictionary mieDictionary;
        success = atmosphereDictionary.getValue(keyMie, mieDictionary);
        if (success) {
            if (!mieDictionary.getValue(keyMieHeightScale, _mieHeightScale)) {
                errorReadingAtmosphereData = true;
                LWARNING("No Mie Height Scale value expecified for Atmosphere Effects of "
                    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }

            if (!mieDictionary.getValue("Coefficients.Scattering", _mieScatteringCoeff)) {
                errorReadingAtmosphereData = true;
                LWARNING("No Mie Scattering parameters expecified for Atmosphere Effects of "
                    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }

            if (!mieDictionary.getValue("Coefficients.Extinction", _mieExtinctionCoeff)) {
                errorReadingAtmosphereData = true;
                LWARNING("No Mie Extinction parameters expecified for Atmosphere Effects of "
                    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }

            if (!mieDictionary.getValue(keyMiePhaseConstant, _miePhaseConstant)) {
                errorReadingAtmosphereData = true;
                LWARNING("No Mie Phase Constant value expecified for Atmosphere Effects of "
                    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }
        }
        else {
            errorReadingAtmosphereData = true;
            LWARNING("No Mie parameters expecified for Atmosphere Effects of "
                << name << " planet.\nDisabling atmosphere effects for this planet.");
        }

        if (!errorReadingAtmosphereData) {
            _atmosphereEnabled = true;

            //// DEBUG:
            //std::stringstream ss;
            //ss << "\n\nAtmosphere Values:\n"
            //    << "Radius: " << _atmosphereRadius << std::endl
            //    << "Planet Radius: " << _atmospherePlanetRadius << std::endl
            //    << "Average Reflection: " << _planetAverageGroundReflectance << std::endl
            //    << "Rayleigh HR: " << _rayleighHeightScale << std::endl
            //    << "Mie HR: " << _mieHeightScale << std::endl
            //    << "Mie G phase constant: " << _miePhaseConstant << std::endl
            //    << "Mie Extinction coeff: " << _mieExtinctionCoeff << std::endl
            //    << "Rayleigh Scattering coeff: " << _rayleighScatteringCoeff << std::endl
            //    << "Mie Scattering coeff: " << _mieScatteringCoeff << std::endl;
            //std::cout << ss.str() << std::endl;
        }
    }
}

RenderablePlanet::~RenderablePlanet() {
}

bool RenderablePlanet::initialize() {
    RenderEngine& renderEngine = OsEng.renderEngine();

    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Checking System State. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    if (_programObject == nullptr && _atmosphereEnabled && _shadowEnabled && _hasNightTexture) {
        // shadow program
        _programObject = renderEngine.buildRenderProgram(
            "atmosphereAndShadowProgram",
            "${MODULE_BASE}/shaders/atmosphere_vs.glsl",
            "${MODULE_BASE}/shaders/atmosphere_fs.glsl");
        if (!_programObject)
            return false;
    } else if (_programObject == nullptr && _shadowEnabled && _hasNightTexture) {
        // shadow program
        _programObject = renderEngine.buildRenderProgram(
            "shadowNightProgram",
            "${MODULE_BASE}/shaders/shadow_nighttexture_vs.glsl",
            "${MODULE_BASE}/shaders/shadow_nighttexture_fs.glsl");
        if (!_programObject)
            return false;
    } 
    else if (_programObject == nullptr && _shadowEnabled) {
        // shadow program
        _programObject = renderEngine.buildRenderProgram(
            "shadowProgram",
            "${MODULE_BASE}/shaders/shadow_vs.glsl",
            "${MODULE_BASE}/shaders/shadow_fs.glsl");
        if (!_programObject)
            return false;
    } 
    else if (_programObject == nullptr && _hasNightTexture) {
        // Night texture program
        _programObject = renderEngine.buildRenderProgram(
            "nightTextureProgram",
            "${MODULE_BASE}/shaders/nighttexture_vs.glsl",
            "${MODULE_BASE}/shaders/nighttexture_fs.glsl");
        if (!_programObject) 
            return false;
    }
    else if (_programObject == nullptr) {
        // pscstandard
        _programObject = renderEngine.buildRenderProgram(
            "pscstandard",
            "${MODULE_BASE}/shaders/renderableplanet_vs.glsl",
            "${MODULE_BASE}/shaders/renderableplanet_fs.glsl");
        if (!_programObject)
            return false;
    }
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _programObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    /*_atmosphereProgramObject = renderEngine.buildRenderProgram(
        "atmosphereDeferredProgram",
        "${MODULE_BASE}/shaders/atmosphere_deferred_vs.glsl",
        "${MODULE_BASE}/shaders/atmosphere_deferred_fs.glsl");
    if (!_atmosphereProgramObject)
        return false;*/

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error after load shading programs. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    loadTexture();

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error loading textures. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }
    
    _geometry->initialize(this);    

    _programObject->deactivate();

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Shader Programs Creation. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    //Atmosphere precomputation and tables
    if (_atmosphereEnabled && !_atmosphereCalculated) {
        _atmosphereCalculated = true;
        
        preCalculateAtmosphereParam();

        //createAtmosphereFBO();

        createRenderQuad(&_atmosphereRenderVAO, &_atmosphereRenderVBO, 6);
    }

    count = 0;

    return isReady();
}

bool RenderablePlanet::deinitialize() {
    if(_geometry) {
        _geometry->deinitialize();
        delete _geometry;
    }

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_programObject) {
        renderEngine.removeRenderProgram(_programObject);
        _programObject = nullptr;
    }

    if (_transmittanceProgramObject) {
        renderEngine.removeRenderProgram(_transmittanceProgramObject);
        _transmittanceProgramObject = nullptr;
    }

    if (_irradianceProgramObject) {
        renderEngine.removeRenderProgram(_irradianceProgramObject);
        _irradianceProgramObject = nullptr;
    }

    if (_irradianceSupTermsProgramObject) {
        renderEngine.removeRenderProgram(_irradianceSupTermsProgramObject);
        _irradianceSupTermsProgramObject = nullptr;
    }

    if (_inScatteringProgramObject) {
        renderEngine.removeRenderProgram(_inScatteringProgramObject);
        _inScatteringProgramObject = nullptr;
    }

    if (_inScatteringSupTermsProgramObject) {
        renderEngine.removeRenderProgram(_inScatteringSupTermsProgramObject);
        _inScatteringSupTermsProgramObject = nullptr;
    }

    if (_deltaEProgramObject) {
        renderEngine.removeRenderProgram(_deltaEProgramObject);
        _deltaEProgramObject = nullptr;
    }

    if (_deltaSProgramObject) {
        renderEngine.removeRenderProgram(_deltaSProgramObject);
        _deltaSProgramObject = nullptr;
    }

    if (_deltaSSupTermsProgramObject) {
        renderEngine.removeRenderProgram(_deltaSSupTermsProgramObject);
        _deltaSSupTermsProgramObject = nullptr;
    }

    if (_deltaJProgramObject) {
        renderEngine.removeRenderProgram(_deltaJProgramObject);
        _deltaJProgramObject = nullptr;
    }

    _geometry                   = nullptr;
    _texture                    = nullptr;
    _nightTexture               = nullptr;
    _reflectanceTexture         = nullptr;
    _cloudsTexture              = nullptr;

    glDeleteTextures(1, &_transmittanceTableTexture);
    glDeleteTextures(1, &_irradianceTableTexture);
    glDeleteTextures(1, &_inScatteringTableTexture);
    glDeleteTextures(1, &_deltaETableTexture);
    glDeleteTextures(1, &_deltaSRayleighTableTexture);
    glDeleteTextures(1, &_deltaSMieTableTexture);
    glDeleteTextures(1, &_deltaJTableTexture);
    glDeleteTextures(1, &_atmosphereTexture);

    glDeleteFramebuffers(1, &_atmosphereFBO);

    return true;
}

bool RenderablePlanet::isReady() const {
    bool ready = true;
    ready &= (_programObject != nullptr);
    ready &= (_texture != nullptr);
    ready &= (_geometry != nullptr);
    return ready;
}

void RenderablePlanet::render(const RenderData& data) {
    // activate shader
    _programObject->activate();

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));

    // scale the planet to appropriate size since the planet is a unit sphere
    //glm::mat4 transform = glm::mat4(1);
    
    //earth needs to be rotated for that to work.
    glm::dmat4 rot = glm::rotate(glm::dmat4(1.0), M_PI_2, glm::dvec3(1, 0, 0));
    glm::dmat4 roty = glm::rotate(glm::dmat4(1.0), M_PI_2, glm::dvec3(0, -1, 0));
    //glm::dmat4 rotProp = glm::rotate(glm::dmat4(1.0), glm::radians(static_cast<double>(_rotation)), glm::dvec3(0, 1, 0));
    modelTransform = modelTransform * rot /** roty*/ /** rotProp*/;

    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _programObject->setUniform("transparency", _alpha);
    _programObject->setUniform(
        "modelViewProjectionTransform", 
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform)
    );
    //_programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _programObject->setUniform("ModelTransform", glm::mat4(modelTransform));

    // Normal Transformation
    glm::mat4 translateObjTrans = glm::translate(glm::mat4(1.0), data.position.vec3());
    glm::mat4 translateCamTrans = glm::translate(glm::mat4(1.0), -data.camera.position().vec3());
    float scaleFactor = data.camera.scaling().x * powf(10.0, data.camera.scaling().y);
    glm::mat4 scaleCamTrans = glm::scale(glm::mat4(1.0), glm::vec3(scaleFactor));


    glm::mat4 ModelViewTrans = data.camera.viewMatrix() * scaleCamTrans *
        translateCamTrans * translateObjTrans * glm::mat4(modelTransform);
    if (_atmosphereEnabled)
        _programObject->setUniform("NormalTransform", 
            glm::transpose(glm::inverse(ModelViewTrans)));

    setPscUniforms(*_programObject.get(), data.camera, data.position);
    
    _programObject->setUniform("_performShading", _performShading);

    _programObject->setUniform("_hasHeightMap", _hasHeightTexture);
    _programObject->setUniform("_heightExaggeration", _heightExaggeration);

    // Bind texture
    ghoul::opengl::TextureUnit dayUnit;
    ghoul::opengl::TextureUnit nightUnit;
    ghoul::opengl::TextureUnit heightUnit;


    dayUnit.activate();
    _texture->bind();
    _programObject->setUniform("texture1", dayUnit);

    // Bind possible night texture
    if (_hasNightTexture) {
        nightUnit.activate();
        _nightTexture->bind();
        _programObject->setUniform("nightTex", nightUnit);
    }

    if (_hasHeightTexture) {
        heightUnit.activate();
        _heightMapTexture->bind();
        _programObject->setUniform("heightTex", heightUnit);
    }

    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    // TODO: Move Calculations to VIEW SPACE (precision problems avoidance...)

    double lt;
    // Shadow calculations..
    if (!_shadowConfArray.empty()) {
        std::vector<ShadowRenderingStruct> shadowDataArray;
        shadowDataArray.reserve(_shadowConfArray.size());

        for (const auto & shadowConf : _shadowConfArray) {
            // TO REMEMBER: all distances and lengths in world coordinates are in meters!!! We need to move this to view space...
            // Getting source and caster:
            glm::dvec3 sourcePos = SpiceManager::ref().targetPosition(shadowConf.source.first, "SUN", "GALACTIC", {}, _time, lt);
            sourcePos           *= 1000.0; // converting to meters
            glm::dvec3 casterPos = SpiceManager::ref().targetPosition(shadowConf.caster.first, "SUN", "GALACTIC", {}, _time, lt);
            casterPos           *= 1000.0; // converting to meters
            psc caster_pos       = PowerScaledCoordinate::CreatePowerScaledCoordinate(casterPos.x, casterPos.y, casterPos.z);

            
            // First we determine if the caster is shadowing the current planet (all calculations in World Coordinates):
            glm::vec3 planetCasterVec   = (caster_pos - data.position).vec3();
            glm::vec3 sourceCasterVec   = glm::vec3(casterPos - sourcePos);
            float sc_length             = glm::length(sourceCasterVec);
            glm::vec3 planetCaster_proj = (glm::dot(planetCasterVec, sourceCasterVec) / (sc_length*sc_length)) * sourceCasterVec;
            float d_test                = glm::length(planetCasterVec - planetCaster_proj);
            float xp_test               = shadowConf.caster.second * sc_length / (shadowConf.source.second + shadowConf.caster.second);
            float rp_test               = shadowConf.caster.second * (glm::length(planetCaster_proj) + xp_test) / xp_test;
                        
            float casterDistSun = glm::length(casterPos);
            float planetDistSun = glm::length(data.position.vec3());

            ShadowRenderingStruct shadowData;
            shadowData.isShadowing = false;

            if (((d_test - rp_test) < _planetRadius) &&
                (casterDistSun < planetDistSun) ) {
                // The current caster is shadowing the current planet
                shadowData.isShadowing       = true;
                shadowData.rs                = shadowConf.source.second;
                shadowData.rc                = shadowConf.caster.second;
                shadowData.sourceCasterVec   = sourceCasterVec;
                shadowData.xp                = xp_test;
                shadowData.xu                = shadowData.rc * sc_length / (shadowData.rs - shadowData.rc);
                shadowData.casterPositionVec = glm::vec3(casterPos);
            }
            shadowDataArray.push_back(shadowData);
        }

        const std::string uniformVarName("shadowDataArray[");
        unsigned int counter = 0;
        for (const auto & sd : shadowDataArray) {
            std::stringstream ss;
            ss << uniformVarName << counter << "].isShadowing";
            _programObject->setUniform(ss.str(), sd.isShadowing);
            if (sd.isShadowing) {
                ss.str(std::string());
                ss << uniformVarName << counter << "].xp";
                _programObject->setUniform(ss.str(), sd.xp);
                ss.str(std::string());
                ss << uniformVarName << counter << "].xu";
                _programObject->setUniform(ss.str(), sd.xu);
                /*ss.str(std::string());
                ss << uniformVarName << counter << "].rs";
                _programObject->setUniform(ss.str(), sd.rs);*/
                ss.str(std::string());
                ss << uniformVarName << counter << "].rc";
                _programObject->setUniform(ss.str(), sd.rc);
                ss.str(std::string());
                ss << uniformVarName << counter << "].sourceCasterVec";
                _programObject->setUniform(ss.str(), sd.sourceCasterVec);
                ss.str(std::string());
                ss << uniformVarName << counter << "].casterPositionVec";
                _programObject->setUniform(ss.str(), sd.casterPositionVec);
            }
            counter++;
        }
    }

    // Atmosphere Data
    if (_atmosphereEnabled) {

//        GLenum err;
//        while ((err = glGetError()) != GL_NO_ERROR) {
//            const GLubyte * errorString = gluErrorString(err);
//            std::stringstream ss;
//            ss << "Error setting up atmosphere framebuffer. OpenGL error: "
//                << err << " - " << errorString << std::endl;
//            LERROR(ss.str());
//        }

        // Object Space (in Km)
        glm::mat4 obj2World = glm::translate(glm::mat4(1.0), data.position.vec3() / 1000.0f);
        /*glm::mat4 M = data.camera.viewMatrix() * scaleCamTrans * glm::mat4(data.camera.viewRotationMatrix()) *
            translateCamTrans * obj2World * transform;
        */
        glm::mat4 M = glm::mat4(data.camera.combinedViewMatrix()) * scaleCamTrans * obj2World * glm::mat4(modelTransform);

        glm::mat4 completeInverse = glm::inverse(M);

        _programObject->setUniform("completeInverse", completeInverse);
        _programObject->setUniform("projInverse", glm::inverse(data.camera.projectionMatrix()));

        // This is camera position and planet position vector in object coordinates, in Km.
        glm::mat4 world2Obj = glm::inverse(obj2World * glm::mat4(modelTransform));
        glm::vec4 cameraPosObj = world2Obj * glm::vec4(data.camera.position().vec3() / 1000.0f, 1.0);
        glm::vec4 planetPositionObj = world2Obj * glm::vec4(data.position.vec3() / 1000.0f, 1.0);
        _programObject->setUniform("cameraPosObj", cameraPosObj);
        _programObject->setUniform("planetPositionObj", planetPositionObj);

        // I know it is (0,0,0). It is here just for sake of sanity. :-p
        glm::dvec3 sunPosWorld =
            SpiceManager::ref().targetPosition("SUN", "SUN", "GALACTIC", {}, _time, lt);
        glm::vec4 sunPosObj = world2Obj * glm::vec4(sunPosWorld.x, sunPosWorld.y, sunPosWorld.z, 1.0);
        _programObject->setUniform("sunPositionObj", glm::vec3(sunPosObj));

        _transmittanceTableTextureUnit.activate();
        _programObject->setUniform("transmittanceTexture", _transmittanceTableTextureUnit);

        _irradianceTableTextureUnit.activate();
        _programObject->setUniform("irradianceTexture", _irradianceTableTextureUnit);

        _inScatteringTableTextureUnit.activate();
        _programObject->setUniform("inscatterTexture", _inScatteringTableTextureUnit);             
        
        GLint m_viewport[4];
        glGetIntegerv(GL_VIEWPORT, m_viewport);
        _programObject->setUniform("screenX", (float)m_viewport[0]);
        _programObject->setUniform("screenY", (float)m_viewport[1]);
        _programObject->setUniform("screenWIDTH", (float)m_viewport[2]);
        _programObject->setUniform("screenHEIGHT", (float)m_viewport[3]);
        

        _programObject->setUniform("Rg", _atmospherePlanetRadius);
        _programObject->setUniform("Rt", _atmosphereRadius);
        _programObject->setUniform("AVERAGE_GROUND_REFLECTANCE", _planetAverageGroundReflectance);
        _programObject->setUniform("HR", _rayleighHeightScale);
        _programObject->setUniform("betaR", _rayleighScatteringCoeff);
        _programObject->setUniform("HM", _mieHeightScale);
        _programObject->setUniform("betaMSca", _mieScatteringCoeff);
        _programObject->setUniform("betaMEx", _mieExtinctionCoeff);
        _programObject->setUniform("mieG", _miePhaseConstant);


        ghoul::opengl::TextureUnit reflectanceUnit;
        if (_hasReflectanceTexture) {
            reflectanceUnit.activate();
            _reflectanceTexture->bind();
            _programObject->setUniform("reflectanceTexture", reflectanceUnit);
        }

        ghoul::opengl::TextureUnit cloudsUnit;
        if (_hasCloudsTexture) {
            cloudsUnit.activate();
            _cloudsTexture->bind();
            _programObject->setUniform("cloudsTexture", cloudsUnit);
        }

        // HDR
        _programObject->setUniform("exposure", 0.4f);

    }


    // render
    _geometry->render();

    // disable shader
    _programObject->deactivate();

}

void RenderablePlanet::update(const UpdateData& data) {
    // set spice-orientation in accordance to timestamp
    _stateMatrix = data.modelTransform.rotation;
    //_stateMatrix = SpiceManager::ref().positionTransformMatrix(_frame, "GALACTIC", data.time);
    _time = data.time;
}

void RenderablePlanet::loadTexture() {
    _texture = nullptr;
    if (_colorTexturePath.value() != "") {
        _texture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath)));
        if (_texture) {
            if (_texture->numberOfChannels() == 1) {
                _texture->setSwizzleMask({ GL_RED, GL_RED, GL_RED, GL_RED });
            }

            LDEBUG("Loaded texture from '" << _colorTexturePath << "'");
            _texture->uploadTexture();

            // Textures of planets looks much smoother with AnisotropicMipMap rather than linear
            // TODO: AnisotropicMipMap crashes on ATI cards ---abock
            //_texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            _texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
        }
    }

    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error after reading memory 1. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    if (_hasNightTexture) {
        _nightTexture = nullptr;
        if (_nightTexturePath.value() != "") {
            _nightTexture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_nightTexturePath)));
            if (_nightTexture) {
                LDEBUG("Loaded texture from '" << _nightTexturePath << "'");
                _nightTexture->uploadTexture();
                _nightTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
                //_nightTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            }
        }
    }

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error after reading memory 2. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    if (_hasReflectanceTexture) {
        _reflectanceTexture = nullptr;
        if (_reflectanceTexturePath.value() != "") {
            _reflectanceTexture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_reflectanceTexturePath)));
            if (_reflectanceTexture) {
                LDEBUG("Loaded texture from '" << _reflectanceTexturePath << "'");
                _reflectanceTexture->uploadTexture();
                _reflectanceTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
                //_reflectanceTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            }
        }
    }

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error after reading memory 3. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    if (_hasHeightTexture) {
        _heightMapTexture = nullptr;
        if (_heightMapTexturePath.value() != "") {
            _heightMapTexture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_heightMapTexturePath)));
            if (_heightMapTexture) {
                LDEBUG("Loaded texture from '" << _heightMapTexturePath << "'");
                _heightMapTexture->uploadTexture();
                _heightMapTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
                //_nightTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            }
        }
    }

//    while ((err = glGetError()) != GL_NO_ERROR) {
//        const GLubyte * errString = gluErrorString(err);
//        std::stringstream ss;
//        ss << "Error after reading memory 4. OpenGL error: " << errString << std::endl;
//        LERROR(ss.str());
//    }

    if (_hasCloudsTexture) {
        _cloudsTexture = nullptr;
        if (_cloudsTexturePath.value() != "") {
            _cloudsTexture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_cloudsTexturePath)));
            if (_cloudsTexture) {
                LDEBUG("Loaded texture from '" << _cloudsTexturePath << "'");
                _cloudsTexture->uploadTexture();
                _cloudsTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
                //_cloudsTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            }
        }
    }

//    while ((err = glGetError()) != GL_NO_ERROR) {
//        const GLubyte * errString = gluErrorString(err);
//        std::stringstream ss;
//        ss << "Error after reading memory 5. OpenGL error: " << errString << std::endl;
//        LERROR(ss.str());
//    }
}

void RenderablePlanet::loadComputationPrograms() {
    
    RenderEngine& renderEngine = OsEng.renderEngine();

    //============== Transmittance =================
    if (_transmittanceProgramObject == nullptr) {
        _transmittanceProgramObject = renderEngine.buildRenderProgram(
            "transmittanceCalcProgram",
            "${MODULE_BASE}/shaders/transmittance_calc_vs.glsl",
            "${MODULE_BASE}/shaders/transmittance_calc_fs.glsl");
        if (!_transmittanceProgramObject) {
            return;
        }
    }
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _transmittanceProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _transmittanceProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== Irradiance =================
    if (_irradianceProgramObject == nullptr) {
        _irradianceProgramObject = renderEngine.buildRenderProgram(
            "irradianceCalcProgram",
            "${MODULE_BASE}/shaders/irradiance_calc_vs.glsl",
            "${MODULE_BASE}/shaders/irradiance_calc_fs.glsl");
        if (!_irradianceProgramObject) {
            if (_transmittanceProgramObject) {
                renderEngine.removeRenderProgram(_transmittanceProgramObject);
                _transmittanceProgramObject = nullptr;
            }

            return;
        }
    }
    _irradianceProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _irradianceProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    if (_irradianceSupTermsProgramObject == nullptr) {
        _irradianceSupTermsProgramObject = renderEngine.buildRenderProgram(
            "irradianceSupTermsCalcProgram",
            "${MODULE_BASE}/shaders/irradiance_sup_calc_vs.glsl",
            "${MODULE_BASE}/shaders/irradiance_sup_calc_fs.glsl");
        if (!_irradianceSupTermsProgramObject) {
            if (_transmittanceProgramObject) {
                renderEngine.removeRenderProgram(_transmittanceProgramObject);
                _transmittanceProgramObject = nullptr;
            }

            if (_irradianceProgramObject) {
                renderEngine.removeRenderProgram(_irradianceProgramObject);
                _irradianceProgramObject = nullptr;
            }

            return;
        }
    }
    _irradianceSupTermsProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _irradianceSupTermsProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== InScattering =================
    if (_inScatteringProgramObject == nullptr) {
        _inScatteringProgramObject = renderEngine.buildRenderProgram(
            "inScatteringCalcProgram",
            "${MODULE_BASE}/shaders/inScattering_calc_vs.glsl",
            "${MODULE_BASE}/shaders/inScattering_calc_fs.glsl",
            "${MODULE_BASE}/shaders/inScattering_calc_gs.glsl");
        if (!_inScatteringProgramObject) {
            if (_transmittanceProgramObject) {
                renderEngine.removeRenderProgram(_transmittanceProgramObject);
                _transmittanceProgramObject = nullptr;
            }

            if (_irradianceProgramObject) {
                renderEngine.removeRenderProgram(_irradianceProgramObject);
                _irradianceProgramObject = nullptr;
            }

            if (_irradianceSupTermsProgramObject) {
                renderEngine.removeRenderProgram(_irradianceSupTermsProgramObject);
                _irradianceSupTermsProgramObject = nullptr;
            }

            return;
        }
    }
    _inScatteringProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _inScatteringProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    if (_inScatteringSupTermsProgramObject == nullptr) {
        _inScatteringSupTermsProgramObject = renderEngine.buildRenderProgram(
            "inScatteringSupTermsCalcProgram",
            "${MODULE_BASE}/shaders/inScattering_sup_calc_vs.glsl",
            "${MODULE_BASE}/shaders/inScattering_sup_calc_fs.glsl",
            "${MODULE_BASE}/shaders/inScattering_sup_calc_gs.glsl");
        if (!_inScatteringSupTermsProgramObject) {
            if (_transmittanceProgramObject) {
                renderEngine.removeRenderProgram(_transmittanceProgramObject);
                _transmittanceProgramObject = nullptr;
            }

            if (_irradianceProgramObject) {
                renderEngine.removeRenderProgram(_irradianceProgramObject);
                _irradianceProgramObject = nullptr;
            }

            if (_irradianceSupTermsProgramObject) {
                renderEngine.removeRenderProgram(_irradianceSupTermsProgramObject);
                _irradianceSupTermsProgramObject = nullptr;
            }

            if (_inScatteringProgramObject) {
                renderEngine.removeRenderProgram(_inScatteringProgramObject);
                _inScatteringProgramObject = nullptr;
            }

            return;
        }
    }
    _inScatteringSupTermsProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _inScatteringSupTermsProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== Delta E =================
    if (_deltaEProgramObject == nullptr) {
        _deltaEProgramObject = renderEngine.buildRenderProgram(
            "deltaECalcProgram",
            "${MODULE_BASE}/shaders/deltaE_calc_vs.glsl",
            "${MODULE_BASE}/shaders/deltaE_calc_fs.glsl");
        if (!_deltaEProgramObject) {
            if (_transmittanceProgramObject) {
                renderEngine.removeRenderProgram(_transmittanceProgramObject);
                _transmittanceProgramObject = nullptr;
            }

            if (_irradianceProgramObject) {
                renderEngine.removeRenderProgram(_irradianceProgramObject);
                _irradianceProgramObject = nullptr;
            }

            if (_irradianceSupTermsProgramObject) {
                renderEngine.removeRenderProgram(_irradianceSupTermsProgramObject);
                _irradianceSupTermsProgramObject = nullptr;
            }

            if (_inScatteringProgramObject) {
                renderEngine.removeRenderProgram(_inScatteringProgramObject);
                _inScatteringProgramObject = nullptr;
            }

            if (_inScatteringSupTermsProgramObject) {
                renderEngine.removeRenderProgram(_inScatteringSupTermsProgramObject);
                _inScatteringSupTermsProgramObject = nullptr;
            }

            return;
        }
    }
    _deltaEProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _deltaEProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== Delta S =================
    if (_deltaSProgramObject == nullptr) {
        _deltaSProgramObject = renderEngine.buildRenderProgram(
            "deltaSCalcProgram",
            "${MODULE_BASE}/shaders/deltaS_calc_vs.glsl",
            "${MODULE_BASE}/shaders/deltaS_calc_fs.glsl",
            "${MODULE_BASE}/shaders/deltaS_calc_gs.glsl");
        if (!_deltaSProgramObject) {
            if (_transmittanceProgramObject) {
                renderEngine.removeRenderProgram(_transmittanceProgramObject);
                _transmittanceProgramObject = nullptr;
            }

            if (_irradianceProgramObject) {
                renderEngine.removeRenderProgram(_irradianceProgramObject);
                _irradianceProgramObject = nullptr;
            }

            if (_irradianceSupTermsProgramObject) {
                renderEngine.removeRenderProgram(_irradianceSupTermsProgramObject);
                _irradianceSupTermsProgramObject = nullptr;
            }

            if (_inScatteringProgramObject) {
                renderEngine.removeRenderProgram(_inScatteringProgramObject);
                _inScatteringProgramObject = nullptr;
            }

            if (_inScatteringSupTermsProgramObject) {
                renderEngine.removeRenderProgram(_inScatteringSupTermsProgramObject);
                _inScatteringSupTermsProgramObject = nullptr;
            }

            if (_deltaEProgramObject) {
                renderEngine.removeRenderProgram(_deltaEProgramObject);
                _deltaEProgramObject = nullptr;
            }

            return;
        }
    }
    _deltaSProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _deltaSProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    if (_deltaSSupTermsProgramObject == nullptr) {
        _deltaSSupTermsProgramObject = renderEngine.buildRenderProgram(
            "deltaSSUPTermsCalcProgram",
            "${MODULE_BASE}/shaders/deltaS_sup_calc_vs.glsl",
            "${MODULE_BASE}/shaders/deltaS_sup_calc_fs.glsl",
            "${MODULE_BASE}/shaders/deltaS_sup_calc_gs.glsl");
        if (!_deltaSSupTermsProgramObject) {
            if (_transmittanceProgramObject) {
                renderEngine.removeRenderProgram(_transmittanceProgramObject);
                _transmittanceProgramObject = nullptr;
            }

            if (_irradianceProgramObject) {
                renderEngine.removeRenderProgram(_irradianceProgramObject);
                _irradianceProgramObject = nullptr;
            }

            if (_irradianceSupTermsProgramObject) {
                renderEngine.removeRenderProgram(_irradianceSupTermsProgramObject);
                _irradianceSupTermsProgramObject = nullptr;
            }

            if (_inScatteringProgramObject) {
                renderEngine.removeRenderProgram(_inScatteringProgramObject);
                _inScatteringProgramObject = nullptr;
            }

            if (_inScatteringSupTermsProgramObject) {
                renderEngine.removeRenderProgram(_inScatteringSupTermsProgramObject);
                _inScatteringSupTermsProgramObject = nullptr;
            }

            if (_deltaEProgramObject) {
                renderEngine.removeRenderProgram(_deltaEProgramObject);
                _deltaEProgramObject = nullptr;
            }

            if (_deltaSProgramObject) {
                renderEngine.removeRenderProgram(_deltaSProgramObject);
                _deltaSProgramObject = nullptr;
            }

            return;
        }
    }
    _deltaSSupTermsProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _deltaSSupTermsProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== Delta J (Radiance Scattered) =================
    if (_deltaJProgramObject == nullptr) {
        // shadow program
        _deltaJProgramObject = renderEngine.buildRenderProgram(
            "deltaJCalcProgram",
            "${MODULE_BASE}/shaders/deltaJ_calc_vs.glsl",
            "${MODULE_BASE}/shaders/deltaJ_calc_fs.glsl",
            "${MODULE_BASE}/shaders/deltaJ_calc_gs.glsl");
        if (!_deltaJProgramObject) {
            if (_transmittanceProgramObject) {
                renderEngine.removeRenderProgram(_transmittanceProgramObject);
                _transmittanceProgramObject = nullptr;
            }

            if (_irradianceProgramObject) {
                renderEngine.removeRenderProgram(_irradianceProgramObject);
                _irradianceProgramObject = nullptr;
            }

            if (_irradianceSupTermsProgramObject) {
                renderEngine.removeRenderProgram(_irradianceSupTermsProgramObject);
                _irradianceSupTermsProgramObject = nullptr;
            }

            if (_inScatteringProgramObject) {
                renderEngine.removeRenderProgram(_inScatteringProgramObject);
                _inScatteringProgramObject = nullptr;
            }

            if (_inScatteringSupTermsProgramObject) {
                renderEngine.removeRenderProgram(_inScatteringSupTermsProgramObject);
                _inScatteringSupTermsProgramObject = nullptr;
            }

            if (_deltaEProgramObject) {
                renderEngine.removeRenderProgram(_deltaEProgramObject);
                _deltaEProgramObject = nullptr;
            }

            if (_deltaSProgramObject) {
                renderEngine.removeRenderProgram(_deltaSProgramObject);
                _deltaSProgramObject = nullptr;
            }

            if (_deltaSSupTermsProgramObject) {
                renderEngine.removeRenderProgram(_deltaSSupTermsProgramObject);
                _deltaSSupTermsProgramObject = nullptr;
            }

            return;
        }

    }
    _deltaJProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _deltaJProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);
}

void RenderablePlanet::unloadComputationPrograms() {
    
    RenderEngine& renderEngine = OsEng.renderEngine();

    if (_transmittanceProgramObject) {
        renderEngine.removeRenderProgram(_transmittanceProgramObject);
        _transmittanceProgramObject = nullptr;
    }

    if (_irradianceProgramObject) {
        renderEngine.removeRenderProgram(_irradianceProgramObject);
        _irradianceProgramObject = nullptr;
    }

    if (_irradianceSupTermsProgramObject) {
        renderEngine.removeRenderProgram(_irradianceSupTermsProgramObject);
        _irradianceSupTermsProgramObject = nullptr;
    }

    if (_inScatteringProgramObject) {
        renderEngine.removeRenderProgram(_inScatteringProgramObject);
        _inScatteringProgramObject = nullptr;
    }

    if (_inScatteringSupTermsProgramObject) {
        renderEngine.removeRenderProgram(_inScatteringSupTermsProgramObject);
        _inScatteringSupTermsProgramObject = nullptr;
    }

    if (_deltaEProgramObject) {
        renderEngine.removeRenderProgram(_deltaEProgramObject);
        _deltaEProgramObject = nullptr;
    }

    if (_deltaSProgramObject) {
        renderEngine.removeRenderProgram(_deltaSProgramObject);
        _deltaSProgramObject = nullptr;
    }

    if (_deltaSSupTermsProgramObject) {
        renderEngine.removeRenderProgram(_deltaSSupTermsProgramObject);
        _deltaSSupTermsProgramObject = nullptr;
    }

    if (_deltaJProgramObject) {
        renderEngine.removeRenderProgram(_deltaJProgramObject);
        _deltaJProgramObject = nullptr;
    }
}

void RenderablePlanet::createComputationTextures() {
    // TODO: Change precision of textures: GL_RGB16F to GL_RGB32F
    //========== Create Tables (textures) ==============

    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error before creating OpenGL textures for Atmosphere computation. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    _dummyTextureUnit.activate();
    glGenTextures(1, &_dummyTexture);
    glBindTexture(GL_TEXTURE_2D, _dummyTexture);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB16F, TRANSMITTANCE_TABLE_WIDTH,
        TRANSMITTANCE_TABLE_HEIGHT, 0, GL_RGB, GL_FLOAT, nullptr);

    //GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error 1 creating OpenGL textures for Atmosphere computation. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    ghoul::opengl::TextureUnit _dummy3DTextureUnit;
    _dummy3DTextureUnit.activate();
    glGenTextures(1, &_dummy3DTexture);
    glBindTexture(GL_TEXTURE_3D, _dummy3DTexture);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage3D(GL_TEXTURE_3D, 0, GL_RGB16F, MU_S_SAMPLES * NU_SAMPLES,
        MU_SAMPLES, R_SAMPLES, 0, GL_RGB, GL_FLOAT, nullptr);

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error 2 creating OpenGL textures for Atmosphere computation. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    //============== Transmittance =================
    _transmittanceTableTextureUnit.activate();
    glGenTextures(1, &_transmittanceTableTexture);
    glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB16F, TRANSMITTANCE_TABLE_WIDTH,
        TRANSMITTANCE_TABLE_HEIGHT, 0, GL_RGB, GL_FLOAT, nullptr);

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error 3 creating OpenGL textures for Atmosphere computation. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    //============== Irradiance =================
    _irradianceTableTextureUnit.activate();
    glGenTextures(1, &_irradianceTableTexture);
    glBindTexture(GL_TEXTURE_2D, _irradianceTableTexture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB16F, IRRADIANCE_TABLE_WIDTH,
        IRRADIANCE_TABLE_HEIGHT, 0, GL_RGB, GL_FLOAT, nullptr);

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error 4 creating OpenGL textures for Atmosphere computation. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    //============== InScattering =================
    _inScatteringTableTextureUnit.activate();
    glGenTextures(1, &_inScatteringTableTexture);
    glBindTexture(GL_TEXTURE_3D, _inScatteringTableTexture);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA16F_ARB, MU_S_SAMPLES * NU_SAMPLES,
        MU_SAMPLES, R_SAMPLES, 0, GL_RGB, GL_FLOAT, nullptr);

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error 5 creating OpenGL textures for Atmosphere computation. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    //============== Delta E =================
    _deltaETableTextureUnit.activate();
    glGenTextures(1, &_deltaETableTexture);
    glBindTexture(GL_TEXTURE_2D, _deltaETableTexture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB16F, DELTA_E_TABLE_WIDTH,
        DELTA_E_TABLE_HEIGHT, 0, GL_RGB, GL_FLOAT, nullptr);

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error 6 creating OpenGL textures for Atmosphere computation. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    //============== Delta S =================
    _deltaSRayleighTableTextureUnit.activate();
    glGenTextures(1, &_deltaSRayleighTableTexture);
    glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage3D(GL_TEXTURE_3D, 0, GL_RGB16F, MU_S_SAMPLES * NU_SAMPLES,
        MU_SAMPLES, R_SAMPLES, 0, GL_RGB, GL_FLOAT, nullptr);

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error 7 creating OpenGL textures for Atmosphere computation. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    _deltaSMieTableTextureUnit.activate();
    glGenTextures(1, &_deltaSMieTableTexture);
    glBindTexture(GL_TEXTURE_3D, _deltaSMieTableTexture);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage3D(GL_TEXTURE_3D, 0, GL_RGB16F, MU_S_SAMPLES * NU_SAMPLES,
        MU_SAMPLES, R_SAMPLES, 0, GL_RGB, GL_FLOAT, nullptr);

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error 8 creating OpenGL textures for Atmosphere computation. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    //============== Delta J (Radiance Scattered) =================
    _deltaJTableTextureUnit.activate();
    glGenTextures(1, &_deltaJTableTexture);
    glBindTexture(GL_TEXTURE_3D, _deltaJTableTexture);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage3D(GL_TEXTURE_3D, 0, GL_RGB16F, MU_S_SAMPLES * NU_SAMPLES,
        MU_SAMPLES, R_SAMPLES, 0, GL_RGB, GL_FLOAT, nullptr);

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error 9 creating OpenGL textures for Atmosphere computation. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }
}

void RenderablePlanet::deleteComputationTextures() {
    // Cleaning up
    glDeleteTextures(1, &_dummyTexture);
    glDeleteTextures(1, &_dummy3DTexture);
    glDeleteTextures(1, &_transmittanceTableTexture);
    glDeleteTextures(1, &_irradianceTableTexture);
    glDeleteTextures(1, &_inScatteringTableTexture);
    glDeleteTextures(1, &_deltaETableTexture);
    glDeleteTextures(1, &_deltaSRayleighTableTexture);
    glDeleteTextures(1, &_deltaSMieTableTexture);
    glDeleteTextures(1, &_deltaJTableTexture);
}

void RenderablePlanet::deleteUnusedComputationTextures() {
    glDeleteTextures(1, &_dummyTexture);
    glDeleteTextures(1, &_dummy3DTexture);
    glDeleteTextures(1, &_deltaETableTexture);
    glDeleteTextures(1, &_deltaSRayleighTableTexture);
    glDeleteTextures(1, &_deltaSMieTableTexture);
    glDeleteTextures(1, &_deltaJTableTexture);
}

void RenderablePlanet::loadAtmosphereDataIntoShaderProgram(std::unique_ptr<ghoul::opengl::ProgramObject> & shaderProg) {
    shaderProg->setUniform("Rg", _atmospherePlanetRadius);
    shaderProg->setUniform("Rt", _atmosphereRadius);
    shaderProg->setUniform("AVERAGE_GROUND_REFLECTANCE", _planetAverageGroundReflectance);
    shaderProg->setUniform("HR", _rayleighHeightScale);
    shaderProg->setUniform("betaR", _rayleighScatteringCoeff);
    shaderProg->setUniform("HM", _mieHeightScale);
    shaderProg->setUniform("betaMSca", _mieScatteringCoeff);
    shaderProg->setUniform("betaMEx", _mieExtinctionCoeff);
    shaderProg->setUniform("mieG", _miePhaseConstant);
}


void RenderablePlanet::executeCalculations(const GLuint vao, const GLenum drawBuffers[2], const GLsizei vertexSize) {
    // ===========================================================
    // See Precomputed Atmosphere Scattering from Bruneton et al. paper, algorithm 4.1:
    // ===========================================================    
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _dummyTexture, 0);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, _transmittanceTableTexture, 0);
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
        LERROR("Computation Framework not built.");
    glViewport(0, 0, TRANSMITTANCE_TABLE_WIDTH, TRANSMITTANCE_TABLE_HEIGHT);
    _transmittanceProgramObject->activate();
    loadAtmosphereDataIntoShaderProgram(_transmittanceProgramObject);
    renderQuadForCalc(vao, vertexSize);
    /*saveTextureToPPMFile(GL_COLOR_ATTACHMENT1, std::string("transmittance_texture.ppm"), 
        TRANSMITTANCE_TABLE_WIDTH, TRANSMITTANCE_TABLE_HEIGHT);*/

    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error executing computation 1. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    // line 2 in algorithm 4.1
    glBindTexture(GL_TEXTURE_2D, _dummyTexture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB16F,
        DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT, 0,
        GL_RGB, GL_FLOAT, nullptr);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _dummyTexture, 0);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, _deltaETableTexture, 0);
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
        LERROR("Computation Framework not built.");
    glViewport(0, 0, DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
    _irradianceProgramObject->activate();
    _irradianceProgramObject->setUniform("transmittanceTexture", _transmittanceTableTextureUnit);
    loadAtmosphereDataIntoShaderProgram(_irradianceProgramObject);
    renderQuadForCalc(vao, vertexSize);
    /*saveTextureToPPMFile(GL_COLOR_ATTACHMENT1, std::string("deltaE_table_texture.ppm"),
        DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);*/

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error executing computation 2. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    // line 3 in algorithm 4.1
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _dummy3DTexture, 0);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, _deltaSRayleighTableTexture, 0);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT2, _deltaSMieTableTexture, 0);
    GLenum colorBuffers[3] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1, GL_COLOR_ATTACHMENT2 };
    glDrawBuffers(3, colorBuffers);
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
        LERROR("Computation Framework not built.");
    glViewport(0, 0, MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
    _inScatteringProgramObject->activate();
    _inScatteringProgramObject->setUniform("transmittanceTexture", _transmittanceTableTextureUnit);
    loadAtmosphereDataIntoShaderProgram(_inScatteringProgramObject);
    for (int layer = 0; layer < R_SAMPLES; ++layer) {
        step3DTexture(_inScatteringProgramObject, layer);
        renderQuadForCalc(vao, vertexSize);
    }
    /*saveTextureToPPMFile(GL_COLOR_ATTACHMENT1, std::string("deltaS_rayleigh_texture.ppm"),
    MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
    saveTextureToPPMFile(GL_COLOR_ATTACHMENT2, std::string("deltaS_mie_texture.ppm"),
    MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);*/
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT2, GL_TEXTURE_2D, 0, 0);
    glDrawBuffers(2, drawBuffers);

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error executing computation 3. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    // line 4 in algorithm 4.1
    glBindTexture(GL_TEXTURE_2D, _dummyTexture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB16F,
        DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT, 0,
        GL_RGB, GL_FLOAT, nullptr);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _dummyTexture, 0);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, _irradianceTableTexture, 0);
    glDrawBuffers(2, drawBuffers);
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
        LERROR("Computation Framework not built.");
    glViewport(0, 0, DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
    _deltaEProgramObject->activate();
    _deltaEProgramObject->setUniform("line", 4);
    _deltaEProgramObject->setUniform("deltaETexture", _deltaETableTextureUnit);
    loadAtmosphereDataIntoShaderProgram(_deltaEProgramObject);
    renderQuadForCalc(vao, vertexSize);
    /*saveTextureToPPMFile(GL_COLOR_ATTACHMENT1, std::string("irradiance_texture.ppm"), 
        DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);*/

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error executing computation 4. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    // line 5 in algorithm 4.1
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _dummy3DTexture, 0);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, _inScatteringTableTexture, 0);
    glViewport(0, 0, MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
    _deltaSProgramObject->activate();
    _deltaSProgramObject->setUniform("deltaSRTexture", _deltaSRayleighTableTextureUnit);
    _deltaSProgramObject->setUniform("deltaSMTexture", _deltaSMieTableTextureUnit);
    loadAtmosphereDataIntoShaderProgram(_deltaSProgramObject);
    for (int layer = 0; layer < R_SAMPLES; ++layer) {
        step3DTexture(_deltaSProgramObject, layer, false);
        renderQuadForCalc(vao, vertexSize);
    }
    /*saveTextureToPPMFile(GL_COLOR_ATTACHMENT1, std::string("S_texture.ppm"),
    MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);*/

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error executing computation 5. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    // loop in line 6 in algorithm 4.1
    for (int scatteringOrder = 2; scatteringOrder <= 4; ++scatteringOrder) {

        // line 7 in algorithm 4.1
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _dummy3DTexture, 0);
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, _deltaJTableTexture, 0);
        if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
            LERROR("Computation Framework not built.");
        glViewport(0, 0, MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
        _deltaJProgramObject->activate();
        if (scatteringOrder == 2)
            _deltaJProgramObject->setUniform("first", 1.0f);
        else
            _deltaJProgramObject->setUniform("first", 0.0f);
        _deltaJProgramObject->setUniform("transmittanceTexture", _transmittanceTableTextureUnit);
        _deltaJProgramObject->setUniform("deltaETexture", _deltaETableTextureUnit);
        _deltaJProgramObject->setUniform("deltaSRTexture", _deltaSRayleighTableTextureUnit);
        _deltaJProgramObject->setUniform("deltaSMTexture", _deltaSMieTableTextureUnit);
        loadAtmosphereDataIntoShaderProgram(_deltaJProgramObject);
        for (int layer = 0; layer < R_SAMPLES; ++layer) {
            step3DTexture(_deltaJProgramObject, layer);
            renderQuadForCalc(vao, vertexSize);
        }
        /*std::stringstream sst;
        sst << "deltaJ_texture-scattering_order-" << scatteringOrder << ".ppm";
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT1, sst.str(),
        MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);*/

        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            std::stringstream ss;
            ss << "Error executing computation 6. OpenGL error: " << errString << std::endl;
            LERROR(ss.str());
        }

        // line 8 in algorithm 4.1
        glBindTexture(GL_TEXTURE_2D, _dummyTexture);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB16F,
            DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT, 0,
            GL_RGB, GL_FLOAT, nullptr);
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _dummyTexture, 0);
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, _deltaETableTexture, 0);
        if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
            LERROR("Computation Framework not built.");
        glViewport(0, 0, DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
        _irradianceSupTermsProgramObject->activate();
        if (scatteringOrder == 2)
            _irradianceSupTermsProgramObject->setUniform("first", 1.0f);
        else
            _irradianceSupTermsProgramObject->setUniform("first", 0.0f);
        _irradianceSupTermsProgramObject->setUniform("transmittanceTexture", _transmittanceTableTextureUnit);
        _irradianceSupTermsProgramObject->setUniform("deltaSRTexture", _deltaSRayleighTableTextureUnit);
        _irradianceSupTermsProgramObject->setUniform("deltaSMTexture", _deltaSMieTableTextureUnit);
        loadAtmosphereDataIntoShaderProgram(_irradianceSupTermsProgramObject);
        renderQuadForCalc(vao, vertexSize);
        /*sst.str(std::string());
        sst << "deltaE_texture-scattering_order-" << scatteringOrder << ".ppm";
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT1, sst.str(),
        DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);*/

        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            std::stringstream ss;
            ss << "Error executing computation 7. OpenGL error: " << errString << std::endl;
            LERROR(ss.str());
        }

        // line 9 in algorithm 4.1
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _dummy3DTexture, 0);
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, _deltaSRayleighTableTexture, 0);
        if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
            LERROR("Computation Framework not built.");
        glViewport(0, 0, MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
        _inScatteringSupTermsProgramObject->activate();
        if (scatteringOrder == 2)
            _inScatteringSupTermsProgramObject->setUniform("first", 1.0f);
        else
            _inScatteringSupTermsProgramObject->setUniform("first", 0.0f);
        _inScatteringSupTermsProgramObject->setUniform("transmittanceTexture", _transmittanceTableTextureUnit);
        _inScatteringSupTermsProgramObject->setUniform("deltaJTexture", _deltaJTableTextureUnit);
        loadAtmosphereDataIntoShaderProgram(_inScatteringSupTermsProgramObject);
        for (int layer = 0; layer < R_SAMPLES; ++layer) {
            step3DTexture(_inScatteringSupTermsProgramObject, layer);
            renderQuadForCalc(vao, vertexSize);
        }
        /*sst.str(std::string());
        sst << "deltaS_texture-scattering_order-" << scatteringOrder << ".ppm";
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT1, sst.str(),
        MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);*/

        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            std::stringstream ss;
            ss << "Error executing computation 8. OpenGL error: " << errString << std::endl;
            LERROR(ss.str());
        }

        glEnable(GL_BLEND);
        glBlendEquationSeparate(GL_FUNC_ADD, GL_FUNC_ADD);
        glBlendFuncSeparate(GL_ONE, GL_ONE, GL_ONE, GL_ONE);

        // line 10 in algorithm 4.1
        glBindTexture(GL_TEXTURE_2D, _dummyTexture);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB16F,
            DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT, 0,
            GL_RGB, GL_FLOAT, nullptr);
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _dummyTexture, 0);
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, _irradianceTableTexture, 0);
        if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
            LERROR("Computation Framework not built.");
        glViewport(0, 0, DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
        _deltaEProgramObject->activate();
        _deltaEProgramObject->setUniform("line", 10);
        _deltaEProgramObject->setUniform("deltaETexture", _deltaETableTextureUnit);
        loadAtmosphereDataIntoShaderProgram(_deltaEProgramObject);
        renderQuadForCalc(vao, vertexSize);
        /*sst.str(std::string());
        sst << "irradianceTable_order-" << scatteringOrder << ".ppm";
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT1, sst.str(),
        DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);*/

        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            std::stringstream ss;
            ss << "Error executing computation 9. OpenGL error: " << errString << std::endl;
            LERROR(ss.str());
        }

        // line 11 in algorithm 4.1
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _dummy3DTexture, 0);
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, _inScatteringTableTexture, 0);
        if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
            LERROR("Computation Framework not built.");
        glViewport(0, 0, MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
        _deltaSSupTermsProgramObject->activate();
        _deltaSSupTermsProgramObject->setUniform("deltaSTexture", _deltaSRayleighTableTextureUnit);
        loadAtmosphereDataIntoShaderProgram(_deltaSSupTermsProgramObject);
        for (int layer = 0; layer < R_SAMPLES; ++layer) {
            step3DTexture(_deltaSSupTermsProgramObject, layer, false);
            renderQuadForCalc(vao, vertexSize);
        }
        /*sst.str(std::string());
        sst << "inscatteringTable_order-" << scatteringOrder << ".ppm";
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT1, sst.str(),
        MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);*/

        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            std::stringstream ss;
            ss << "Error executing computation 10. OpenGL error: " << errString << std::endl;
            LERROR(ss.str());
        }

        glDisable(GL_BLEND);
    }

}

void RenderablePlanet::preCalculateAtmosphereParam() {

    //==========================================================
    //========= Load Shader Programs for Calculations ==========
    //==========================================================
    loadComputationPrograms();

    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error loading shader programs for Atmosphere computation. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    //==========================================================
    //============ Create Textures for Calculations ============
    //==========================================================
    createComputationTextures();

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error creating textures for Atmosphere computation. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    // Preparing FBO...
    GLint defaultFBO;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);
    
    GLuint calcFBO;
    glGenFramebuffers(1, &calcFBO);
    glBindFramebuffer(GL_FRAMEBUFFER, calcFBO);
    glReadBuffer(GL_COLOR_ATTACHMENT1);
    //glDrawBuffer(GL_COLOR_ATTACHMENT1);
    GLenum drawBuffers[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
    glDrawBuffers(2, drawBuffers);

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        std::stringstream ss;
        ss << "Error creating FrameBuffer Object for Atmosphere computation. OpenGL error: " << errString << std::endl;
        LERROR(ss.str());
    }

    // Prepare for rendering/calculations
    GLuint calcVAO;
    GLuint calcVBO;
    createRenderQuad(&calcVAO, &calcVBO, 1.0f);

    GLint m_viewport[4];
    glGetIntegerv(GL_VIEWPORT, m_viewport);

    // Execute Calculations...
    LDEBUG("Starting precalculations for scattering effects...");

    //==========================================================
    //=================== Execute Calculations =================
    //==========================================================
    executeCalculations(calcVAO, drawBuffers, 6);

    deleteUnusedComputationTextures();
    
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glViewport(m_viewport[0], m_viewport[1],
        m_viewport[2], m_viewport[3]);
    glDeleteBuffers(1, &calcVBO);
    glDeleteVertexArrays(1, &calcVAO);
    glDeleteFramebuffers(1, &calcFBO);

    LDEBUG("Ended precalculations for scattering effects...");
}

void RenderablePlanet::createAtmosphereFBO() {
    
    GLint m_viewport[4];
    glGetIntegerv(GL_VIEWPORT, m_viewport);

    /*GLint defaultFBO;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);    */

    _atmosphereTextureUnit.activate();
    glGenTextures(1, &_atmosphereTexture);
    glBindTexture(GL_TEXTURE_2D, _atmosphereTexture);
    //glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _atmosphereTexture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, m_viewport[2],
        m_viewport[2], 0, GL_RGB, GL_UNSIGNED_BYTE, nullptr);
    /*glTexImage2DMultisample(GL_TEXTURE_2D_MULTISAMPLE, 8, GL_RGBA,
        m_viewport[2], m_viewport[3], true);*/
    glGenFramebuffers(1, &_atmosphereFBO);
    /*glBindFramebuffer(GL_FRAMEBUFFER, _atmosphereFBO);
    glReadBuffer(GL_COLOR_ATTACHMENT1);
    GLenum drawBuffers[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
    glDrawBuffers(2, drawBuffers);

    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        std::stringstream ss;
        ss << "Error creating atmosphere framebuffer. OpenGL error: " << err << std::endl;
        LERROR(ss.str());
    }

    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _dummyTexture, 0);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, _atmosphereTexture, 0);
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
        LERROR("Atmosphere Framework not built.");

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glViewport(m_viewport[0], m_viewport[1],
        m_viewport[2], m_viewport[3]);*/

}

void RenderablePlanet::createRenderQuad(GLuint * vao, GLuint * vbo, const GLfloat size) {
    
    glGenVertexArrays(1, vao);    
    glGenBuffers(1, vbo);
    glBindVertexArray(*vao);
    glBindBuffer(GL_ARRAY_BUFFER, *vbo);
    
    const GLfloat vertex_data[] = {
        //      x      y     z     w
        -size, -size, 0.0f, 1.0f,
        size,    size, 0.0f, 1.0f,
        -size,  size, 0.0f, 1.0f,
        -size, -size, 0.0f, 1.0f,
        size, -size, 0.0f, 1.0f,
        size,    size, 0.0f, 1.0f
    };

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 4, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(0);

    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        std::stringstream ss;
        ss << "Error creating vertexbuffer for computation. OpenGL error: " << err << std::endl;
        LERROR(ss.str());
    }
}

void RenderablePlanet::renderQuadForCalc(const GLuint vao, const GLsizei size)
{
    glBindVertexArray(vao);
    glDrawArrays(GL_TRIANGLES, 0, size);   
    glBindVertexArray(0);
}

void RenderablePlanet::step3DTexture(std::unique_ptr<ghoul::opengl::ProgramObject> & shaderProg,
                                     const int layer, const bool doCalc) 
{
    // See OpenGL redbook 8th Edition page 556 for Layered Rendering
    //if (doCalc) 
    {
        
        float earth2 = _atmospherePlanetRadius * _atmospherePlanetRadius;
        float atm2   = _atmosphereRadius * _atmosphereRadius;
        float diff   = atm2 - earth2;
        float r      = static_cast<double>(layer) / static_cast<double>(R_SAMPLES - 1);
        float r2     = r * r;
        float c = 0.0;
        if (layer == 0)
            c = 0.01f;
        else if (layer == (R_SAMPLES - 1))
            c = -0.001f;
        else
            c = 0.0;
        r = sqrtf(earth2 + r2 * diff) + c;
        float dmin  = _atmosphereRadius - r;
        float dmax  = sqrtf(r * r - earth2) + sqrtf(diff);
        float dminp = r - _atmospherePlanetRadius;
        float dmaxp = sqrtf(r * r - earth2);

        shaderProg->setUniform("r", r);
        shaderProg->setUniform("dhdH", dmin, dmax, dminp, dmaxp);
    }

    shaderProg->setUniform("layer", static_cast<int>(layer));    
}

void RenderablePlanet::saveTextureToPPMFile(const GLenum color_buffer_attachment, const std::string & fileName,
    const int width, const int height) const {
    std::fstream ppmFile;

    ppmFile.open(fileName.c_str(), std::fstream::out);
    if (ppmFile.is_open()) {
        unsigned char * pixels = new unsigned char[width*height * 3];
        for (int t = 0; t < width*height * 3; ++t)
            pixels[t] = 255;


        // check OpenGL error
        GLenum err;
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errorString = gluErrorString(err);

            std::cout << "\n\nBefore Reading Texture from card. OpenGL error: "
                << err << " - " << errorString << std::endl;
        }

        glReadBuffer(color_buffer_attachment);
        glReadPixels(0, 0, width, height, GL_RGB, GL_UNSIGNED_BYTE, pixels);

        ppmFile << "P3" << std::endl;
        ppmFile << width << " " << height << std::endl;
        ppmFile << "255" << std::endl;

        std::cout << "\n\nFILE\n\n";
        int k = 0;
        for (int i = 0; i < width; i++) {
            for (int j = 0; j < height; j++) {
                ppmFile << (unsigned int)pixels[k] << " " << (unsigned int)pixels[k + 1] << " " << (unsigned int)pixels[k + 2] << " ";
                k += 3;
            }
            ppmFile << std::endl;
        }
        delete[] pixels;

        ppmFile.close();
    }
}

}  // namespace openspace
