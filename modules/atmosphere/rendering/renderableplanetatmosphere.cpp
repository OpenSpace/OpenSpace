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
#include <modules/atmosphere/rendering/renderableplanetatmosphere.h>

#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <modules/space/rendering/planetgeometry.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtx/string_cast.hpp>

#include <memory>
#include <fstream>
#include <ostream>

#define _USE_MATH_DEFINES
#include <math.h>

#define _ATMOSPHERE_DEBUG
#define _SAVE_ATMOSPHERE_TEXTURES

namespace {
    const std::string _loggerCat = "RenderablePlanetAtmosphere";

    const std::string keyFrame                    = "Frame";
    const std::string keyGeometry                 = "Geometry";
    const std::string keyRadius                   = "Radius";
    const std::string keyShading                  = "PerformShading";
    const std::string keyShadowGroup              = "Shadow_Group";
    const std::string keyShadowSource             = "Source";
    const std::string keyShadowCaster             = "Caster";
    const std::string keyAtmosphere               = "Atmosphere";
    const std::string keyAtmosphereRadius         = "AtmoshereRadius";
    const std::string keyPlanetRadius             = "PlanetRadius";
    const std::string keyAverageGroundReflectance = "PlanetAverageGroundReflectance";
    const std::string keyRayleigh                 = "Rayleigh";
    const std::string keyRayleighHeightScale      = "H_R";
    const std::string keyMie                      = "Mie";
    const std::string keyMieHeightScale           = "H_M";
    const std::string keyMiePhaseConstant         = "G";
    const std::string keyBody                     = "Body";
}

namespace openspace {

    RenderablePlanetAtmosphere::RenderablePlanetAtmosphere(const ghoul::Dictionary& dictionary)
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
        , _irradianceFinalProgramObject(nullptr)
        , _deltaSProgramObject(nullptr)
        , _deltaSSupTermsProgramObject(nullptr)
        , _deltaJProgramObject(nullptr)
        , _cleanTextureProgramObject(nullptr)
        , _atmosphereProgramObject(nullptr)
        , _texture(nullptr)
        , _nightTexture(nullptr)
        , _reflectanceTexture(nullptr)
        , _heightMapTexture(nullptr)
        , _cloudsTexture(nullptr)
        , _geometry(nullptr)
        , _performShading("performShading", "Perform Shading", true)
        , _rotation("rotation", "Rotation", 0, 0, 360)
        , _saveDeferredFramebuffer("save deferred framebuffer to disk", "Save deferred framebuffer to disk", false)
        , _alpha(1.f)
        , _planetRadius(0.f)
        , _transmittanceTableTexture(0)
        , _irradianceTableTexture(0)
        , _inScatteringTableTexture(0)
        , _deltaETableTexture(0)
        , _deltaSRayleighTableTexture(0)
        , _deltaSMieTableTexture(0)
        , _deltaJTableTexture(0)
        , _atmosphereTexture(0)
        , _atmosphereDepthTexture(0)
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
        , _sunRadianceIntensity(50.0f)
        , _hasNightTexture(false)
        , _hasHeightTexture(false)
        , _hasReflectanceTexture(false)
        , _hasCloudsTexture(false)
        , _shadowEnabled(false)
        , _atmosphereHeightP("atmmosphereHeight", "Atmosphere Height (KM)", 60.0f, 0.1f, 100.0f)
        , _groundAverageReflectanceP("averageGroundReflectance", "Average Ground Reflectance (%)", 0.1f, 0.0f, 1.0f)
        , _rayleighHeightScaleP("rayleighHeightScale", "Rayleigh Height Scale (KM)", 8.0f, 0.1f, 20.0f)
        , _mieHeightScaleP("mieHeightScale", "Mie Height Scale (KM)", 1.2f, 0.1f, 5.0f)
        , _mieScatteringCoefficientP("mieScatteringCoefficient", "Mie Scattering Coefficient (x10e-3)", 4.0f, 1.0f, 20.0f)
        , _mieScatteringExtinctionPropCoefficientP("mieScatteringExtinctionPropCoefficient",
            "Mie Scattering/Extinction Proportion Coefficient (%)", 0.9f, 0.1f, 1.0f)
        , _mieAsymmetricFactorGP("mieAsymmetricFactorG", "Mie Asymmetric Factor G", 1.0f, -1.0f, 1.0f)
        , _sunIntensityP("sunIntensity", "Sun Intensity", 50.0f, 0.1f, 100.0f)
    {
        std::string name;
        bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
        ghoul_assert(success,
            "RenderablePlanetAtmosphere need the '" << SceneGraphNode::KeyName << "' be specified");

        //=======================================================
        //======== Reads Geometry Entries in mod file =============
        //=======================================================
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

        //===============================================================
        //======== Reads Body and Frame Entries in mod file =============
        //===============================================================
        dictionary.getValue(keyFrame, _frame);
        dictionary.getValue(keyBody, _target);


        //============================================================
        //======== Reads the Texture Entries in mod file =============
        //============================================================
        // TODO: textures need to be replaced by a good system similar to the geometry as soon
        // as the requirements are fixed (ab)
        std::string texturePath = "";
        success = dictionary.getValue("Textures.Color", texturePath);
        if (success)
            _colorTexturePath = absPath(texturePath);

        std::string nightTexturePath = "";
        dictionary.getValue("Textures.Night", nightTexturePath);
        if (nightTexturePath != "") {
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

        //=======================================================
        //=========== Adding Textures as Properties =============
        //=======================================================
        addPropertySubOwner(_geometry);

        addProperty(_colorTexturePath);
        _colorTexturePath.onChange(std::bind(&RenderablePlanetAtmosphere::loadTexture, this));

        addProperty(_nightTexturePath);
        _nightTexturePath.onChange(std::bind(&RenderablePlanetAtmosphere::loadTexture, this));

        addProperty(_heightMapTexturePath);
        _heightMapTexturePath.onChange(std::bind(&RenderablePlanetAtmosphere::loadTexture, this));

        addProperty(_reflectanceTexturePath);
        _reflectanceTexturePath.onChange(std::bind(&RenderablePlanetAtmosphere::loadTexture, this));

        addProperty(_cloudsTexturePath);
        _cloudsTexturePath.onChange(std::bind(&RenderablePlanetAtmosphere::loadTexture, this));

        addProperty(_heightExaggeration);


        //=========================================================
        //======== Shading and Rotation as Properties =============
        //=========================================================
        if (dictionary.hasKeyAndValue<bool>(keyShading)) {
            bool shading;
            dictionary.getValue(keyShading, shading);
            _performShading = shading;
        }

        addProperty(_performShading);
        // Mainly for debugging purposes @AA
        addProperty(_rotation);


        //================================================================
        //======== Reads Shadow (Eclipses) Entries in mod file ===========
        //================================================================
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

        //================================================================
        //=========== Reads Atmosphere Entries in mod file ===============
        //================================================================
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

                //========================================================
                //============== Atmosphere Properties ===================
                //========================================================

                _atmosphereHeightP.set(_atmosphereRadius - _atmospherePlanetRadius);
                _atmosphereHeightP.onChange(std::bind(&RenderablePlanetAtmosphere::updateAtmosphereParameters, this));
                addProperty(_atmosphereHeightP);

                _groundAverageReflectanceP.set(_planetAverageGroundReflectance);
                _groundAverageReflectanceP.onChange(std::bind(&RenderablePlanetAtmosphere::updateAtmosphereParameters, this));
                addProperty(_groundAverageReflectanceP);

                _rayleighHeightScaleP.set(_rayleighHeightScale);
                _rayleighHeightScaleP.onChange(std::bind(&RenderablePlanetAtmosphere::updateAtmosphereParameters, this));
                addProperty(_rayleighHeightScaleP);

                _mieHeightScaleP.set(_mieHeightScale);
                _mieHeightScaleP.onChange(std::bind(&RenderablePlanetAtmosphere::updateAtmosphereParameters, this));
                addProperty(_mieHeightScaleP);

                _mieScatteringCoefficientP.set(_mieScatteringCoeff.r * 1000.0f);
                _mieScatteringCoefficientP.onChange(std::bind(&RenderablePlanetAtmosphere::updateAtmosphereParameters, this));
                addProperty(_mieScatteringCoefficientP);

                _mieScatteringExtinctionPropCoefficientP.set(_mieScatteringCoeff.r / _mieExtinctionCoeff.r);
                _mieScatteringExtinctionPropCoefficientP.onChange(std::bind(&RenderablePlanetAtmosphere::updateAtmosphereParameters, this));
                addProperty(_mieScatteringExtinctionPropCoefficientP);

                _mieAsymmetricFactorGP.set(_miePhaseConstant);
                _mieAsymmetricFactorGP.onChange(std::bind(&RenderablePlanetAtmosphere::updateAtmosphereParameters, this));
                addProperty(_mieAsymmetricFactorGP);

                _sunIntensityP.set(_sunRadianceIntensity);
                _sunIntensityP.onChange(std::bind(&RenderablePlanetAtmosphere::updateAtmosphereParameters, this));
                addProperty(_sunIntensityP);
            }


#ifdef _ATMOSPHERE_DEBUG
            _saveDeferredFramebuffer = false;
            addProperty(_saveDeferredFramebuffer);
#endif
        }
    }

    RenderablePlanetAtmosphere::~RenderablePlanetAtmosphere() {
    }

    bool RenderablePlanetAtmosphere::initialize() {
        RenderEngine& renderEngine = OsEng.renderEngine();

        GLenum err;
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Checking System State before initialization. OpenGL error: " << errString);
        }

        //===================================================================
        //=========== Defines the shading program to be executed ============
        //===================================================================
        if (_programObject == nullptr && _atmosphereEnabled ) {
            // atmosphere program
            _programObject = renderEngine.buildRenderProgram(
                "atmosphereAndShadowProgram",
                "${MODULE_ATMOSPHERE}/shaders/atmosphere_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/atmosphere_fs.glsl");
            if (!_programObject)
                return false;
        }
        else if (_programObject == nullptr) {
            // pscstandard
            _programObject = renderEngine.buildRenderProgram(
                "pscstandard",
                "${MODULE_ATMOSPHERE}/shaders/pscstandard_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/pscstandard_fs.glsl");
            if (!_programObject)
                return false;
        }

        using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
        _programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _programObject->setIgnoreUniformLocationError(IgnoreError::Yes);

#ifdef _ATMOSPHERE_DEBUG
        // DEBUG: Deferred rendering of the Atmosphere
        _deferredAtmosphereProgramObject = ghoul::opengl::ProgramObject::Build(
            "atmosphereDeferredProgram",
            "${MODULE_ATMOSPHERE}/shaders/atmosphere_deferred_vs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/atmosphere_deferred_fs.glsl");
        _deferredAtmosphereProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _deferredAtmosphereProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);
        if (!_deferredAtmosphereProgramObject)
            return false;
#endif

        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error after loading shading programs. OpenGL error: " << errString);
        }

        //===================================================================
        //=========== Load textures defined in mod file to GPU ==============
        //===================================================================
        loadTexture();

        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error loading textures. OpenGL error: " << errString);
        }

        //========================================================================
        //======== Initialize the current geometry (SimpleSphereGeometry) ========
        //========================================================================
        _geometry->initialize(this);

        // Deactivate any previously activated shader program.
        _programObject->deactivate();

        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error before atmosphere computations. OpenGL error: " << errString);
        }

        //========================================================================
        //============ Pre-compute all necessary Atmosphere Tables  ==============
        //========================================================================
        if (_atmosphereEnabled && !_atmosphereCalculated) {
            preCalculateAtmosphereParam();
#ifdef _ATMOSPHERE_DEBUG
            // DEBUG: FBO for atmosphere deferred rendering.
            createAtmosphereFBO();
            createRenderQuad(&_atmosphereRenderVAO, &_atmosphereRenderVBO, 1.0f);
            count = 0;
#endif
            _atmosphereCalculated = true;
        }
        
        return isReady();
    }

    bool RenderablePlanetAtmosphere::deinitialize() {
        if (_geometry) {
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

        if (_cleanTextureProgramObject) {
            renderEngine.removeRenderProgram(_cleanTextureProgramObject);
            _cleanTextureProgramObject = nullptr;
        }

        _geometry = nullptr;
        _texture = nullptr;
        _nightTexture = nullptr;
        _reflectanceTexture = nullptr;
        _cloudsTexture = nullptr;

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

    bool RenderablePlanetAtmosphere::isReady() const {
        bool ready = true;
        ready &= (_programObject != nullptr);
        ready &= (_texture != nullptr);
        ready &= (_geometry != nullptr);
        return ready;
    }

    void RenderablePlanetAtmosphere::render(const RenderData& data) {
        // activate shader
        _programObject->activate();

        // scale the planet to appropriate size since the planet is a unit sphere
        glm::mat4 transform = glm::mat4(1);

        //earth needs to be rotated for that to work.
        glm::mat4 rot = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(1, 0, 0));
        glm::mat4 roty = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(0, -1, 0));
        glm::mat4 rotProp = glm::rotate(transform, glm::radians(static_cast<float>(_rotation)), glm::vec3(0, 1, 0));

        // _stateMatrix is the Matrix transformation from _frame coordinate system (Earth in this case)
        // to "GALATIC" coordinate system.
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
            }
        }
        transform = transform * rot * roty * rotProp;

        // setup the data to the shader
        double  lt;
        glm::dvec3 sunPosFromPlanet =
            SpiceManager::ref().targetPosition("SUN", _target, "GALACTIC", {}, _time, lt);
        sunPosFromPlanet *= 1000.0; // from Km to m
        psc sunPosFromPlanetPSC = PowerScaledCoordinate::CreatePowerScaledCoordinate(
            sunPosFromPlanet.x, sunPosFromPlanet.y, sunPosFromPlanet.z);

        glm::dvec3 planetPosFromSun =
            SpiceManager::ref().targetPosition(_target, "SUN", "GALACTIC", {}, _time, lt);
        psc planetPosFronSunPSC = PowerScaledCoordinate::CreatePowerScaledCoordinate(
            planetPosFromSun.x, planetPosFromSun.y, planetPosFromSun.z);

        // Camera direction (vector)
        glm::vec3 cam_dir = glm::normalize(data.camera.position().vec3() - planetPosFronSunPSC.vec3());

        _programObject->setUniform("transparency", _alpha);
        _programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
        _programObject->setUniform("ModelTransform", transform);

        // Normal Transformation
        glm::mat4 translateObjTransf = glm::translate(glm::mat4(1.0), data.position.vec3());
        glm::mat4 translateCamTransf = glm::translate(glm::mat4(1.0), -data.camera.position().vec3());
        // The following scale comes from PSC transformations.
        float scaleFactor = data.camera.scaling().x * powf(10.0, data.camera.scaling().y);
        glm::mat4 scaleCamTransf = glm::scale(glm::mat4(1.0), glm::vec3(scaleFactor));

        glm::mat4 ModelViewTransf = data.camera.viewMatrix() * scaleCamTransf *
            translateCamTransf * translateObjTransf * transform;

        if (_atmosphereEnabled)
            _programObject->setUniform("NormalTransform",
                glm::transpose(glm::inverse(ModelViewTransf)));

        //=== Sets campos, objpos, camrot and scaling in PSC coords for PSC calc in shader file ===
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

        //=============================================================================
        //============= Eclipse Shadow Calculations and Uniforms Loading ==============
        //=============================================================================
        // TODO: Move Calculations to VIEW SPACE (let's avoid precision problems...)
        if (!_shadowConfArray.empty()) {
            std::vector<ShadowRenderingStruct> shadowDataArray;
            shadowDataArray.reserve(_shadowConfArray.size());

            for (const auto & shadowConf : _shadowConfArray) {
                // TO REMEMBER: all distances and lengths in world coordinates are in meters!!! We need to move this to view space...
                // Getting source and caster:
                glm::dvec3 sourcePos = SpiceManager::ref().targetPosition(shadowConf.source.first, "SUN", "GALACTIC", {}, _time, lt);
                sourcePos *= 1000.0; // converting to meters
                glm::dvec3 casterPos = SpiceManager::ref().targetPosition(shadowConf.caster.first, "SUN", "GALACTIC", {}, _time, lt);
                casterPos *= 1000.0; // converting to meters
                psc caster_pos = PowerScaledCoordinate::CreatePowerScaledCoordinate(casterPos.x, casterPos.y, casterPos.z);

                // First we determine if the caster is shadowing the current planet (all calculations in World Coordinates):
                glm::vec3 planetCasterVec = (caster_pos - data.position).vec3();
                glm::vec3 sourceCasterVec = glm::vec3(casterPos - sourcePos);
                float sc_length = glm::length(sourceCasterVec);
                glm::vec3 planetCaster_proj = (glm::dot(planetCasterVec, sourceCasterVec) / (sc_length*sc_length)) * sourceCasterVec;
                float d_test = glm::length(planetCasterVec - planetCaster_proj);
                float xp_test = shadowConf.caster.second * sc_length / (shadowConf.source.second + shadowConf.caster.second);
                float rp_test = shadowConf.caster.second * (glm::length(planetCaster_proj) + xp_test) / xp_test;

                float casterDistSun = glm::length(casterPos);
                float planetDistSun = glm::length(data.position.vec3());

                ShadowRenderingStruct shadowData;
                shadowData.isShadowing = false;

                if (((d_test - rp_test) < _planetRadius) &&
                    (casterDistSun < planetDistSun)) {
                    // The current caster is shadowing the current planet
                    shadowData.isShadowing = true;
                    shadowData.rs = shadowConf.source.second;
                    shadowData.rc = shadowConf.caster.second;
                    shadowData.sourceCasterVec = sourceCasterVec;
                    shadowData.xp = xp_test;
                    shadowData.xu = shadowData.rc * sc_length / (shadowData.rs - shadowData.rc);
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


        //=============================================================================
        //================== Atmosphere Rendering and Uniforms Loading ================
        //=============================================================================
        if (_atmosphereEnabled) {
            // Object Space (in Km)
            glm::mat4 obj2World = glm::translate(glm::mat4(1.0), data.position.vec3() / 1000.0f);

            glm::mat4 M = glm::mat4(data.camera.combinedViewMatrix()) * scaleCamTransf * obj2World * transform;

            glm::mat4 completeInverse = glm::inverse(M);

            _programObject->setUniform("completeInverse", completeInverse);
            _programObject->setUniform("projInverse", glm::inverse(data.camera.projectionMatrix()));

            // This is camera position and planet position vector in object coordinates, in Km.
            glm::mat4 world2Obj = glm::inverse(obj2World * transform);
            glm::vec4 cameraPosObj = world2Obj * glm::vec4(data.camera.position().vec3() / 1000.0f, 1.0);
            glm::vec4 planetPositionObj = world2Obj * glm::vec4(data.position.vec3() / 1000.0f, 1.0);
            _programObject->setUniform("cameraPosObj", cameraPosObj);
            _programObject->setUniform("planetPositionObj", planetPositionObj);

            // I know it is (0,0,0). It is here just for sake of sanity. :-p
            glm::dvec3 sunPosWorld =
                SpiceManager::ref().targetPosition("SUN", "SUN", "GALACTIC", {}, _time, lt);
            glm::vec4 sunPosObj = world2Obj * glm::vec4(sunPosWorld.x, sunPosWorld.y, sunPosWorld.z, 1.0);
            _programObject->setUniform("sunPositionObj", glm::vec3(sunPosObj));
            
            ghoul::opengl::TextureUnit transmittanceTableTextureUnit;
            transmittanceTableTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
            _programObject->setUniform("transmittanceTexture", transmittanceTableTextureUnit);

            ghoul::opengl::TextureUnit irradianceTableTextureUnit;
            irradianceTableTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _irradianceTableTexture);
            _programObject->setUniform("irradianceTexture", irradianceTableTextureUnit);

            ghoul::opengl::TextureUnit inScatteringTableTextureUnit;
            inScatteringTableTextureUnit.activate();
            glBindTexture(GL_TEXTURE_3D, _inScatteringTableTexture);
            _programObject->setUniform("inscatterTexture", inScatteringTableTextureUnit);

            GLint m_viewport[4];
            glGetIntegerv(GL_VIEWPORT, m_viewport);
            _programObject->setUniform("screenX", (float)m_viewport[0]);
            _programObject->setUniform("screenY", (float)m_viewport[1]);
            _programObject->setUniform("screenWIDTH", (float)m_viewport[2]);
            _programObject->setUniform("screenHEIGHT", (float)m_viewport[3]);


            _programObject->setUniform("Rg", _atmospherePlanetRadius);
            _programObject->setUniform("Rt", _atmosphereRadius);
            _programObject->setUniform("AverageGroundReflectance", _planetAverageGroundReflectance);
            _programObject->setUniform("HR", _rayleighHeightScale);
            _programObject->setUniform("betaRayleigh", _rayleighScatteringCoeff);
            _programObject->setUniform("HM", _mieHeightScale);
            _programObject->setUniform("betaMieScattering", _mieScatteringCoeff);
            _programObject->setUniform("betaMieExtinction", _mieExtinctionCoeff);
            _programObject->setUniform("mieG", _miePhaseConstant);
            _programObject->setUniform("sunRadiance", _sunRadianceIntensity);


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

#ifdef _ATMOSPHERE_DEBUG
        // DEBUG: Deferred Rendering of the atmosphere to a texture.
        // Render Atmosphere to a texture:
        if (_atmosphereEnabled) {

            /*std::cout << "\nTestes..." << std::endl;
            glm::dvec3 sunPosSun = SpiceManager::ref().targetPosition("SUN", "SUN", "GALACTIC", {}, _time, lt);
            glm::dvec3 earthPosSun = SpiceManager::ref().targetPosition("EARTH", "SUN", "GALACTIC", {}, _time, lt);
            std::cout << "\n\nSun in Sun: " << sunPosSun.x << ", " << sunPosSun.y << ", " << sunPosSun.z << std::endl;
            std::cout << "\n\nEarth in Sun: " << earthPosSun.x << ", " << earthPosSun.y << ", " << earthPosSun.z << std::endl;
            std::cout << "\n\nCam Position in Sun: " << data.camera.position().vec3().x << ", " << data.camera.position().vec3().y << ", " << data.camera.position().vec3().z << std::endl;
            std::cout << "\n\nCam Position from Earth in Sun: " << cam_dir.x << ", " << cam_dir.y << ", " << cam_dir.z << std::endl;

            glm::dmat3 sun2earthMat = SpiceManager::ref().frameTransformationMatrix("GALACTIC", "IAU_EARTH", _time);
            glm::dvec3 sunPosEarth = sun2earthMat * sunPosSun;
            glm::dvec3 earthPosEarth = sun2earthMat * earthPosSun;
            glm::dvec3 camDirEarth = sun2earthMat * cam_dir;
            glm::dvec3 camPosEarth = sun2earthMat * data.camera.position().vec3();
            std::cout << "\n\nSun in Earth: " << sunPosEarth.x << ", " << sunPosEarth.y << ", " << sunPosEarth.z << std::endl;
            std::cout << "\n\nEarth in Earth: " << earthPosEarth.x << ", " << earthPosEarth.y << ", " << earthPosEarth.z << std::endl;
            std::cout << "\n\nCam Position in Earth: " << camPosEarth.x << ", " << camPosEarth.y << ", " << camPosEarth.z << std::endl;
            std::cout << "\n\nCam Position from Earth in Earth: " << camDirEarth.x << ", " << camDirEarth.y << ", " << camDirEarth.z << std::endl;

            glm::dvec3 sunPosView = glm::dvec3(data.camera.viewMatrix() * glm::dvec4(sunPosSun.x, sunPosSun.y, sunPosSun.z, 1.0));
            glm::dvec3 earthPosView = glm::dvec3(data.camera.viewMatrix() * glm::dvec4(earthPosSun.x, earthPosSun.y, earthPosSun.z, 1.0));
            glm::dvec3 camDirView = glm::dvec3(data.camera.viewMatrix() * glm::dvec4(cam_dir.x, cam_dir.y, cam_dir.z, 0.0));
            glm::dvec3 camPosView = glm::dvec3(data.camera.viewMatrix() * glm::dvec4(data.camera.position().vec3().x, data.camera.position().vec3().y, data.camera.position().vec3().z, 1.0));
            std::cout << "\n\nSun in View: " << sunPosView.x << ", " << sunPosView.y << ", " << sunPosView.z << std::endl;
            std::cout << "\n\nEarth in View: " << earthPosView.x << ", " << earthPosView.y << ", " << earthPosView.z << std::endl;
            std::cout << "\n\nCam Position in View: " << camPosView.x << ", " << camPosView.y << ", " << camPosView.z << std::endl;
            std::cout << "\n\nCam Position from Earth in View: " << camDirView.x << ", " << camDirView.y << ", " << camDirView.z << std::endl;*/


            GLint defaultFBO;
            glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);

            GLint m_viewport[4];
            glGetIntegerv(GL_VIEWPORT, m_viewport);

            glBindFramebuffer(GL_FRAMEBUFFER, _atmosphereFBO);
            GLenum drawBuffers[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
            glDrawBuffers(2, drawBuffers);

            ghoul::opengl::TextureUnit dummyTextureUnit;
            if (!glIsTexture(_dummyTexture)) {
                dummyTextureUnit.activate();
                glGenTextures(1, &_dummyTexture);
                //glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _dummyTexture);
                glBindTexture(GL_TEXTURE_2D, _dummyTexture);
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
                glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
                glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, m_viewport[2],
                    m_viewport[3], 0, GL_RGB, GL_UNSIGNED_BYTE, nullptr);
                /*glTexImage2DMultisample(GL_TEXTURE_2D_MULTISAMPLE, 8, GL_RGBA,
                m_viewport[2], m_viewport[3], true);*/
            }

            glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, _dummyTexture, 0);
            checkFrameBufferState("dummy framebuffer - line 955");
            //glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, _atmosphereTexture, 0);
            glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, _atmosphereDepthTexture, 0);
            checkFrameBufferState("deferred atmosphere framebuffer - line 958");

            GLenum err;
            while ((err = glGetError()) != GL_NO_ERROR) {
                const GLubyte * errorString = gluErrorString(err);
                std::stringstream ss;
                ss << "Error after setting up atmosphere framebuffer. OpenGL error: "
                    << err << " - " << errorString << std::endl;
                LERROR(ss.str());
            }

            glClearColor(0.0, 0.0, 0.0, 1.0);
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

            _deferredAtmosphereProgramObject->activate();

            // check OpenGL error
            while ((err = glGetError()) != GL_NO_ERROR) {
                const GLubyte * errorString = gluErrorString(err);
                std::cout << "\n\nActivated Deferred Program. OpenGL error: "
                    << err << " - " << errorString << std::endl;
            }


            // Object ModelTransform
            //std::cout << "\n transform: " << glm::to_string(transform) << std::endl;

            // The following scale comes from PSC transformations.
            float fScaleFactor = data.camera.scaling().x * pow(10.0, data.camera.scaling().y);
            //std::cout << "\n Scaling Factor: " << fScaleFactor << std::endl;
            glm::mat4 fScaleCamTransf = glm::scale(glm::mat4(1.0), glm::vec3(fScaleFactor));
            _deferredAtmosphereProgramObject->setUniform("scaleTransformMatrix", fScaleCamTransf);
            //std::cout << "\n fScaleCamTransf: " << glm::to_string(fScaleCamTransf) << std::endl;

            // Object Space to World Space (in meters)
            glm::mat4 obj2World = glm::translate(glm::mat4(1.0), data.position.vec3()) * transform;
            _deferredAtmosphereProgramObject->setUniform("objToWorldTransform", obj2World);
            glm::mat4 world2Obj = glm::inverse(obj2World);
            _deferredAtmosphereProgramObject->setUniform("worldToObjectTransform", world2Obj);

            // World to Eye Space in OS
            glm::mat4 world2Eye = fScaleCamTransf * glm::mat4(data.camera.viewRotationMatrix()) *
                glm::translate(glm::mat4(1.0), -data.camera.position().vec3());
            _deferredAtmosphereProgramObject->setUniform("worldToEyeTransform", world2Eye);
            glm::mat4 eye2World = glm::inverse(world2Eye);
            _deferredAtmosphereProgramObject->setUniform("eyeToWorldTransform", eye2World);

            // Eye Space in OS to Eye Space in SGCT
            glm::mat4 eye2View = data.camera.viewMatrix();
            _deferredAtmosphereProgramObject->setUniform("eyeToViewTranform", eye2View);
            _deferredAtmosphereProgramObject->setUniform("viewToEyeTranform", glm::inverse(eye2View));

            glm::mat4 inverseProjection = glm::inverse(data.camera.projectionMatrix());
            _deferredAtmosphereProgramObject->setUniform("inverseSgctProjectionMatrix", inverseProjection);
            /*std::cout << "\nProjection: " << glm::to_string(data.camera.projectionMatrix()) << std::endl;
            std::cout << "\nInverse Projection: " << glm::to_string(inverseProjection) << std::endl;*/

            glm::mat4 completeVertexTransformations = data.camera.viewProjectionMatrix() *
                    glm::mat4(data.camera.viewRotationMatrix()) *
                    glm::translate(glm::mat4(1.0), -data.camera.position().vec3()) *
                    glm::translate(glm::mat4(1.0), data.position.vec3())
                    * transform;
            _deferredAtmosphereProgramObject->setUniform("completeVertexTransform", completeVertexTransformations);
            glm::mat4 inverseCompleteVertexTransformations = glm::inverse(completeVertexTransformations);
            _deferredAtmosphereProgramObject->setUniform("inverseCompleteVertexTransform", inverseCompleteVertexTransformations);

            _deferredAtmosphereProgramObject->setUniform("inverseSgctProjectionMatrix", inverseProjection);
            /*std::cout << "\nProjection: " << glm::to_string(data.camera.projectionMatrix()) << std::endl;
            std::cout << "\nInverse Projection: " << glm::to_string(inverseProjection) << std::endl;*/


            // Camera Position in Object Space in Meters
            glm::vec4 cameraPosObjecCoords = glm::vec4(0.0, 0.0, 0.0, 1.0);
            cameraPosObjecCoords = world2Obj * glm::vec4(data.camera.positionVec3(), 1.0);
            _deferredAtmosphereProgramObject->setUniform("cameraPositionObjectCoords", cameraPosObjecCoords);
            std::cout << "\n== Camera position Object Space: " << glm::to_string(cameraPosObjecCoords) << std::endl;
            std::cout << "\n== Camera position World Space: " << glm::to_string(data.camera.positionVec3()) << std::endl;

            std::cout << "\n-- Object position World Space: " << glm::to_string(data.position.vec3()) << std::endl;
            std::cout << "\n-- Object position Obj Space: " << glm::to_string(world2Obj * glm::vec4(data.position.vec3(), 1.0)) << std::endl;

            _deferredAtmosphereProgramObject->setUniform("objpos", glm::vec4(data.position.vec3(),1.0));

            ghoul::opengl::TextureUnit transmittanceTableTextureUnit;
            transmittanceTableTextureUnit.activate();
            _deferredAtmosphereProgramObject->setUniform("transmittanceTexture", transmittanceTableTextureUnit);

            ghoul::opengl::TextureUnit irradianceTableTextureUnit;
            irradianceTableTextureUnit.activate();
            _deferredAtmosphereProgramObject->setUniform("irradianceTexture", irradianceTableTextureUnit);

            ghoul::opengl::TextureUnit inScatteringTableTextureUnit;
            inScatteringTableTextureUnit.activate();
            _deferredAtmosphereProgramObject->setUniform("inscatterTexture", inScatteringTableTextureUnit);

            _deferredAtmosphereProgramObject->setUniform("screenX", (float)m_viewport[0]);
            _deferredAtmosphereProgramObject->setUniform("screenY", (float)m_viewport[1]);
            _deferredAtmosphereProgramObject->setUniform("screenWIDTH", (float)m_viewport[2]);
            _deferredAtmosphereProgramObject->setUniform("screenHEIGHT", (float)m_viewport[3]);


            _deferredAtmosphereProgramObject->setUniform("Rg", _atmospherePlanetRadius);
            _deferredAtmosphereProgramObject->setUniform("Rt", _atmosphereRadius);
            _deferredAtmosphereProgramObject->setUniform("AverageGroundReflectance", _planetAverageGroundReflectance);
            _deferredAtmosphereProgramObject->setUniform("HR", _rayleighHeightScale);
            _deferredAtmosphereProgramObject->setUniform("betaRayleigh", _rayleighScatteringCoeff);
            _deferredAtmosphereProgramObject->setUniform("HM", _mieHeightScale);
            _deferredAtmosphereProgramObject->setUniform("betaMieScattering", _mieScatteringCoeff);
            _deferredAtmosphereProgramObject->setUniform("betaMieExtinction", _mieExtinctionCoeff);
            _deferredAtmosphereProgramObject->setUniform("mieG", _miePhaseConstant);
            _deferredAtmosphereProgramObject->setUniform("sunRadiance", _sunRadianceIntensity);


            ghoul::opengl::TextureUnit reflectanceUnit;
            if (_hasReflectanceTexture) {
                reflectanceUnit.activate();
                _reflectanceTexture->bind();
                _deferredAtmosphereProgramObject->setUniform("reflectanceTexture", reflectanceUnit);
            }

            ghoul::opengl::TextureUnit cloudsUnit;
            if (_hasCloudsTexture) {
                cloudsUnit.activate();
                _cloudsTexture->bind();
                _deferredAtmosphereProgramObject->setUniform("cloudsTexture", cloudsUnit);
            }

            // HDR
            _deferredAtmosphereProgramObject->setUniform("exposure", 0.4f);

            renderQuadForCalc(_atmosphereRenderVAO, 6);

            if (_saveDeferredFramebuffer) {
                std::stringstream ss;
                ss << "atmosphere-" << count << ".ppm";
                saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, ss.str(), m_viewport[2], m_viewport[3]);
                ss.str("");
                ss << "atmosphere-depth-" << count++ << ".ppm";
                saveTextureToPPMFile(GL_DEPTH_ATTACHMENT, ss.str(), m_viewport[2], m_viewport[3]);
                _saveDeferredFramebuffer = false;
            }


            // check OpenGL error
            while ((err = glGetError()) != GL_NO_ERROR) {
                const GLubyte * errorString = gluErrorString(err);
                std::cout << "\n\nRendering Deferred Program. OpenGL error: "
                    << err << " - " << errorString << std::endl;
            }


            /*std::stringstream ss;
            ss << "atmosphere-" << count++ << ".ppm";
            saveTextureToPPMFile(GL_COLOR_ATTACHMENT1, ss.str(), m_viewport[2], m_viewport[3]);*/

            _deferredAtmosphereProgramObject->deactivate();

            glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
            glViewport(m_viewport[0], m_viewport[1],
                m_viewport[2], m_viewport[3]);
        }
#endif
    }

    void RenderablePlanetAtmosphere::update(const UpdateData& data) {
        // set spice-orientation in accordance to timestamp
        _stateMatrix = SpiceManager::ref().positionTransformMatrix(_frame, "GALACTIC", data.time);
        _time = data.time;
    }

    void RenderablePlanetAtmosphere::loadTexture() {
        _texture = nullptr;
        if (_colorTexturePath.value() != "") {
            _texture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath)));
            if (_texture) {
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
            LERROR("Error after loading color texture. OpenGL error: " << errString);
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
            LERROR("Error after loading night texture. OpenGL error: " << errString);
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
            LERROR("Error after loading reflectance texture. OpenGL error: " << errString);
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

        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error after loading height texture. OpenGL error: " << errString);
        }

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

        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error after loading clouds texture. OpenGL error: " << errString);
        }
    }

    void RenderablePlanetAtmosphere::loadComputationPrograms() {

        RenderEngine& renderEngine = OsEng.renderEngine();

        //============== Transmittance T =================
        if (_transmittanceProgramObject == nullptr) {
            _transmittanceProgramObject = ghoul::opengl::ProgramObject::Build(
                "transmittanceCalcProgram",
                "${MODULE_ATMOSPHERE}/shaders/transmittance_calc_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/transmittance_calc_fs.glsl");
            if (!_transmittanceProgramObject) {
                return;
            }
        }
        using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
        _transmittanceProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _transmittanceProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

        //============== Irradiance E =================
        if (_irradianceProgramObject == nullptr) {
            _irradianceProgramObject = ghoul::opengl::ProgramObject::Build(
                "irradianceCalcProgram",
                "${MODULE_ATMOSPHERE}/shaders/irradiance_calc_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/irradiance_calc_fs.glsl");
            if (!_irradianceProgramObject) {
                if (_transmittanceProgramObject) {
                    _transmittanceProgramObject.reset();
                    _transmittanceProgramObject = nullptr;
                }

                return;
            }
        }
        _irradianceProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _irradianceProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

        if (_irradianceSupTermsProgramObject == nullptr) {
            _irradianceSupTermsProgramObject = ghoul::opengl::ProgramObject::Build(
                "irradianceSupTermsCalcProgram",
                "${MODULE_ATMOSPHERE}/shaders/irradiance_sup_calc_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/irradiance_sup_calc_fs.glsl");
            if (!_irradianceSupTermsProgramObject) {
                if (_transmittanceProgramObject) {
                    _transmittanceProgramObject.reset();
                    _transmittanceProgramObject = nullptr;
                }

                if (_irradianceProgramObject) {
                    _irradianceProgramObject.reset();
                    _irradianceProgramObject = nullptr;
                }

                return;
            }
        }
        _irradianceSupTermsProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _irradianceSupTermsProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

        //============== InScattering S =================
        if (_inScatteringProgramObject == nullptr) {
            _inScatteringProgramObject = ghoul::opengl::ProgramObject::Build(
                "inScatteringCalcProgram",
                "${MODULE_ATMOSPHERE}/shaders/inScattering_calc_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/inScattering_calc_fs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/inScattering_calc_gs.glsl");
            if (!_inScatteringProgramObject) {
                if (_transmittanceProgramObject) {
                    _transmittanceProgramObject.reset();
                    _transmittanceProgramObject = nullptr;
                }

                if (_irradianceProgramObject) {
                    _irradianceProgramObject.reset();
                    _irradianceProgramObject = nullptr;
                }

                if (_irradianceSupTermsProgramObject) {
                    _irradianceSupTermsProgramObject.reset();
                    _irradianceSupTermsProgramObject = nullptr;
                }

                return;
            }
        }
        _inScatteringProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _inScatteringProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

        if (_inScatteringSupTermsProgramObject == nullptr) {
            _inScatteringSupTermsProgramObject = ghoul::opengl::ProgramObject::Build(
                "inScatteringSupTermsCalcProgram",
                "${MODULE_ATMOSPHERE}/shaders/inScattering_sup_calc_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/inScattering_sup_calc_fs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/inScattering_sup_calc_gs.glsl");
            if (!_inScatteringSupTermsProgramObject) {
                if (_transmittanceProgramObject) {
                    _transmittanceProgramObject.reset();
                    _transmittanceProgramObject = nullptr;
                }

                if (_irradianceProgramObject) {
                    _irradianceProgramObject.reset();
                    _irradianceProgramObject = nullptr;
                }

                if (_irradianceSupTermsProgramObject) {
                    _irradianceSupTermsProgramObject.reset();
                    _irradianceSupTermsProgramObject = nullptr;
                }

                if (_inScatteringProgramObject) {
                    _inScatteringProgramObject.reset();
                    _inScatteringProgramObject = nullptr;
                }

                return;
            }
        }
        _inScatteringSupTermsProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _inScatteringSupTermsProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

        //============== Delta E =================
        if (_deltaEProgramObject == nullptr) {
            _deltaEProgramObject = ghoul::opengl::ProgramObject::Build(
                "deltaECalcProgram",
                "${MODULE_ATMOSPHERE}/shaders/deltaE_calc_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/deltaE_calc_fs.glsl");
            if (!_deltaEProgramObject) {
                if (_transmittanceProgramObject) {
                    _transmittanceProgramObject.reset();
                    _transmittanceProgramObject = nullptr;
                }

                if (_irradianceProgramObject) {
                    _irradianceProgramObject.reset();
                    _irradianceProgramObject = nullptr;
                }

                if (_irradianceSupTermsProgramObject) {
                    _irradianceSupTermsProgramObject.reset();
                    _irradianceSupTermsProgramObject = nullptr;
                }

                if (_inScatteringProgramObject) {
                    _inScatteringProgramObject.reset();
                    _inScatteringProgramObject = nullptr;
                }

                if (_inScatteringSupTermsProgramObject) {
                    _inScatteringSupTermsProgramObject.reset();
                    _inScatteringSupTermsProgramObject = nullptr;
                }

                return;
            }
        }
        _deltaEProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _deltaEProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

        //============== Irradiance finel E =================
        if (_irradianceFinalProgramObject == nullptr) {
            _irradianceFinalProgramObject = ghoul::opengl::ProgramObject::Build(
                "irradianceEFinalProgram",
                "${MODULE_ATMOSPHERE}/shaders/irradiance_final_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/irradiance_final_fs.glsl");
            if (!_irradianceFinalProgramObject) {
                if (_transmittanceProgramObject) {
                    _transmittanceProgramObject.reset();
                    _transmittanceProgramObject = nullptr;
                }

                if (_irradianceProgramObject) {
                    _irradianceProgramObject.reset();
                    _irradianceProgramObject = nullptr;
                }

                if (_irradianceSupTermsProgramObject) {
                    _irradianceSupTermsProgramObject.reset();
                    _irradianceSupTermsProgramObject = nullptr;
                }

                if (_inScatteringProgramObject) {
                    _inScatteringProgramObject.reset();
                    _inScatteringProgramObject = nullptr;
                }

                if (_inScatteringSupTermsProgramObject) {
                    _inScatteringSupTermsProgramObject.reset();
                    _inScatteringSupTermsProgramObject = nullptr;
                }

                if (_deltaEProgramObject) {
                    _deltaEProgramObject.reset();
                    _deltaEProgramObject = nullptr;
                }

                return;
            }
        }
        _irradianceFinalProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _irradianceFinalProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

        //============== Delta S =================
        if (_deltaSProgramObject == nullptr) {
            _deltaSProgramObject = ghoul::opengl::ProgramObject::Build(
                "deltaSCalcProgram",
                "${MODULE_ATMOSPHERE}/shaders/deltaS_calc_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/deltaS_calc_fs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/deltaS_calc_gs.glsl");
            if (!_deltaSProgramObject) {
                if (_transmittanceProgramObject) {
                    _transmittanceProgramObject.reset();
                    _transmittanceProgramObject = nullptr;
                }

                if (_irradianceProgramObject) {
                    _irradianceProgramObject.reset();
                    _irradianceProgramObject = nullptr;
                }

                if (_irradianceSupTermsProgramObject) {
                    _irradianceSupTermsProgramObject.reset();
                    _irradianceSupTermsProgramObject = nullptr;
                }

                if (_inScatteringProgramObject) {
                    _inScatteringProgramObject.reset();
                    _inScatteringProgramObject = nullptr;
                }

                if (_inScatteringSupTermsProgramObject) {
                    _inScatteringSupTermsProgramObject.reset();
                    _inScatteringSupTermsProgramObject = nullptr;
                }

                if (_deltaEProgramObject) {
                    _deltaEProgramObject.reset();
                    _deltaEProgramObject = nullptr;
                }

                if (_irradianceFinalProgramObject) {
                    _irradianceFinalProgramObject.reset();
                    _irradianceFinalProgramObject = nullptr;
                }

                return;
            }
        }
        _deltaSProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _deltaSProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

        if (_deltaSSupTermsProgramObject == nullptr) {
            _deltaSSupTermsProgramObject = ghoul::opengl::ProgramObject::Build(
                "deltaSSUPTermsCalcProgram",
                "${MODULE_ATMOSPHERE}/shaders/deltaS_sup_calc_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/deltaS_sup_calc_fs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/deltaS_sup_calc_gs.glsl");
            if (!_deltaSSupTermsProgramObject) {
                if (_transmittanceProgramObject) {
                    _transmittanceProgramObject.reset();
                    _transmittanceProgramObject = nullptr;
                }

                if (_irradianceProgramObject) {
                    _irradianceProgramObject.reset();
                    _irradianceProgramObject = nullptr;
                }

                if (_irradianceSupTermsProgramObject) {
                    _irradianceSupTermsProgramObject.reset();
                    _irradianceSupTermsProgramObject = nullptr;
                }

                if (_inScatteringProgramObject) {
                    _inScatteringProgramObject.reset();
                    _inScatteringProgramObject = nullptr;
                }

                if (_inScatteringSupTermsProgramObject) {
                    _inScatteringSupTermsProgramObject.reset();
                    _inScatteringSupTermsProgramObject = nullptr;
                }

                if (_deltaEProgramObject) {
                    _deltaEProgramObject.reset();
                    _deltaEProgramObject = nullptr;
                }

                if (_irradianceFinalProgramObject) {
                    _irradianceFinalProgramObject.reset();
                    _irradianceFinalProgramObject = nullptr;
                }

                if (_deltaSProgramObject) {
                    _deltaSProgramObject.reset();
                    _deltaSProgramObject = nullptr;
                }

                return;
            }
        }
        _deltaSSupTermsProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _deltaSSupTermsProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

        //============== Delta J (Radiance Scattered) =================
        if (_deltaJProgramObject == nullptr) {
            _deltaJProgramObject = ghoul::opengl::ProgramObject::Build(
                "deltaJCalcProgram",
                "${MODULE_ATMOSPHERE}/shaders/deltaJ_calc_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/deltaJ_calc_fs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/deltaJ_calc_gs.glsl");
            if (!_deltaJProgramObject) {
                if (_transmittanceProgramObject) {
                    _transmittanceProgramObject.reset();
                    _transmittanceProgramObject = nullptr;
                }

                if (_irradianceProgramObject) {
                    _irradianceProgramObject.reset();
                    _irradianceProgramObject = nullptr;
                }

                if (_irradianceSupTermsProgramObject) {
                    _irradianceSupTermsProgramObject.reset();
                    _irradianceSupTermsProgramObject = nullptr;
                }

                if (_inScatteringProgramObject) {
                    _inScatteringProgramObject.reset();
                    _inScatteringProgramObject = nullptr;
                }

                if (_inScatteringSupTermsProgramObject) {
                    _inScatteringSupTermsProgramObject.reset();
                    _inScatteringSupTermsProgramObject = nullptr;
                }

                if (_deltaEProgramObject) {
                    _deltaEProgramObject.reset();
                    _deltaEProgramObject = nullptr;
                }

                if (_irradianceFinalProgramObject) {
                    _irradianceFinalProgramObject.reset();
                    _irradianceFinalProgramObject = nullptr;
                }

                if (_deltaSProgramObject) {
                    _deltaSProgramObject.reset();
                    _deltaSProgramObject = nullptr;
                }

                if (_deltaSSupTermsProgramObject) {
                    _deltaSSupTermsProgramObject.reset();
                    _deltaSSupTermsProgramObject = nullptr;
                }

                return;
            }

        }
        _deltaJProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _deltaJProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

        //============== Clean Texture Program =================
        if (_cleanTextureProgramObject == nullptr) {
            // shadow program
            _cleanTextureProgramObject = ghoul::opengl::ProgramObject::Build(
                "cleanTextureProgram",
                "${MODULE_ATMOSPHERE}/shaders/texture_clean_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/texture_clean_fs.glsl");
            if (!_cleanTextureProgramObject) {
                if (_transmittanceProgramObject) {
                    _transmittanceProgramObject.reset();
                    _transmittanceProgramObject = nullptr;
                }

                if (_irradianceProgramObject) {
                    _irradianceProgramObject.reset();
                    _irradianceProgramObject = nullptr;
                }

                if (_irradianceSupTermsProgramObject) {
                    _irradianceSupTermsProgramObject.reset();
                    _irradianceSupTermsProgramObject = nullptr;
                }

                if (_inScatteringProgramObject) {
                    _inScatteringProgramObject.reset();
                    _inScatteringProgramObject = nullptr;
                }

                if (_inScatteringSupTermsProgramObject) {
                    _inScatteringSupTermsProgramObject.reset();
                    _inScatteringSupTermsProgramObject = nullptr;
                }

                if (_deltaEProgramObject) {
                    _deltaEProgramObject.reset();
                    _deltaEProgramObject = nullptr;
                }

                if (_irradianceFinalProgramObject) {
                    _irradianceFinalProgramObject.reset();
                    _irradianceFinalProgramObject = nullptr;
                }

                if (_deltaSProgramObject) {
                    _deltaSProgramObject.reset();
                    _deltaSProgramObject = nullptr;
                }

                if (_deltaSSupTermsProgramObject) {
                    _deltaSSupTermsProgramObject.reset();
                    _deltaSSupTermsProgramObject = nullptr;
                }

                if (_deltaJProgramObject) {
                    _deltaJProgramObject.reset();
                    _deltaEProgramObject = nullptr;
                }

                return;
            }

        }
        _cleanTextureProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _cleanTextureProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);


    }

    void RenderablePlanetAtmosphere::unloadComputationPrograms() {

        RenderEngine& renderEngine = OsEng.renderEngine();

        if (_transmittanceProgramObject) {
            _transmittanceProgramObject.reset();
            _transmittanceProgramObject = nullptr;
        }

        if (_irradianceProgramObject) {
            _irradianceProgramObject.reset();
            _irradianceProgramObject = nullptr;
        }

        if (_irradianceSupTermsProgramObject) {
            _irradianceSupTermsProgramObject.reset();
            _irradianceSupTermsProgramObject = nullptr;
        }

        if (_inScatteringProgramObject) {
            _inScatteringProgramObject.reset();
            _inScatteringProgramObject = nullptr;
        }

        if (_inScatteringSupTermsProgramObject) {
            _inScatteringSupTermsProgramObject.reset();
            _inScatteringSupTermsProgramObject = nullptr;
        }

        if (_deltaEProgramObject) {
            _deltaEProgramObject.reset();
            _deltaEProgramObject = nullptr;
        }

        if (_irradianceFinalProgramObject) {
            _irradianceFinalProgramObject.reset();
            _irradianceFinalProgramObject = nullptr;
        }

        if (_deltaSProgramObject) {
            _deltaSProgramObject.reset();
            _deltaSProgramObject = nullptr;
        }

        if (_deltaSSupTermsProgramObject) {
            _deltaSSupTermsProgramObject.reset();
            _deltaSSupTermsProgramObject = nullptr;
        }

        if (_deltaJProgramObject) {
            _deltaJProgramObject.reset();
            _deltaJProgramObject = nullptr;
        }

        if (_cleanTextureProgramObject) {
            _cleanTextureProgramObject.reset();
            _cleanTextureProgramObject = nullptr;
        }
    }

    void RenderablePlanetAtmosphere::createComputationTextures() {

        //========== Create Atmosphere Tables (textures) ==============

        GLenum err;
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error before creating OpenGL textures for Atmosphere computation. OpenGL error: " << errString);
        }
              
        if (!_atmosphereCalculated) {
            //============== Transmittance =================
            ghoul::opengl::TextureUnit transmittanceTableTextureUnit;
            transmittanceTableTextureUnit.activate();
            glGenTextures(1, &_transmittanceTableTexture);
            glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
            // Stopped using a buffer object for GL_PIXEL_UNPACK_BUFFER
            glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
            glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB16F, TRANSMITTANCE_TABLE_WIDTH,
                TRANSMITTANCE_TABLE_HEIGHT, 0, GL_RGB, GL_FLOAT, nullptr);
            while ((err = glGetError()) != GL_NO_ERROR) {
                const GLubyte * errString = gluErrorString(err);
                LERROR("Error creating Transmittance T texture for Atmosphere computation. OpenGL error: " << errString);
            }
            //glBindTexture(GL_TEXTURE_2D, 0);

            //============== Irradiance =================
            ghoul::opengl::TextureUnit irradianceTableTextureUnit;
            irradianceTableTextureUnit.activate();
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
                LERROR("Error creating Irradiance E texture for Atmosphere computation. OpenGL error: " << errString);
            }
            //glBindTexture(GL_TEXTURE_2D, 0);

            //============== InScattering =================
            ghoul::opengl::TextureUnit inScatteringTableTextureUnit;
            inScatteringTableTextureUnit.activate();
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
                LERROR("Error creating InScattering S texture for Atmosphere computation. OpenGL error: " << errString);
            }
            //glBindTexture(GL_TEXTURE_3D, 0);
        }               

        //============== Delta E =================
        ghoul::opengl::TextureUnit deltaETableTextureUnit;
        deltaETableTextureUnit.activate();
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
            LERROR("Error creating Irradiance Delta E texture for Atmosphere computation. OpenGL error: " << errString);
        }
        //glBindTexture(GL_TEXTURE_2D, 0);

        //============== Delta S =================
        ghoul::opengl::TextureUnit deltaSRayleighTableTextureUnit;
        deltaSRayleighTableTextureUnit.activate();
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
            LERROR("Error creating Rayleigh InScattering Delta S exture for Atmosphere computation. OpenGL error: " << errString);
        }
        //glBindTexture(GL_TEXTURE_3D, 0);

        ghoul::opengl::TextureUnit deltaSMieTableTextureUnit;
        deltaSMieTableTextureUnit.activate();
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
            LERROR("Error creating Mie InScattering Delta S texture for Atmosphere computation. OpenGL error: " << errString);
        }
        //glBindTexture(GL_TEXTURE_3D, 0);

        //============== Delta J (Radiance Scattered) =================
        ghoul::opengl::TextureUnit deltaJTableTextureUnit;
        deltaJTableTextureUnit.activate();
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
            LERROR("Error creating Inscattering Irradiance Delta J texture for Atmosphere computation. OpenGL error: " << errString);
        }
        //glBindTexture(GL_TEXTURE_3D, 0);
    }

    void RenderablePlanetAtmosphere::deleteComputationTextures() {
        // Cleaning up
        glDeleteTextures(1, &_transmittanceTableTexture);
        glDeleteTextures(1, &_irradianceTableTexture);
        glDeleteTextures(1, &_inScatteringTableTexture);
        glDeleteTextures(1, &_deltaETableTexture);
        glDeleteTextures(1, &_deltaSRayleighTableTexture);
        glDeleteTextures(1, &_deltaSMieTableTexture);
        glDeleteTextures(1, &_deltaJTableTexture);
    }

    void RenderablePlanetAtmosphere::deleteUnusedComputationTextures() {
        glDeleteTextures(1, &_deltaETableTexture);
        glDeleteTextures(1, &_deltaSRayleighTableTexture);
        glDeleteTextures(1, &_deltaSMieTableTexture);
        glDeleteTextures(1, &_deltaJTableTexture);
    }

    void RenderablePlanetAtmosphere::updateAtmosphereParameters() {
        _atmosphereRadius               = _atmospherePlanetRadius + _atmosphereHeightP;
        _planetAverageGroundReflectance = _groundAverageReflectanceP;
        _rayleighHeightScale            = _rayleighHeightScaleP;
        _mieHeightScale                 = _mieHeightScaleP;
        _mieScatteringCoeff             = glm::vec3(_mieScatteringCoefficientP * 0.001f);
        _mieExtinctionCoeff             = _mieScatteringCoeff * (1.0f / static_cast<float>(_mieScatteringExtinctionPropCoefficientP));
        _miePhaseConstant               = _mieAsymmetricFactorGP;
        _sunRadianceIntensity           = _sunIntensityP;
        
        preCalculateAtmosphereParam();

    }

    void RenderablePlanetAtmosphere::loadAtmosphereDataIntoShaderProgram(std::unique_ptr<ghoul::opengl::ProgramObject> & shaderProg) {
        shaderProg->setUniform("Rg", _atmospherePlanetRadius);
        shaderProg->setUniform("Rt", _atmosphereRadius);
        shaderProg->setUniform("AverageGroundReflectance", _planetAverageGroundReflectance);
        shaderProg->setUniform("HR", _rayleighHeightScale);
        shaderProg->setUniform("betaRayleigh", _rayleighScatteringCoeff);
        shaderProg->setUniform("HM", _mieHeightScale);
        shaderProg->setUniform("betaMieScattering", _mieScatteringCoeff);
        shaderProg->setUniform("betaMieExtinction", _mieExtinctionCoeff);
        shaderProg->setUniform("mieG", _miePhaseConstant);
        shaderProg->setUniform("sunRadiance", _sunRadianceIntensity);
    }


    void RenderablePlanetAtmosphere::executeCalculations(const GLuint quadCalcVAO, const GLenum drawBuffers[1], const GLsizei vertexSize) {

        ghoul::opengl::TextureUnit transmittanceTableTextureUnit;
        ghoul::opengl::TextureUnit irradianceTableTextureUnit;
        ghoul::opengl::TextureUnit inScatteringTableTextureUnit;
        ghoul::opengl::TextureUnit deltaETableTextureUnit;
        ghoul::opengl::TextureUnit deltaSRayleighTableTextureUnit;
        ghoul::opengl::TextureUnit deltaSMieTableTextureUnit;
        ghoul::opengl::TextureUnit deltaJTableTextureUnit;

        // Saving current OpenGL state
        bool blendEnabled = glIsEnabled(GL_BLEND);
        GLint blendEquationRGB;
        GLint blendEquationAlpha;
        GLint blendDestAlpha;
        GLint blendDestRGB;
        GLint blendSrcAlpha;
        GLint blendSrcRGB;

        if (blendEnabled)
            glDisable(GL_BLEND);
        glGetIntegerv(GL_BLEND_EQUATION_RGB, &blendEquationRGB);
        glGetIntegerv(GL_BLEND_EQUATION_ALPHA, &blendEquationAlpha);
        glGetIntegerv(GL_BLEND_DST_ALPHA, &blendDestAlpha);
        glGetIntegerv(GL_BLEND_DST_RGB, &blendDestRGB);
        glGetIntegerv(GL_BLEND_SRC_ALPHA, &blendSrcAlpha);
        glGetIntegerv(GL_BLEND_SRC_RGB, &blendSrcRGB);

        // ===========================================================
        // See Precomputed Atmosphere Scattering from Bruneton et al. paper, algorithm 4.1:
        // ===========================================================
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _transmittanceTableTexture, 0);
        checkFrameBufferState("_transmittanceTableTexture");
        glViewport(0, 0, TRANSMITTANCE_TABLE_WIDTH, TRANSMITTANCE_TABLE_HEIGHT);
        _transmittanceProgramObject->activate();
        loadAtmosphereDataIntoShaderProgram(_transmittanceProgramObject);
        //glClear(GL_COLOR_BUFFER_BIT);
        static const float black[] = { 0.0f, 0.0f, 0.0f, 0.0f };
        glClearBufferfv(GL_COLOR, 0, black);
        renderQuadForCalc(quadCalcVAO, vertexSize);
#ifdef _SAVE_ATMOSPHERE_TEXTURES
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, std::string("transmittance_texture.ppm"),
            TRANSMITTANCE_TABLE_WIDTH, TRANSMITTANCE_TABLE_HEIGHT);
#endif
        _transmittanceProgramObject->deactivate();
        GLenum err;
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error computing Transmittance T Table. OpenGL error: " << errString);
        }

        // line 2 in algorithm 4.1
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _deltaETableTexture, 0);
        checkFrameBufferState("_deltaETableTexture");
        glViewport(0, 0, DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
        _irradianceProgramObject->activate();
        transmittanceTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
        _irradianceProgramObject->setUniform("transmittanceTexture", transmittanceTableTextureUnit);
        loadAtmosphereDataIntoShaderProgram(_irradianceProgramObject);
        glClear(GL_COLOR_BUFFER_BIT);        
        renderQuadForCalc(quadCalcVAO, vertexSize);
#ifdef _SAVE_ATMOSPHERE_TEXTURES
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, std::string("deltaE_table_texture.ppm"),
            DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
#endif
        _irradianceProgramObject->deactivate();
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error computing Irradiance Delta E Table. OpenGL error: " << errString);
        }
        
        // line 3 in algorithm 4.1
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _deltaSRayleighTableTexture, 0);
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, _deltaSMieTableTexture, 0);
        GLenum colorBuffers[2] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
        glDrawBuffers(2, colorBuffers);
        checkFrameBufferState("_deltaSRay and _deltaSMie TableTexture");
        glViewport(0, 0, MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
        _inScatteringProgramObject->activate();
        transmittanceTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
        _inScatteringProgramObject->setUniform("transmittanceTexture", transmittanceTableTextureUnit);
        loadAtmosphereDataIntoShaderProgram(_inScatteringProgramObject);
        glClear(GL_COLOR_BUFFER_BIT);        
        for (int layer = 0; layer < R_SAMPLES; ++layer) {
            step3DTexture(_inScatteringProgramObject, layer);
            renderQuadForCalc(quadCalcVAO, vertexSize);
        }
#ifdef _SAVE_ATMOSPHERE_TEXTURES
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, std::string("deltaS_rayleigh_texture.ppm"),
            MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT1, std::string("deltaS_mie_texture.ppm"),
            MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
#endif
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, 0, 0);
        glDrawBuffers(1, drawBuffers);

        _inScatteringProgramObject->deactivate();
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error computing InScattering Rayleigh and Mie Delta Tables. OpenGL error: " << errString);
        }
        
        // line 4 in algorithm 4.1
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _irradianceTableTexture, 0);
        checkFrameBufferState("_irradianceTableTexture");
        glDrawBuffer(GL_COLOR_ATTACHMENT0);

        glViewport(0, 0, DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
        _deltaEProgramObject->activate();
        //_deltaEProgramObject->setUniform("line", 4);
        deltaETableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _deltaETableTexture);
        _deltaEProgramObject->setUniform("deltaETexture", deltaETableTextureUnit);
        loadAtmosphereDataIntoShaderProgram(_deltaEProgramObject);
        glClear(GL_COLOR_BUFFER_BIT);        
        renderQuadForCalc(quadCalcVAO, vertexSize);
#ifdef _SAVE_ATMOSPHERE_TEXTURES
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, std::string("irradiance_texture.ppm"),
            DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
#endif
        _deltaEProgramObject->deactivate();
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error computing Irradiance E Table. OpenGL error: " << errString);
        }
        
        // line 5 in algorithm 4.1
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _inScatteringTableTexture, 0);
        checkFrameBufferState("_inScatteringTableTexture");
        glViewport(0, 0, MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
        _deltaSProgramObject->activate();
        deltaSRayleighTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
        deltaSMieTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_3D, _deltaSMieTableTexture);
        _deltaSProgramObject->setUniform("deltaSRTexture", deltaSRayleighTableTextureUnit);
        _deltaSProgramObject->setUniform("deltaSMTexture", deltaSMieTableTextureUnit);
        loadAtmosphereDataIntoShaderProgram(_deltaSProgramObject);
        glClear(GL_COLOR_BUFFER_BIT);        
        for (int layer = 0; layer < R_SAMPLES; ++layer) {
            step3DTexture(_deltaSProgramObject, layer, false);
            renderQuadForCalc(quadCalcVAO, vertexSize);
        }
#ifdef _SAVE_ATMOSPHERE_TEXTURES
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, std::string("S_texture.ppm"),
            MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
#endif
        _deltaSProgramObject->deactivate();
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error computing InScattering S Table. OpenGL error: " << errString);
        }
        
        // loop in line 6 in algorithm 4.1
        for (int scatteringOrder = 2; scatteringOrder <= 4; ++scatteringOrder) {

            // line 7 in algorithm 4.1
            glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _deltaJTableTexture, 0);
            checkFrameBufferState("_deltaJTableTexture");
            glViewport(0, 0, MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
            _deltaJProgramObject->activate();
            if (scatteringOrder == 2)
                _deltaJProgramObject->setUniform("firstIteraction", 1);
            else
                _deltaJProgramObject->setUniform("firstIteraction", 0);
            transmittanceTableTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
            deltaETableTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _deltaETableTexture);
            deltaSRayleighTableTextureUnit.activate();
            glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
            deltaSMieTableTextureUnit.activate();
            glBindTexture(GL_TEXTURE_3D, _deltaSMieTableTexture);
            _deltaJProgramObject->setUniform("transmittanceTexture", transmittanceTableTextureUnit);
            _deltaJProgramObject->setUniform("deltaETexture", deltaETableTextureUnit);
            _deltaJProgramObject->setUniform("deltaSRTexture", deltaSRayleighTableTextureUnit);
            _deltaJProgramObject->setUniform("deltaSMTexture", deltaSMieTableTextureUnit);
            loadAtmosphereDataIntoShaderProgram(_deltaJProgramObject);            
            for (int layer = 0; layer < R_SAMPLES; ++layer) {
                step3DTexture(_deltaJProgramObject, layer);
                renderQuadForCalc(quadCalcVAO, vertexSize);
            }
#ifdef _SAVE_ATMOSPHERE_TEXTURES
            std::stringstream sst;
            sst << "deltaJ_texture-scattering_order-" << scatteringOrder << ".ppm";
            saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, sst.str(),
                MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
#endif
            _deltaJProgramObject->deactivate();
            while ((err = glGetError()) != GL_NO_ERROR) {
                const GLubyte * errString = gluErrorString(err);
                LERROR("Error computing Delta J Table (Sup. Terms). OpenGL error: " << errString);
            }
        
            // line 8 in algorithm 4.1
            glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _deltaETableTexture, 0);
            checkFrameBufferState("_deltaETableTexture");
            glViewport(0, 0, DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
            _irradianceSupTermsProgramObject->activate();
            if (scatteringOrder == 2)
                _irradianceSupTermsProgramObject->setUniform("firstIteraction", (int)1);
            else
                _irradianceSupTermsProgramObject->setUniform("firstIteraction", (int)0);
            transmittanceTableTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
            deltaSRayleighTableTextureUnit.activate();
            glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
            deltaSMieTableTextureUnit.activate();
            glBindTexture(GL_TEXTURE_3D, _deltaSMieTableTexture);
            _irradianceSupTermsProgramObject->setUniform("transmittanceTexture", transmittanceTableTextureUnit);
            _irradianceSupTermsProgramObject->setUniform("deltaSRTexture", deltaSRayleighTableTextureUnit);
            _irradianceSupTermsProgramObject->setUniform("deltaSMTexture", deltaSMieTableTextureUnit);
            loadAtmosphereDataIntoShaderProgram(_irradianceSupTermsProgramObject);            
            renderQuadForCalc(quadCalcVAO, vertexSize);
#ifdef _SAVE_ATMOSPHERE_TEXTURES
            sst.str(std::string());
            sst << "deltaE_texture-scattering_order-" << scatteringOrder << ".ppm";
            saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, sst.str(),
                DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
#endif
            _irradianceSupTermsProgramObject->deactivate();
            while ((err = glGetError()) != GL_NO_ERROR) {
                const GLubyte * errString = gluErrorString(err);
                LERROR("Error computing Delta E Table (Sup. Terms). OpenGL error: " << errString);
            }
            
            // line 9 in algorithm 4.1
            glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _deltaSRayleighTableTexture, 0);
            checkFrameBufferState("_deltaSRayleighTableTexture");
            glViewport(0, 0, MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
            _inScatteringSupTermsProgramObject->activate();
            /*if (scatteringOrder == 2)
                _inScatteringSupTermsProgramObject->setUniform("firstIteraction", (int)1);
            else
                _inScatteringSupTermsProgramObject->setUniform("firstIteraction", (int)0);*/
            transmittanceTableTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
            deltaJTableTextureUnit.activate();
            glBindTexture(GL_TEXTURE_3D, _deltaJTableTexture);
            _inScatteringSupTermsProgramObject->setUniform("transmittanceTexture", transmittanceTableTextureUnit);
            _inScatteringSupTermsProgramObject->setUniform("deltaJTexture", deltaJTableTextureUnit);
            loadAtmosphereDataIntoShaderProgram(_inScatteringSupTermsProgramObject);            
            for (int layer = 0; layer < R_SAMPLES; ++layer) {
                step3DTexture(_inScatteringSupTermsProgramObject, layer);
                renderQuadForCalc(quadCalcVAO, vertexSize);
            }
#ifdef _SAVE_ATMOSPHERE_TEXTURES
            sst.str(std::string());
            sst << "deltaS_texture-scattering_order-" << scatteringOrder << ".ppm";
            saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, sst.str(),
                MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
#endif
            _inScatteringSupTermsProgramObject->deactivate();
            while ((err = glGetError()) != GL_NO_ERROR) {
                const GLubyte * errString = gluErrorString(err);
                LERROR("Error computing Delta S Table (Sup. Terms). OpenGL error: " << errString);
            }
            
            glEnable(GL_BLEND);
            glBlendEquationSeparate(GL_FUNC_ADD, GL_FUNC_ADD);
            glBlendFuncSeparate(GL_ONE, GL_ONE, GL_ONE, GL_ONE);

//            // line 10 in algorithm 4.1
//            glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _irradianceTableTexture, 0);
//            checkFrameBufferState("_irradianceTableTexture");
//            glViewport(0, 0, DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
//            _deltaEProgramObject->activate();
//            _deltaEProgramObject->setUniform("line", 10);
//            deltaETableTextureUnit.activate();
//            glBindTexture(GL_TEXTURE_2D, _deltaETableTexture);
//            _deltaEProgramObject->setUniform("deltaETexture", deltaETableTextureUnit);
//            loadAtmosphereDataIntoShaderProgram(_deltaEProgramObject);            
//            renderQuadForCalc(quadCalcVAO, vertexSize);
//#ifdef _SAVE_ATMOSPHERE_TEXTURES
//            sst.str(std::string());
//            sst << "irradianceTable_order-" << scatteringOrder << ".ppm";
//            saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, sst.str(),
//                DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
//#endif
//            _deltaEProgramObject->deactivate();
//            while ((err = glGetError()) != GL_NO_ERROR) {
//                const GLubyte * errString = gluErrorString(err);
//                LERROR("Error computing E Table (Sup. Terms). OpenGL error: " << errString);
//            }

            // line 10 in algorithm 4.1
            glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _irradianceTableTexture, 0);
            checkFrameBufferState("_irradianceTableTexture");
            glViewport(0, 0, DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
            _irradianceFinalProgramObject->activate();
            deltaETableTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _deltaETableTexture);
            _irradianceFinalProgramObject->setUniform("deltaETexture", deltaETableTextureUnit);
            loadAtmosphereDataIntoShaderProgram(_irradianceFinalProgramObject);
            renderQuadForCalc(quadCalcVAO, vertexSize);
#ifdef _SAVE_ATMOSPHERE_TEXTURES
            sst.str(std::string());
            sst << "irradianceTable_order-" << scatteringOrder << ".ppm";
            saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, sst.str(),
                DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
#endif
            _irradianceFinalProgramObject->deactivate();
            while ((err = glGetError()) != GL_NO_ERROR) {
                const GLubyte * errString = gluErrorString(err);
                LERROR("Error computing E Table (Sup. Terms). OpenGL error: " << errString);
            }
            
            // line 11 in algorithm 4.1
            glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _inScatteringTableTexture, 0);
            checkFrameBufferState("_inScatteringTableTexture");
            glViewport(0, 0, MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
            _deltaSSupTermsProgramObject->activate();
            deltaSRayleighTableTextureUnit.activate();
            glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
            _deltaSSupTermsProgramObject->setUniform("deltaSTexture", deltaSRayleighTableTextureUnit);
            loadAtmosphereDataIntoShaderProgram(_deltaSSupTermsProgramObject);            
            for (int layer = 0; layer < R_SAMPLES; ++layer) {
                step3DTexture(_deltaSSupTermsProgramObject, layer, false);
                renderQuadForCalc(quadCalcVAO, vertexSize);
            }
#ifdef _SAVE_ATMOSPHERE_TEXTURES
            sst.str(std::string());
            sst << "inscatteringTable_order-" << scatteringOrder << ".ppm";
            saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, sst.str(),
                MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
#endif
            _deltaSSupTermsProgramObject->deactivate();
            while ((err = glGetError()) != GL_NO_ERROR) {
                const GLubyte * errString = gluErrorString(err);
                LERROR("Error computing S Table (Sup. Terms). OpenGL error: " << errString);
            }

            glDisable(GL_BLEND);            
        }

        // Restores OpenGL blending state
        if (blendEnabled)
            glEnable(GL_BLEND);
        
        glBlendEquationSeparate(blendEquationRGB, blendEquationAlpha);
        glBlendFuncSeparate(blendSrcRGB, blendDestRGB, blendSrcAlpha, blendDestAlpha);

    }

    void RenderablePlanetAtmosphere::preCalculateAtmosphereParam() {

        std::stringstream ss;
        ss << "\n\n==== Atmosphere Values Used in Pre-Computation ====\n"
            << "Atmosphere Radius: " << _atmosphereRadius << std::endl
            << "Planet Radius: " << _atmospherePlanetRadius << std::endl
            << "Average Reflection: " << _planetAverageGroundReflectance << std::endl
            << "Rayleigh HR: " << _rayleighHeightScale << std::endl
            << "Mie HR: " << _mieHeightScale << std::endl
            << "Mie G phase constant: " << _miePhaseConstant << std::endl
            << "Mie Extinction coeff: " << glm::to_string(_mieExtinctionCoeff) << std::endl
            << "Rayleigh Scattering coeff: " << glm::to_string(_rayleighScatteringCoeff) << std::endl
            << "Mie Scattering coeff: " << glm::to_string(_mieScatteringCoeff) << std::endl
            << "Textures:" << std::endl
            << "NightTexture: " << _hasNightTexture << std::endl
            << "ReflectanceTexture: " << _hasReflectanceTexture << std::endl
            << "HeightTexture: " << _hasHeightTexture << std::endl
            << "CloudsTextures: " << _hasCloudsTexture << std::endl;
        std::cout << ss.str() << std::endl;

        //==========================================================
        //========= Load Shader Programs for Calculations ==========
        //==========================================================
        loadComputationPrograms();

        GLenum err;
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error loading shader programs for Atmosphere computation. OpenGL error: " << errString);
        }

        //==========================================================
        //============ Create Textures for Calculations ============
        //==========================================================
        createComputationTextures();

        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error creating textures for Atmosphere computation. OpenGL error: " << errString);
        }

        // Saves current FBO first
        GLint defaultFBO;
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);

        GLint m_viewport[4];
        glGetIntegerv(GL_VIEWPORT, m_viewport);

        // Creates the FBO for the calculations
        GLuint calcFBO;
        glGenFramebuffers(1, &calcFBO);
        glBindFramebuffer(GL_FRAMEBUFFER, calcFBO);
        GLenum drawBuffers[1] = { GL_COLOR_ATTACHMENT0 };
        glDrawBuffers(1, drawBuffers);

        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error creating FrameBuffer Object for Atmosphere pre-computation. OpenGL error: " << errString);
        }

        // Prepare for rendering/calculations
        GLuint quadCalcVAO;
        GLuint quadCalcVBO;
        createRenderQuad(&quadCalcVAO, &quadCalcVBO, 1.0f);


        /*if (_atmosphereCalculated) {
        LDEBUG("Cleanning Atmosphere Textures...");
        resetAtmosphereTextures(calcVAO, drawBuffers, 6);
        }*/

        // Starting Calculations...
        LDEBUG("Starting precalculations for scattering effects...");

        //==========================================================
        //=================== Execute Calculations =================
        //==========================================================
        executeCalculations(quadCalcVAO, drawBuffers, 6);

        deleteUnusedComputationTextures();

        // Restores system state
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
        glViewport(m_viewport[0], m_viewport[1],
            m_viewport[2], m_viewport[3]);
        glDeleteBuffers(1, &quadCalcVBO);
        glDeleteVertexArrays(1, &quadCalcVAO);
        glDeleteFramebuffers(1, &calcFBO);

        LDEBUG("Ended precalculations for Atmosphere effects...");
    }

    void RenderablePlanetAtmosphere::resetAtmosphereTextures(const GLuint vao, const GLenum drawBuffers[1], const GLsizei vertexSize) {
        RenderEngine& renderEngine = OsEng.renderEngine();

        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _transmittanceTableTexture, 0);
        checkFrameBufferState("_transmittanceTableTexture");
        glViewport(0, 0, TRANSMITTANCE_TABLE_WIDTH, TRANSMITTANCE_TABLE_HEIGHT);
        _cleanTextureProgramObject->activate();
        glClear(GL_COLOR_BUFFER_BIT);
        renderQuadForCalc(vao, vertexSize);
#ifdef _SAVE_ATMOSPHERE_TEXTURES
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, std::string("transmittance_texture_clean.ppm"),
            TRANSMITTANCE_TABLE_WIDTH, TRANSMITTANCE_TABLE_HEIGHT);
#endif
        GLenum err;
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error computing Transmittance T Table. OpenGL error: " << errString);
        }

        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _irradianceTableTexture, 0);
        checkFrameBufferState("_irradianceTableTexture");
        //glDrawBuffers(1, drawBuffers);
        glDrawBuffer(GL_COLOR_ATTACHMENT0);

        glViewport(0, 0, DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
        _cleanTextureProgramObject->activate();
        glClear(GL_COLOR_BUFFER_BIT);
        renderQuadForCalc(vao, vertexSize);
#ifdef _SAVE_ATMOSPHERE_TEXTURES
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, std::string("irradiance_texture_clean.ppm"),
            DELTA_E_TABLE_WIDTH, DELTA_E_TABLE_HEIGHT);
#endif
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error computing Irradiance E Table. OpenGL error: " << errString);
        }

        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _inScatteringTableTexture, 0);
        checkFrameBufferState("_inScatteringTableTexture");
        glViewport(0, 0, MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
        _cleanTextureProgramObject->activate();
        //_deltaSProgramObject->setUniform("deltaSRTexture", _deltaSRayleighTableTextureUnit);
        //_deltaSProgramObject->setUniform("deltaSMTexture", _deltaSMieTableTextureUnit);
        for (int layer = 0; layer < R_SAMPLES; ++layer) {
            step3DTexture(_deltaSProgramObject, layer, false);
            glClear(GL_COLOR_BUFFER_BIT);
            renderQuadForCalc(vao, vertexSize);
        }
#ifdef _SAVE_ATMOSPHERE_TEXTURES
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, std::string("S_texture_clean.ppm"),
            MU_S_SAMPLES * NU_SAMPLES, MU_SAMPLES);
#endif
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error computing InScattering S Table. OpenGL error: " << errString);
        }
    }

    void RenderablePlanetAtmosphere::createAtmosphereFBO() {

        GLint m_viewport[4];
        glGetIntegerv(GL_VIEWPORT, m_viewport);

        /*GLint defaultFBO;
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);    */

        ghoul::opengl::TextureUnit atmosphereTextureUnit;
        atmosphereTextureUnit.activate();
        glGenTextures(1, &_atmosphereTexture);

        glBindTexture(GL_TEXTURE_2D, _atmosphereTexture);
        //glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _atmosphereTexture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, m_viewport[2],
            m_viewport[3], 0, GL_RGB, GL_UNSIGNED_BYTE, nullptr);
        /*glTexImage2DMultisample(GL_TEXTURE_2D_MULTISAMPLE, 8, GL_RGBA,
        m_viewport[2], m_viewport[3], true);*/

        ghoul::opengl::TextureUnit atmosphereDepthTexUnit;
        atmosphereDepthTexUnit.activate();
        glGenTextures(1, &_atmosphereDepthTexture);
        glBindTexture(GL_TEXTURE_2D, _atmosphereDepthTexture);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, m_viewport[2],
            m_viewport[3], 0, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE, nullptr);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

        GLenum err;
        while ((err = glGetError()) != GL_NO_ERROR) {
            LERROR("Error creating atmosphere framebuffer. OpenGL error: " << err);
        }

        glGenFramebuffers(1, &_atmosphereFBO);
        checkFrameBufferState("creating atmosphere FBO line 2146");

    }

    void RenderablePlanetAtmosphere::createRenderQuad(GLuint * vao, GLuint * vbo, const GLfloat size) {

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
        glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 4, reinterpret_cast<GLvoid*>(0));
        glEnableVertexAttribArray(0);

        glBindVertexArray(0);

        GLenum err;
        while ((err = glGetError()) != GL_NO_ERROR) {
            LERROR("Error creating vertexbuffer for computation. OpenGL error: " << err);
        }
    }

    void RenderablePlanetAtmosphere::renderQuadForCalc(const GLuint vao, const GLsizei numberOfVertices)
    {
        glBindVertexArray(vao);
        glDrawArrays(GL_TRIANGLES, 0, numberOfVertices);
        glBindVertexArray(0);
    }

    void RenderablePlanetAtmosphere::step3DTexture(std::unique_ptr<ghoul::opengl::ProgramObject> & shaderProg,
        const int layer, const bool doCalc)
    {
        // See OpenGL redbook 8th Edition page 556 for Layered Rendering
        if (doCalc)
        {
            float earth2  = _atmospherePlanetRadius * _atmospherePlanetRadius;
            float atm2    = _atmosphereRadius * _atmosphereRadius;
            float diff    = atm2 - earth2;
            float ri      = static_cast<float>(layer) / static_cast<float>(R_SAMPLES - 1);
            float ri_2    = ri * ri;
            float epsilon = (layer == 0) ? 0.01f : (layer == (R_SAMPLES - 1)) ? -0.001f : 0.0f;
            float r       = sqrtf(earth2 + ri_2 * diff) + epsilon;
            float dminG   = r - _atmospherePlanetRadius;
            float dminT   = _atmosphereRadius - r;
            float dh      = sqrtf(r * r - earth2);
            float dH      = dh + sqrtf(diff);            
            
            shaderProg->setUniform("r", r);
            shaderProg->setUniform("dhdH", dminT, dH, dminG, dh);
        }

        shaderProg->setUniform("layer", static_cast<int>(layer));
    }

    void RenderablePlanetAtmosphere::saveTextureToPPMFile(const GLenum color_buffer_attachment, const std::string & fileName,
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

            if (color_buffer_attachment != GL_DEPTH_ATTACHMENT) {
                glReadBuffer(color_buffer_attachment);
                glReadPixels(0, 0, width, height, GL_RGB, GL_UNSIGNED_BYTE, pixels);

            }
            else {
                glReadPixels(0, 0, width, height, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE, pixels);
            }

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

    void RenderablePlanetAtmosphere::checkFrameBufferState(const std::string & codePosition) const {
        if (glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
            LERROR("Framework not built. " + codePosition);
            GLenum fbErr = glCheckFramebufferStatus(GL_FRAMEBUFFER);
            switch (fbErr) {
            case GL_FRAMEBUFFER_UNDEFINED:
                LERROR("Indefined framebuffer.");
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
                LERROR("Incomplete, missing attachement.");
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:
                LERROR("Framebuffer doesn't have at least one image attached to it.");
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER:
                LERROR("Returned if the value of GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is GL_NONE \
            for any color attachment point(s) named by GL_DRAW_BUFFERi.");
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER:
                LERROR("Returned if GL_READ_BUFFER is not GL_NONE and the value of \
                GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is GL_NONE for the color attachment point \
                named by GL_READ_BUFFER.");
                break;
            case GL_FRAMEBUFFER_UNSUPPORTED:
                LERROR("Returned if the combination of internal formats of the attached images \
                violates an implementation - dependent set of restrictions.");
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE:
                LERROR("Returned if the value of GL_RENDERBUFFER_SAMPLES is not the same for all \
            attached renderbuffers; if the value of GL_TEXTURE_SAMPLES is the not same for all \
            attached textures; or , if the attached images are a mix of renderbuffers and textures, \
            the value of GL_RENDERBUFFER_SAMPLES does not match the value of GL_TEXTURE_SAMPLES.");
                LERROR("Returned if the value of GL_TEXTURE_FIXED_SAMPLE_LOCATIONS is not the same \
            for all attached textures; or , if the attached images are a mix of renderbuffers and \
            textures, the value of GL_TEXTURE_FIXED_SAMPLE_LOCATIONS is not GL_TRUE for all attached textures.");
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS:
                LERROR("Returned if any framebuffer attachment is layered, and any populated attachment \
            is not layered, or if all populated color attachments are not from textures of the same target.");
                break;
            default:
                LDEBUG("No error found checking framebuffer: " + codePosition);
                break;
            }
        }
    }

}  // namespace openspace
