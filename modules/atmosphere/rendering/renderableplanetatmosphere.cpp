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
#include <modules/atmosphere/rendering/atmospheredeferredcaster.h>

#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/deferredcastermanager.h>
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
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtx/transform.hpp>

#include <memory>
#include <fstream>
#include <ostream>

#define _USE_MATH_DEFINES
#include <math.h>

#define _ATMOSPHERE_DEBUG
//#define _SAVE_ATMOSPHERE_TEXTURES

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
        , _heightExaggeration("heightExaggeration", "Height Exaggeration", 1.f, 0.f, 10.f)
        , _programObject(nullptr)
        , _texture(nullptr)
        , _nightTexture(nullptr)
        , _heightMapTexture(nullptr)
        , _geometry(nullptr)
        , _performShading("performShading", "Perform Shading", true)
        , _rotation("rotation", "Rotation", 0, 0, 360)
        , _alpha(1.f)
        , _planetRadius(0.f)
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
        , _hdrConstant(0.4f)
        , _atmosphereEnabled(false)
        , _hasNightTexture(false)
        , _hasHeightTexture(false)
        , _shadowEnabled(false)
        , _atmosphereHeightP("atmmosphereHeight", "Atmosphere Height (KM)", 60.0f, 0.1f, 1000.0f)
        , _groundAverageReflectanceP("averageGroundReflectance", "Average Ground Reflectance (%)", 0.1f, 0.0f, 1.0f)
        , _rayleighHeightScaleP("rayleighHeightScale", "Rayleigh Height Scale (KM)", 8.0f, 0.1f, 20.0f)
        , _rayleighScatteringCoeffXP("rayleighScatteringCoeffX", "Rayleigh Scattering Coeff X (x10e-3)", 1.0f, 0.01f, 100.0f)
        , _rayleighScatteringCoeffYP("rayleighScatteringCoeffY", "Rayleigh Scattering Coeff Y (x10e-3)", 1.0f, 0.01f, 100.0f)
        , _rayleighScatteringCoeffZP("rayleighScatteringCoeffZ", "Rayleigh Scattering Coeff Z (x10e-3)", 1.0f, 0.01f, 100.0f)
        , _mieHeightScaleP("mieHeightScale", "Mie Height Scale (KM)", 1.2f, 0.1f, 5.0f)
        , _mieScatteringCoefficientP("mieScatteringCoefficient", "Mie Scattering Coefficient (x10e-3)", 4.0f, 1.0f, 20.0f)
        , _mieScatteringExtinctionPropCoefficientP("mieScatteringExtinctionPropCoefficient",
            "Mie Scattering/Extinction Proportion Coefficient (%)", 0.9f, 0.1f, 1.0f)
        , _mieAsymmetricFactorGP("mieAsymmetricFactorG", "Mie Asymmetric Factor G", 1.0f, -1.0f, 1.0f)
        , _sunIntensityP("sunIntensity", "Sun Intensity", 50.0f, 0.1f, 200.0f)
        , _hdrExpositionP("hdrExposition", "HDR", 0.0f, 0.05f, 5.0f)
    {
        std::string name;
        bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
        ghoul_assert(success,
            "RenderablePlanetAtmosphere need the '" + SceneGraphNode::KeyName + "' be specified");

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

        std::string heightMapTexturePath = "";
        dictionary.getValue("Textures.Height", heightMapTexturePath);
        if (heightMapTexturePath != "") {
            _hasHeightTexture = true;
            _heightMapTexturePath = absPath(heightMapTexturePath);
        }

        //=======================================================
        //=========== Adding Textures as Properties =============
        //=======================================================
        addPropertySubOwner(_geometry.get());

        addProperty(_colorTexturePath);
        _colorTexturePath.onChange(std::bind(&RenderablePlanetAtmosphere::loadTexture, this));

        addProperty(_nightTexturePath);
        _nightTexturePath.onChange(std::bind(&RenderablePlanetAtmosphere::loadTexture, this));

        addProperty(_heightMapTexturePath);
        _heightMapTexturePath.onChange(std::bind(&RenderablePlanetAtmosphere::loadTexture, this));

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
        //========== Reads Atmosphere Entries from mod file ==============
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

                _rayleighScatteringCoeffXP.set(_rayleighScatteringCoeff.x * 1000.0f);
                _rayleighScatteringCoeffXP.onChange(std::bind(&RenderablePlanetAtmosphere::updateAtmosphereParameters, this));
                addProperty(_rayleighScatteringCoeffXP);

                _rayleighScatteringCoeffYP.set(_rayleighScatteringCoeff.y * 1000.0f);
                _rayleighScatteringCoeffYP.onChange(std::bind(&RenderablePlanetAtmosphere::updateAtmosphereParameters, this));
                addProperty(_rayleighScatteringCoeffYP);

                _rayleighScatteringCoeffZP.set(_rayleighScatteringCoeff.z * 1000.0f);
                _rayleighScatteringCoeffZP.onChange(std::bind(&RenderablePlanetAtmosphere::updateAtmosphereParameters, this));
                addProperty(_rayleighScatteringCoeffZP);

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

                _hdrExpositionP.set(_hdrConstant);
                _hdrExpositionP.onChange(std::bind(&RenderablePlanetAtmosphere::updateAtmosphereParameters, this));
                addProperty(_hdrExpositionP);
            }
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
        if (_programObject == nullptr && _shadowEnabled && _hasNightTexture) {
            // shadow program
            _programObject = renderEngine.buildRenderProgram(
                "shadowNightProgram",
                "${MODULE_ATMOSPHERE}/shaders/shadow_nighttexture_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/shadow_nighttexture_fs.glsl");
            std::cout << "Building shadow night..." << std::endl;
            if (!_programObject)
                return false;
        }
        else if (_programObject == nullptr && _shadowEnabled) {
            // shadow program
            _programObject = renderEngine.buildRenderProgram(
                "shadowProgram",
                "${MODULE_ATMOSPHERE}/shaders/shadow_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/shadow_fs.glsl");
            std::cout << "Building shadow..." << std::endl;
            if (!_programObject)
                return false;
        }
        else if (_programObject == nullptr && _hasNightTexture) {
            // Night texture program
            _programObject = renderEngine.buildRenderProgram(
                "nightTextureProgram",
                "${MODULE_ATMOSPHERE}/shaders/nighttexture_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/nighttexture_fs.glsl");
            std::cout << "Building night..." << std::endl;
            if (!_programObject)
                return false;
        }
        else if (_programObject == nullptr) {
            // pscstandard
            _programObject = renderEngine.buildRenderProgram(
                "pscstandard",
                "${MODULE_ATMOSPHERE}/shaders/renderableplanet_vs.glsl",
                "${MODULE_ATMOSPHERE}/shaders/renderableplanet_fs.glsl");
            std::cout << "Building renderableplanet..." << std::endl;
            if (!_programObject)
                return false;
        }
        using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
        _programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _programObject->setIgnoreUniformLocationError(IgnoreError::Yes);

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

        // Testing Deferredcaster
        _deferredcaster = std::make_unique<AtmosphereDeferredcaster>();
        if (_deferredcaster) {
            _deferredcaster->setAtmosphereRadius(_atmosphereRadius);
            _deferredcaster->setPlanetRadius(_atmospherePlanetRadius);
            _deferredcaster->setPlanetAverageGroundReflectance(_planetAverageGroundReflectance);
            _deferredcaster->setRayleighHeightScale(_rayleighHeightScale);
            _deferredcaster->setMieHeightScale(_mieHeightScale);
            _deferredcaster->setMiePhaseConstant(_miePhaseConstant);
            _deferredcaster->setSunRadianceIntensity(_sunRadianceIntensity);
            _deferredcaster->setHDRConstant(_hdrConstant);
            _deferredcaster->setRayleighScatteringCoefficients(_rayleighScatteringCoeff);
            _deferredcaster->setMieScatteringCoefficients(_mieScatteringCoeff);
            _deferredcaster->setMieExtinctionCoefficients(_mieExtinctionCoeff);

            _deferredcaster->initialize();
        }

        OsEng.renderEngine().deferredcasterManager().attachDeferredcaster(*_deferredcaster.get());

        std::function<void(bool)> onChange = [&](bool enabled) {
            if (enabled) {
                OsEng.renderEngine().deferredcasterManager().attachDeferredcaster(*_deferredcaster.get());
            }
            else {
                OsEng.renderEngine().deferredcasterManager().detachDeferredcaster(*_deferredcaster.get());
            }
        };

        onEnabledChange(onChange);

        return isReady();
    }

    bool RenderablePlanetAtmosphere::deinitialize() {
        if (_geometry) {
            _geometry->deinitialize();
            _geometry = nullptr;
        }

        RenderEngine& renderEngine = OsEng.renderEngine();
        if (_programObject) {
            renderEngine.removeRenderProgram(_programObject);
            _programObject = nullptr;
        }

        _geometry = nullptr;
        _texture = nullptr;
        _nightTexture = nullptr;

        // Testing Deferredcaster
        if (_deferredcaster) {
            OsEng.renderEngine().deferredcasterManager().detachDeferredcaster(*_deferredcaster.get());
            _deferredcaster = nullptr;
        }

        return true;
    }

    bool RenderablePlanetAtmosphere::isReady() const {
        bool ready = true;
        ready &= (_programObject != nullptr);
        ready &= (_texture != nullptr);
        ready &= (_geometry != nullptr);
        return ready;
    }

    void RenderablePlanetAtmosphere::computeModelTransformMatrix(glm::mat4 * modelTransform) {
        // scale the planet to appropriate size since the planet is a unit sphere
        *modelTransform = glm::mat4(1);

        //earth needs to be rotated for that to work.
        glm::mat4 rot = glm::rotate(*modelTransform, static_cast<float>(M_PI_2), glm::vec3(1, 0, 0));
        glm::mat4 roty = glm::rotate(*modelTransform, static_cast<float>(M_PI_2), glm::vec3(0, -1, 0));
        glm::mat4 rotProp = glm::rotate(*modelTransform, glm::radians(static_cast<float>(_rotation)), glm::vec3(0, 1, 0));

        // _stateMatrix is the Matrix transformation from _frame coordinate system (Earth in this case)
        // to "GALATIC" coordinate system.
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                (*modelTransform)[i][j] = static_cast<float>(_stateMatrix[i][j]);
            }
        }
        *modelTransform = *modelTransform * rot * roty * rotProp;
    }

    void RenderablePlanetAtmosphere::computeModelTransformMatrix(glm::dmat4 * modelTransform) {
        // scale the planet to appropriate size since the planet is a unit sphere
        *modelTransform = glm::dmat4(1);

        //earth needs to be rotated for that to work.
        glm::dmat4 rot = glm::rotate(*modelTransform, static_cast<double>(M_PI_2), glm::dvec3(1, 0, 0));
        glm::dmat4 roty = glm::rotate(*modelTransform, static_cast<double>(M_PI_2), glm::dvec3(0, -1, 0));
        glm::dmat4 rotProp = glm::rotate(*modelTransform, glm::radians(static_cast<double>(_rotation)), glm::dvec3(0, 1, 0));

        // _stateMatrix is the Matrix transformation from _frame coordinate system (Earth in this case)
        // to "GALATIC" coordinate system.
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                (*modelTransform)[i][j] = _stateMatrix[i][j];
            }
        }
        *modelTransform = *modelTransform * rot * roty * rotProp;
    }

    void RenderablePlanetAtmosphere::render(const RenderData& data, RendererTasks& tasks) {

        GLint defaultFBO;
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);        

        // activate shader
        _programObject->activate();

        // scale the planet to appropriate size since the planet is a unit sphere
        glm::mat4 transform = glm::mat4(1);

        //earth needs to be rotated for that to work.
        computeModelTransformMatrix(&transform);

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

        dayUnit.activate();
        _texture->bind();
        _programObject->setUniform("texture1", dayUnit);

        // Bind possible night texture
        if (_hasNightTexture) {
            ghoul::opengl::TextureUnit nightUnit;
            nightUnit.activate();
            _nightTexture->bind();
            _programObject->setUniform("nightTex", nightUnit);
        }

        if (_hasHeightTexture) {
            ghoul::opengl::TextureUnit heightUnit;
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

        // render
        _geometry->render();

        // disable shader
        _programObject->deactivate();

        // Testing Deferredcaster
        DeferredcasterTask task{ _deferredcaster.get(), data };
        tasks.deferredcasterTasks.push_back(task);
    }


    void RenderablePlanetAtmosphere::update(const UpdateData& data) {
        // set spice-orientation in accordance to timestamp
        _stateMatrix = SpiceManager::ref().positionTransformMatrix(_frame, "GALACTIC", data.time);
        _time = data.time;

        if (_programObject && _programObject->isDirty())
            _programObject->rebuildFromFile();

        if (_deferredcaster) {
            _deferredcaster->setTime(data.time);
            glm::dmat4 modelTransform;
            computeModelTransformMatrix(&modelTransform);
            _deferredcaster->setModelTransform(modelTransform);
        }       
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
    }

    void RenderablePlanetAtmosphere::updateAtmosphereParameters() {
        _atmosphereRadius               = _atmospherePlanetRadius + _atmosphereHeightP;
        _planetAverageGroundReflectance = _groundAverageReflectanceP;
        _rayleighHeightScale            = _rayleighHeightScaleP;
        _rayleighScatteringCoeff        = glm::vec3(_rayleighScatteringCoeffXP * 0.001f, _rayleighScatteringCoeffYP * 0.001f,
                                                    _rayleighScatteringCoeffZP * 0.001f);
        _mieHeightScale                 = _mieHeightScaleP;
        _mieScatteringCoeff             = glm::vec3(_mieScatteringCoefficientP * 0.001f);
        _mieExtinctionCoeff             = _mieScatteringCoeff * (1.0f / static_cast<float>(_mieScatteringExtinctionPropCoefficientP));
        _miePhaseConstant               = _mieAsymmetricFactorGP;
        _sunRadianceIntensity           = _sunIntensityP;
        _hdrConstant                    = _hdrExpositionP;

        if (_deferredcaster) {
            _deferredcaster->setAtmosphereRadius(_atmosphereRadius);
            _deferredcaster->setPlanetRadius(_atmospherePlanetRadius);
            _deferredcaster->setPlanetAverageGroundReflectance(_planetAverageGroundReflectance);
            _deferredcaster->setRayleighHeightScale(_rayleighHeightScale);
            _deferredcaster->setMieHeightScale(_mieHeightScale);
            _deferredcaster->setMiePhaseConstant(_miePhaseConstant);
            _deferredcaster->setSunRadianceIntensity(_sunRadianceIntensity);
            _deferredcaster->setHDRConstant(_hdrConstant);
            _deferredcaster->setRayleighScatteringCoefficients(_rayleighScatteringCoeff);
            _deferredcaster->setMieScatteringCoefficients(_mieScatteringCoeff);
            _deferredcaster->setMieExtinctionCoefficients(_mieExtinctionCoeff);

            _deferredcaster->preCalculateAtmosphereParam();
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
