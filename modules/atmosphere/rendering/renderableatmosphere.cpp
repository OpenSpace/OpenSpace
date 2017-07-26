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


#include <modules/atmosphere/rendering/renderableatmosphere.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

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
#include <ghoul/misc/invariants.h>

#include <openspace/rendering/deferredcastermanager.h>
#include <modules/atmosphere/rendering/atmospheredeferredcaster.h>
#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderer.h>

#include <glm/gtx/string_cast.hpp>

#include <memory>
#include <fstream>

#define _USE_MATH_DEFINES
#include <math.h>


namespace {
    const char* KeyGeometry = "Geometry";
    const char* KeyRadius   = "Radius";
    
    static const char* _loggerCat = "RenderableAtmosphere";

    const char* keyFrame        = "Frame";
    const char* keyShadowGroup  = "Shadow_Group";
    const char* keyShadowSource = "Source";
    const char* keyShadowCaster = "Caster";
    const char* keyBody         = "Body";

    const char* keyAtmosphere               = "Atmosphere";
    const char* keyAtmosphereType           = "Type";
    const char* keyAtmosphereRadius         = "AtmosphereRadius";
    const char* keyPlanetRadius             = "PlanetRadius";
    const char* keyAverageGroundReflectance = "PlanetAverageGroundReflectance";
    const char* keyRayleigh                 = "Rayleigh";
    const char* keyRayleighHeightScale      = "H_R";
    const char* keyOzone                    = "Ozone";
    const char* keyOzoneHeightScale         = "H_O";
    const char* keyMie                      = "Mie";
    const char* keyMieHeightScale           = "H_M";
    const char* keyMiePhaseConstant         = "G";
    const char* keyImage                    = "Image";
    const char* keyToneMappingOp            = "ToneMapping";
    const char* keyExposure                 = "Exposure";
    const char* keyBackground               = "Background";
    const char* keyGamma                    = "Gamma";
    const char* keyATMDebug                 = "Debug";
    const char* keyTextureScale             = "PreCalculatedTextureScale";
    const char* keySaveTextures             = "SaveCalculatedTextures";
} // namespace

namespace openspace {

    documentation::Documentation RenderableAtmosphere::Documentation() {
        using namespace documentation;
        return {
            "RenderableAtmosphere",
            "atmosphere_renderable_atmosphere",
            {   /*
                {
                    keyAtmosphereRadius,
                    new ReferencingVerifier("atmosphere"),
                    "Specifies the atmosphere's height in this RenderableAtmosphere.",
                    Optional::No
                },
                
                {
                    KeyShading,
                    new BoolVerifier,
                    "Specifies whether the atmosphere should be rendered shaded by the Sun. If "
                    "this value is 'false', any existing night texture will not be used. "
                    "This value defaults to 'true'.",
                    Optional::Yes
                }
                */
            }
        };
    }

    RenderableAtmosphere::RenderableAtmosphere(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary)
        , _atmosphereHeightP("atmmosphereHeight", "Atmosphere Height (KM)", 60.0f, 0.1f, 99.0f)
        , _groundAverageReflectanceP("averageGroundReflectance", "Average Ground Reflectance (%)", 0.1f, 0.0f, 1.0f)
        , _rayleighHeightScaleP("rayleighHeightScale", "Rayleigh Height Scale (KM)", 8.0f, 0.1f, 20.0f)
        , _rayleighScatteringCoeffXP("rayleighScatteringCoeffX", "Rayleigh Scattering Coeff X (x10e-3)", 1.0f, 0.01f, 100.0f)
        , _rayleighScatteringCoeffYP("rayleighScatteringCoeffY", "Rayleigh Scattering Coeff Y (x10e-3)", 1.0f, 0.01f, 100.0f)
        , _rayleighScatteringCoeffZP("rayleighScatteringCoeffZ", "Rayleigh Scattering Coeff Z (x10e-3)", 1.0f, 0.01f, 100.0f)
        , _ozoneEnabledP("ozone", "Ozone Layer Enabled", true)
        , _ozoneHeightScaleP("ozoneLayerHeightScale", "Ozone Height Scale (KM)", 8.0f, 0.1f, 20.0f)
        , _ozoneCoeffXP("ozoneLayerCoeffX", "Ozone Layer Extinction Coeff X (x10e-5)", 3.426f, 0.01f, 100.0f)
        , _ozoneCoeffYP("ozoneLayerCoeffY", "Ozone Layer Extinction Coeff Y (x10e-5)", 8.298f, 0.01f, 100.0f)
        , _ozoneCoeffZP("ozoneLayerCoeffZ", "Ozone Layer Extinction Coeff Z (x10e-5)", 0.356f, 0.01f, 100.0f)
        , _mieHeightScaleP("mieHeightScale", "Mie Height Scale (KM)", 1.2f, 0.1f, 20.0f)
        , _mieScatteringCoeffXP("mieScatteringCoeffX", "Mie Scattering Coeff X (x10e-3)", 4.0f, 0.01f, 1000.0f)
        , _mieScatteringCoeffYP("mieScatteringCoeffY", "Mie Scattering Coeff Y (x10e-3)", 4.0f, 0.01f, 1000.0f)
        , _mieScatteringCoeffZP("mieScatteringCoeffZ", "Mie Scattering Coeff Z (x10e-3)", 4.0f, 0.01f, 1000.0f)
        , _mieScatteringExtinctionPropCoefficientP("mieScatteringExtinctionPropCoefficient",
            "Mie Scattering/Extinction Proportion Coefficient (%)", 0.9f, 0.01f, 1.0f)
        , _mieAsymmetricFactorGP("mieAsymmetricFactorG", "Mie Asymmetric Factor G", 0.85f, -1.0f, 1.0f)
        , _sunIntensityP("sunIntensity", "Sun Intensity", 50.0f, 0.1f, 1000.0f)
        , _hdrExpositionP("hdrExposition", "HDR", 0.4f, 0.01f, 5.0f)
        , _gammaConstantP("gamma", "Gamma Correction", 1.8f, 0.1f, 3.0f)
        , _sunFollowingCameraEnabledP("sunFollowingCamera", "Enable Sun On Camera Position", false)
        , _atmosphereEnabled(false)
        , _ozoneLayerEnabled(false)
        , _sunFollowingCameraEnabled(false)
        , _atmosphereRadius(0.f)
        , _atmospherePlanetRadius(0.f)
        , _planetAverageGroundReflectance(0.f)
        , _rayleighHeightScale(0.f)
        , _ozoneHeightScale(0.f)
        , _mieHeightScale(0.f)
        , _miePhaseConstant(0.f)
        , _sunRadianceIntensity(50.f)
        , _hdrConstant(0.f)
        , _exposureBackgroundConstant(2.8f)
        , _gammaConstant(1.8f)
        , _mieExtinctionCoeff(glm::vec3(0.f))
        , _rayleighScatteringCoeff(glm::vec3(0.f))
        , _ozoneExtinctionCoeff(glm::vec3(0.f))
        , _mieScatteringCoeff(glm::vec3(0.f))
        , _saveCalculationsToTexture(false)
        , _preCalculatedTexturesScale(1.0)
        , _shadowEnabled(false)
        , _time(0.f)
    {
        ghoul_precondition(
            dictionary.hasKeyAndValue<std::string>(SceneGraphNode::KeyName),
            "RenderableAtmosphere needs the name to be specified"
        );

        documentation::testSpecificationAndThrow(
            Documentation(),
            dictionary,
            "RenderableAtmosphere"
        );

        const std::string name = dictionary.value<std::string>(SceneGraphNode::KeyName);
                
        //================================================================
        //======== Reads Shadow (Eclipses) Entries in mod file ===========
        //================================================================
        ghoul::Dictionary shadowDictionary;
        bool success = dictionary.getValue(keyShadowGroup, shadowDictionary);
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
                    float sourceRadius;
                    ss.str(std::string());
                    ss << keyShadowSource << sourceCounter << ".Radius";
                    success = shadowDictionary.getValue(ss.str(), sourceRadius);
                    if (success) {
                        sourceArray.push_back(std::pair< std::string, float>(
                            sourceName, sourceRadius));
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
                        float casterRadius;
                        ss.str(std::string());
                        ss << keyShadowCaster << casterCounter << ".Radius";
                        success = shadowDictionary.getValue(ss.str(), casterRadius);
                        if (success) {
                            casterArray.push_back(std::pair< std::string, float>(
                                casterName, casterRadius));
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
            std::string atmTypeString;
            if (!atmosphereDictionary.getValue(keyAtmosphereType, atmTypeString)) {
                errorReadingAtmosphereData = true;
                LWARNING("No Atmosphere Type value expecified for Atmosphere Effects " << name 
                    << " planet. Types allowed: RenderableGlobe or RenderablePlanet.\nDisabling atmosphere effects for this planet.");
            }
            else {
                if (atmTypeString.compare("RenderableGlobe") == 0) {
                    _atmosphereType = AtmosphereDeferredcaster::RenderableGlobe;
                }
                else if (atmTypeString.compare("RenderablePlanet") == 0) 
                {
                    _atmosphereType = AtmosphereDeferredcaster::RenderablePlanet;
                }
                else 
                {
                    errorReadingAtmosphereData = true;
                    LWARNING("Wrong atmosphere type specified for " << name 
                        << " planet. Types allowed: RenderableGlobe or RenderablePlanet.\nDisabling atmosphere effects for this planet.");
                }
            }

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

            ghoul::Dictionary ozoneDictionary;
            success = atmosphereDictionary.getValue(keyOzone, ozoneDictionary);
            if (success) {
                _ozoneLayerEnabled = true;
                if (!ozoneDictionary.getValue(keyOzoneHeightScale, _ozoneHeightScale)) {
                    _ozoneLayerEnabled = false;
                }

                if (!ozoneDictionary.getValue("Coefficients.Extinction", _ozoneExtinctionCoeff)) {
                    _ozoneLayerEnabled = false;
                }
            }
            else {
                _ozoneLayerEnabled = false;
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

            ghoul::Dictionary ImageDictionary;
            success = atmosphereDictionary.getValue(keyImage, ImageDictionary);
            if (success) {
                if (ImageDictionary.getValue(keyToneMappingOp, _preCalculatedTexturesScale)) {
                    LDEBUG("Atmosphere Texture Scaled to " << _preCalculatedTexturesScale);
                }

                if (ImageDictionary.getValue(keyExposure, _hdrConstant)) {
                    LDEBUG("Saving Precalculated Atmosphere Textures.");
                }

                if (ImageDictionary.getValue(keyGamma, _gammaConstant)) {
                    LDEBUG("Saving Precalculated Atmosphere Textures.");
                }
            }

            ghoul::Dictionary debugDictionary;
            success = atmosphereDictionary.getValue(keyATMDebug, debugDictionary);
            if (success) {
                if (debugDictionary.getValue(keyTextureScale, _preCalculatedTexturesScale)) {
                    LDEBUG("Atmosphere Texture Scaled to " << _preCalculatedTexturesScale);
                }

                if (debugDictionary.getValue(keySaveTextures, _saveCalculationsToTexture)) {
                    LDEBUG("Saving Precalculated Atmosphere Textures.");
                }

            }

            if (!errorReadingAtmosphereData) {
                _atmosphereEnabled = true;

                //========================================================
                //============== Atmosphere Properties ===================
                //========================================================

                _atmosphereHeightP.set(_atmosphereRadius - _atmospherePlanetRadius);
                _atmosphereHeightP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_atmosphereHeightP);

                _groundAverageReflectanceP.set(_planetAverageGroundReflectance);
                _groundAverageReflectanceP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_groundAverageReflectanceP);

                _rayleighHeightScaleP.set(_rayleighHeightScale);
                _rayleighHeightScaleP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_rayleighHeightScaleP);

                _rayleighScatteringCoeffXP.set(_rayleighScatteringCoeff.x * 1000.0f);
                _rayleighScatteringCoeffXP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_rayleighScatteringCoeffXP);

                _rayleighScatteringCoeffYP.set(_rayleighScatteringCoeff.y * 1000.0f);
                _rayleighScatteringCoeffYP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_rayleighScatteringCoeffYP);

                _rayleighScatteringCoeffZP.set(_rayleighScatteringCoeff.z * 1000.0f);
                _rayleighScatteringCoeffZP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_rayleighScatteringCoeffZP);

                _ozoneEnabledP.set(_ozoneLayerEnabled);
                _ozoneEnabledP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_ozoneEnabledP);

                _ozoneHeightScaleP.set(_ozoneHeightScale);
                _ozoneHeightScaleP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_ozoneHeightScaleP);

                _ozoneCoeffXP.set(_ozoneExtinctionCoeff.x * 100000.0f);
                _ozoneCoeffXP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_ozoneCoeffXP);

                _ozoneCoeffYP.set(_ozoneExtinctionCoeff.y * 100000.0f);
                _ozoneCoeffYP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_ozoneCoeffYP);

                _ozoneCoeffZP.set(_ozoneExtinctionCoeff.z * 100000.0f);
                _ozoneCoeffZP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_ozoneCoeffZP);

                _mieHeightScaleP.set(_mieHeightScale);
                _mieHeightScaleP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_mieHeightScaleP);

                _mieScatteringCoeffXP.set(_mieScatteringCoeff.x * 1000.0f);
                _mieScatteringCoeffXP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_mieScatteringCoeffXP);

                _mieScatteringCoeffYP.set(_mieScatteringCoeff.y * 1000.0f);
                _mieScatteringCoeffYP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_mieScatteringCoeffYP);

                _mieScatteringCoeffZP.set(_mieScatteringCoeff.z * 1000.0f);
                _mieScatteringCoeffZP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_mieScatteringCoeffZP);

                _mieScatteringExtinctionPropCoefficientP.set(_mieScatteringCoeff.x / _mieExtinctionCoeff.x);
                _mieScatteringExtinctionPropCoefficientP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_mieScatteringExtinctionPropCoefficientP);

                _mieAsymmetricFactorGP.set(_miePhaseConstant);
                _mieAsymmetricFactorGP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_mieAsymmetricFactorGP);

                _sunIntensityP.set(_sunRadianceIntensity);
                _sunIntensityP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_sunIntensityP);

                _hdrExpositionP.set(_hdrConstant);
                _hdrExpositionP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_hdrExpositionP);
                
                _gammaConstantP.set(_gammaConstant);
                _gammaConstantP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_gammaConstantP);

                _sunFollowingCameraEnabledP.set(_sunFollowingCameraEnabled);
                _sunFollowingCameraEnabledP.onChange(std::bind(&RenderableAtmosphere::updateAtmosphereParameters, this));
                addProperty(_sunFollowingCameraEnabledP);
            }
        }
    }

    bool RenderableAtmosphere::initialize() {
        RenderEngine& renderEngine = OsEng.renderEngine();

        if (_atmosphereEnabled) {
            _deferredcaster = std::make_unique<AtmosphereDeferredcaster>();
            if (_deferredcaster) {
                _deferredcaster->setAtmosphereRadius(_atmosphereRadius);
                _deferredcaster->setPlanetRadius(_atmospherePlanetRadius);
                _deferredcaster->setPlanetAverageGroundReflectance(_planetAverageGroundReflectance);
                _deferredcaster->setRayleighHeightScale(_rayleighHeightScale);
                _deferredcaster->enableOzone(_ozoneLayerEnabled);
                _deferredcaster->setOzoneHeightScale(_ozoneHeightScale);
                _deferredcaster->setMieHeightScale(_mieHeightScale);
                _deferredcaster->setMiePhaseConstant(_miePhaseConstant);
                _deferredcaster->setSunRadianceIntensity(_sunRadianceIntensity);
                _deferredcaster->setHDRConstant(_hdrConstant);
                _deferredcaster->setBackgroundConstant(_exposureBackgroundConstant);
                _deferredcaster->setGammaConstant(_gammaConstant);
                _deferredcaster->setRayleighScatteringCoefficients(_rayleighScatteringCoeff);
                _deferredcaster->setOzoneExtinctionCoefficients(_ozoneExtinctionCoeff);
                _deferredcaster->setMieScatteringCoefficients(_mieScatteringCoeff);
                _deferredcaster->setMieExtinctionCoefficients(_mieExtinctionCoeff);
                // TODO: Fix the ellipsoid nature of the renderable globe (JCC)
                //_deferredcaster->setEllipsoidRadii(_ellipsoid.radii());
                _deferredcaster->setRenderableClass(_atmosphereType);
                _deferredcaster->enableSunFollowing(_sunFollowingCameraEnabled);

                _deferredcaster->setPrecalculationTextureScale(_preCalculatedTexturesScale);
                if (_saveCalculationsToTexture)
                    _deferredcaster->enablePrecalculationTexturesSaving();

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
        }

        return isReady();
    }

    bool RenderableAtmosphere::deinitialize() {

        if (_deferredcaster) {
            OsEng.renderEngine().deferredcasterManager().detachDeferredcaster(*_deferredcaster.get());
            _deferredcaster = nullptr;
        }

        return true;
    }

    bool RenderableAtmosphere::isReady() const {
        bool ready = true;
        ready &= (_deferredcaster != nullptr);
        return ready;
    }

    void RenderableAtmosphere::computeModelTransformMatrix(const openspace::TransformData & transformData, 
                                                           glm::dmat4 * modelTransform) {
        // scale the planet to appropriate size since the planet is a unit sphere    
        *modelTransform =
            glm::translate(glm::dmat4(1.0), transformData.translation) * // Translation
            glm::dmat4(transformData.rotation) *  // Spice rotation
            glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(transformData.scale)));          
    }

    void RenderableAtmosphere::render(const RenderData& data, RendererTasks& renderTask) {

        if (_atmosphereEnabled) {
            DeferredcasterTask task{ _deferredcaster.get(), data };
            renderTask.deferredcasterTasks.push_back(task);
        }

    }

    void RenderableAtmosphere::update(const UpdateData& data) {
        _stateMatrix = data.modelTransform.rotation;
        _time = data.time.j2000Seconds();

        if (_deferredcaster) {
            _deferredcaster->setTime(data.time.j2000Seconds());
            glm::dmat4 modelTransform;
            computeModelTransformMatrix(data.modelTransform, &modelTransform);
            if (_atmosphereType == AtmosphereDeferredcaster::RenderablePlanet) {
                //earth needs to be rotated
                glm::dmat4 rot = glm::rotate(glm::dmat4(1.0), M_PI_2, glm::dvec3(1, 0, 0));
                glm::dmat4 roty = glm::rotate(glm::dmat4(1.0), M_PI_2, glm::dvec3(0, -1, 0));
                modelTransform = modelTransform * rot * roty;
            }
            _deferredcaster->setModelTransform(modelTransform);

            if (_exposureBackgroundConstant != OsEng.renderEngine().renderer()->hdrBackground())
                updateAtmosphereParameters();
        }        
    }   

    void RenderableAtmosphere::updateAtmosphereParameters() {
        bool executeComputation = true;

        if (_sunRadianceIntensity != _sunIntensityP ||
            _hdrConstant != _hdrExpositionP ||
            _exposureBackgroundConstant != OsEng.renderEngine().renderer()->hdrBackground() ||
            _gammaConstant != _gammaConstantP ||
            _sunFollowingCameraEnabled != _sunFollowingCameraEnabledP)
            executeComputation = false;

        _atmosphereRadius = _atmospherePlanetRadius + _atmosphereHeightP;
        _planetAverageGroundReflectance = _groundAverageReflectanceP;
        _rayleighHeightScale = _rayleighHeightScaleP;
        _rayleighScatteringCoeff = glm::vec3(_rayleighScatteringCoeffXP * 0.001f, _rayleighScatteringCoeffYP * 0.001f,
            _rayleighScatteringCoeffZP * 0.001f);
        _ozoneLayerEnabled = _ozoneEnabledP;
        _ozoneHeightScale = _ozoneHeightScaleP;
        _ozoneExtinctionCoeff = glm::vec3(_ozoneCoeffXP.value() * 0.00001f,
            _ozoneCoeffYP.value() * 0.00001f,
            _ozoneCoeffZP.value() * 0.00001f);
        _mieHeightScale = _mieHeightScaleP;
        _mieScatteringCoeff = glm::vec3(_mieScatteringCoeffXP * 0.001f, _mieScatteringCoeffYP * 0.001f, 
            _mieScatteringCoeffZP * 0.001f);
        _mieExtinctionCoeff = _mieScatteringCoeff * (1.0f / static_cast<float>(_mieScatteringExtinctionPropCoefficientP));
        _miePhaseConstant = _mieAsymmetricFactorGP;
        _sunRadianceIntensity = _sunIntensityP;
        _hdrConstant = _hdrExpositionP;
        _exposureBackgroundConstant = OsEng.renderEngine().renderer()->hdrBackground();
        _gammaConstant = _gammaConstantP;
        _sunFollowingCameraEnabled = _sunFollowingCameraEnabledP;


        if (_deferredcaster) {
            _deferredcaster->setAtmosphereRadius(_atmosphereRadius);
            _deferredcaster->setPlanetRadius(_atmospherePlanetRadius);
            _deferredcaster->setPlanetAverageGroundReflectance(_planetAverageGroundReflectance);
            _deferredcaster->setRayleighHeightScale(_rayleighHeightScale);
            _deferredcaster->enableOzone(_ozoneLayerEnabled);
            _deferredcaster->setOzoneHeightScale(_ozoneHeightScale);
            _deferredcaster->setMieHeightScale(_mieHeightScale);
            _deferredcaster->setMiePhaseConstant(_miePhaseConstant);
            _deferredcaster->setSunRadianceIntensity(_sunRadianceIntensity);
            _deferredcaster->setHDRConstant(_hdrConstant);
            _deferredcaster->setBackgroundConstant(_exposureBackgroundConstant);
            _deferredcaster->setGammaConstant(_gammaConstant);
            _deferredcaster->setRayleighScatteringCoefficients(_rayleighScatteringCoeff);
            _deferredcaster->setOzoneExtinctionCoefficients(_ozoneExtinctionCoeff);
            _deferredcaster->setMieScatteringCoefficients(_mieScatteringCoeff);
            _deferredcaster->setMieExtinctionCoefficients(_mieExtinctionCoeff);
            _deferredcaster->setRenderableClass(_atmosphereType);
            _deferredcaster->enableSunFollowing(_sunFollowingCameraEnabled);
            //_deferredcaster->setEllipsoidRadii(_ellipsoid.radii());
            if (executeComputation)
                _deferredcaster->preCalculateAtmosphereParam();
        }
    }
}  // namespace openspace