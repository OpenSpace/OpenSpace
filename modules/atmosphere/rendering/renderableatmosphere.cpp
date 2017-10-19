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

    static const openspace::properties::Property::PropertyInfo AtmosphereHeightInfo = {
        "atmmosphereHeight",
        "Atmosphere Height (KM)",
        "" // @TODO Missing documentation
    };
        
    static const openspace::properties::Property::PropertyInfo AverageGroundReflectanceInfo = {
        "AverageGroundReflectance", 
        "Average Ground Reflectance (%)",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo RayleighHeightScaleInfo = {
        "RayleighHeightScale", 
        "Rayleigh Height Scale (KM)",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo RayleighScatteringCoeffXInfo = {
        "RayleighScatteringCoeffX", 
        "Rayleigh Scattering Coeff X (x10e-3)",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo RayleighScatteringCoeffYInfo = {
        "RayleighScatteringCoeffY",
        "Rayleigh Scattering Coeff Y (x10e-3)",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo RayleighScatteringCoeffZInfo = {
        "RayleighScatteringCoeffZ",
        "Rayleigh Scattering Coeff Z (x10e-3)",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo OzoneLayerInfo = {
        "Ozone", 
        "Ozone Layer Enabled",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo OzoneHeightScaleInfo = {
        "OzoneLayerHeightScale", 
        "Ozone Height Scale (KM)",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo OzoneLayerCoeffXInfo = {
        "OzoneLayerCoeffX", 
        "Ozone Layer Extinction Coeff X (x10e-5)",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo OzoneLayerCoeffYInfo = {
        "OzoneLayerCoeffY",
        "Ozone Layer Extinction Coeff Y (x10e-5)",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo OzoneLayerCoeffZInfo = {
        "OzoneLayerCoeffZ",
        "Ozone Layer Extinction Coeff Z (x10e-5)",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo MieHeightScaleInfo = {
        "MieHeightScale", 
        "Mie Height Scale (KM)",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo MieScatteringCoeffXInfo = {
        "MieScatteringCoeffX",
        "Mie Scattering Coeff X (x10e-3)",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo MieScatteringCoeffYInfo = {
        "MieScatteringCoeffY",
        "Mie Scattering Coeff Y (x10e-3)",
        "" // @TODO Missing documentation
    };
    
    static const openspace::properties::Property::PropertyInfo MieScatteringCoeffZInfo = {
        "MieScatteringCoeffZ",
        "Mie Scattering Coeff Z (x10e-3)",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo MieScatteringExtinctionPropCoeffInfo = {
        "MieScatteringExtinctionPropCoefficient",
        "Mie Scattering/Extinction Proportion Coefficient (%)",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo MieAsymmetricFactorGInfo = {
        "MieAsymmetricFactorG", 
        "Mie Asymmetric Factor G",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo SunIntensityInfo = {
        "SunIntensity", 
        "Sun Intensity",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo AtmosphereExposureInfo = {
        "HdrExposure", 
        "Atmosphere Exposure",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo AtmosphereGammaInfo = {
        "Gamma", 
        "Gamma Correction",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo EnableSunOnCameraPositionInfo = {
        "SunFollowingCamera", 
        "Enable Sun On Camera Position",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo EclipseHardShadowsInfo = {
        "EclipseHardShadowsInfo",
        "Enable Hard Shadows for Eclipses",
        "" // @TODO Missing documentation
    };
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
        , _atmosphereHeightP(AtmosphereHeightInfo, 60.0f, 0.1f, 99.0f)
        , _groundAverageReflectanceP(AverageGroundReflectanceInfo, 0.1f, 0.0f, 1.0f)
        , _rayleighHeightScaleP(RayleighHeightScaleInfo, 8.0f, 0.1f, 20.0f)
        , _rayleighScatteringCoeffXP(RayleighScatteringCoeffXInfo, 1.0f, 0.01f, 100.0f)
        , _rayleighScatteringCoeffYP(RayleighScatteringCoeffYInfo, 1.0f, 0.01f, 100.0f)
        , _rayleighScatteringCoeffZP(RayleighScatteringCoeffZInfo, 1.0f, 0.01f, 100.0f)
        , _ozoneEnabledP(OzoneLayerInfo, true)
        , _ozoneHeightScaleP(OzoneHeightScaleInfo, 8.0f, 0.1f, 20.0f)
        , _ozoneCoeffXP(OzoneLayerCoeffXInfo, 3.426f, 0.01f, 100.0f)
        , _ozoneCoeffYP(OzoneLayerCoeffYInfo, 8.298f, 0.01f, 100.0f)
        , _ozoneCoeffZP(OzoneLayerCoeffZInfo, 0.356f, 0.01f, 100.0f)
        , _mieHeightScaleP(MieHeightScaleInfo, 1.2f, 0.1f, 20.0f)
        , _mieScatteringCoeffXP(MieScatteringCoeffXInfo, 4.0f, 0.01f, 1000.0f)
        , _mieScatteringCoeffYP(MieScatteringCoeffYInfo, 4.0f, 0.01f, 1000.0f)
        , _mieScatteringCoeffZP(MieScatteringCoeffZInfo, 4.0f, 0.01f, 1000.0f)
        , _mieScatteringExtinctionPropCoefficientP(MieScatteringExtinctionPropCoeffInfo, 0.9f, 0.01f, 1.0f)
        , _mieAsymmetricFactorGP(MieAsymmetricFactorGInfo, 0.85f, -1.0f, 1.0f)
        , _sunIntensityP(SunIntensityInfo, 50.0f, 0.1f, 1000.0f)
        , _hdrExpositionP(AtmosphereExposureInfo, 0.4f, 0.01f, 5.0f)
        , _gammaConstantP(AtmosphereGammaInfo, 1.8f, 0.1f, 3.0f)
        , _sunFollowingCameraEnabledP(EnableSunOnCameraPositionInfo, false)
        , _hardShadowsEnabledP(EclipseHardShadowsInfo, false)
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
        , _hardShadows(false)
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
            std::vector<std::pair<std::string, double>> sourceArray;
            unsigned int sourceCounter = 1; 
            while (success) {
                std::string sourceName;
                success = shadowDictionary.getValue(keyShadowSource + 
                    std::to_string(sourceCounter) + ".Name", sourceName);
                if (success) {
                    double sourceRadius;
                    success = shadowDictionary.getValue(keyShadowSource + 
                        std::to_string(sourceCounter) + ".Radius", sourceRadius);
                    if (success) {
                        sourceArray.emplace_back(sourceName, sourceRadius);
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
                std::vector<std::pair<std::string, double>> casterArray;
                unsigned int casterCounter = 1;
                while (success) {
                    std::string casterName;
                    success = shadowDictionary.getValue(keyShadowCaster +
                        std::to_string(casterCounter) + ".Name", casterName);
                    if (success) {
                        double casterRadius;
                        success = shadowDictionary.getValue(keyShadowCaster +
                            std::to_string(casterCounter) + ".Radius", casterRadius);
                        if (success) {
                            casterArray.emplace_back(casterName, casterRadius);
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
                    for (const auto & source : sourceArray) {
                        for (const auto & caster : casterArray) {
                            ShadowConfiguration sc;
                            sc.source = source;
                            sc.caster = caster;
                            _shadowConfArray.push_back(sc);
                        }
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

                _atmosphereHeightP =_atmosphereRadius - _atmospherePlanetRadius;
                _atmosphereHeightP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_atmosphereHeightP);

                _groundAverageReflectanceP = _planetAverageGroundReflectance;
                _groundAverageReflectanceP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_groundAverageReflectanceP);

                _rayleighHeightScaleP = _rayleighHeightScale;
                _rayleighHeightScaleP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_rayleighHeightScaleP);

                _rayleighScatteringCoeffXP = _rayleighScatteringCoeff.x * 1000.0f;
                _rayleighScatteringCoeffXP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_rayleighScatteringCoeffXP);

                _rayleighScatteringCoeffYP = _rayleighScatteringCoeff.y * 1000.0f;
                _rayleighScatteringCoeffYP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_rayleighScatteringCoeffYP);

                _rayleighScatteringCoeffZP = _rayleighScatteringCoeff.z * 1000.0f;
                _rayleighScatteringCoeffZP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_rayleighScatteringCoeffZP);

                _ozoneEnabledP = _ozoneLayerEnabled;
                _ozoneEnabledP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_ozoneEnabledP);

                _ozoneHeightScaleP = _ozoneHeightScale;
                _ozoneHeightScaleP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_ozoneHeightScaleP);

                _ozoneCoeffXP = _ozoneExtinctionCoeff.x * 100000.0f;
                _ozoneCoeffXP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_ozoneCoeffXP);

                _ozoneCoeffYP = _ozoneExtinctionCoeff.y * 100000.0f;
                _ozoneCoeffYP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_ozoneCoeffYP);


                _ozoneCoeffZP = _ozoneExtinctionCoeff.z * 100000.0f;
                _ozoneCoeffZP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_ozoneCoeffZP);

                _mieHeightScaleP = _mieHeightScale;
                _mieHeightScaleP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_mieHeightScaleP);

                _mieScatteringCoeffXP = _mieScatteringCoeff.x * 1000.0f;
                _mieScatteringCoeffXP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_mieScatteringCoeffXP);

                _mieScatteringCoeffYP = _mieScatteringCoeff.y * 1000.0f;
                _mieScatteringCoeffYP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_mieScatteringCoeffYP);

                _mieScatteringCoeffZP = _mieScatteringCoeff.z * 1000.0f;
                _mieScatteringCoeffZP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_mieScatteringCoeffZP);

                _mieScatteringExtinctionPropCoefficientP = _mieScatteringCoeff.x / _mieExtinctionCoeff.x;
                _mieScatteringExtinctionPropCoefficientP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_mieScatteringExtinctionPropCoefficientP);

                _mieAsymmetricFactorGP = _miePhaseConstant;
                _mieAsymmetricFactorGP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_mieAsymmetricFactorGP);

                _sunIntensityP = _sunRadianceIntensity;
                _sunIntensityP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_sunIntensityP);

                _hdrExpositionP = _hdrConstant;
                _hdrExpositionP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_hdrExpositionP);
                
                _gammaConstantP = _gammaConstant;
                _gammaConstantP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_gammaConstantP);

                _sunFollowingCameraEnabledP = _sunFollowingCameraEnabled;
                _sunFollowingCameraEnabledP.onChange([this](){ updateAtmosphereParameters(); });
                addProperty(_sunFollowingCameraEnabledP);

                _hardShadowsEnabledP = _hardShadows;
                _hardShadowsEnabledP.onChange([this]() { updateAtmosphereParameters(); });
                if (_shadowEnabled) {
                    addProperty(_hardShadowsEnabledP);
                }                
            }
        }
    }

    void RenderableAtmosphere::initialize() {
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
                _deferredcaster->enableSunFollowing(_sunFollowingCameraEnabled);

                _deferredcaster->setPrecalculationTextureScale(_preCalculatedTexturesScale);
                if (_saveCalculationsToTexture)
                    _deferredcaster->enablePrecalculationTexturesSaving();

                if (_shadowEnabled) {
                    _deferredcaster->setShadowConfigArray(_shadowConfArray);
                    _deferredcaster->setHardShadows(_hardShadows);
                }

                _deferredcaster->initialize();
            }

            OsEng.renderEngine().deferredcasterManager().attachDeferredcaster(*_deferredcaster.get());
        }

        return;
    }

    void RenderableAtmosphere::deinitialize() {
        if (_deferredcaster) {
            OsEng.renderEngine().deferredcasterManager().detachDeferredcaster(*_deferredcaster.get());
            _deferredcaster = nullptr;
        }

        return;
    }

    bool RenderableAtmosphere::isReady() const {
        bool ready = true;
        ready &= (_deferredcaster != nullptr);
        return ready;
    }

    glm::dmat4 RenderableAtmosphere::computeModelTransformMatrix(const openspace::TransformData& transformData) {
        // scale the planet to appropriate size since the planet is a unit sphere    
        return glm::translate(glm::dmat4(1.0), transformData.translation) * // Translation
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
            glm::dmat4 modelTransform = computeModelTransformMatrix(data.modelTransform);
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
            _sunFollowingCameraEnabled != _sunFollowingCameraEnabledP ||
            _hardShadows != _hardShadowsEnabledP) {
            executeComputation = false;
        }
            

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
        _hardShadows = _hardShadowsEnabledP;


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
            _deferredcaster->enableSunFollowing(_sunFollowingCameraEnabled);
            //_deferredcaster->setEllipsoidRadii(_ellipsoid.radii());

            if (_shadowEnabled) {
                _deferredcaster->setHardShadows(_hardShadows);
            }

            if (executeComputation)
                _deferredcaster->preCalculateAtmosphereParam();
        }
    }
}  // namespace openspace
