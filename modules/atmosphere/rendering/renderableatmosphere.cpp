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

#include <modules/atmosphere/rendering/renderableatmosphere.h>

#include <modules/atmosphere/rendering/atmospheredeferredcaster.h>
#include <modules/space/rendering/planetgeometry.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/deferredcastermanager.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderer.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/invariants.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtx/string_cast.hpp>
#include <fstream>
#include <memory>

#ifdef WIN32
#define _USE_MATH_DEFINES
#endif // WIN32
#include <math.h>

namespace {
    static const char* _loggerCat = "RenderableAtmosphere";

    const char* keyShadowGroup  = "ShadowGroup";
    const char* keyShadowSource = "Source";
    const char* keyShadowCaster = "Caster";

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
    const char* keyATMDebug                 = "Debug";
    const char* keyTextureScale             = "PreCalculatedTextureScale";
    const char* keySaveTextures             = "SaveCalculatedTextures";

    constexpr openspace::properties::Property::PropertyInfo AtmosphereHeightInfo = {
        "atmmosphereHeight",
        "Atmosphere Height (KM)",
        "The thickness of the atmosphere in Km"
    };

    constexpr openspace::properties::Property::PropertyInfo AverageGroundReflectanceInfo =
    {
        "AverageGroundReflectance",
        "Average Ground Reflectance (%)",
        "Average percentage of light reflected by the ground during the pre-calculation "
        "phase"
    };

    constexpr openspace::properties::Property::PropertyInfo GroundRadianceEmittioninfo = {
        "GroundRadianceEmittion",
        "Percentage of initial radiance emitted from ground",
        "Multiplier of the ground radiance color during the rendering phase"
    };

    constexpr openspace::properties::Property::PropertyInfo RayleighHeightScaleInfo = {
        "RayleighHeightScale",
        "Rayleigh Scale Height (KM)",
        "It is the vertical distance over which the density and pressure fall by a "
        "constant factor"
    };

    constexpr openspace::properties::Property::PropertyInfo RayleighScatteringCoeffXInfo =
    {
        "RayleighScatteringCoeffX",
        "Rayleigh Scattering Coeff X (x10e-3)",
        "Rayleigh sea-level scattering coefficients in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo RayleighScatteringCoeffYInfo =
    {
        "RayleighScatteringCoeffY",
        "Rayleigh Scattering Coeff Y (x10e-3)",
        "Rayleigh sea-level scattering coefficients in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo RayleighScatteringCoeffZInfo =
    {
        "RayleighScatteringCoeffZ",
        "Rayleigh Scattering Coeff Z (x10e-3)",
        "Rayleigh sea-level scattering coefficients in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo OzoneLayerInfo = {
        "Ozone",
        "Ozone Layer Enabled",
        "Enables/Disable Ozone Layer during pre-calculation phase"
    };

    constexpr openspace::properties::Property::PropertyInfo OzoneHeightScaleInfo = {
        "OzoneLayerHeightScale",
        "Ozone Scale Height (KM)",
        "It is the vertical distance over which the density and pressure fall by a "
        "constant factor"
    };

    constexpr openspace::properties::Property::PropertyInfo OzoneLayerCoeffXInfo = {
        "OzoneLayerCoeffX",
        "Ozone Layer Extinction Coeff X (x10e-5)",
        "Ozone scattering coefficients in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo OzoneLayerCoeffYInfo = {
        "OzoneLayerCoeffY",
        "Ozone Layer Extinction Coeff Y (x10e-5)",
        "Ozone scattering coefficients in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo OzoneLayerCoeffZInfo = {
        "OzoneLayerCoeffZ",
        "Ozone Layer Extinction Coeff Z (x10e-5)",
        "Ozone scattering coefficients in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo MieHeightScaleInfo = {
        "MieHeightScale",
        "Mie Scale Height (KM)",
        "It is the vertical distance over which the density and pressure fall by a "
        "constant factor"
    };

    constexpr openspace::properties::Property::PropertyInfo MieScatteringCoeffXInfo = {
        "MieScatteringCoeffX",
        "Mie Scattering Coeff X (x10e-3)",
        "Mie sea-level scattering coefficients in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo MieScatteringCoeffYInfo = {
        "MieScatteringCoeffY",
        "Mie Scattering Coeff Y (x10e-3)",
        "Mie sea-level scattering coefficients in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo MieScatteringCoeffZInfo = {
        "MieScatteringCoeffZ",
        "Mie Scattering Coeff Z (x10e-3)",
        "Mie sea-level scattering coefficients in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo
    MieScatteringExtinctionPropCoeffInfo =
    {
        "MieScatteringExtinctionPropCoefficient",
        "Mie Scattering/Extinction Proportion Coefficient (%)",
        "Mie Scattering/Extinction Proportion Coefficient (%)"
    };

    constexpr openspace::properties::Property::PropertyInfo MieAsymmetricFactorGInfo = {
        "MieAsymmetricFactorG",
        "Mie Asymmetric Factor G",
        "Averaging of the scattering angle over a high number of scattering events"
    };

    constexpr openspace::properties::Property::PropertyInfo SunIntensityInfo = {
        "SunIntensity",
        "Sun Intensity",
        "Unitless for now"
    };

    constexpr openspace::properties::Property::PropertyInfo AtmosphereExposureInfo = {
        "HdrExposure",
        "Atmosphere Exposure",
        "Constant to controls the exposure of the radiance range"
    };

    constexpr openspace::properties::Property::PropertyInfo AtmosphereGammaInfo = {
        "Gamma",
        "Gamma Correction",
        "Gamma Correction"
    };

    constexpr openspace::properties::Property::PropertyInfo
        EnableSunOnCameraPositionInfo =
    {
        "SunFollowingCamera",
        "Enable Sun On Camera Position",
        "When selected the Sun is artificially positioned behind the observer all times"
    };

    constexpr openspace::properties::Property::PropertyInfo EclipseHardShadowsInfo = {
        "EclipseHardShadowsInfo",
        "Enable Hard Shadows for Eclipses",
        "Enable/Disables hard shadows through the atmosphere"
    };
} // namespace

namespace openspace {

documentation::Documentation RenderableAtmosphere::Documentation() {
    using namespace documentation;
    return {
        "RenderableAtmosphere",
        "atmosphere_renderable_atmosphere",
        {}
    };
}

RenderableAtmosphere::RenderableAtmosphere(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _atmosphereHeightP(AtmosphereHeightInfo, 60.0f, 0.1f, 99.0f)
    , _groundAverageReflectanceP(AverageGroundReflectanceInfo, 0.1f, 0.0f, 1.0f)
    , _groundRadianceEmittionP(GroundRadianceEmittioninfo, 0.3f, 0.0f, 1.0f)
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
    , _mieScatteringExtinctionPropCoefficientP(
        MieScatteringExtinctionPropCoeffInfo,
        0.9f,
        0.01f,
        1.0f
    )
    , _mieAsymmetricFactorGP(MieAsymmetricFactorGInfo, 0.85f, -1.0f, 1.0f)
    , _sunIntensityP(SunIntensityInfo, 50.0f, 0.1f, 1000.0f)
    , _sunFollowingCameraEnabledP(EnableSunOnCameraPositionInfo, false)
    , _hardShadowsEnabledP(EclipseHardShadowsInfo, false)
    , _atmosphereEnabled(false)
    , _ozoneLayerEnabled(false)
    , _sunFollowingCameraEnabled(false)
    , _atmosphereRadius(0.f)
    , _atmospherePlanetRadius(0.f)
    , _planetAverageGroundReflectance(0.f)
    , _planetGroundRadianceEmittion(0.f)
    , _rayleighHeightScale(0.f)
    , _ozoneHeightScale(0.f)
    , _mieHeightScale(0.f)
    , _miePhaseConstant(0.f)
    , _sunRadianceIntensity(50.f)
    , _mieExtinctionCoeff(glm::vec3(0.f))
    , _rayleighScatteringCoeff(glm::vec3(0.f))
    , _ozoneExtinctionCoeff(glm::vec3(0.f))
    , _mieScatteringCoeff(glm::vec3(0.f))
    , _saveCalculationsToTexture(false)
    , _preCalculatedTexturesScale(1.0)
    , _shadowEnabled(false)
    , _hardShadows(false)
 {
    ghoul_precondition(
        dictionary.hasKeyAndValue<std::string>(SceneGraphNode::KeyIdentifier),
        "RenderableAtmosphere needs the identifier to be specified"
    );

    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableAtmosphere"
    );

    const std::string identifier = dictionary.value<std::string>(
        SceneGraphNode::KeyIdentifier
    );
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
                    LWARNING(fmt::format(
                        "No Radius value expecified for Shadow Source Name '{}' from "
                        "'{}' planet. Disabling shadows for this planet.",
                        sourceName,
                        identifier
                    ));
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
                        LWARNING(fmt::format(
                            "No Radius value expecified for Shadow Caster Name '{}' from "
                            "'{}' planet. Disabling shadows for this planet.",
                            casterName,
                            identifier
                        ));
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
            LWARNINGC(
                identifier,
                "No Atmosphere Radius value specified for Atmosphere Effects. "
                "Disabling atmosphere effects for this planet."
            );
        }

        if (!atmosphereDictionary.getValue(keyPlanetRadius, _atmospherePlanetRadius)) {
            errorReadingAtmosphereData = true;
            LWARNINGC(
                identifier,
                "No Planet Radius value expecified for Atmosphere Effects. "
                "Disabling atmosphere effects for this planet."
            );
        }

        if (!atmosphereDictionary.getValue(
                keyAverageGroundReflectance,
                _planetAverageGroundReflectance))
        {
            errorReadingAtmosphereData = true;
            LWARNINGC(
                identifier,
                "No Average Atmosphere Ground Reflectance value specified for "
                "Atmosphere Effects. Disabling atmosphere effects for this planet."
            );
        }

        if (!atmosphereDictionary.getValue(
                GroundRadianceEmittioninfo.identifier,
                _planetGroundRadianceEmittion))
        {
            errorReadingAtmosphereData = true;
            LWARNINGC(
                identifier,
                "No Ground Radiance Emitted percentage value specified for Atmosphere "
                "Effects. Disabling atmosphere effects for this planet."
            );
        }

        ghoul::Dictionary rayleighDictionary;
        success = atmosphereDictionary.getValue(keyRayleigh, rayleighDictionary);

        if (success) {
            // Not using right now.
            glm::vec3 rayleighWavelengths;
            rayleighDictionary.getValue(
                "Coefficients.Wavelengths",
                rayleighWavelengths
            );

            if (!rayleighDictionary.getValue(
                "Coefficients.Scattering",
                _rayleighScatteringCoeff))
            {
                errorReadingAtmosphereData = true;
                LWARNINGC(
                    identifier,
                    "No Rayleigh Scattering parameters specified for Atmosphere Effects. "
                    "Disabling atmosphere effects for this planet."
                );
            }

            if (!rayleighDictionary.getValue(
                    keyRayleighHeightScale,
                    _rayleighHeightScale))
            {
                errorReadingAtmosphereData = true;
                LWARNINGC(
                    identifier,
                    "No Rayleigh Height Scale value specified for Atmosphere Effects. "
                    "Disabling atmosphere effects for this planet."
                );
            }
        }
        else {
            errorReadingAtmosphereData = true;
            LWARNINGC(
                identifier,
                "No Rayleigh parameters specified for Atmosphere Effects. "
                "Disabling atmosphere effects for this planet."
            );
        }

        ghoul::Dictionary ozoneDictionary;
        success = atmosphereDictionary.getValue(keyOzone, ozoneDictionary);
        if (success) {
            _ozoneLayerEnabled = true;
            if (!ozoneDictionary.getValue(keyOzoneHeightScale, _ozoneHeightScale)) {
                _ozoneLayerEnabled = false;
            }

            if (!ozoneDictionary.getValue(
                "Coefficients.Extinction",
                _ozoneExtinctionCoeff))
            {
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
                LWARNINGC(
                    identifier,
                    "No Mie Height Scale value specified for Atmosphere Effects. "
                    "Disabling atmosphere effects for this planet."
                );
            }

            if (!mieDictionary.getValue("Coefficients.Scattering", _mieScatteringCoeff)) {
                errorReadingAtmosphereData = true;
                LWARNINGC(
                    identifier,
                    "No Mie Scattering parameters specified for Atmosphere Effects. "
                    "Disabling atmosphere effects for this planet."
                );
            }

            if (!mieDictionary.getValue("Coefficients.Extinction", _mieExtinctionCoeff)) {
                errorReadingAtmosphereData = true;
                LWARNINGC(
                    identifier,
                    "No Mie Extinction parameters specified for Atmosphere Effects. "
                    "Disabling atmosphere effects for this planet."
                );
            }

            if (!mieDictionary.getValue(keyMiePhaseConstant, _miePhaseConstant)) {
                errorReadingAtmosphereData = true;
                LWARNINGC(
                    identifier,
                    "No Mie Phase Constant value specified for Atmosphere Effects. "
                    "Disabling atmosphere effects for this planet."
                );
            }
        }
        else {
            errorReadingAtmosphereData = true;
            LWARNINGC(
                identifier,
                "No Mie parameters specified for Atmosphere Effects. "
                "Disabling atmosphere effects for this planet."
            );
        }

        ghoul::Dictionary ImageDictionary;
        success = atmosphereDictionary.getValue(keyImage, ImageDictionary);
        if (success) {
            if (ImageDictionary.getValue(keyToneMappingOp, _preCalculatedTexturesScale)) {
                LDEBUG(fmt::format(
                    "Atmosphere Texture Scaled to {}",
                    _preCalculatedTexturesScale
                ));
            }
        }

        ghoul::Dictionary debugDictionary;
        success = atmosphereDictionary.getValue(keyATMDebug, debugDictionary);
        if (success) {
            if (debugDictionary.getValue(keyTextureScale, _preCalculatedTexturesScale)) {
                LDEBUG(fmt::format(
                    "Atmosphere Texture Scaled to {}",
                    _preCalculatedTexturesScale
                ));
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

            auto updateAtmosphere = [this]() { updateAtmosphereParameters(); };

            _atmosphereHeightP =_atmosphereRadius - _atmospherePlanetRadius;
            _atmosphereHeightP.onChange(updateAtmosphere);
            addProperty(_atmosphereHeightP);

            _groundAverageReflectanceP = _planetAverageGroundReflectance;
            _groundAverageReflectanceP.onChange(updateAtmosphere);
            addProperty(_groundAverageReflectanceP);

            _groundRadianceEmittionP = _planetGroundRadianceEmittion;
            _groundRadianceEmittionP.onChange(updateAtmosphere);
            addProperty(_groundRadianceEmittionP);

            _rayleighHeightScaleP = _rayleighHeightScale;
            _rayleighHeightScaleP.onChange(updateAtmosphere);
            addProperty(_rayleighHeightScaleP);

            _rayleighScatteringCoeffXP = _rayleighScatteringCoeff.x * 1000.0f;
            _rayleighScatteringCoeffXP.onChange(updateAtmosphere);
            addProperty(_rayleighScatteringCoeffXP);

            _rayleighScatteringCoeffYP = _rayleighScatteringCoeff.y * 1000.0f;
            _rayleighScatteringCoeffYP.onChange(updateAtmosphere);
            addProperty(_rayleighScatteringCoeffYP);

            _rayleighScatteringCoeffZP = _rayleighScatteringCoeff.z * 1000.0f;
            _rayleighScatteringCoeffZP.onChange(updateAtmosphere);
            addProperty(_rayleighScatteringCoeffZP);

            _ozoneEnabledP = _ozoneLayerEnabled;
            _ozoneEnabledP.onChange(updateAtmosphere);
            addProperty(_ozoneEnabledP);

            _ozoneHeightScaleP = _ozoneHeightScale;
            _ozoneHeightScaleP.onChange(updateAtmosphere);
            addProperty(_ozoneHeightScaleP);

            _ozoneCoeffXP = _ozoneExtinctionCoeff.x * 100000.0f;
            _ozoneCoeffXP.onChange(updateAtmosphere);
            addProperty(_ozoneCoeffXP);

            _ozoneCoeffYP = _ozoneExtinctionCoeff.y * 100000.0f;
            _ozoneCoeffYP.onChange(updateAtmosphere);
            addProperty(_ozoneCoeffYP);


            _ozoneCoeffZP = _ozoneExtinctionCoeff.z * 100000.0f;
            _ozoneCoeffZP.onChange(updateAtmosphere);
            addProperty(_ozoneCoeffZP);

            _mieHeightScaleP = _mieHeightScale;
            _mieHeightScaleP.onChange(updateAtmosphere);
            addProperty(_mieHeightScaleP);

            _mieScatteringCoeffXP = _mieScatteringCoeff.x * 1000.0f;
            _mieScatteringCoeffXP.onChange(updateAtmosphere);
            addProperty(_mieScatteringCoeffXP);

            _mieScatteringCoeffYP = _mieScatteringCoeff.y * 1000.0f;
            _mieScatteringCoeffYP.onChange(updateAtmosphere);
            addProperty(_mieScatteringCoeffYP);

            _mieScatteringCoeffZP = _mieScatteringCoeff.z * 1000.0f;
            _mieScatteringCoeffZP.onChange(updateAtmosphere);
            addProperty(_mieScatteringCoeffZP);

            _mieScatteringExtinctionPropCoefficientP =
                _mieScatteringCoeff.x / _mieExtinctionCoeff.x;
            _mieScatteringExtinctionPropCoefficientP.onChange(updateAtmosphere);
            addProperty(_mieScatteringExtinctionPropCoefficientP);

            _mieAsymmetricFactorGP = _miePhaseConstant;
            _mieAsymmetricFactorGP.onChange(updateAtmosphere);
            addProperty(_mieAsymmetricFactorGP);

            _sunIntensityP = _sunRadianceIntensity;
            _sunIntensityP.onChange(updateAtmosphere);
            addProperty(_sunIntensityP);

            _sunFollowingCameraEnabledP = _sunFollowingCameraEnabled;
            _sunFollowingCameraEnabledP.onChange(updateAtmosphere);
            addProperty(_sunFollowingCameraEnabledP);

            _hardShadowsEnabledP = _hardShadows;
            _hardShadowsEnabledP.onChange(updateAtmosphere);
            if (_shadowEnabled) {
                addProperty(_hardShadowsEnabledP);
            }
        }
    }
}

void RenderableAtmosphere::deinitialize() {
    if (_deferredcaster) {
        global::deferredcasterManager.detachDeferredcaster(*_deferredcaster);
        _deferredcaster = nullptr;
    }
}

void RenderableAtmosphere::initializeGL() {
    if (_atmosphereEnabled) {
        _deferredcaster = std::make_unique<AtmosphereDeferredcaster>();
        if (_deferredcaster) {
            _deferredcaster->setAtmosphereRadius(_atmosphereRadius);
            _deferredcaster->setPlanetRadius(_atmospherePlanetRadius);
            _deferredcaster->setPlanetAverageGroundReflectance(
                _planetAverageGroundReflectance
            );
            _deferredcaster->setPlanetGroundRadianceEmittion(
                _planetGroundRadianceEmittion
            );
            _deferredcaster->setRayleighHeightScale(_rayleighHeightScale);
            _deferredcaster->enableOzone(_ozoneLayerEnabled);
            _deferredcaster->setOzoneHeightScale(_ozoneHeightScale);
            _deferredcaster->setMieHeightScale(_mieHeightScale);
            _deferredcaster->setMiePhaseConstant(_miePhaseConstant);
            _deferredcaster->setSunRadianceIntensity(_sunRadianceIntensity);
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

        global::deferredcasterManager.attachDeferredcaster(*_deferredcaster);
    }

    return;
}

void RenderableAtmosphere::deinitializeGL() {
}

bool RenderableAtmosphere::isReady() const {
    bool ready = true;
    ready &= (_deferredcaster != nullptr);
    return ready;
}

glm::dmat4 RenderableAtmosphere::computeModelTransformMatrix(
                                            const openspace::TransformData& transformData)
{
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

    if (_deferredcaster) {
        _deferredcaster->setTime(data.time.j2000Seconds());
        glm::dmat4 modelTransform = computeModelTransformMatrix(data.modelTransform);
        _deferredcaster->setModelTransform(modelTransform);
        _deferredcaster->update(data);
    }
}

void RenderableAtmosphere::updateAtmosphereParameters() {
    bool executeComputation = true;

    if (_sunRadianceIntensity != _sunIntensityP ||
        _planetGroundRadianceEmittion != _groundRadianceEmittionP ||
        _sunFollowingCameraEnabled != _sunFollowingCameraEnabledP ||
        _hardShadows != _hardShadowsEnabledP) {
        executeComputation = false;
    }

    _atmosphereRadius               = _atmospherePlanetRadius + _atmosphereHeightP;
    _planetAverageGroundReflectance = _groundAverageReflectanceP;
    _planetGroundRadianceEmittion   = _groundRadianceEmittionP;
    _rayleighHeightScale            = _rayleighHeightScaleP;
    _rayleighScatteringCoeff = glm::vec3(
        _rayleighScatteringCoeffXP * 0.001f,
        _rayleighScatteringCoeffYP * 0.001f,
        _rayleighScatteringCoeffZP * 0.001f
    );
    _ozoneLayerEnabled    = _ozoneEnabledP;
    _ozoneHeightScale     = _ozoneHeightScaleP;
    _ozoneExtinctionCoeff = glm::vec3(_ozoneCoeffXP.value() * 0.00001f,
        _ozoneCoeffYP.value() * 0.00001f,
        _ozoneCoeffZP.value() * 0.00001f);
    _mieHeightScale     = _mieHeightScaleP;
    _mieScatteringCoeff = glm::vec3(
        _mieScatteringCoeffXP * 0.001f,
        _mieScatteringCoeffYP * 0.001f,
        _mieScatteringCoeffZP * 0.001f
    );
    _mieExtinctionCoeff         = _mieScatteringCoeff * (1.0f /
                            static_cast<float>(_mieScatteringExtinctionPropCoefficientP));
    _miePhaseConstant           = _mieAsymmetricFactorGP;
    _sunRadianceIntensity       = _sunIntensityP;
    _sunFollowingCameraEnabled  = _sunFollowingCameraEnabledP;
    _hardShadows                = _hardShadowsEnabledP;


    if (_deferredcaster) {
        _deferredcaster->setAtmosphereRadius(_atmosphereRadius);
        _deferredcaster->setPlanetRadius(_atmospherePlanetRadius);
        _deferredcaster->setPlanetAverageGroundReflectance(
            _planetAverageGroundReflectance
        );
        _deferredcaster->setPlanetGroundRadianceEmittion(_planetGroundRadianceEmittion);
        _deferredcaster->setRayleighHeightScale(_rayleighHeightScale);
        _deferredcaster->enableOzone(_ozoneLayerEnabled);
        _deferredcaster->setOzoneHeightScale(_ozoneHeightScale);
        _deferredcaster->setMieHeightScale(_mieHeightScale);
        _deferredcaster->setMiePhaseConstant(_miePhaseConstant);
        _deferredcaster->setSunRadianceIntensity(_sunRadianceIntensity);
        _deferredcaster->setRayleighScatteringCoefficients(_rayleighScatteringCoeff);
        _deferredcaster->setOzoneExtinctionCoefficients(_ozoneExtinctionCoeff);
        _deferredcaster->setMieScatteringCoefficients(_mieScatteringCoeff);
        _deferredcaster->setMieExtinctionCoefficients(_mieExtinctionCoeff);
        _deferredcaster->enableSunFollowing(_sunFollowingCameraEnabled);
        //_deferredcaster->setEllipsoidRadii(_ellipsoid.radii());

        if (_shadowEnabled) {
            _deferredcaster->setHardShadows(_hardShadows);
        }

        if (executeComputation) {
            _deferredcaster->preCalculateAtmosphereParam();
        }
    }
}

}  // namespace openspace
