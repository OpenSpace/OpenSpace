/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

    const char* KeyShadowGroup  = "ShadowGroup";
    const char* KeyShadowSource = "Source";
    const char* KeyShadowCaster = "Caster";

    const char* keyAtmosphere               = "Atmosphere";
    const char* keyAtmosphereRadius         = "AtmosphereRadius";
    const char* keyPlanetRadius             = "PlanetRadius";
    const char* keyAverageGroundReflectance = "PlanetAverageGroundReflectance";
    const char* keyRayleigh                 = "Rayleigh";
    const char* keyRayleighHeightScale      = "H_R";
    const char* keyOzone                    = "Ozone";
    const char* keyOxygen                   = "Oxygen";
    const char* keyOxygenHeightScale        = "H_O2";
    const char* keyMie                      = "Mie";
    const char* keyMieHeightScale           = "H_M";
    const char* keyMiePhaseFunction         = "G";
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

    constexpr openspace::properties::Property::PropertyInfo RayleighScatteringCoeffInfo =
    {
        "RayleighScatteringCoeff",
        "Rayleigh Scattering Coeff (x10e-3)",
        "Rayleigh sea-level scattering coefficients in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo OzoneLayerInfo = {
        "Ozone",
        "Ozone Layer Enabled",
        "Enables/Disable Ozone Layer during pre-calculation phase"
    };

    constexpr openspace::properties::Property::PropertyInfo OzoneLayerAbsCrossSectionInfo = {
        "OzoneLayerAbsCrossSections",
        "Ozone Layer Absorption Cross-Sections Coefficients (x10e-22)",
        "Ozone absorption cross-sections in centimeters."
    };

    constexpr openspace::properties::Property::PropertyInfo OxygenAbsInfo = {
        "Oxygen",
        "Oxygen Absorption Enabled",
        "Enables/Disable Oxygen Absorption during pre-calculation phase"
    };

    constexpr openspace::properties::Property::PropertyInfo OxygenHeightScaleInfo = {
        "OxygenLayerHeightScale",
        "Oxygen Scale Height (KM)",
        "It is the vertical distance over which the density and pressure fall by a "
        "constant factor"
    };

    constexpr openspace::properties::Property::PropertyInfo OxygenAbsCrossSectionInfo = {
        "OxygenAbsCrossSections",
        "Oxygen Molecules Absorption Cross-Sections Coefficients (x10e-22)",
        "Oxygen molecules absorption cross-sections in centimeters."
    };

    constexpr openspace::properties::Property::PropertyInfo MieHeightScaleInfo = {
        "MieHeightScale",
        "Mie Scale Height (KM)",
        "It is the vertical distance over which the density and pressure fall by a "
        "constant factor"
    };

    constexpr openspace::properties::Property::PropertyInfo MieScatteringCoeffInfo = {
        "MieScatteringCoeff",
        "Mie Scattering Coeff (x10e-3)",
        "Mie sea-level scattering coefficients in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo
    MieScatteringExtinctionPropCoeffInfo =
    {
        "MieScatteringExtinctionPropCoefficient",
        "Mie Scattering/Extinction Proportion Coefficient (%)",
        "Mie Scattering/Extinction Proportion Coefficient (%)"
    };

    constexpr openspace::properties::Property::PropertyInfo MieAbsorptionCoeffInfo = {
        "MieAbsorptionCoeff",
        "Mie Absorption Coeff (x10e-3)",
        "Mie sea-level absorption coefficients in 1/centimeters"
    };

    constexpr openspace::properties::Property::PropertyInfo MieExtinctionCoeffInfo = {
        "MieExtinctionCoeff",
        "Mie Extinction Coeff (x10e-3)",
        "Mie sea-level absorption coefficients in 1/centimeters"
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

    constexpr openspace::properties::Property::PropertyInfo AdvancedModeInfo = {
        "AdvancedMode",
        "Enable/Disable atmosphere advanced mode",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo UseOnlyAdvancedMieInfo = {
        "UseOnlyAdvancedMie",
        "Use =only= advanced Mie's parameters",
        ""
    };

    openspace::properties::PropertyOwner::PropertyOwnerInfo AdvancedModeOwnerInfo = {
        "Advanced Mode Parameters",
        "Advanced Mode Parameters",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo RayleighRealRefractIndexInfo = {
        "RayleighRealRefract",
        "Rayleigh Re{n}",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo RayleighComplexRefractIndexInfo = {
        "RayleighComplexRefract",
        "Rayleigh Im{n}",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo MieRealRefractIndexInfo = {
        "MieRealRefract",
        "Mie Re{n}",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo MieComplexRefractIndexInfo = {
        "MieComplexRefract",
        "Mie Im{n}",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo WavelenghArrayInfo = {
        "Wavelengths",
        "Light Incident Wavelenghts",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo KappaInfo = {
        "Kappa",
        "Kappa",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo PolarizabilityInfo = {
        "Polarizability",
        "Polarizability",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo NRayleighInfo = {
        "NRayleigh",
        "N Rayleigh x10^25",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo NMieInfo = {
        "NMie",
        "N Mie x10^8",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo NRayleighAbsInfo = {
        "NRayleighAbs",
        "N Rayliegh Absorption x 10^25",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo RayleighRadiusAbsParticleInfo = {
        "RayleighAbsParticleRadius",
        "Rayleigh Absorption Particle Radius x10-10",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo MieMeanRadiusParticleInfo = {
        "MieMeanParticleRadius",
        "Mie Mean Particle Radius x10^-6",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo TurbidityInfo = {
        "Turbidity",
        "Turbidity",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo JungeExponentInfo = {
        "JungeExponent",
        "Junge's Exponent",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo G1Info = {
        "G1",
        "G1",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo G2Info = {
        "G2",
        "G2",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo AlphaInfo = {
        "Alpha",
        "Alpha",
        ""
    };

    /*constexpr openspace::properties::Property::PropertyInfo Info = {
        "",
        "",
        ""
    };*/

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
    , _atmosphereHeightP(AtmosphereHeightInfo, 60.0f, 0.1f, 300.0f)
    , _groundAverageReflectanceP(AverageGroundReflectanceInfo, 0.1f, 0.0f, 1.0f)
    , _groundRadianceEmittionP(GroundRadianceEmittioninfo, 0.3f, 0.0f, 1.0f)
    , _rayleighHeightScaleP(RayleighHeightScaleInfo, 8.0f, 0.1f, 100.0f)
    , _rayleighScatteringCoeffP(
        RayleighScatteringCoeffInfo, 
        glm::vec3(1.0f), 
        glm::vec3(0.01f), 
        glm::vec3(100.0f)
    )
    , _ozoneEnabledP(OzoneLayerInfo, false)
    , _oxygenEnableP(OxygenAbsInfo, false)
    , _ozoneAbsorptionCrossSectionP(
        OzoneLayerAbsCrossSectionInfo,
        glm::vec3(1.368209f, 3.314053f, 13.60173f),
        glm::vec3(0.01f),
        glm::vec3(100.f))
    , _oxygenHeightScaleP(OxygenHeightScaleInfo, 9.0f, 0.1f, 20.0f)
    , _oxygenAbsorptionCrossSectionP(
        OxygenAbsCrossSectionInfo,
        glm::vec3(4.164f, 5.06f, 7.525f),
        glm::vec3(0.01f),
        glm::vec3(100.f)
    )
    , _mieHeightScaleP(MieHeightScaleInfo, 1.2f, 0.1f, 100.0f)
    , _mieScatteringCoeffP(
        MieScatteringCoeffInfo, 
        glm::vec3(4.0f), 
        glm::vec3(0.01f), 
        glm::vec3(100.0f)
    )
    , _mieAbsorptionCoeffP(
        MieAbsorptionCoeffInfo, 
        glm::vec3(0.0f), 
        glm::vec3(0.01f), 
        glm::vec3(100.f)
    )
    , _mieExtinctionCoeffP(
        MieExtinctionCoeffInfo,
        glm::vec3(4.f/0.9f),
        glm::vec3(0.01f),
        glm::vec3(100.f)
    )
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
    , _enableAdvancedModeP(AdvancedModeInfo, false)
    , _useOnlyAdvancedMieP(UseOnlyAdvancedMieInfo, false)
    , _nRealRayleighP(
        RayleighRealRefractIndexInfo,
        glm::vec3(1.00027598f, 1.00027783f, 1.00028276f),
        glm::vec3(0.01f),
        glm::vec3(5.f))
    , _nComplexRayleighP(
        RayleighComplexRefractIndexInfo,
        glm::vec3(0.f),
        glm::vec3(0.f),
        glm::vec3(2.f))
    , _nRealMieP(
        MieRealRefractIndexInfo,
        glm::vec3(1.33f),
        glm::vec3(0.01f),
        glm::vec3(5.f))
    , _nComplexMieP(
        MieComplexRefractIndexInfo,
        glm::vec3(0.01f),
        glm::vec3(0.f),
        glm::vec3(2.f))
    , _lambdaArrayP(
        WavelenghArrayInfo,
        glm::vec3(680.f, 550.f, 440.f),
        glm::vec3(400.f),
        glm::vec3(700.f))
    , _KappaP(
        KappaInfo,
        glm::vec3(0.0096f, 0.0092f, 0.0089f),
        glm::vec3(0.f),
        glm::vec3(1.f))
    , _g1P(
        G1Info,
        glm::vec3(0.889f, 0.889f, 0.889f),
        glm::vec3(-1.f),
        glm::vec3(1.f))
    , _g2P(
        G2Info,
        glm::vec3(0.094f, 0.094f, 0.094f),
        glm::vec3(-1.f),
        glm::vec3(1.f))
    , _alphaP(
        AlphaInfo,
        glm::vec3(0.743f, 0.743f, 0.743f),
        glm::vec3(0.f),
        glm::vec3(1.f))
    , _deltaPolarizabilityP(
        PolarizabilityInfo,
        0.0279f,
        0.01f,
        1.f)
    , _NRayleighP(
        NRayleighInfo,
        2.68731f,
        0.01f,
        1000.f)
    , _NMieP(
        NMieInfo,
        2.8f,
        0.f,
        100.f)
    , _NRayleighAbsMoleculeP(
        NRayleighAbsInfo,
        1.f,
        0.f,
        10000.f)
    , _radiusAbsMoleculeRayleighP(
        RayleighRadiusAbsParticleInfo,
        0.5f,
        0.0f,
        10.f)
    , _meanRadiusParticleMieP(
        MieMeanRadiusParticleInfo,
        1.6f,
        0.f,
        50.f)
    , _turbidityP(
        TurbidityInfo,
        2.f,
        1.f,
        10.f)
    , _jungeExponentP(
        JungeExponentInfo,
        4.f,
        2.f,
        6.f)
    , _advancedModeOwner(AdvancedModeOwnerInfo)
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
    bool success = dictionary.getValue(KeyShadowGroup, shadowDictionary);
    bool disableShadows = false;
    if (success) {
        std::vector<std::pair<std::string, double>> sourceArray;
        unsigned int sourceCounter = 1;
        while (success) {
            std::string sourceName;
            success = shadowDictionary.getValue(KeyShadowSource +
                std::to_string(sourceCounter) + ".Name", sourceName);
            if (success) {
                double sourceRadius;
                success = shadowDictionary.getValue(KeyShadowSource +
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
                success = shadowDictionary.getValue(KeyShadowCaster +
                    std::to_string(casterCounter) + ".Name", casterName);
                if (success) {
                    double casterRadius;
                    success = shadowDictionary.getValue(KeyShadowCaster +
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
        
        if (atmosphereDictionary.hasKey(SunIntensityInfo.identifier)) {
            _sunRadianceIntensity =
                atmosphereDictionary.value<float>(SunIntensityInfo.identifier);
        }
        
        if (atmosphereDictionary.hasKey(MieScatteringExtinctionPropCoeffInfo.identifier)) {
            _mieScattExtPropCoefProp = atmosphereDictionary.value<float>(
                MieScatteringExtinctionPropCoeffInfo.identifier
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

        if (atmosphereDictionary.hasKey(AdvancedModeInfo.identifier)) {
            _enableAdvancedModeP = true;
        }

        // ---- Rayleigh Scattering -----
        ghoul::Dictionary rayleighDictionary;
        success = atmosphereDictionary.getValue(keyRayleigh, rayleighDictionary);
        if (success) {
            // Not using right now.
            glm::vec3 rayleighWavelengths = glm::vec3(0.f);
            rayleighDictionary.getValue(
                "Coefficients.Wavelengths",
                rayleighWavelengths
            );
            glm::vec3 loadDataVec(0.f);
            if (!rayleighDictionary.getValue("Coefficients.Scattering", loadDataVec)) {
                errorReadingAtmosphereData = true;
                LWARNINGC(
                    identifier,
                    "No Rayleigh Scattering parameters specified for Atmosphere Effects. "
                    "Disabling atmosphere effects for this planet."
                );
            }
            else {
                // Adjusting the UI values for more readable values
                _rayleighScatteringCoeffP = loadDataVec * 1e3f;
            }

            if (rayleighDictionary.hasKey(keyRayleighHeightScale)) {
                _rayleighHeightScaleP = rayleighDictionary.value<float>(keyRayleighHeightScale);
            } 
            else {
                errorReadingAtmosphereData = true;
                LWARNINGC(
                    identifier,
                    "No Rayleigh Height Scale value specified for Atmosphere Effects. "
                    "Disabling atmosphere effects for this planet."
                );
            }

            if (rayleighDictionary.hasKey(RayleighRealRefractIndexInfo.identifier)) {
                _nRealRayleighP = rayleighDictionary.value<glm::vec3>(RayleighRealRefractIndexInfo.identifier);
            }

            if (rayleighDictionary.hasKey(RayleighComplexRefractIndexInfo.identifier)) {
                _nComplexRayleighP = rayleighDictionary.value<glm::vec3>(RayleighComplexRefractIndexInfo.identifier);
            }

            if (rayleighDictionary.hasKey(PolarizabilityInfo.identifier)) {
               _deltaPolarizabilityP = rayleighDictionary.value<float>(PolarizabilityInfo.identifier);
            }

            if (rayleighDictionary.hasKey(NRayleighInfo.identifier)) {
               _NRayleighP = rayleighDictionary.value<float>(NRayleighInfo.identifier);
            }
            
            if (rayleighDictionary.hasKey(NRayleighAbsInfo.identifier)) {
                _NRayleighAbsMoleculeP = rayleighDictionary.value<float>(NRayleighAbsInfo.identifier);
            }

            if (rayleighDictionary.hasKey(RayleighRadiusAbsParticleInfo.identifier)) {
                _radiusAbsMoleculeRayleighP = rayleighDictionary.value<float>(RayleighRadiusAbsParticleInfo.identifier);
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

        // ---- Ozone Scattering -----
        ghoul::Dictionary ozoneDictionary;
        success = atmosphereDictionary.getValue(keyOzone, ozoneDictionary);
        if (success) {
            _ozoneLayerEnabled = true;
            
            if (!ozoneDictionary.getValue(
                "Coefficients.Absorption",
                _ozoneAbsorptionCrossSectionP))
            {
                _ozoneLayerEnabled = false;
            }
        }
        else {
            _ozoneLayerEnabled = false;
        }

        // ---- Oxygen Absorption -----
        ghoul::Dictionary oxygenDictionary;
        success = atmosphereDictionary.getValue(keyOxygen, oxygenDictionary);
        if (success) {
            _oxygenAbsEnabled = true;
            if (!oxygenDictionary.getValue(
                keyOxygenHeightScale, 
                _oxygenHeightScaleP)) 
            {
                _oxygenAbsEnabled = false;
            }

            if (!oxygenDictionary.getValue(
                "Coefficients.Absorption",
                _oxygenAbsorptionCrossSectionP))
            {
                _oxygenAbsEnabled = false;
            }
        }
        else {
            _oxygenAbsEnabled = false;
        }

        // ---- Mie Scattering -----
        ghoul::Dictionary mieDictionary;
        success = atmosphereDictionary.getValue(keyMie, mieDictionary);
        if (success) {
            if (mieDictionary.hasKey(keyMieHeightScale)) {
                _mieHeightScaleP = mieDictionary.value<float>(keyMieHeightScale);
            }
            else {
                errorReadingAtmosphereData = true;
                LWARNINGC(
                    identifier,
                    "No Mie Height Scale value specified for Atmosphere Effects. "
                    "Disabling atmosphere effects for this planet."
                );
            }

            glm::vec3 loadDataVec(0.f);
            if (!mieDictionary.getValue("Coefficients.Scattering", loadDataVec)) {
                errorReadingAtmosphereData = true;
                LWARNINGC(
                    identifier,
                    "No Mie Scattering parameters specified for Atmosphere Effects. "
                    "Disabling atmosphere effects for this planet."
                );
            }
            else {
                // Adjusting the UI values for more readable values
                _mieScatteringCoeffP = loadDataVec * 1e3f;
            }

            if (!mieDictionary.getValue("Coefficients.Extinction", _mieExtinctionCoeffP)) {
                if (!mieDictionary.getValue(MieAbsorptionCoeffInfo.identifier,
                    _mieAbsorptionCoeffP)) {
                    errorReadingAtmosphereData = true;
                    LWARNINGC(
                        identifier,
                        "No Mie Extinction or Absorption parameters specified for "
                        "Atmosphere Effects. Disabling atmosphere effects for this planet."
                    );
                }
            } 
            
            if (mieDictionary.hasKey(keyMiePhaseFunction)) {
                _mieAsymmetricFactorGP = mieDictionary.value<float>(keyMiePhaseFunction);
            }
            else {
                errorReadingAtmosphereData = true;
                LWARNINGC(
                    identifier,
                    "No Mie Phase Function value specified for Atmosphere Effects. "
                    "Disabling atmosphere effects for this planet."
                );
            }

            if (mieDictionary.hasKey(MieRealRefractIndexInfo.identifier)) {
                _nRealMieP = mieDictionary.value<glm::vec3>(MieRealRefractIndexInfo.identifier);
            }

            if (mieDictionary.hasKey(MieComplexRefractIndexInfo.identifier)) {
                _nComplexMieP = mieDictionary.value<glm::vec3>(MieComplexRefractIndexInfo.identifier);
            }

            if (mieDictionary.hasKey(KappaInfo.identifier)) {
                _KappaP = mieDictionary.value<glm::vec3>(KappaInfo.identifier);
            }

            if (mieDictionary.hasKey(NMieInfo.identifier)) {
                _NMieP = mieDictionary.value<float>(NMieInfo.identifier);
            }

            if (mieDictionary.hasKey(MieMeanRadiusParticleInfo.identifier)) {
                _meanRadiusParticleMieP = mieDictionary.value<float>(MieMeanRadiusParticleInfo.identifier);
            }

            if (mieDictionary.hasKey(TurbidityInfo.identifier)) {
                _turbidityP = mieDictionary.value<float>(TurbidityInfo.identifier);
            }

            if (mieDictionary.hasKey(JungeExponentInfo.identifier)) {
                _jungeExponentP = mieDictionary.value<float>(JungeExponentInfo.identifier);
            }

            if (mieDictionary.hasKey(G1Info.identifier)) {
                _g1P = mieDictionary.value<glm::vec3>(G1Info.identifier);
            }

            if (mieDictionary.hasKey(G2Info.identifier)) {
                _g2P = mieDictionary.value<glm::vec3>(G2Info.identifier);
            }

            if (mieDictionary.hasKey(AlphaInfo.identifier)) {
                _alphaP = mieDictionary.value<glm::vec3>(AlphaInfo.identifier);
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

            // Rayleigh
            _rayleighHeightScaleP.onChange(updateAtmosphere);
            addProperty(_rayleighHeightScaleP);

            _rayleighScatteringCoeffP.onChange(updateAtmosphere);
            addProperty(_rayleighScatteringCoeffP);

            // Oxygen
            _oxygenEnableP = _oxygenAbsEnabled;
            _oxygenEnableP.onChange(updateAtmosphere);
            addProperty(_oxygenEnableP);

            _oxygenHeightScaleP.onChange(updateAtmosphere);
            addProperty(_oxygenHeightScaleP);

            _oxygenAbsorptionCrossSectionP.onChange(updateAtmosphere);
            addProperty(_oxygenAbsorptionCrossSectionP);

            // Ozone
            _ozoneEnabledP = _ozoneLayerEnabled;
            _ozoneEnabledP.onChange(updateAtmosphere);
            addProperty(_ozoneEnabledP);

            _ozoneAbsorptionCrossSectionP.onChange(updateAtmosphere);
            addProperty(_ozoneAbsorptionCrossSectionP);


            // Mie
            _mieHeightScaleP.onChange(updateAtmosphere);
            addProperty(_mieHeightScaleP);

            _mieScatteringCoeffP.onChange(updateAtmosphere);
            addProperty(_mieScatteringCoeffP);

            
            _mieAbsorptionCoeffP.onChange(updateAtmosphere);
            addProperty(_mieAbsorptionCoeffP);

            _mieScatteringExtinctionPropCoefficientP =
                glm::vec3(_mieScatteringCoeffP).x / glm::vec3(_mieExtinctionCoeffP).x;

            _mieExtinctionCoeffP = glm::vec3(_mieScatteringCoeffP) * (1e-3f /
                static_cast<float>(_mieScatteringExtinctionPropCoefficientP));

            _mieScatteringExtinctionPropCoefficientP.onChange(updateAtmosphere);
            addProperty(_mieScatteringExtinctionPropCoefficientP);

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

            _enableAdvancedModeP.onChange(updateAtmosphere);
            addProperty(_enableAdvancedModeP);

            _useOnlyAdvancedMieP.onChange(updateAtmosphere);
            _nRealRayleighP.onChange(updateAtmosphere);
            _nComplexRayleighP.onChange(updateAtmosphere);
            _nRealMieP.onChange(updateAtmosphere);
            _nComplexMieP.onChange(updateAtmosphere);
            _lambdaArrayP.onChange(updateAtmosphere);
            _KappaP.onChange(updateAtmosphere);
            _deltaPolarizabilityP.onChange(updateAtmosphere);
            _NRayleighP.onChange(updateAtmosphere);
            _NMieP.onChange(updateAtmosphere);
            _NRayleighAbsMoleculeP.onChange(updateAtmosphere);
            _radiusAbsMoleculeRayleighP.onChange(updateAtmosphere);
            _meanRadiusParticleMieP.onChange(updateAtmosphere);
            _turbidityP.onChange(updateAtmosphere);
            _jungeExponentP.onChange(updateAtmosphere);
            _g1P.onChange(updateAtmosphere);
            _g2P.onChange(updateAtmosphere);
            _alphaP.onChange(updateAtmosphere);

            _advancedModeOwner.addProperty(_useOnlyAdvancedMieP);
            _advancedModeOwner.addProperty(_nRealRayleighP);
            _advancedModeOwner.addProperty(_nComplexRayleighP);
            _advancedModeOwner.addProperty(_nRealMieP);
            _advancedModeOwner.addProperty(_nComplexMieP);
            _advancedModeOwner.addProperty(_lambdaArrayP);
            _advancedModeOwner.addProperty(_KappaP);
            _advancedModeOwner.addProperty(_deltaPolarizabilityP);
            _advancedModeOwner.addProperty(_NRayleighP);
            _advancedModeOwner.addProperty(_NMieP);
            _advancedModeOwner.addProperty(_g1P);
            _advancedModeOwner.addProperty(_g2P);
            _advancedModeOwner.addProperty(_alphaP);
            _advancedModeOwner.addProperty(_NRayleighAbsMoleculeP);
            _advancedModeOwner.addProperty(_radiusAbsMoleculeRayleighP);
            _advancedModeOwner.addProperty(_meanRadiusParticleMieP);
            _advancedModeOwner.addProperty(_turbidityP);
            _advancedModeOwner.addProperty(_jungeExponentP);
            addPropertySubOwner(_advancedModeOwner);
            
            updateAtmosphereParameters();
        }
    }
}

void RenderableAtmosphere::deinitializeGL() {
    if (_deferredcaster) {
        global::deferredcasterManager.detachDeferredcaster(*_deferredcaster);
        _deferredcaster = nullptr;
    }
}

void RenderableAtmosphere::initializeGL() {
    if (_atmosphereEnabled) {
        _deferredcaster = std::make_unique<AtmosphereDeferredcaster>();
        if (_deferredcaster) {
            setDeferredCasterParameters(false);
            _deferredcaster->initialize();
        }

        global::deferredcasterManager.attachDeferredcaster(*_deferredcaster);
    }

    return;
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

    _planetAverageGroundReflectance = _groundAverageReflectanceP;
    _planetGroundRadianceEmittion   = _groundRadianceEmittionP;
    _hardShadows                    = _hardShadowsEnabledP;
    _sunRadianceIntensity           = _sunIntensityP;

    glm::vec3 mieA(_mieAbsorptionCoeffP);
    if (mieA.x != 0.0f ||
        mieA.y != 0.0f ||
        mieA.z != 0.0f) {
        _mieExtinctionCoeffP = (mieA + glm::vec3(_mieScatteringCoeffP)) * 1e-3f;
    }
    else {
        _mieExtinctionCoeffP = glm::vec3(_mieScatteringCoeffP) * (1e-3f /
            static_cast<float>(_mieScatteringExtinctionPropCoefficientP));
    }

    if (_deferredcaster) {
        setDeferredCasterParameters(executeComputation);
    }
}

void RenderableAtmosphere::setDeferredCasterParameters(const bool executePreCalculations) {
    _deferredcaster->setAtmosphereRadius(_atmospherePlanetRadius + _atmosphereHeightP);
    _deferredcaster->setPlanetRadius(_atmospherePlanetRadius);
    _deferredcaster->setPlanetAverageGroundReflectance(
        _groundAverageReflectanceP
    );
    _deferredcaster->setPlanetGroundRadianceEmittion(
        _groundRadianceEmittionP
    );
    _deferredcaster->setRayleighHeightScale(_rayleighHeightScaleP);

    _deferredcaster->enableOxygen(_oxygenEnableP);
    _deferredcaster->setOxygenHeightScale(_oxygenHeightScaleP);

    _deferredcaster->enableOzone(_ozoneEnabledP);
    _deferredcaster->setOzoneAbsCrossSections(_ozoneAbsorptionCrossSectionP);

    _deferredcaster->setMieHeightScale(_mieHeightScaleP);
    _deferredcaster->setMiePhaseConstant(_mieAsymmetricFactorGP);
    _deferredcaster->setSunRadianceIntensity(_sunIntensityP);
    _deferredcaster->setRayleighScatteringCoefficients(glm::vec3(_rayleighScatteringCoeffP) * 1e-3f);

    _deferredcaster->setMieScatteringCoefficients(glm::vec3(_mieScatteringCoeffP) * 1e-3f);
    _deferredcaster->setMieExtinctionCoefficients(_mieExtinctionCoeffP);
    _deferredcaster->setMieAbsorptionCoefficients(glm::vec3(_mieAbsorptionCoeffP) * 1e-3f);
    _deferredcaster->enableSunFollowing(_sunFollowingCameraEnabledP);
    //_deferredcaster->setEllipsoidRadii(_ellipsoid.radii());

    _deferredcaster->setPrecalculationTextureScale(_preCalculatedTexturesScale);
    
    if (_saveCalculationsToTexture) {
        _deferredcaster->enablePrecalculationTexturesSaving();
    }

    if (_shadowEnabled) {
        _deferredcaster->setShadowConfigArray(_shadowConfArray);
        _deferredcaster->setHardShadows(_hardShadows);
    }

    _deferredcaster->enableAdvancedMode(_enableAdvancedModeP);

    AtmosphereDeferredcaster::AdvancedATMModeData advModeData;
    advModeData.useOnlyAdvancedMie        = _useOnlyAdvancedMieP;
    advModeData.deltaPolarizability       = _deltaPolarizabilityP;
    advModeData.jungeExponent             = _jungeExponentP;
    advModeData.Kappa                     = _KappaP;
    advModeData.lambdaArray               = glm::vec3(_lambdaArrayP) * 1e-9f;
    advModeData.meanRadiusParticleMie     = _meanRadiusParticleMieP * 1e-6f;
    advModeData.nComplexMie               = _nComplexMieP;
    advModeData.nComplexRayleigh          = _nComplexRayleighP;
    advModeData.nRealMie                  = _nRealMieP;
    advModeData.nRealRayleigh             = _nRealRayleighP;
    advModeData.NMie                      = _NMieP * 1e8f;
    advModeData.NRayleigh                 = _NRayleighP * 1e25f;
    advModeData.NRayleighAbsMolecule      = _NRayleighAbsMoleculeP * 1e16f;
    advModeData.radiusAbsMoleculeRayleigh = _radiusAbsMoleculeRayleighP * 1e-10f;
    advModeData.turbidity                 = _turbidityP;
    advModeData.g1                        = _g1P;
    advModeData.g2                        = _g2P;
    advModeData.alpha                     = _alphaP;
    

    _deferredcaster->setAdvancedModeParameters(advModeData);

    if (executePreCalculations) {
        _deferredcaster->preCalculateAtmosphereParam();
    }
}

}  // namespace openspace
