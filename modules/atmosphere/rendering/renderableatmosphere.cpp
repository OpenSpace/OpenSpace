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

    constexpr openspace::properties::Property::PropertyInfo MieAbsorptionCoeffInfo = {
        "MieAbsorptionCoeff",
        "Mie Absorption Coeff (x10e-3)",
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

    constexpr openspace::properties::Property::PropertyInfo AdvancedModelInfo = {
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
        "N Mie x10^10",
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
    , _rayleighScatteringCoeffXP(RayleighScatteringCoeffXInfo, 1.0f, 0.01f, 100.0f)
    , _rayleighScatteringCoeffYP(RayleighScatteringCoeffYInfo, 1.0f, 0.01f, 100.0f)
    , _rayleighScatteringCoeffZP(RayleighScatteringCoeffZInfo, 1.0f, 0.01f, 100.0f)
    , _ozoneEnabledP(OzoneLayerInfo, false)
    , _oxygenEnableP(OxygenAbsInfo, false)
    , _ozoneAbsorptionCrossSectionP(
        OzoneLayerAbsCrossSectionInfo,
        glm::vec3(1.368209f, 3.314053f, 13.60173f),
        glm::vec3(0.01f),
        glm::vec3(100.f))
    , _oxygenHeightScaleP(OxygenHeightScaleInfo, 8.0f, 0.1f, 20.0f)
    , _oxygenAbsorptionCrossSectionP(
        OxygenAbsCrossSectionInfo,
        glm::vec3(4.164f, 5.06f, 7.525f),
        glm::vec3(0.01f),
        glm::vec3(100.f)
    )
    , _mieHeightScaleP(MieHeightScaleInfo, 1.2f, 0.1f, 100.0f)
    , _mieScatteringCoeffXP(MieScatteringCoeffXInfo, 4.0f, 0.01f, 100.0f)
    , _mieScatteringCoeffYP(MieScatteringCoeffYInfo, 4.0f, 0.01f, 100.0f)
    , _mieScatteringCoeffZP(MieScatteringCoeffZInfo, 4.0f, 0.01f, 100.0f)
    , _mieAbsorptionCoeffP(
        MieAbsorptionCoeffInfo, 
        glm::vec3(0.0f), 
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
    , _enableAdvancedModeP(AdvancedModelInfo, false)
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
        100.f)
    , _NMieP(
        NMieInfo,
        2.8f,
        0.f,
        100.f)
    , _NRayleighAbsMoleculeP(
        NRayleighAbsInfo,
        1.f,
        0.f,
        1000.f)
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

        ghoul::Dictionary rayleighDictionary;
        success = atmosphereDictionary.getValue(keyRayleigh, rayleighDictionary);

        if (success) {
            // Not using right now.
            glm::vec3 rayleighWavelengths = glm::vec3(0.f);
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
                _rayleighHeightScale)
            )
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

            // Rayleigh
            _rayleighHeightScaleP = _rayleighHeightScale;
            _rayleighHeightScaleP.onChange(updateAtmosphere);
            addProperty(_rayleighHeightScaleP);

            // We multiply the scattering coefficients by 1000.0 to have a good
            // display value in the GUI.
            _rayleighScatteringCoeffXP = _rayleighScatteringCoeff.x * 1000.0f;
            _rayleighScatteringCoeffXP.onChange(updateAtmosphere);
            addProperty(_rayleighScatteringCoeffXP);

            _rayleighScatteringCoeffYP = _rayleighScatteringCoeff.y * 1000.0f;
            _rayleighScatteringCoeffYP.onChange(updateAtmosphere);
            addProperty(_rayleighScatteringCoeffYP);

            _rayleighScatteringCoeffZP = _rayleighScatteringCoeff.z * 1000.0f;
            _rayleighScatteringCoeffZP.onChange(updateAtmosphere);
            addProperty(_rayleighScatteringCoeffZP);

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
            _mieHeightScaleP = _mieHeightScale;
            _mieHeightScaleP.onChange(updateAtmosphere);
            addProperty(_mieHeightScaleP);

            // We multiply the scattering coefficients by 1000.0 to have a good
            // display value in the GUI.
            _mieScatteringCoeffXP = _mieScatteringCoeff.x * 1000.0f;
            _mieScatteringCoeffXP.onChange(updateAtmosphere);
            addProperty(_mieScatteringCoeffXP);

            _mieScatteringCoeffYP = _mieScatteringCoeff.y * 1000.0f;
            _mieScatteringCoeffYP.onChange(updateAtmosphere);
            addProperty(_mieScatteringCoeffYP);

            _mieScatteringCoeffZP = _mieScatteringCoeff.z * 1000.0f;
            _mieScatteringCoeffZP.onChange(updateAtmosphere);
            addProperty(_mieScatteringCoeffZP);

            _mieAbsorptionCoeffP.onChange(updateAtmosphere);
            addProperty(_mieAbsorptionCoeffP);

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

    _atmosphereRadius               = _atmospherePlanetRadius + _atmosphereHeightP;
    _planetAverageGroundReflectance = _groundAverageReflectanceP;
    _planetGroundRadianceEmittion   = _groundRadianceEmittionP;
    
    // Rayleigh
    _rayleighHeightScale     = _rayleighHeightScaleP;
    _rayleighScatteringCoeff = glm::vec3(
        _rayleighScatteringCoeffXP,
        _rayleighScatteringCoeffYP,
        _rayleighScatteringCoeffZP
    ) * glm::vec3(1e-3f);

    // Oxygen
    _oxygenAbsEnabled  = _oxygenEnableP;
    _oxygenHeightScale = _oxygenHeightScaleP;

    // Ozone 
    _ozoneLayerEnabled = _ozoneEnabledP;
    
    // Mie
    _mieHeightScale     = _mieHeightScaleP;
    _mieScatteringCoeff = glm::vec3(
        _mieScatteringCoeffXP,
        _mieScatteringCoeffYP,
        _mieScatteringCoeffZP
    ) * glm::vec3(1e-3f);

    glm::vec3 mieA = _mieAbsorptionCoeffP;
    if (mieA.x != 0.0f ||
        mieA.y != 0.0f ||
        mieA.z != 0.0f) {
        _mieScatteringCoeff = mieA * 0.001f + _mieScatteringCoeff;
    }
    else {
        _mieExtinctionCoeff = _mieScatteringCoeff * (1.0f /
            static_cast<float>(_mieScatteringExtinctionPropCoefficientP));
    }

    _miePhaseConstant           = _mieAsymmetricFactorGP;
    _sunRadianceIntensity       = _sunIntensityP;
    _sunFollowingCameraEnabled  = _sunFollowingCameraEnabledP;
    _hardShadows                = _hardShadowsEnabledP;

    if (_enableAdvancedMode != _enableAdvancedModeP) {
        executeComputation = true;
        _enableAdvancedMode = _enableAdvancedModeP;
    }

    if (_deferredcaster) {
        setDeferredCasterParameters(executeComputation);
    }
}

void RenderableAtmosphere::setDeferredCasterParameters(const bool executePreCalculations) {
    _deferredcaster->setAtmosphereRadius(_atmosphereRadius);
    _deferredcaster->setPlanetRadius(_atmospherePlanetRadius);
    _deferredcaster->setPlanetAverageGroundReflectance(
        _planetAverageGroundReflectance
    );
    _deferredcaster->setPlanetGroundRadianceEmittion(
        _planetGroundRadianceEmittion
    );
    _deferredcaster->setRayleighHeightScale(_rayleighHeightScale);

    _deferredcaster->enableOxygen(_oxygenAbsEnabled);
    _deferredcaster->setOxygenHeightScale(_oxygenHeightScale);

    _deferredcaster->enableOzone(_ozoneLayerEnabled);
    _deferredcaster->setOzoneAbsCrossSections(_ozoneAbsorptionCrossSectionP);

    _deferredcaster->setMieHeightScale(_mieHeightScale);
    _deferredcaster->setMiePhaseConstant(_miePhaseConstant);
    _deferredcaster->setSunRadianceIntensity(_sunRadianceIntensity);
    _deferredcaster->setRayleighScatteringCoefficients(_rayleighScatteringCoeff);

    _deferredcaster->setMieScatteringCoefficients(_mieScatteringCoeff);
    _deferredcaster->setMieExtinctionCoefficients(_mieExtinctionCoeff);
    glm::vec3 mieAbsorptionCoeff = glm::vec3(_mieAbsorptionCoeffP) * glm::vec3(0.001f);
    _deferredcaster->setMieAbsorptionCoefficients(mieAbsorptionCoeff);
    _deferredcaster->enableSunFollowing(_sunFollowingCameraEnabled);
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
    advModeData.lambdaArray               = glm::vec3(_lambdaArrayP) * pow(10.f, -9);
    advModeData.meanRadiusParticleMie     = _meanRadiusParticleMieP * pow(10.f, -6);
    advModeData.nComplexMie               = _nComplexMieP;
    advModeData.nComplexRayleigh          = _nComplexRayleighP;
    advModeData.nRealMie                  = _nRealMieP;
    advModeData.nRealRayleigh             = _nRealRayleighP;
    advModeData.NMie                      = _NMieP * 1e10f;
    advModeData.NRayleigh                 = _NRayleighP * 1e25f;
    advModeData.NRayleighAbsMolecule      = _NRayleighAbsMoleculeP * 1e25f;
    advModeData.radiusAbsMoleculeRayleigh = _radiusAbsMoleculeRayleighP * pow(10.f, -10);
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
