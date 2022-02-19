/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/deferredcastermanager.h>
#include <math.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo AtmosphereHeightInfo = {
        "AtmosphereHeight",
        "Atmosphere Height (KM)",
        "The thickness of the atmosphere in km"
    };

    constexpr openspace::properties::Property::PropertyInfo AverageGroundReflectanceInfo =
    {
        "AverageGroundReflectance",
        "Average Ground Reflectance (%)",
        "Average percentage of light reflected by the ground during the pre-calculation "
        "phase"
    };

    constexpr openspace::properties::Property::PropertyInfo GroundRadianceEmissionInfo = {
        "GroundRadianceEmission",
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
        "Rayleigh Scattering Coeff",
        "Rayleigh sea-level scattering coefficients in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo OzoneLayerInfo = {
        "Ozone",
        "Ozone Layer Enabled",
        "Enables/Disable Ozone Layer during pre-calculation phase"
    };

    constexpr openspace::properties::Property::PropertyInfo
        OzoneLayerAbsCrossSectionInfo =
    {
        "OzoneLayerAbsCrossSections",
        "Ozone Layer Absorption Cross-Sections Coefficients (x10e-22)",
        "Ozone absorption cross-sections in centimeters."
    };

    constexpr openspace::properties::Property::PropertyInfo OzoneHeightScaleInfo = {
        "OzoneLayerHeightScale",
        "Ozone Scale Height (KM)",
        "It is the vertical distance over which the density and pressure fall by a "
        "constant factor"
    };

    constexpr openspace::properties::Property::PropertyInfo OzoneLayerCoeffInfo = {
        "OzoneLayerCoeff",
        "Ozone Layer Extinction Coeff",
        "Ozone scattering coefficients in meters"
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
        "Mie Scattering Coeff",
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

    constexpr openspace::properties::Property::PropertyInfo UseCornettePhaseFunctionInfo = {
        "UseCornettePhaseFunction",
        "Use Cornette's phase function instead of DHG phase function",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo UsePenndorfPhaseFunctionInfo = {
        "UsePenndorfPhaseFunction",
        "Use Penndorf's phase function approximation for Rayleigh's scattering",
        ""
    };

    openspace::properties::PropertyOwner::PropertyOwnerInfo AdvancedModeOwnerInfo = {
        "AdvancedModeParameters",
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

    constexpr openspace::properties::Property::PropertyInfo
        RayleighRadiusAbsParticleInfo =
    {
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

    struct [[codegen::Dictionary(RenderableAtmosphere)]] Parameters {
        struct ShadowGroup {
            // Individual light sources
            struct SourceElement {
                // The scene graph node name of the source
                std::string name;
                // The radius of the object in meters
                double radius;
            };
            // A list of light sources
            std::vector<SourceElement> sources;

            // Individual shadow casters
            struct CasterElement {
                // The scene graph node name of the source
                std::string name;
                // The radius of the object in meters
                double radius;
            };

            // A list of objects that cast light on this atmosphere
            std::vector<CasterElement> casters;
        };
        // Declares shadow groups, meaning which nodes are considered in shadow
        // calculations
        std::optional<ShadowGroup> shadowGroup;

        // [[codegen::verbatim(AtmosphereHeightInfo.description)]]
        float atmosphereHeight;

        // The radius of the planet in meters
        float planetRadius;

        float planetAverageGroundReflectance;

        // [[codegen::verbatim(SunIntensityInfo.description)]]
        std::optional<float> sunIntensity;

        // [[codegen::verbatim(MieScatteringExtinctionPropCoeffInfo.description)]]
        std::optional<float> mieScatteringExtinctionPropCoefficient;

        // [[codegen::verbatim(GroundRadianceEmissionInfo.description)]]
        float groundRadianceEmission;

        struct Rayleigh {
            struct Coefficients {
                glm::dvec3 wavelengths;
                glm::dvec3 scattering;
            };
            Coefficients coefficients;
            float heightScale [[codegen::key("H_R")]];

            std::optional<glm::vec3> rayleighRealRefract;
            std::optional<glm::vec3> rayleighComplexRefract;
            std::optional<float> polarizability;
            std::optional<float> nRayleigh;
            std::optional<float> nRayleighAbs;
            std::optional<float> rayleighAbsParticleRadius;
            std::optional<bool> usePenndorfPhaseFunctionInfo;

        };
        Rayleigh rayleigh;

        struct Ozone {
            struct Coefficients {
                std::optional<glm::vec3> absorption;
            };
            std::optional<Coefficients> coefficients;
            std::optional<float> heightScale [[codegen::key("H_O")]];
        };
        std::optional<Ozone> ozone;

        struct Oxygen {
            struct Coefficients {
                std::optional<glm::vec3> absorption;
            };
            std::optional<Coefficients> coefficients;
            std::optional<float> heightScale [[codegen::key("H_O2")]];
        };
        std::optional<Oxygen> oxygen;

        struct Mie {
            struct Coefficients {
                glm::dvec3 scattering;
                glm::dvec3 extinction;
            };
            Coefficients coefficients;
            float heightScale [[codegen::key("H_M")]];
            float phaseConstant [[codegen::key("G"), codegen::inrange(-1.0, 1.0)]];

            std::optional<glm::vec3> mieRealRefract;
            std::optional<glm::vec3> mieComplexRefract;
            std::optional<glm::vec3> kappa;
            std::optional<float> nMie;
            std::optional<float> mieMeanParticleRadius;
            std::optional<float> turibidity;
            std::optional<float> jungeExponent;
            std::optional<glm::vec3> g1;
            std::optional<glm::vec3> g2;
            std::optional<glm::vec3> alpha;
            std::optional<bool> useCornettePhaseFunction;
        };
        Mie mie;

        std::optional<bool> advancedMode;
        std::optional<bool> useOnlyAdvancedMie;

        struct ATMDebug {
            std::optional<float> preCalculatedTextureScale [[codegen::inrange(0.0, 1.0)]];
            std::optional<bool> saveCalculatedTextures;
        };
        std::optional<ATMDebug> debug;
    };
#include "renderableatmosphere_codegen.cpp"

} // namespace

namespace openspace {

documentation::Documentation RenderableAtmosphere::Documentation() {
    return codegen::doc<Parameters>("atmosphere_renderable_atmosphere");
}

RenderableAtmosphere::RenderableAtmosphere(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _atmosphereHeight(AtmosphereHeightInfo, 60.f, 0.1f, 300.f)
    , _groundAverageReflectance(AverageGroundReflectanceInfo, 0.f, 0.f, 1.f)
    , _groundRadianceEmission(GroundRadianceEmissionInfo, 0.f, 0.f, 1.f)
    , _rayleighHeightScale(RayleighHeightScaleInfo, 8.f, 0.1f, 100.f)
    , _rayleighScatteringCoeff(
        RayleighScatteringCoeffInfo,
        glm::vec3(0.f), glm::vec3(0.00001f), glm::vec3(0.1f)
    )
    , _ozoneEnabled(OzoneLayerInfo, false)
    , _oxygenEnabled(OxygenAbsInfo, false)
    , _ozoneAbsorptionCrossSection(
        OzoneLayerAbsCrossSectionInfo,
        glm::vec3(1.368209f / 1000.f, 3.314053f / 1000.f, 13.60173f / 1000.f),
        glm::vec3(0.00001f),
        glm::vec3(0.001f)
    )
    , _oxygenHeightScale(OxygenHeightScaleInfo, 9.f, 0.1f, 20.f)
    , _oxygenAbsorptionCrossSection(
        OxygenAbsCrossSectionInfo,
        glm::vec3(4.164f / 1000.f, 5.06f / 1000.f, 7.525f / 1000.f),
        glm::vec3(0.00001f),
        glm::vec3(0.001f)
    )
    , _mieHeightScale(MieHeightScaleInfo, 1.2f, 0.1f, 100.f)
    , _mieScatteringCoeff(
        MieScatteringCoeffInfo,
        glm::vec3(0.004f),
        glm::vec3(0.00001f),
        glm::vec3(1.f)
    )
    , _mieAbsorptionCoeff(
        MieAbsorptionCoeffInfo,
        glm::vec3(0.f),
        glm::vec3(0.00001f),
        glm::vec3(0.001f)
    )
    , _mieExtinctionCoeff(
        MieExtinctionCoeffInfo,
        glm::vec3((4.f / 0.9f) / 1000.f),
        glm::vec3(0.00001f),
        glm::vec3(0.001f)
    )
    , _mieScatteringExtinctionPropCoeff(
        MieScatteringExtinctionPropCoeffInfo,
        0.9f, 0.01f, 1.f
    )
    , _mieAsymmetricFactorG(MieAsymmetricFactorGInfo, 0.f, -1.f, 1.f)
    , _sunIntensity(SunIntensityInfo, 5.f, 0.1f, 1000.f)
    , _sunFollowingCameraEnabled(EnableSunOnCameraPositionInfo, false)
    , _hardShadowsEnabled(EclipseHardShadowsInfo, false)
    , _advancedModeOwner(AdvancedModeOwnerInfo)
    , _enableAdvancedMode(AdvancedModeInfo, false)
    , _useOnlyAdvancedMie(UseOnlyAdvancedMieInfo, false)
    , _useCornettePhaseFunction(UseCornettePhaseFunctionInfo , false)
    , _usePenndorfPhaseFunction(UsePenndorfPhaseFunctionInfo , false)
    , _nRealRayleigh(
        RayleighRealRefractIndexInfo,
        glm::vec3(1.00027598f, 1.00027783f, 1.00028276f),
        glm::vec3(0.01f),
        glm::vec3(5.f)
    )
    , _nComplexRayleigh(
        RayleighComplexRefractIndexInfo,
        glm::vec3(0.f),
        glm::vec3(0.f),
        glm::vec3(2.f)
    )
    , _nRealMie(
        MieRealRefractIndexInfo,
        glm::vec3(1.33f),
        glm::vec3(0.01f),
        glm::vec3(5.f)
    )
    , _nComplexMie(
        MieComplexRefractIndexInfo,
        glm::vec3(0.01f),
        glm::vec3(0.f),
        glm::vec3(2.f)
    )
    , _lambdaArray(
        WavelenghArrayInfo,
        glm::vec3(680.f, 550.f, 440.f),
        glm::vec3(400.f),
        glm::vec3(700.f)
    )
    , _kappa(
        KappaInfo,
        glm::vec3(0.0096f, 0.0092f, 0.0089f),
        glm::vec3(0.f),
        glm::vec3(1.f)
    )
    , _g1(
        G1Info,
        glm::vec3(0.889f, 0.889f, 0.889f),
        glm::vec3(-1.f),
        glm::vec3(1.f)
    )
    , _g2(
        G2Info,
        glm::vec3(0.094f, 0.094f, 0.094f),
        glm::vec3(-1.f),
        glm::vec3(1.f)
    )
    , _alpha(
        AlphaInfo,
        glm::vec3(0.743f, 0.743f, 0.743f),
        glm::vec3(0.f),
        glm::vec3(1.f)
    )
    , _deltaPolarizability(PolarizabilityInfo, 0.0279f, 0.01f, 1.f)
    , _NRayleigh(NRayleighInfo, 2.68731f, 0.01f, 1000.f)
    , _NMie(NMieInfo, 2.8f, 0.f, 100.f)
    , _NRayleighAbsMolecule(NRayleighAbsInfo, 1.f, 0.f, 10000.f)
    , _radiusAbsMoleculeRayleigh(RayleighRadiusAbsParticleInfo, 0.5f, 0.0f, 10.f)
    , _meanRadiusParticleMie(MieMeanRadiusParticleInfo, 1.6f, 0.f, 50.f)
    , _turbidity(TurbidityInfo, 2.f, 1.f, 10.f)
    , _jungeExponent(JungeExponentInfo, 4.f, 2.f, 6.f)
 {
    auto updateWithCalculation = [this]() {
        _deferredCasterNeedsUpdate = true;
        _deferredCasterNeedsCalculation = true;
    };
    auto updateWithoutCalculation = [this]() { _deferredCasterNeedsUpdate = true; };

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _shadowEnabled = p.shadowGroup.has_value();
    if (_shadowEnabled) {
        for (const Parameters::ShadowGroup::SourceElement& s : p.shadowGroup->sources) {
            for (const Parameters::ShadowGroup::CasterElement& c :
                 p.shadowGroup->casters)
            {
                ShadowConfiguration sc;
                sc.source = std::pair(s.name, s.radius);
                sc.caster = std::pair(c.name, c.radius);
                _shadowConfArray.push_back(sc);
            }
        }
    }

    _atmosphereHeight = p.atmosphereHeight;
    _atmosphereHeight.onChange(updateWithCalculation);
    addProperty(_atmosphereHeight);

    _planetRadius = p.planetRadius;

    _groundAverageReflectance = p.planetAverageGroundReflectance;
    _groundAverageReflectance.onChange(updateWithCalculation);
    addProperty(_groundAverageReflectance);

    _sunIntensity = p.sunIntensity.value_or(_sunIntensity);
    _sunIntensity.onChange(updateWithoutCalculation);
    addProperty(_sunIntensity);

    _mieScattExtPropCoefProp =
        p.mieScatteringExtinctionPropCoefficient.value_or(_mieScattExtPropCoefProp);

    _enableAdvancedMode = p.advancedMode.value_or(_enableAdvancedMode);
    _enableAdvancedMode.onChange(updateWithCalculation);
    addProperty(_enableAdvancedMode);

    _useOnlyAdvancedMie = p.useOnlyAdvancedMie.value_or(_useOnlyAdvancedMie);
    _useOnlyAdvancedMie.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_useOnlyAdvancedMie);

    _rayleighScatteringCoeff = p.rayleigh.coefficients.scattering;
    _rayleighScatteringCoeff.onChange(updateWithCalculation);
    addProperty(_rayleighScatteringCoeff);

    _rayleighHeightScale = p.rayleigh.heightScale;
    _rayleighHeightScale.onChange(updateWithCalculation);
    addProperty(_rayleighHeightScale);

    _nRealRayleigh = p.rayleigh.rayleighRealRefract.value_or(_nRealRayleigh);
    _nRealRayleigh.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_nRealRayleigh);

    _nComplexRayleigh = p.rayleigh.rayleighComplexRefract.value_or(_nComplexRayleigh);
    _nComplexRayleigh.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_nComplexRayleigh);

    _deltaPolarizability = p.rayleigh.polarizability.value_or(_deltaPolarizability);
    _deltaPolarizability.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_deltaPolarizability);

    _NRayleigh = p.rayleigh.nRayleigh.value_or(_NRayleigh);
    _NRayleigh.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_NRayleigh);

    _NRayleighAbsMolecule = p.rayleigh.nRayleighAbs.value_or(_NRayleighAbsMolecule);
    _NRayleighAbsMolecule.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_NRayleighAbsMolecule);

    _radiusAbsMoleculeRayleigh =
        p.rayleigh.rayleighAbsParticleRadius.value_or(_radiusAbsMoleculeRayleigh);
    _radiusAbsMoleculeRayleigh.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_radiusAbsMoleculeRayleigh);

    _usePenndorfPhaseFunction =
        p.rayleigh.usePenndorfPhaseFunctionInfo.value_or(_usePenndorfPhaseFunction);
    _usePenndorfPhaseFunction.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_usePenndorfPhaseFunction);

    if (p.ozone.has_value()) {
        // @TODO:  Should be _ozoneEnabled = p.ozone.has_value() and heightScale made
        // not optional
        _ozoneEnabled = p.ozone->heightScale.has_value();

        if (p.ozone->coefficients.has_value()) {
            _ozoneAbsorptionCrossSection =
                p.ozone->coefficients->absorption.value_or(_ozoneAbsorptionCrossSection);
        }
    }
    _ozoneEnabled.onChange(updateWithCalculation);
    addProperty(_ozoneEnabled);
    _ozoneAbsorptionCrossSection.onChange(updateWithCalculation);
    addProperty(_ozoneAbsorptionCrossSection);

    if (p.oxygen.has_value()) {
        // @TODO:  Should be _oxygenEnabled = p.oxygen.has_value() and heightScale made
        // not optional
        _oxygenEnabled = true;

        _oxygenHeightScale = p.oxygen->heightScale.value_or(_oxygenHeightScale);

        // @TODO this is not optional in the original code
        if (p.oxygen->coefficients.has_value()) {
            _oxygenAbsorptionCrossSection = p.oxygen->coefficients->absorption.value_or(_oxygenAbsorptionCrossSection);
        }
    }
    _oxygenEnabled.onChange(updateWithCalculation);
    addProperty(_oxygenEnabled);

    _oxygenHeightScale.onChange(updateWithCalculation);
    addProperty(_oxygenHeightScale);

    _oxygenAbsorptionCrossSection.onChange(updateWithCalculation);
    addProperty(_oxygenAbsorptionCrossSection);

    _mieHeightScale = p.mie.heightScale;
    _mieHeightScale.onChange(updateWithCalculation);
    addProperty(_mieHeightScale);

    _mieScatteringCoeff = p.mie.coefficients.scattering;
    _mieScatteringCoeff.onChange(updateWithCalculation);
    addProperty(_mieScatteringCoeff);

    _mieExtinctionCoeff = p.mie.coefficients.extinction;
    _mieExtinctionCoeff.onChange(updateWithCalculation);
    addProperty(_mieExtinctionCoeff);

    _mieAsymmetricFactorG = p.mie.phaseConstant;
    _mieAsymmetricFactorG.onChange(updateWithCalculation);
    addProperty(_mieAsymmetricFactorG);

    _nRealMie = p.mie.mieRealRefract.value_or(_nRealMie);
    _nRealMie.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_nRealMie);

    _nComplexMie = p.mie.mieComplexRefract.value_or(_nComplexMie);
    _nComplexMie.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_nComplexMie);

    _lambdaArray.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_lambdaArray);

    _kappa = p.mie.kappa.value_or(_kappa);
    _kappa.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_kappa);

    _NMie = p.mie.nMie.value_or(_NMie);
    _NMie.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_NMie);

    _meanRadiusParticleMie = p.mie.mieMeanParticleRadius.value_or(_meanRadiusParticleMie);
    _meanRadiusParticleMie.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_meanRadiusParticleMie);

    _turbidity = p.mie.turibidity.value_or(_turbidity);
    _turbidity.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_turbidity);

    _jungeExponent = p.mie.jungeExponent.value_or(_jungeExponent);
    _jungeExponent.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_jungeExponent);

    _g1 = p.mie.g1.value_or(_g1);
    _g1.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_g1);

    _g2 = p.mie.g2.value_or(_g2);
    _g2.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_g2);

    _alpha = p.mie.alpha.value_or(_alpha);
    _alpha.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_alpha);

    _useCornettePhaseFunction =
        p.mie.useCornettePhaseFunction.value_or(_useCornettePhaseFunction);
    _useCornettePhaseFunction.onChange(updateWithCalculation);
    _advancedModeOwner.addProperty(_useCornettePhaseFunction);

    _mieScatteringExtinctionPropCoeff =
        _mieScattExtPropCoefProp != 1.f ? _mieScattExtPropCoefProp :
        _mieScatteringCoeff.value().x / _mieExtinctionCoeff.value().x;
    _mieScatteringExtinctionPropCoeff.onChange(updateWithCalculation);
    addProperty(_mieScatteringExtinctionPropCoeff);

    _mieExtinctionCoeff =
        _mieScatteringCoeff.value() * _mieScatteringExtinctionPropCoeff.value();

    if (p.debug.has_value()) {
        _textureScale = p.debug->preCalculatedTextureScale.value_or(_textureScale);

        _saveCalculationsToTexture =
            p.debug->saveCalculatedTextures.value_or(_saveCalculationsToTexture);
    }

    _groundRadianceEmission = p.groundRadianceEmission;
    _groundRadianceEmission.onChange(updateWithoutCalculation);
    addProperty(_groundRadianceEmission);

    _sunFollowingCameraEnabled.onChange(updateWithoutCalculation);
    addProperty(_sunFollowingCameraEnabled);

    if (_shadowEnabled) {
        _hardShadowsEnabled.onChange(updateWithoutCalculation);
        addProperty(_hardShadowsEnabled);
    }

    addPropertySubOwner(_advancedModeOwner);

    setBoundingSphere(_planetRadius * 1000.0);
}

void RenderableAtmosphere::initializeGL() {
    _deferredcaster = std::make_unique<AtmosphereDeferredcaster>(
        _textureScale,
        _shadowEnabled ? std::move(_shadowConfArray) : std::vector<ShadowConfiguration>(),
        _saveCalculationsToTexture
    );
    _shadowConfArray.clear();
    updateAtmosphereParameters();
    _deferredcaster->initialize();

    global::deferredcasterManager->attachDeferredcaster(*_deferredcaster);
}

void RenderableAtmosphere::deinitializeGL() {
    global::deferredcasterManager->detachDeferredcaster(*_deferredcaster);
    _deferredcaster = nullptr;
}

bool RenderableAtmosphere::isReady() const {
    return true;
}

glm::dmat4 RenderableAtmosphere::computeModelTransformMatrix(const TransformData& data) {
    // scale the planet to appropriate size since the planet is a unit sphere
    return glm::translate(glm::dmat4(1.0), data.translation) *
        glm::dmat4(data.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.scale));
}

void RenderableAtmosphere::render(const RenderData& data, RendererTasks& renderTask) {
    ZoneScoped

    DeferredcasterTask task{ _deferredcaster.get(), data };
    renderTask.deferredcasterTasks.push_back(task);
}

void RenderableAtmosphere::update(const UpdateData& data) {
    if (_deferredCasterNeedsUpdate) {
        updateAtmosphereParameters();
        _deferredCasterNeedsUpdate = false;
    }
    if (_deferredCasterNeedsCalculation) {
        _deferredcaster->calculateAtmosphereParameters();
        _deferredCasterNeedsCalculation = false;
    }

    glm::dmat4 modelTransform = computeModelTransformMatrix(data.modelTransform);
    _deferredcaster->setModelTransform(modelTransform);
    _deferredcaster->update(data);
}

void RenderableAtmosphere::updateAtmosphereParameters() {
    glm::vec3 mieA = _mieAbsorptionCoeff;
    if (mieA.x != 0.f || mieA.y != 0.f || mieA.z != 0.f) {
        _mieExtinctionCoeff = (mieA + _mieScatteringCoeff.value());
    }
    else {
        _mieExtinctionCoeff =
            _mieScatteringCoeff.value() / _mieScatteringExtinctionPropCoeff.value();
    }

    _deferredcaster->setParameters(
        _planetRadius + _atmosphereHeight,
        _planetRadius,
        _groundAverageReflectance,
        _groundRadianceEmission,
        _rayleighHeightScale,
        _ozoneEnabled,
        _mieHeightScale,
        _mieAsymmetricFactorG,
        _sunIntensity,
        _rayleighScatteringCoeff,
        _mieScatteringCoeff,
        _mieExtinctionCoeff,
        _sunFollowingCameraEnabled
    );
    _deferredcaster->setHardShadows(_hardShadowsEnabled);

    _deferredcaster->enableAdvancedMode(_enableAdvancedMode);

    AtmosphereDeferredcaster::AdvancedATMModeData advModeData;
    advModeData.useOnlyAdvancedMie = _useOnlyAdvancedMie;
    advModeData.usePenndorfPhaseFunction = _usePenndorfPhaseFunction;
    advModeData.useCornettePhaseFunction = _useCornettePhaseFunction;
    advModeData.deltaPolarizability = _deltaPolarizability;
    advModeData.jungeExponent = _jungeExponent;
    advModeData.Kappa = _kappa;
    advModeData.lambdaArray = glm::vec3(_lambdaArray) * 1e-9f;
    advModeData.meanRadiusParticleMie = _meanRadiusParticleMie * 1e-6f;
    advModeData.nComplexMie = _nComplexMie;
    advModeData.nComplexRayleigh = _nComplexRayleigh;
    advModeData.nRealMie = _nRealMie;
    advModeData.nRealRayleigh = _nRealRayleigh;
    advModeData.NMie = _NMie * 1e8f;
    advModeData.NRayleigh = _NRayleigh * 1e25f;
    advModeData.NRayleighAbsMolecule = _NRayleighAbsMolecule * 1e16f;
    advModeData.radiusAbsMoleculeRayleigh = _radiusAbsMoleculeRayleigh * 1e-10f;
    advModeData.turbidity = _turbidity;
    advModeData.g1 = _g1;
    advModeData.g2 = _g2;
    advModeData.alpha = _alpha;

    _deferredcaster->setAdvancedParameters(
        _oxygenEnabled,
        _oxygenHeightScale,
        _rayleighScatteringCoeff,
        _oxygenAbsorptionCrossSection,
        _ozoneAbsorptionCrossSection,
        _mieScatteringCoeff,
        _mieAbsorptionCoeff,
        _mieExtinctionCoeff,
        advModeData
    );
}

}  // namespace openspace
