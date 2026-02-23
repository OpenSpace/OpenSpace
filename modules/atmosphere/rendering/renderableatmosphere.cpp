/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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
#include <openspace/camera/camera.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/properties/property.h>
#include <openspace/query/query.h>
#include <openspace/rendering/deferredcastermanager.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/profiling.h>
#include <algorithm>
#include <cmath>
#include <cstdlib>

namespace {
    using namespace openspace;

    constexpr float KM_TO_M = 1000.f;

    constexpr Property::PropertyInfo AtmosphereHeightInfo = {
        "AtmosphereHeight",
        "Atmosphere height (km)",
        "The thickness of the atmosphere in kilometers.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo AverageGroundReflectanceInfo = {
        "AverageGroundReflectance",
        "Average ground reflectance (%)",
        "Average percentage of light reflected by the ground during the pre-calculation "
        "phase.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo GroundRadianceEmissionInfo = {
        "GroundRadianceEmission",
        "Percentage of initial radiance emitted from ground",
        "Multiplier of the ground radiance color during the rendering phase.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo RayleighHeightScaleInfo = {
        "RayleighHeightScale",
        "Rayleigh scale height (KM)",
        "The vertical distance over which the density and pressure falls by a constant "
        "factor.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo RayleighScatteringCoeffInfo = {
        "RayleighScatteringCoeff",
        "Rayleigh scattering coefficient",
        "Rayleigh sea-level scattering coefficients in meters.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo OzoneLayerInfo = {
        "Ozone",
        "Ozone layer enabled",
        "Enables/Disable Ozone Layer during pre-calculation phase.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo OzoneHeightScaleInfo = {
        "OzoneLayerHeightScale",
        "Ozone scale height (km)",
        "The vertical distance over which the density and pressure fall by a constant "
        "factor, given in kilometers.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo OzoneLayerCoeffInfo = {
        "OzoneLayerCoeff",
        "Ozone layer extinction coefficient",
        "Ozone scattering coefficients in meters.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo MieHeightScaleInfo = {
        "MieHeightScale",
        "Mie scale height (km)",
        "The vertical distance over which the density and pressure fall by a constant "
        "factor, given in kilometers.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo MieScatteringCoeffInfo = {
        "MieScatteringCoeff",
        "Mie scattering coefficient",
        "Mie sea-level scattering coefficients in meters.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo MieScatteringExtinctionPropCoeffInfo = {
        "MieScatteringExtinctionPropCoefficient",
        "Mie scattering/extinction proportion coefficient (%)",
        "Mie Scattering/Extinction Proportion Coefficient.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo MieAsymmetricFactorGInfo = {
        "MieAsymmetricFactorG",
        "Mie asymmetric factor G",
        "Averaging of the scattering angle over a high number of scattering events.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo SunIntensityInfo = {
        "SunIntensity",
        "Sun intensity",
        "A unitless value that controls the intensity/brightness of the Sun.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo EnableSunOnCameraPositionInfo = {
        "SunFollowingCamera",
        "Enable Sun on camera position",
        "When selected the Sun is artificially positioned behind the observer all times.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo EclipseHardShadowsInfo = {
        "EclipseHardShadows",
        "Enable hard shadows for eclipses",
        "Enables/Disables hard shadows through the atmosphere.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo AtmosphereDimmingHeightInfo = {
        "AtmosphereDimmingHeight",
        "Atmosphere dimming height",
        "Percentage of the atmosphere where other objects, such as the stars, are faded.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo SunsetAngleInfo = {
        "AtmosphereDimmingSunsetAngle",
        "Atmosphere dimming sunset angle",
        "The angle (degrees) between the Camera and the Sun where the sunset starts, and "
        "the atmosphere starts to fade in objects such as the stars.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo SunAngularSize = {
        "SunAngularSize",
        "Angular size of the Sun",
        "The angular size of the Sun in degrees.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo LightSourceNodeInfo = {
        "LightSourceNode",
        "Light source",
        "The name of a scene graph node to be used as the source of illumination "
        "for the atmosphere. If not specified, the solar system's Sun is used.",
        Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableAtmosphere)]] Parameters {
        struct ShadowGroup {
            // Individual light sources.
            struct SourceElement {
                // The scene graph node name of the source.
                std::string name;
                // The radius of the object in meters.
                double radius;
            };
            // A list of light sources.
            std::vector<SourceElement> sources;

            // Individual shadow casters.
            struct CasterElement {
                // The scene graph node name of the source.
                std::string name;
                // The radius of the object in meters.
                double radius;
            };

            // A list of objects that cast light on this atmosphere.
            std::vector<CasterElement> casters;
        };
        // Declares shadow groups, meaning which nodes are considered in shadow
        // calculations.
        std::optional<ShadowGroup> shadowGroup;

        // [[codegen::verbatim(AtmosphereHeightInfo.description)]]
        float atmosphereHeight;

        // The radius of the planet in meters.
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
        };
        Rayleigh rayleigh;

        struct Ozone {
            struct Coefficients {
                std::optional<glm::vec3> extinction;
            };
            std::optional<Coefficients> coefficients;
            std::optional<float> heightScale [[codegen::key("H_O")]];
        };
        std::optional<Ozone> ozone;

        struct Mie {
            struct Coefficients {
                glm::dvec3 scattering;
                glm::dvec3 extinction;
            };
            Coefficients coefficients;
            float heightScale [[codegen::key("H_M")]];
            float phaseConstant [[codegen::key("G"), codegen::inrange(-1.0, 1.0)]];
        };
        Mie mie;

        struct ATMDebug {
            std::optional<float> preCalculatedTextureScale [[codegen::inrange(0.0, 1.0)]];
            std::optional<bool> saveCalculatedTextures;
        };
        std::optional<ATMDebug> debug;

        // [[codegen::verbatim(AtmosphereDimmingHeightInfo.description)]]
        std::optional<float> atmosphereDimmingHeight;

        // [[codegen::verbatim(SunsetAngleInfo.description)]]
        std::optional<glm::vec2> sunsetAngle;

        // [[codegen::verbatim(SunAngularSize.description)]]
        std::optional<float> sunAngularSize [[codegen::inrange(0.0, 180.0)]];

        // [[codegen::verbatim(LightSourceNodeInfo.description)]]
        std::optional<std::string> lightSourceNode;
    };
} // namespace
#include "renderableatmosphere_codegen.cpp"

namespace openspace {

Documentation RenderableAtmosphere::Documentation() {
    return codegen::doc<Parameters>("atmosphere_renderable_atmosphere");
}

RenderableAtmosphere::RenderableAtmosphere(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _atmosphereHeight(AtmosphereHeightInfo, 60.f, 0.1f, 99.f)
    , _groundAverageReflectance(AverageGroundReflectanceInfo, 0.f, 0.f, 1.f)
    , _groundRadianceEmission(GroundRadianceEmissionInfo, 0.f, 0.f, 1.f)
    , _rayleighHeightScale(RayleighHeightScaleInfo, 0.f, 0.1f, 50.f)
    , _rayleighScatteringCoeff(
        RayleighScatteringCoeffInfo,
        glm::vec3(0.f), glm::vec3(0.00001f), glm::vec3(0.1f)
    )
    , _ozoneEnabled(OzoneLayerInfo, false)
    , _ozoneHeightScale(OzoneHeightScaleInfo, 0.f, 0.1f, 50.f)
    , _ozoneCoeff(
        OzoneLayerCoeffInfo,
        glm::vec3(0.f), glm::vec3(0.00001f), glm::vec3(0.001f)
    )
    , _mieHeightScale(MieHeightScaleInfo, 0.f, 0.1f, 50.f)
    , _mieScatteringCoeff(
        MieScatteringCoeffInfo,
        glm::vec3(0.004f), glm::vec3(0.00001f), glm::vec3(1.f)
    )
    , _mieScatteringExtinctionPropCoeff(
        MieScatteringExtinctionPropCoeffInfo,
        0.9f, 0.01f, 1.f
    )
    , _miePhaseConstant(MieAsymmetricFactorGInfo, 0.f, -1.f, 1.f)
    , _sunIntensity(SunIntensityInfo, 5.f, 0.1f, 1000.f)
    , _sunFollowingCameraEnabled(EnableSunOnCameraPositionInfo, false)
    , _hardShadowsEnabled(EclipseHardShadowsInfo, false)
    , _sunAngularSize(SunAngularSize, 0.3f, 0.f, 180.f)
    , _lightSourceNodeName(LightSourceNodeInfo)
    , _atmosphereDimmingHeight(AtmosphereDimmingHeightInfo, 0.7f, 0.f, 1.f)
    , _atmosphereDimmingSunsetAngle(
        SunsetAngleInfo,
        glm::vec2(95.f, 100.f), glm::vec2(0.f), glm::vec2(180.f)
    )
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

    _rayleighScatteringCoeff = p.rayleigh.coefficients.scattering;
    _rayleighScatteringCoeff.onChange(updateWithCalculation);
    addProperty(_rayleighScatteringCoeff);

    _rayleighHeightScale = p.rayleigh.heightScale;
    _rayleighHeightScale.onChange(updateWithCalculation);
    addProperty(_rayleighHeightScale);

    if (p.ozone.has_value()) {
        _ozoneHeightScale = p.ozone->heightScale.value_or(_ozoneHeightScale);
        _ozoneEnabled = p.ozone->heightScale.has_value();

        if (p.ozone->coefficients.has_value()) {
            _ozoneCoeff = p.ozone->coefficients->extinction.value_or(_ozoneCoeff);
        }
    }
    _ozoneEnabled.onChange(updateWithCalculation);
    addProperty(_ozoneEnabled);
    _ozoneHeightScale.onChange(updateWithCalculation);
    addProperty(_ozoneHeightScale);
    _ozoneCoeff.onChange(updateWithCalculation);
    addProperty(_ozoneCoeff);

    _mieHeightScale = p.mie.heightScale;
    _mieHeightScale.onChange(updateWithCalculation);
    addProperty(_mieHeightScale);

    _mieScatteringCoeff = p.mie.coefficients.scattering;
    _mieScatteringCoeff.onChange(updateWithCalculation);
    addProperty(_mieScatteringCoeff);

    _mieExtinctionCoeff = p.mie.coefficients.extinction;
    _miePhaseConstant = p.mie.phaseConstant;
    _miePhaseConstant.onChange(updateWithCalculation);
    addProperty(_miePhaseConstant);

    _mieScatteringExtinctionPropCoeff =
        _mieScattExtPropCoefProp != 1.f ? _mieScattExtPropCoefProp :
        _mieScatteringCoeff.value().x / _mieExtinctionCoeff.x;

    _mieScatteringExtinctionPropCoeff.onChange(updateWithCalculation);
    addProperty(_mieScatteringExtinctionPropCoeff);

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

    setBoundingSphere(_planetRadius * 1000.0);

    _atmosphereDimmingHeight =
        p.atmosphereDimmingHeight.value_or(_atmosphereDimmingHeight);
    addProperty(_atmosphereDimmingHeight);

    _atmosphereDimmingSunsetAngle = p.sunsetAngle.value_or(
        _atmosphereDimmingSunsetAngle
    );
    _atmosphereDimmingSunsetAngle.setViewOption(Property::ViewOptions::MinMaxRange);
    addProperty(_atmosphereDimmingSunsetAngle);

    _sunAngularSize = p.sunAngularSize.value_or(_sunAngularSize);
    _sunAngularSize.onChange(updateWithoutCalculation);
    addProperty(_sunAngularSize);

    _lightSourceNodeName.onChange([this]() {
        if (_lightSourceNodeName.value().empty()) {
            _lightSourceNode = nullptr;
            return;
        }

        SceneGraphNode* n = sceneGraphNode(_lightSourceNodeName);
        if (!n) {
            LERRORC(
                "RenderabeAtmosphere",
                std::format(
                    "Could not find node '{}' as illumination for '{}'",
                    _lightSourceNodeName.value(), identifier()
                )
            );
        }
        else {
            _lightSourceNode = n;
            _deferredCasterNeedsUpdate = true;
        }
    });
    _lightSourceNodeName = p.lightSourceNode.value_or("");
    addProperty(_lightSourceNodeName);
}

void RenderableAtmosphere::deinitializeGL() {
    global::deferredcasterManager->detachDeferredcaster(*_deferredcaster);
    _deferredcaster = nullptr;
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

bool RenderableAtmosphere::isReady() const {
    return true;
}

glm::dmat4 RenderableAtmosphere::computeModelTransformMatrix(const TransformData& data) {
    // scale the planet to appropriate size since the planet is a unit sphere
    return glm::translate(glm::dmat4(1.0), data.translation) *
        glm::dmat4(data.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.scale));
}

void RenderableAtmosphere::render(const RenderData& data, RendererTasks& rendererTask) {
    ZoneScoped;

    DeferredcasterTask task = { _deferredcaster.get(), data };
    rendererTask.deferredcasterTasks.push_back(std::move(task));
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
    _deferredcaster->setModelTransform(std::move(modelTransform));
    _deferredcaster->setOpacity(opacity());
    _deferredcaster->update(data);
    setDimmingCoefficient(computeModelTransformMatrix(data.modelTransform));
}

void RenderableAtmosphere::updateAtmosphereParameters() {
    _mieExtinctionCoeff =
        _mieScatteringCoeff.value() / _mieScatteringExtinctionPropCoeff.value();

    _deferredcaster->setParameters(
        _planetRadius + _atmosphereHeight,
        _planetRadius,
        _groundAverageReflectance,
        _groundRadianceEmission,
        _rayleighHeightScale,
        _ozoneEnabled,
        _ozoneHeightScale,
        _mieHeightScale,
        _miePhaseConstant,
        _sunIntensity,
        _rayleighScatteringCoeff,
        _ozoneCoeff,
        _mieScatteringCoeff,
        _mieExtinctionCoeff,
        _sunFollowingCameraEnabled,
        _sunAngularSize,
        _lightSourceNode
    );
    _deferredcaster->setHardShadows(_hardShadowsEnabled);
}

// Calculate atmosphere dimming coefficient
void RenderableAtmosphere::setDimmingCoefficient(const glm::dmat4& modelTransform) {
    // Calculate if the camera is in the atmosphere and if it is in the sunny region
    const glm::dvec3 cameraPos = global::navigationHandler->camera()->positionVec3();
    // TODO: change the assumption that the Sun is placed in the origin
    const glm::dvec3 planetPos =
        glm::dvec3(modelTransform * glm::dvec4(0.0, 0.0, 0.0, 1.0));
    const glm::dvec3 normalUnderCamera = glm::normalize(cameraPos - planetPos);
    const glm::dvec3 vecToSun = glm::normalize(-planetPos);

    const float cameraDistance = static_cast<float>(glm::distance(planetPos, cameraPos));
    const float cameraSunAngle = static_cast<float>(
        glm::degrees(glm::acos(glm::dot(vecToSun, normalUnderCamera))
    ));
    const float sunsetEnd = _atmosphereDimmingSunsetAngle.value().y;

    // If cameraSunAngle is more than 90 degrees, we are in shaded part of globe
    const bool cameraIsInSun = cameraSunAngle <= sunsetEnd;
    // Atmosphere height is in KM
    const float atmosphereEdge = KM_TO_M * (_planetRadius + _atmosphereHeight);
    const bool cameraIsInAtmosphere = cameraDistance < atmosphereEdge;

    // Don't fade if camera is not in the sunny part of an atmosphere
    if (!cameraIsInAtmosphere || !cameraIsInSun) {
        return;
    }
    // Else we need to fade the objects
    // Height of the atmosphere where the objects will be faded
    const float atmosphereFadingHeight =
        KM_TO_M * _atmosphereDimmingHeight * _atmosphereHeight;
    const float atmosphereInnerEdge = atmosphereEdge - atmosphereFadingHeight;
    const bool cameraIsInFadingRegion = cameraDistance > atmosphereInnerEdge;

    // Check if camera is in sunset
    const float sunsetStart = _atmosphereDimmingSunsetAngle.value().x;
    const bool cameraIsInSunset = cameraSunAngle > sunsetStart && cameraIsInSun;

    // See if we are inside of an eclipse shadow
    float eclipseShadow = _deferredcaster->eclipseShadow(cameraPos);
    const bool cameraIsInEclipse = std::abs(eclipseShadow - 1.f) > glm::epsilon<float>();
    // Invert shadow and multiply with itself to make it more narrow
    eclipseShadow = std::pow(1.f - eclipseShadow, 2.f);
    float atmosphereDimming = 0.f;

    if (cameraIsInSunset) {
        // Fading - linear interpolation
        atmosphereDimming = (cameraSunAngle - sunsetStart) /
            (sunsetEnd - sunsetStart);
    }
    else if (cameraIsInFadingRegion && cameraIsInEclipse) {
        // Fade with regards to altitude & eclipse shadow
        // Fading - linear interpolation
        const float fading =
            (cameraDistance - atmosphereInnerEdge) / atmosphereFadingHeight;
        atmosphereDimming = std::clamp(eclipseShadow + fading, 0.f, 1.f);
    }
    else if (cameraIsInFadingRegion) {
        // Fade with regards to altitude
        // Fading - linear interpolation
        atmosphereDimming = (cameraDistance - atmosphereInnerEdge) /
            atmosphereFadingHeight;
    }
    else if (cameraIsInEclipse) {
        atmosphereDimming = eclipseShadow;
    }
    else {
        // Camera is below fading region - atmosphere dims objects completely
        atmosphereDimming = 0.f;
    }
    // Calculate dimming coefficient for stars, labels etc that are dimmed in the
    // atmosphere
    global::navigationHandler->camera()->setAtmosphereDimmingFactor(
        atmosphereDimming
    );
}

} // namespace openspace
