/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <ghoul/misc/profiling.h>
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

    constexpr const char* KeyShadowGroup  = "ShadowGroup";
    constexpr const char* KeyShadowSource = "Source";
    constexpr const char* KeyShadowCaster = "Caster";

    constexpr const char* keyPlanetRadius = "PlanetRadius";
    constexpr const char* keyAverageGroundReflectance = "PlanetAverageGroundReflectance";
    constexpr const char* keyRayleigh = "Rayleigh";
    constexpr const char* keyRayleighHeightScale = "H_R";
    constexpr const char* keyOzone = "Ozone";
    constexpr const char* keyOzoneHeightScale = "H_O";
    constexpr const char* keyMie = "Mie";
    constexpr const char* keyMieHeightScale = "H_M";
    constexpr const char* keyMiePhaseConstant = "G";
    constexpr const char* keyImage = "Image";
    constexpr const char* keyToneMappingOp = "ToneMapping";
    constexpr const char* keyATMDebug = "Debug";
    constexpr const char* keyTextureScale = "PreCalculatedTextureScale";
    constexpr const char* keySaveTextures = "SaveCalculatedTextures";

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

    constexpr openspace::properties::Property::PropertyInfo GroundRadianceEmittioninfo = {
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
} // namespace

namespace openspace {

documentation::Documentation RenderableAtmosphere::Documentation() {
    using namespace documentation;

    TableVerifier* shadowGroupTable = new TableVerifier({
        {
            "*",
            new TableVerifier({
                {
                    "Name",
                    new StringVerifier,
                    Optional::No,
                    "The scene graph node name of the caster/source"
                },
                {
                    "Radius",
                    new DoubleVerifier,
                    Optional::No,
                    "The radius of the object in meters"
                }
            }),
            Optional::No,
            "A list of casters and sources. The sources must be named SourceX and the "
            "casters be named CasterX where X is a whole number starting at 1"
        }
    });

    TableVerifier* rayleighTable = new TableVerifier({
        {
            "Coefficients",
            new TableVerifier({
                {
                    "Wavelengths",
                    new DoubleVector3Verifier,
                    Optional::Yes,
                    ""
                },
                {
                    "Scattering",
                    new DoubleVector3Verifier,
                    Optional::No,
                    ""
                }
            }),
            Optional::No,
            ""
        },
        {
            keyRayleighHeightScale,
            new DoubleVerifier,
            Optional::No,
            ""
        },
    });

    TableVerifier* ozoneTable = new TableVerifier({
        {
            keyOzoneHeightScale,
            new DoubleVerifier,
            Optional::Yes,
            ""
        },
        {
            "Coefficients",
            new TableVerifier({
                {
                    "Extinction",
                    new DoubleVector4Verifier,
                    Optional::Yes,
                    ""
                }
            }),
            Optional::Yes,
            ""
        }
    });

    TableVerifier* mieTable = new TableVerifier({
        {
            keyMieHeightScale,
            new DoubleVerifier,
            Optional::No,
            ""
        },
        {
            "Coefficients",
            new TableVerifier({
                {
                    "Scattering",
                    new DoubleVector3Verifier,
                    Optional::No,
                    ""
                },
                {
                    "Extinction",
                    new DoubleVector3Verifier,
                    Optional::No,
                    ""
                }
            }),
            Optional::No,
            ""
        },
        {
            keyMiePhaseConstant,
            new DoubleVerifier,
            Optional::No,
            ""
        }
    });

    return {
        "RenderableAtmosphere",
        "atmosphere_renderable_atmosphere",
        {
            {
                KeyShadowGroup,
                shadowGroupTable,
                Optional::Yes,
                "Declares shadow groups, meaning which nodes are considered in shadow "
                "calculations"
            },
            {
                AtmosphereHeightInfo.identifier,
                new DoubleVerifier,
                Optional::No,
                AtmosphereHeightInfo.description
            },
            {
                keyPlanetRadius,
                new DoubleVerifier,
                Optional::No,
                "The radius of the planet in meters"
            },
            {
                keyAverageGroundReflectance,
                new DoubleVerifier,
                Optional::No,
                ""
            },
            {
                SunIntensityInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                SunIntensityInfo.description
            },
            {
                MieScatteringExtinctionPropCoeffInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                MieScatteringExtinctionPropCoeffInfo.description
            },
            {
                GroundRadianceEmittioninfo.identifier,
                new DoubleVerifier,
                Optional::No,
                GroundRadianceEmittioninfo.description
            },
            {
                keyRayleigh,
                rayleighTable,
                Optional::No,
                ""
            },
            {
                keyOzone,
                ozoneTable,
                Optional::Yes,
                ""
            },
            {
                keyMie,
                mieTable,
                Optional::No,
                ""
            },
            {
                keyATMDebug,
                new TableVerifier({
                    {
                        keyTextureScale,
                        new DoubleInRangeVerifier(0.0, 1.0),
                        Optional::Yes,
                        ""
                    },
                    {
                        keySaveTextures,
                        new BoolVerifier,
                        Optional::Yes,
                        ""
                    }
                }),
                Optional::Yes,
                ""
            }
        }
    };
}

RenderableAtmosphere::RenderableAtmosphere(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _atmosphereHeight(AtmosphereHeightInfo, 60.f, 0.1f, 99.0f)
    , _groundAverageReflectance(AverageGroundReflectanceInfo, 0.f, 0.f, 1.f)
    , _groundRadianceEmission(GroundRadianceEmittioninfo, 0.f, 0.f, 1.f)
    , _rayleighHeightScale(RayleighHeightScaleInfo, 0.f, 0.1f, 20.f)
    , _rayleighScatteringCoeff(
        RayleighScatteringCoeffInfo,
        glm::vec3(0.f), glm::vec3(0.00001f), glm::vec3(0.1f)
    )
    , _ozoneEnabled(OzoneLayerInfo, false)
    , _ozoneHeightScale(OzoneHeightScaleInfo, 0.f, 0.1f, 20.f)
    , _ozoneCoeff(
        OzoneLayerCoeffInfo,
        glm::vec3(0.f), glm::vec3(0.00001f), glm::vec3(0.001f)
    )
    , _mieHeightScale(MieHeightScaleInfo, 0.f, 0.1f, 20.f)
    , _mieScatteringCoeff(
        MieScatteringCoeffInfo,
        glm::vec3(0.004f), glm::vec3(0.00001f), glm::vec3(1.f)
    )
    , _mieScatteringExtinctionPropCoefficient(
        MieScatteringExtinctionPropCoeffInfo,
        0.9f, 0.01f, 1.f
    )
    , _miePhaseConstant(MieAsymmetricFactorGInfo, 0.f, -1.f, 1.f)
    , _sunIntensity(SunIntensityInfo, 5.f, 0.1f, 1000.f)
    , _sunFollowingCameraEnabled(EnableSunOnCameraPositionInfo, false)
    , _hardShadowsEnabled(EclipseHardShadowsInfo, false)
 {
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableAtmosphere"
    );

    //================================================================
    //======== Reads Shadow (Eclipses) Entries in asset file =========
    //================================================================
    if (dictionary.hasKey(KeyShadowGroup)) {
        ghoul::Dictionary shadowDictionary =
            dictionary.value<ghoul::Dictionary>(KeyShadowGroup);
        bool success = true;
        std::vector<std::pair<std::string, double>> sourceArray;
        unsigned int sourceCounter = 1;
        while (success) {
            std::string baseKey = KeyShadowSource + std::to_string(sourceCounter);
            success = shadowDictionary.hasKey(baseKey);

            if (success) {
                ghoul::Dictionary s = shadowDictionary.value<ghoul::Dictionary>(baseKey);

                std::string sourceName = s.value<std::string>("Name");
                double sourceRadius = s.value<double>("Radius");
                sourceArray.emplace_back(sourceName, sourceRadius);
            }

            sourceCounter++;
        }

        success = true;
        std::vector<std::pair<std::string, double>> casterArray;
        unsigned int casterCounter = 1;
        while (success) {
            std::string baseKey = KeyShadowCaster + std::to_string(casterCounter);
            success = shadowDictionary.hasKey(baseKey);

            if (success) {
                ghoul::Dictionary s = shadowDictionary.value<ghoul::Dictionary>(baseKey);
                std::string casterName = s.value<std::string>("Name");
                double casterRadius = s.value<double>("Radius");
                casterArray.emplace_back(casterName, casterRadius);
            }

            casterCounter++;
        }

        if (!sourceArray.empty() && !casterArray.empty()) {
            for (const std::pair<std::string, double>& source : sourceArray) {
                for (const std::pair<std::string, double>& caster : casterArray) {
                    ShadowConfiguration sc;
                    sc.source = source;
                    sc.caster = caster;
                    _shadowConfArray.push_back(sc);
                }
            }
            _shadowEnabled = true;
        }
    }

    //================================================================
    //========== Reads Atmosphere Entries from asset file ============
    //================================================================
    _atmosphereHeight = static_cast<float>(
        dictionary.value<double>(AtmosphereHeightInfo.identifier)
    );
    _planetRadius = static_cast<float>(dictionary.value<double>(keyPlanetRadius));
    _groundAverageReflectance = static_cast<float>(
        dictionary.value<double>(keyAverageGroundReflectance)
    );
    _groundRadianceEmission = static_cast<float>(
        dictionary.value<double>(GroundRadianceEmittioninfo.identifier)
    );

    if (dictionary.hasKey(SunIntensityInfo.identifier)) {
        _sunIntensity = static_cast<float>(
            dictionary.value<double>(SunIntensityInfo.identifier)
        );
    }

    if (dictionary.hasKey(MieScatteringExtinctionPropCoeffInfo.identifier)) {
        _mieScattExtPropCoefProp = static_cast<float>(
            dictionary.value<double>(MieScatteringExtinctionPropCoeffInfo.identifier)
        );
    }

    {
        ghoul::Dictionary rayleighDict = dictionary.value<ghoul::Dictionary>(keyRayleigh);

        ghoul::Dictionary coeffs = rayleighDict.value<ghoul::Dictionary>("Coefficients");
        _rayleighScatteringCoeff = coeffs.value<glm::dvec3>("Scattering");

        _rayleighHeightScale = static_cast<float>(
            rayleighDict.value<double>(keyRayleighHeightScale)
        );
    }

    if (dictionary.hasValue<ghoul::Dictionary>(keyOzone)) {
        ghoul::Dictionary ozoneDict = dictionary.value<ghoul::Dictionary>(keyOzone);

        if (ozoneDict.hasValue<double>(keyOzoneHeightScale)) {
            _ozoneHeightScale = static_cast<float>(
                ozoneDict.value<double>(keyOzoneHeightScale)
            );
            _ozoneEnabled = true;
        }

        if (ozoneDict.hasValue<ghoul::Dictionary>("Coefficients")) {
            ghoul::Dictionary coeff = ozoneDict.value<ghoul::Dictionary>("Coefficients");
            if (coeff.hasValue<glm::dvec3>("Extinction")) {
                _ozoneCoeff = coeff.value<glm::dvec3>("Extinction");
            }
        }
    }

    {
        ghoul::Dictionary mieDict = dictionary.value<ghoul::Dictionary>(keyMie);
        _mieHeightScale = static_cast<float>(mieDict.value<double>(keyMieHeightScale));

        ghoul::Dictionary coeffs = mieDict.value<ghoul::Dictionary>("Coefficients");

        _mieScatteringCoeff = coeffs.value<glm::dvec3>("Scattering");
        _mieExtinctionCoeff = coeffs.value<glm::dvec3>("Extinction");

        _miePhaseConstant = static_cast<float>(
            mieDict.value<double>(keyMiePhaseConstant)
        );
    }

    if (dictionary.hasValue<ghoul::Dictionary>(keyATMDebug)) {
        ghoul::Dictionary debugDict = dictionary.value<ghoul::Dictionary>(keyATMDebug);
        if (debugDict.hasKey(keyTextureScale)) {
            _preCalculatedTexturesScale = static_cast<float>(
                debugDict.value<double>(keyTextureScale)
            );
            LDEBUG(fmt::format(
                "Atmosphere Texture Scaled to {}", _preCalculatedTexturesScale
            ));
        }

        if (debugDict.hasKey(keySaveTextures)) {
            _saveCalculationsToTexture = debugDict.value<bool>(keySaveTextures);
            LDEBUG("Saving Precalculated Atmosphere Textures");
        }
    }

    //========================================================
    //============== Atmosphere Properties ===================
    //========================================================
    auto updateWithCalculation = [this]() {
        _deferredCasterNeedsUpdate = true;
        _deferredCasterNeedsCalculation = true;
    };
    auto updateWithoutCalculation = [this]() {
        _deferredCasterNeedsUpdate = true;
    };

    _atmosphereHeight.onChange(updateWithCalculation);
    addProperty(_atmosphereHeight);

    _groundAverageReflectance.onChange(updateWithCalculation);
    addProperty(_groundAverageReflectance);

    _groundRadianceEmission.onChange(updateWithoutCalculation);
    addProperty(_groundRadianceEmission);

    _rayleighHeightScale.onChange(updateWithCalculation);
    addProperty(_rayleighHeightScale);

    _rayleighScatteringCoeff.onChange(updateWithCalculation);
    addProperty(_rayleighScatteringCoeff);

    _ozoneEnabled.onChange(updateWithCalculation);
    addProperty(_ozoneEnabled);

    _ozoneHeightScale.onChange(updateWithCalculation);
    addProperty(_ozoneHeightScale);

    _ozoneCoeff.onChange(updateWithCalculation);
    addProperty(_ozoneCoeff);

    _mieHeightScale.onChange(updateWithCalculation);
    addProperty(_mieHeightScale);

    _mieScatteringCoeff.onChange(updateWithCalculation);
    addProperty(_mieScatteringCoeff);

    _mieScatteringExtinctionPropCoefficient =
        _mieScattExtPropCoefProp != 1.f ? _mieScattExtPropCoefProp :
        _mieScatteringCoeff.value().x / _mieExtinctionCoeff.x;

    _mieScatteringExtinctionPropCoefficient.onChange(updateWithCalculation);
    addProperty(_mieScatteringExtinctionPropCoefficient);

    _miePhaseConstant.onChange(updateWithCalculation);
    addProperty(_miePhaseConstant);

    _sunIntensity.onChange(updateWithoutCalculation);
    addProperty(_sunIntensity);

    _sunFollowingCameraEnabled.onChange(updateWithoutCalculation);
    addProperty(_sunFollowingCameraEnabled);

    if (_shadowEnabled) {
        _hardShadowsEnabled.onChange(updateWithoutCalculation);
        addProperty(_hardShadowsEnabled);
    }
}

void RenderableAtmosphere::deinitializeGL() {
    global::deferredcasterManager->detachDeferredcaster(*_deferredcaster);
    _deferredcaster = nullptr;
}

void RenderableAtmosphere::initializeGL() {
    _deferredcaster = std::make_unique<AtmosphereDeferredcaster>();
    _deferredcaster->setAtmosphereRadius(_planetRadius + _atmosphereHeight);
    _deferredcaster->setPlanetRadius(_planetRadius);
    _deferredcaster->setPlanetAverageGroundReflectance(_groundAverageReflectance);
    _deferredcaster->setPlanetGroundRadianceEmittion(_groundRadianceEmission);
    _deferredcaster->setRayleighHeightScale(_rayleighHeightScale);
    _deferredcaster->enableOzone(_ozoneEnabled);
    _deferredcaster->setOzoneHeightScale(_ozoneHeightScale);
    _deferredcaster->setMieHeightScale(_mieHeightScale);
    _deferredcaster->setMiePhaseConstant(_miePhaseConstant);
    _deferredcaster->setSunRadianceIntensity(_sunIntensity);
    _deferredcaster->setRayleighScatteringCoefficients(_rayleighScatteringCoeff);
    _deferredcaster->setOzoneExtinctionCoefficients(_ozoneCoeff);
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
        // We no longer need it
        _shadowConfArray.clear();
        _deferredcaster->setHardShadows(_hardShadowsEnabled);
    }

    _deferredcaster->initialize();

    global::deferredcasterManager->attachDeferredcaster(*_deferredcaster);
}

bool RenderableAtmosphere::isReady() const {
    return true;
}

glm::dmat4 RenderableAtmosphere::computeModelTransformMatrix(
                                                       const TransformData& transformData)
{
    // scale the planet to appropriate size since the planet is a unit sphere
    return glm::translate(glm::dmat4(1.0), transformData.translation) * // Translation
        glm::dmat4(transformData.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(transformData.scale));
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
        _deferredcaster->preCalculateAtmosphereParam();
        _deferredCasterNeedsCalculation = false;
    }

    _deferredcaster->setTime(data.time.j2000Seconds());
    glm::dmat4 modelTransform = computeModelTransformMatrix(data.modelTransform);
    _deferredcaster->setModelTransform(modelTransform);
    _deferredcaster->update(data);
}

void RenderableAtmosphere::updateAtmosphereParameters() {
    _mieExtinctionCoeff =
        _mieScatteringCoeff.value() / _mieScatteringExtinctionPropCoefficient.value();

    _deferredcaster->setAtmosphereRadius(_planetRadius + _atmosphereHeight);
    _deferredcaster->setPlanetRadius(_planetRadius);
    _deferredcaster->setPlanetAverageGroundReflectance(_groundAverageReflectance);
    _deferredcaster->setPlanetGroundRadianceEmittion(_groundRadianceEmission);
    _deferredcaster->setRayleighHeightScale(_rayleighHeightScale);
    _deferredcaster->enableOzone(_ozoneEnabled);
    _deferredcaster->setOzoneHeightScale(_ozoneHeightScale);
    _deferredcaster->setMieHeightScale(_mieHeightScale);
    _deferredcaster->setMiePhaseConstant(_miePhaseConstant);
    _deferredcaster->setSunRadianceIntensity(_sunIntensity);
    _deferredcaster->setRayleighScatteringCoefficients(_rayleighScatteringCoeff);
    _deferredcaster->setOzoneExtinctionCoefficients(_ozoneCoeff);
    _deferredcaster->setMieScatteringCoefficients(_mieScatteringCoeff);
    _deferredcaster->setMieExtinctionCoefficients(_mieExtinctionCoeff);
    _deferredcaster->enableSunFollowing(_sunFollowingCameraEnabled);
    //_deferredcaster->setEllipsoidRadii(_ellipsoid.radii());

    if (_shadowEnabled) {
        _deferredcaster->setHardShadows(_hardShadowsEnabled);
    }
}

}  // namespace openspace
