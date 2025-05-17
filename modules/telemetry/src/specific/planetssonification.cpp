/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/telemetry/include/specific/planetssonification.h>

#include <modules/telemetry/include/util.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/distanceconversion.h>
#include <openspace/util/memorymanager.h>

#include "planetssonification_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "PlanetsSonification";

    // Number of data items for planets and moons, which is used to calculate the total
    // size of the data vector sent to the Open Sound Control receiver.
    constexpr int NumDataItemsPlanet = 4;
    constexpr int NumDataItemsMoon = 3;

    // Indices for the planets
    constexpr int MercuryIndex = 0;
    constexpr int VenusIndex = 1;
    constexpr int EarthIndex = 2;
    constexpr int MarsIndex = 3;
    constexpr int JupiterIndex = 4;
    constexpr int SaturnIndex = 5;
    constexpr int UranusIndex = 6;
    constexpr int NeptuneIndex = 7;

    // Indices for the settings for the planets
    constexpr int NumSettings = 6;
    constexpr int SizeDayIndex = 0;
    constexpr int GravityIndex = 1;
    constexpr int TemperatureIndex = 2;
    constexpr int AtmosphereIndex = 3;
    constexpr int MoonsIndex = 4;
    constexpr int RingsIndex = 5;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        PlanetsSonificationInfo =
    {
        "PlanetsSonification",
        "Planets Sonification",
        "Sonification of the planets in the solar system."
    };

    constexpr openspace::properties::Property::PropertyInfo ToggleAllInfo = {
        "ToggleAll",
        "Toggle All",
        "Toggle all sonifications for all planets.",
        openspace::properties::Property::Visibility::User
    };

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo MercuryInfo = {
        "Mercury",
        "Mercury",
        "Settings for the sonification of Mercury."
    };

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo VenusInfo = {
        "Venus",
        "Venus",
        "Settings for the sonification of Venus."
    };

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo EarthInfo = {
        "Earth",
        "Earth",
        "Settings for the sonification of Earth."
    };

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo MarsInfo = {
        "Mars",
        "Mars",
        "Settings for the sonification of Mars."
    };

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo JupiterInfo = {
        "Jupiter",
        "Jupiter",
        "Settings for the sonification of Jupiter."
    };

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo SaturnInfo = {
        "Saturn",
        "Saturn",
        "Settings for the sonification of Saturn."
    };

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo UranusInfo = {
        "Uranus",
        "Uranus",
        "Settings for the sonification of Uranus."
    };

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo NeptuneInfo = {
        "Neptune",
        "Neptune",
        "Settings for the sonification of Neptune."
    };

    constexpr openspace::properties::Property::PropertyInfo PlanetToggleAllInfo = {
        "ToggleAll",
        "Toggle All",
        "Toggle all sonifications for the planet.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SizeDayInfo = {
        "SizeDay",
        "Size/Day",
        "Toggle the size/day sonification for the planet.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo GravityInfo = {
        "Gravity",
        "Gravity",
        "Toggle the gravity sonification for the planet.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo TemperatureInfo = {
        "Temperature",
        "Temperature",
        "Toggle the temperature sonification for the planet.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo AtmosphereInfo = {
        "Atmosphere",
        "Atmosphere",
        "Toggle the atmosphere sonification for the planet.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo MoonsInfo = {
        "Moons",
        "Moons",
        "Toggle the moons sonification for the planet.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo RingsInfo = {
        "Rings",
        "Rings",
        "Toggle the rings sonification for the planet.",
        openspace::properties::Property::Visibility::User
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo PrecisionInfo = {
        "Precision",
        "Precision",
        "Settings for the distance and angle precision of the planets sonification."
    };

    constexpr openspace::properties::Property::PropertyInfo LowDistancePrecisionInfo = {
        "LowDistancePrecision",
        "Distance Precision (Low)",
        "The precision in meters used to determine when to send updated distance data "
        "to the Open Sound Control receiver. This is the low precision value (low level "
        "of detail) that is used for objects that are not the current focus, saving "
        "performance.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo HighDistancePrecisionInfo = {
        "HighDistancePrecision",
        "Distance Precision (High)",
        "The precision in meters used to determine when to send updated distance data "
        "to the Open Sound Control receiver. This is the high precision value (high "
        "level of detail) that is used when the monitored object is the current focus, "
        "providing more accurate data.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LowAnglePrecisionInfo = {
        "LowAnglePrecision",
        "Angle Precision (Low)",
        "The precision in radians used to determine when to send updated angle data "
        "to the Open Sound Control receiver. This is the low precision value (low level "
        "of detail) that is used for objects that are not the current focus, saving "
        "performance.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo HighAnglePrecisionInfo = {
        "HighAnglePrecision",
        "Angle Precision (High)",
        "The precision in radians used to determine when to send updated angle data "
        "to the Open Sound Control receiver. This is the high precision value (high "
        "level of detail) that is used when the monitored object is the current focus, "
        "providing more accurate data.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(PlanetsSonification)]] Parameters {
        // The name of the planet
        std::string name;

        // Names of the moons for the planet
        std::optional<std::vector<std::string>> moons;
    };
#include "planetssonification_codegen.cpp"
} // namespace

namespace openspace {

PlanetsSonification::PlanetsSonification(const std::string& ip, int port)
    : TelemetryBase(PlanetsSonificationInfo, ip, port)
    , _toggleAll(ToggleAllInfo, false)
    , _mercuryProperty(PlanetsSonification::PlanetProperties(MercuryInfo))
    , _venusProperty(PlanetsSonification::PlanetProperties(VenusInfo))
    , _earthProperty(PlanetsSonification::PlanetProperties(EarthInfo))
    , _marsProperty(PlanetsSonification::PlanetProperties(MarsInfo))
    , _jupiterProperty(PlanetsSonification::PlanetProperties(JupiterInfo))
    , _saturnProperty(PlanetsSonification::PlanetProperties(SaturnInfo))
    , _uranusProperty(PlanetsSonification::PlanetProperties(UranusInfo))
    , _neptuneProperty(PlanetsSonification::PlanetProperties(NeptuneInfo))
    , _precisionProperties(PlanetsSonification::PrecisionProperties(PrecisionInfo))
{
    _toggleAll.onChange([this]() { onToggleAllChanged(); });
    addProperty(_toggleAll);

    // Mercury
    _mercuryProperty.toggleAll.onChange([this]() { onMercuryAllChanged(); });
    _mercuryProperty.sizeDayEnabled.onChange([this]() { onMercurySettingChanged(); });
    _mercuryProperty.gravityEnabled.onChange([this]() { onMercurySettingChanged(); });
    _mercuryProperty.temperatureEnabled.onChange([this]() { onMercurySettingChanged(); });
    // No atmosphere
    // No moons
    // No rings
    addPropertySubOwner(_mercuryProperty);

    // Venus
    _venusProperty.toggleAll.onChange([this]() { onVenusAllChanged(); });
    _venusProperty.sizeDayEnabled.onChange([this]() { onVenusSettingChanged(); });
    _venusProperty.gravityEnabled.onChange([this]() { onVenusSettingChanged(); });
    _venusProperty.temperatureEnabled.onChange([this]() { onVenusSettingChanged(); });
    _venusProperty.atmosphereEnabled.onChange([this]() { onVenusSettingChanged(); });
    // No moons
    // No rings
    addPropertySubOwner(_venusProperty);

    // Earth
    _earthProperty.toggleAll.onChange([this]() { onEarthAllChanged(); });
    _earthProperty.sizeDayEnabled.onChange([this]() { onEarthSettingChanged(); });
    _earthProperty.gravityEnabled.onChange([this]() { onEarthSettingChanged(); });
    _earthProperty.temperatureEnabled.onChange([this]() { onEarthSettingChanged(); });
    _earthProperty.atmosphereEnabled.onChange([this]() { onEarthSettingChanged(); });
    _earthProperty.moonsEnabled.onChange([this]() { onEarthSettingChanged(); });
    // No rings
    addPropertySubOwner(_earthProperty);

    // Mars
    _marsProperty.toggleAll.onChange([this]() { onMarsAllChanged(); });
    _marsProperty.sizeDayEnabled.onChange([this]() { onMarsSettingChanged(); });
    _marsProperty.gravityEnabled.onChange([this]() { onMarsSettingChanged(); });
    _marsProperty.temperatureEnabled.onChange([this]() { onMarsSettingChanged(); });
    _marsProperty.atmosphereEnabled.onChange([this]() { onMarsSettingChanged(); });
    _marsProperty.moonsEnabled.onChange([this]() { onMarsSettingChanged(); });
    // No rings
    addPropertySubOwner(_marsProperty);

    // Jupiter
    _jupiterProperty.toggleAll.onChange([this]() { onJupiterAllChanged(); });
    _jupiterProperty.sizeDayEnabled.onChange([this]() { onJupiterSettingChanged(); });
    _jupiterProperty.gravityEnabled.onChange([this]() { onJupiterSettingChanged(); });
    _jupiterProperty.temperatureEnabled.onChange([this]() { onJupiterSettingChanged(); });
    _jupiterProperty.atmosphereEnabled.onChange([this]() { onJupiterSettingChanged(); });
    _jupiterProperty.moonsEnabled.onChange([this]() { onJupiterSettingChanged(); });
    // No rings visible in OpenSpace
    addPropertySubOwner(_jupiterProperty);

    // Saturn
    _saturnProperty.toggleAll.onChange([this]() { onSaturnAllChanged(); });
    _saturnProperty.sizeDayEnabled.onChange([this]() { onSaturnSettingChanged(); });
    _saturnProperty.gravityEnabled.onChange([this]() { onSaturnSettingChanged(); });
    _saturnProperty.temperatureEnabled.onChange([this]() { onSaturnSettingChanged(); });
    _saturnProperty.atmosphereEnabled.onChange([this]() { onSaturnSettingChanged(); });
    _saturnProperty.moonsEnabled.onChange([this]() { onSaturnSettingChanged(); });
    _saturnProperty.ringsEnabled.onChange([this]() { onSaturnSettingChanged(); });
    addPropertySubOwner(_saturnProperty);

    // Uranus
    _uranusProperty.toggleAll.onChange([this]() { onUranusAllChanged(); });
    _uranusProperty.sizeDayEnabled.onChange([this]() { onUranusSettingChanged(); });
    _uranusProperty.gravityEnabled.onChange([this]() { onUranusSettingChanged(); });
    _uranusProperty.temperatureEnabled.onChange([this]() { onUranusSettingChanged(); });
    _uranusProperty.atmosphereEnabled.onChange([this]() { onUranusSettingChanged(); });
    _uranusProperty.moonsEnabled.onChange([this]() { onUranusSettingChanged(); });
    // No rings visible in OpenSpace
    addPropertySubOwner(_uranusProperty);

    // Neptune
    _neptuneProperty.toggleAll.onChange([this]() { onNeptuneAllChanged(); });
    _neptuneProperty.sizeDayEnabled.onChange([this]() { onNeptuneSettingChanged(); });
    _neptuneProperty.gravityEnabled.onChange([this]() { onNeptuneSettingChanged(); });
    _neptuneProperty.temperatureEnabled.onChange([this]() { onNeptuneSettingChanged(); });
    _neptuneProperty.atmosphereEnabled.onChange([this]() { onNeptuneSettingChanged(); });
    _neptuneProperty.moonsEnabled.onChange([this]() { onNeptuneSettingChanged(); });
    // No rings visible in OpenSpace
    addPropertySubOwner(_neptuneProperty);

    _anglePrecision = _precisionProperties.lowAnglePrecision;
    _distancePrecision = _precisionProperties.lowDistancePrecision;
    addPropertySubOwner(_precisionProperties);
}

PlanetsSonification::DataBody::DataBody(std::string inName)
    : name(std::move(inName))
{}

PlanetsSonification::PlanetProperties::PlanetProperties(
                                  properties::PropertyOwner::PropertyOwnerInfo planetInfo)
    : properties::PropertyOwner(planetInfo)
    , toggleAll(PlanetToggleAllInfo, false)
    , sizeDayEnabled(SizeDayInfo, false)
    , gravityEnabled(GravityInfo, false)
    , temperatureEnabled(TemperatureInfo, false)
    , atmosphereEnabled(AtmosphereInfo, false)
    , moonsEnabled(MoonsInfo, false)
    , ringsEnabled(RingsInfo, false)
{
    // Add the common settings
    addProperty(toggleAll);
    addProperty(sizeDayEnabled);
    addProperty(gravityEnabled);
    addProperty(temperatureEnabled);
    addProperty(atmosphereEnabled);
    addProperty(moonsEnabled);
    addProperty(ringsEnabled);

    // Not all planets have all features, set them as read only if they do not exist
    // Mercury is the only planet that does not have any form of atmosphere
    if (planetInfo.guiName == "Mercury") {
        atmosphereEnabled.setReadOnly(true);
    }
    // Mercury and Venus are the only planets that does not have any moon
    if (planetInfo.guiName == "Mercury" || planetInfo.guiName == "Venus") {
        moonsEnabled.setReadOnly(true);
    }
    // Saturn is the only planet with rings (visible in OpenSpace)
    if (planetInfo.guiName != "Saturn") {
        ringsEnabled.setReadOnly(true);
    }
}

PlanetsSonification::PrecisionProperties::PrecisionProperties(
                               properties::PropertyOwner::PropertyOwnerInfo precisionInfo)
    : properties::PropertyOwner(precisionInfo)
    , lowDistancePrecision(LowDistancePrecisionInfo, 10000.0, 0.0, 1.0e+25)
    , highDistancePrecision(HighDistancePrecisionInfo, 1000.0, 0.0, 1.0e+25)
    , lowAnglePrecision(LowAnglePrecisionInfo, 0.1, 0.0, 10.0)
    , highAnglePrecision(HighAnglePrecisionInfo, 0.05, 0.0, 10.0)
{
    addProperty(lowDistancePrecision);
    lowDistancePrecision.setExponent(20.f);
    addProperty(highDistancePrecision);
    highDistancePrecision.setExponent(20.f);
    addProperty(lowAnglePrecision);
    addProperty(highAnglePrecision);
}

void PlanetsSonification::update(const Camera* camera) {
    TelemetryModule* module = global::moduleEngine->module<TelemetryModule>();
    if (!module) {
        LERROR("Could not find the TelemetryModule");
        return;
    }

    // The compare planets and planets overview sonifications depend on this
    // sonification. If any of them are enabled, then this sonificaiton sends planetary
    // data to the Open Sound Control receiver, even if this sonificaiton is disabled.
    // @NOTE (malej 2024-11-28): Enabling any combination of these three sonifications
    // at the same time is not supported, only one will be used by SuperCollider
    const TelemetryBase* compare = module->telemetry("PlanetsCompareSonification");
    if (!compare) {
        LERROR("Could not find the PlanetsCompareSonification");
        return;
    }
    const TelemetryBase* overview = module->telemetry("PlanetsOverviewSonification");
    if (!overview) {
        LERROR("Could not find the PlanetsOverviewSonification");
        return;
    }

    bool compareEnabled = compare->enabled();
    _overviewEnabled = overview->enabled();

    if (!_enabled && !compareEnabled && !_overviewEnabled) {
        return;
    }

    // Get the angle settings from the module
    TelemetryModule::AngleCalculationMode angleMode = module->angleCalculationMode();
    bool includeElevation = module->includeElevationAngle();

    const SceneGraphNode* focusNode =
        global::navigationHandler->orbitalNavigator().anchorNode();

    if (!focusNode) {
        // The scene is likely not yet initialized
        return;
    }

    // Update data for all planets
    for (int i = 0; i < _planets.size(); ++i) {
        // Increase presision if the planet is in focus
        if (focusNode->identifier() == _planets[i].name) {
            _anglePrecision = _precisionProperties.highAnglePrecision;
            _distancePrecision = _precisionProperties.highDistancePrecision;
        }
        else {
            _anglePrecision = _precisionProperties.lowAnglePrecision;
            _distancePrecision = _precisionProperties.lowDistancePrecision;
        }

        const bool dataWasUpdated = updateData(camera, i, angleMode, includeElevation);

        // Only send data if something new has happened
        if (dataWasUpdated) {
            sendData(i);
        }
    }
}

void PlanetsSonification::stop() {
    _toggleAll = false;
}

void PlanetsSonification::addPlanet(const ghoul::Dictionary& dict) {
    const Parameters p = codegen::bake<Parameters>(dict);
    DataBody planet = DataBody(p.name);

    if (p.moons.has_value()) {
        for (const std::string& moon : *p.moons) {
            planet.moons.push_back(moon);
        }
    }

    _planets.push_back(planet);
}

osc::Blob PlanetsSonification::createSettingsBlob(int planetIndex) const {
    int8_t* settings = reinterpret_cast<int8_t*>(
        global::memoryManager->TemporaryMemory.allocate(NumSettings)
    );

    // Initialize the settings that does not exist for all planets
    settings[AtmosphereIndex] = 0;
    settings[MoonsIndex] = 0;
    settings[RingsIndex] = 0;

    switch (planetIndex) {
        case MercuryIndex:
            settings[SizeDayIndex] = _mercuryProperty.sizeDayEnabled;
            settings[GravityIndex] = _mercuryProperty.gravityEnabled;
            settings[TemperatureIndex] = _mercuryProperty.temperatureEnabled;
            break;
        case VenusIndex:
            settings[SizeDayIndex] = _venusProperty.sizeDayEnabled;
            settings[GravityIndex] = _venusProperty.gravityEnabled;
            settings[TemperatureIndex] = _venusProperty.temperatureEnabled;
            settings[AtmosphereIndex] = _venusProperty.atmosphereEnabled;
            break;
        case EarthIndex:
            settings[SizeDayIndex] = _earthProperty.sizeDayEnabled;
            settings[GravityIndex] = _earthProperty.gravityEnabled;
            settings[TemperatureIndex] = _earthProperty.temperatureEnabled;
            settings[AtmosphereIndex] = _earthProperty.atmosphereEnabled;
            settings[MoonsIndex] = _earthProperty.moonsEnabled;
            break;
        case MarsIndex:
            settings[SizeDayIndex] = _marsProperty.sizeDayEnabled;
            settings[GravityIndex] = _marsProperty.gravityEnabled;
            settings[TemperatureIndex] = _marsProperty.temperatureEnabled;
            settings[AtmosphereIndex] = _marsProperty.atmosphereEnabled;
            settings[MoonsIndex] = _marsProperty.moonsEnabled;
            break;
        case JupiterIndex:
            settings[SizeDayIndex] = _jupiterProperty.sizeDayEnabled;
            settings[GravityIndex] = _jupiterProperty.gravityEnabled;
            settings[TemperatureIndex] = _jupiterProperty.temperatureEnabled;
            settings[AtmosphereIndex] = _jupiterProperty.atmosphereEnabled;
            settings[MoonsIndex] = _jupiterProperty.moonsEnabled;
            break;
        case SaturnIndex:
            settings[SizeDayIndex] = _saturnProperty.sizeDayEnabled;
            settings[GravityIndex] = _saturnProperty.gravityEnabled;
            settings[TemperatureIndex] = _saturnProperty.temperatureEnabled;
            settings[AtmosphereIndex] = _saturnProperty.atmosphereEnabled;
            settings[MoonsIndex] = _saturnProperty.moonsEnabled;
            settings[RingsIndex] = _saturnProperty.ringsEnabled;
            break;
        case UranusIndex:
            settings[SizeDayIndex] = _uranusProperty.sizeDayEnabled;
            settings[GravityIndex] = _uranusProperty.gravityEnabled;
            settings[TemperatureIndex] = _uranusProperty.temperatureEnabled;
            settings[AtmosphereIndex] = _uranusProperty.atmosphereEnabled;
            settings[MoonsIndex] = _uranusProperty.moonsEnabled;
            break;
        case NeptuneIndex:
            settings[SizeDayIndex] = _neptuneProperty.sizeDayEnabled;
            settings[GravityIndex] = _neptuneProperty.gravityEnabled;
            settings[TemperatureIndex] = _neptuneProperty.temperatureEnabled;
            settings[AtmosphereIndex] = _neptuneProperty.atmosphereEnabled;
            settings[MoonsIndex] = _neptuneProperty.moonsEnabled;
            break;
        default:
            throw ghoul::MissingCaseException();
            break;
    }

    return osc::Blob(settings, NumSettings);
}

// Empty overidded functions
bool PlanetsSonification::updateData(const Camera*) {
    return false;
}
void PlanetsSonification::sendData() {}


bool PlanetsSonification::updateData(const Camera* camera, int planetIndex,
                               TelemetryModule::AngleCalculationMode angleCalculationMode,
                                                                    bool includeElevation)
{
    double distance = calculateDistanceTo(
        camera,
        _planets[planetIndex].name,
        DistanceUnit::Kilometer
    );

    if (std::abs(distance) < std::numeric_limits<double>::epsilon()) {
        // The scene is likely not yet initialized
        return false;
    }

    // Calculate the angles depending on the Angle calculation mode and if the overview
    // mode is enabled, then calculate the angles with respect to the Sun instead of
    // the camera.
    double horizontalAngle = 0.0;
    if (_overviewEnabled) {
        horizontalAngle = calculateAngleFromAToB(
            camera,
            "Sun",
            _planets[planetIndex].name,
            angleCalculationMode
        );
    }
    else {
        horizontalAngle =
            calculateAngleTo(camera, _planets[planetIndex].name, angleCalculationMode);
    }

    double verticalAngle = 0.0;
    if (includeElevation) {
        if (_overviewEnabled) {
            verticalAngle = calculateElevationAngleFromAToB(
                camera,
                "Sun",
                _planets[planetIndex].name,
                angleCalculationMode
            );
        }
        else {
            verticalAngle = calculateElevationAngleTo(
                camera,
                _planets[planetIndex].name,
                angleCalculationMode
            );
        }
    }

    // Calculate the angles to the moons from the planet. These angles are used for the
    // sonification of the moons. The reason why the angle is calculated from the planet
    // and not the camera is to give a feeling that the moons are orbiting the audience.
    bool dataWasUpdated = false;
    for (DataBody& moon : _planets[planetIndex].moons) {
        double dist = calculateDistanceTo(camera, moon.name, DistanceUnit::Kilometer);

        if (std::abs(dist) < std::numeric_limits<double>::epsilon()) {
            // The scene is likely not yet initialized
            return false;
        }

        if (std::abs(moon.distance - dist) > _distancePrecision) {
            dataWasUpdated = true;
            moon.distance = dist;
        }

        double moonHAngle = calculateAngleFromAToB(
            camera,
            _planets[planetIndex].name,
            moon.name,
            angleCalculationMode
        );

        if (std::abs(moon.horizontalAngle - moonHAngle) > _anglePrecision) {
            dataWasUpdated = true;
            moon.horizontalAngle = moonHAngle;
        }

        double moonVAngle = 0.0;
        if (includeElevation) {
            moonVAngle = calculateElevationAngleFromAToB(
                camera,
                _planets[planetIndex].name,
                moon.name,
                angleCalculationMode
            );
        }

        if (std::abs(moon.verticalAngle - moonVAngle) > _anglePrecision) {
            dataWasUpdated = true;
            moon.verticalAngle = moonVAngle;
        }
    }

    // Check if this data is new, otherwise don't send it
    double prevDistance = _planets[planetIndex].distance;
    double prevHorizontalAngle = _planets[planetIndex].horizontalAngle;
    double prevVerticalAngle = _planets[planetIndex].verticalAngle;

    if (std::abs(prevDistance - distance) > _distancePrecision ||
        std::abs(prevHorizontalAngle - horizontalAngle) > _anglePrecision ||
        std::abs(prevVerticalAngle - verticalAngle) > _anglePrecision ||
        dataWasUpdated)
    {
        // Update the saved data for the planet
        _planets[planetIndex].distance = distance;
        _planets[planetIndex].horizontalAngle = horizontalAngle;
        _planets[planetIndex].verticalAngle = verticalAngle;
        dataWasUpdated = true;
    }

    return dataWasUpdated;
}

void PlanetsSonification::sendData(int planetIndex) {
    if (planetIndex < 0 || planetIndex > _planets.size() - 1) {
        LWARNING(std::format("Planet list does not include index {}", planetIndex));
        return;
    }

    std::string label = "/" + _planets[planetIndex].name;
    std::vector<OpenSoundControlDataType> data;

    // The total size of the data vector is NumDataItemsPlanet for the
    // planet, and then NumDataItemsMoon per moon
    data.reserve(
        NumDataItemsPlanet + NumDataItemsMoon * _planets[planetIndex].moons.size()
    );

    data.push_back(_planets[planetIndex].distance);
    data.push_back(_planets[planetIndex].horizontalAngle);
    data.push_back(_planets[planetIndex].verticalAngle);
    osc::Blob settingsBlob = createSettingsBlob(planetIndex);
    data.push_back(settingsBlob);

    // Moons
    for (const DataBody& moon : _planets[planetIndex].moons) {
        data.push_back(moon.distance);
        data.push_back(moon.horizontalAngle);
        data.push_back(moon.verticalAngle);
    }

    _connection->send(label, data);
}

void PlanetsSonification::onToggleAllChanged() {
    _mercuryProperty.toggleAll.setValue(_toggleAll);
    _venusProperty.toggleAll.setValue(_toggleAll);
    _earthProperty.toggleAll.setValue(_toggleAll);
    _marsProperty.toggleAll.setValue(_toggleAll);
    _jupiterProperty.toggleAll.setValue(_toggleAll);
    _saturnProperty.toggleAll.setValue(_toggleAll);
    _uranusProperty.toggleAll.setValue(_toggleAll);
    _neptuneProperty.toggleAll.setValue(_toggleAll);
}

// Mercury
void PlanetsSonification::onMercuryAllChanged() {
    _mercuryProperty.sizeDayEnabled.setValue(_mercuryProperty.toggleAll);
    _mercuryProperty.gravityEnabled.setValue(_mercuryProperty.toggleAll);
    _mercuryProperty.temperatureEnabled.setValue(_mercuryProperty.toggleAll);
}
void PlanetsSonification::onMercurySettingChanged() {
    sendData(MercuryIndex);
}

// Venus
void PlanetsSonification::onVenusAllChanged() {
    _venusProperty.sizeDayEnabled.setValue(_venusProperty.toggleAll);
    _venusProperty.gravityEnabled.setValue(_venusProperty.toggleAll);
    _venusProperty.temperatureEnabled.setValue(_venusProperty.toggleAll);
    _venusProperty.atmosphereEnabled.setValue(_venusProperty.toggleAll);
}
void PlanetsSonification::onVenusSettingChanged() {
    sendData(VenusIndex);
}

// Earth
void PlanetsSonification::onEarthAllChanged() {
    _earthProperty.sizeDayEnabled.setValue(_earthProperty.toggleAll);
    _earthProperty.gravityEnabled.setValue(_earthProperty.toggleAll);
    _earthProperty.temperatureEnabled.setValue(_earthProperty.toggleAll);
    _earthProperty.atmosphereEnabled.setValue(_earthProperty.toggleAll);
    _earthProperty.moonsEnabled.setValue(_earthProperty.toggleAll);
}
void PlanetsSonification::onEarthSettingChanged() {
    sendData(EarthIndex);
}

// Mars
void PlanetsSonification::onMarsAllChanged() {
    _marsProperty.sizeDayEnabled.setValue(_marsProperty.toggleAll);
    _marsProperty.gravityEnabled.setValue(_marsProperty.toggleAll);
    _marsProperty.temperatureEnabled.setValue(_marsProperty.toggleAll);
    _marsProperty.atmosphereEnabled.setValue(_marsProperty.toggleAll);
    _marsProperty.moonsEnabled.setValue(_marsProperty.toggleAll);
}
void PlanetsSonification::onMarsSettingChanged() {
    sendData(MarsIndex);
}

// Jupiter
void PlanetsSonification::onJupiterAllChanged() {
    _jupiterProperty.sizeDayEnabled.setValue(_jupiterProperty.toggleAll);
    _jupiterProperty.gravityEnabled.setValue(_jupiterProperty.toggleAll);
    _jupiterProperty.temperatureEnabled.setValue(_jupiterProperty.toggleAll);
    _jupiterProperty.atmosphereEnabled.setValue(_jupiterProperty.toggleAll);
    _jupiterProperty.moonsEnabled.setValue(_jupiterProperty.toggleAll);
}
void PlanetsSonification::onJupiterSettingChanged() {
    sendData(JupiterIndex);
}

// Saturn
void PlanetsSonification::onSaturnAllChanged() {
    _saturnProperty.sizeDayEnabled.setValue(_saturnProperty.toggleAll);
    _saturnProperty.gravityEnabled.setValue(_saturnProperty.toggleAll);
    _saturnProperty.temperatureEnabled.setValue(_saturnProperty.toggleAll);
    _saturnProperty.atmosphereEnabled.setValue(_saturnProperty.toggleAll);
    _saturnProperty.moonsEnabled.setValue(_saturnProperty.toggleAll);
    _saturnProperty.ringsEnabled.setValue(_saturnProperty.toggleAll);
}
void PlanetsSonification::onSaturnSettingChanged() {
    sendData(SaturnIndex);
}

// Uranus
void PlanetsSonification::onUranusAllChanged() {
    _uranusProperty.sizeDayEnabled.setValue(_uranusProperty.toggleAll);
    _uranusProperty.gravityEnabled.setValue(_uranusProperty.toggleAll);
    _uranusProperty.temperatureEnabled.setValue(_uranusProperty.toggleAll);
    _uranusProperty.atmosphereEnabled.setValue(_uranusProperty.toggleAll);
    _uranusProperty.moonsEnabled.setValue(_uranusProperty.toggleAll);
}
void PlanetsSonification::onUranusSettingChanged() {
    sendData(UranusIndex);
}

// Neptune
void PlanetsSonification::onNeptuneAllChanged() {
    _neptuneProperty.sizeDayEnabled.setValue(_neptuneProperty.toggleAll);
    _neptuneProperty.gravityEnabled.setValue(_neptuneProperty.toggleAll);
    _neptuneProperty.temperatureEnabled.setValue(_neptuneProperty.toggleAll);
    _neptuneProperty.atmosphereEnabled.setValue(_neptuneProperty.toggleAll);
    _neptuneProperty.moonsEnabled.setValue(_neptuneProperty.toggleAll);
}
void PlanetsSonification::onNeptuneSettingChanged() {
    sendData(NeptuneIndex);
}

scripting::LuaLibrary PlanetsSonification::luaLibrary() {
    return {
        "sonification",
        {
            codegen::lua::AddPlanets
        }
    };
}

} // namespace openspace
