/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/sonification/include/planetssonification.h>

#include <modules/sonification/sonificationmodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/distanceconversion.h>

namespace {
    constexpr std::string_view _loggerCat = "PlanetsSonification";

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        PlanetsSonificationInfo =
    {
       "PlanetsSonification",
       "Planets Sonification",
       "Sonification of the planets in our solarsystem"
    };

    constexpr openspace::properties::Property::PropertyInfo ToggleAllInfo = {
        "ToggleAll",
        "All",
        "Toggle all sonifications for all planets"
    };

    // Planets
    const openspace::properties::PropertyOwner::PropertyOwnerInfo MercuryInfo = {
        "Mercury",
        "Mercury",
        "Settings for the sonification of Mercury"
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo VenusInfo = {
        "Venus",
        "Venus",
        "Settings for the sonification of Venus"
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo EarthInfo = {
        "Earth",
        "Earth",
        "Settings for the sonification of Earth"
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo MarsInfo = {
        "Mars",
        "Mars",
        "Settings for the sonification of Mars"
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo JupiterInfo = {
        "Jupiter",
        "Jupiter",
        "Settings for the sonification of Jupiter"
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo SaturnInfo = {
        "Saturn",
        "Saturn",
        "Settings for the sonification of Saturn"
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo UranusInfo = {
        "Uranus",
        "Uranus",
        "Settings for the sonification of Uranus"
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo NeptuneInfo = {
        "Neptune",
        "Neptune",
        "Settings for the sonification of  Neptune"
    };

    // Per planet settings
    constexpr openspace::properties::Property::PropertyInfo PlanetToggleAllInfo = {
        "ToggleAll",
        "All",
        "Toggle all sonifications for the planet"
    };

    constexpr openspace::properties::Property::PropertyInfo SizeDayInfo = {
        "SizeDay",
        "Size/Day",
        "Toggle the size/day sonification for the planet"
    };

    constexpr openspace::properties::Property::PropertyInfo GravityInfo = {
        "Gravity",
        "Gravity",
        "Toggle the gravity sonification for the planet"
    };

    constexpr openspace::properties::Property::PropertyInfo TemperatureInfo = {
        "Temperature",
        "Temperature",
        "Toggle the temperature sonification for the planet"
    };

    constexpr openspace::properties::Property::PropertyInfo AtmosphereInfo = {
        "Atmosphere",
        "Atmosphere",
        "Toggle the atmosphere sonification for the planet"
    };

    constexpr openspace::properties::Property::PropertyInfo MoonsInfo = {
        "Moons",
        "Moons",
        "Toggle the moons sonification for the planet"
    };

    constexpr openspace::properties::Property::PropertyInfo RingsInfo = {
        "Rings",
        "Rings",
        "Toggle the rings sonification for the planet"
    };

    struct [[codegen::Dictionary(PlanetsSonification)]] Parameters {
        // The name of the planet
        std::string name;

        // Names of the moons for the planet
        std::optional<std::vector<std::string>> moons;
    };
#include "planetssonification_codegen.cpp"
} // namespace
#include "planetssonification_lua.inl"

namespace openspace {

PlanetsSonification::PlanetsSonification(const std::string& ip, int port)
    : SonificationBase(PlanetsSonificationInfo, ip, port)
    , _toggleAll(ToggleAllInfo, false)
    , _mercuryProperty(PlanetsSonification::PlanetProperty(MercuryInfo))
    , _venusProperty(PlanetsSonification::PlanetProperty(VenusInfo))
    , _earthProperty(PlanetsSonification::PlanetProperty(EarthInfo))
    , _marsProperty(PlanetsSonification::PlanetProperty(MarsInfo))
    , _jupiterProperty(PlanetsSonification::PlanetProperty(JupiterInfo))
    , _saturnProperty(PlanetsSonification::PlanetProperty(SaturnInfo))
    , _uranusProperty(PlanetsSonification::PlanetProperty(UranusInfo))
    , _neptuneProperty(PlanetsSonification::PlanetProperty(NeptuneInfo))
{
    _anglePrecision = LowAnglePrecision;
    _distancePrecision = LowDistancePrecision;

    // Add onChange for the properties
    _toggleAll.onChange([this]() { onToggleAllChanged(); });

    // Mercury
    _mercuryProperty.toggleAll.onChange([this]() { onMercuryAllChanged(); });
    _mercuryProperty.sizeDayEnabled.onChange([this]() { onMercurySettingChanged(); });
    _mercuryProperty.gravityEnabled.onChange([this]() { onMercurySettingChanged(); });
    _mercuryProperty.temperatureEnabled.onChange([this]() { onMercurySettingChanged(); });

    // Venus
    _venusProperty.toggleAll.onChange([this]() { onVenusAllChanged(); });
    _venusProperty.sizeDayEnabled.onChange([this]() { onVenusSettingChanged(); });
    _venusProperty.gravityEnabled.onChange([this]() { onVenusSettingChanged(); });
    _venusProperty.temperatureEnabled.onChange([this]() { onVenusSettingChanged(); });
    _venusProperty.atmosphereEnabled.onChange([this]() { onVenusSettingChanged(); });

    // Earth
    _earthProperty.toggleAll.onChange([this]() { onEarthAllChanged(); });
    _earthProperty.sizeDayEnabled.onChange([this]() { onEarthSettingChanged(); });
    _earthProperty.gravityEnabled.onChange([this]() { onEarthSettingChanged(); });
    _earthProperty.temperatureEnabled.onChange([this]() { onEarthSettingChanged(); });
    _earthProperty.atmosphereEnabled.onChange([this]() { onEarthSettingChanged(); });
    _earthProperty.moonsEnabled.onChange([this]() { onEarthSettingChanged(); });

    // Mars
    _marsProperty.toggleAll.onChange([this]() { onMarsAllChanged(); });
    _marsProperty.sizeDayEnabled.onChange([this]() { onMarsSettingChanged(); });
    _marsProperty.gravityEnabled.onChange([this]() { onMarsSettingChanged(); });
    _marsProperty.temperatureEnabled.onChange([this]() { onMarsSettingChanged(); });
    _marsProperty.atmosphereEnabled.onChange([this]() { onMarsSettingChanged(); });
    _marsProperty.moonsEnabled.onChange([this]() { onMarsSettingChanged(); });

    // Jupiter
    _jupiterProperty.toggleAll.onChange([this]() { onJupiterAllChanged(); });
    _jupiterProperty.sizeDayEnabled.onChange([this]() { onJupiterSettingChanged(); });
    _jupiterProperty.gravityEnabled.onChange([this]() { onJupiterSettingChanged(); });
    _jupiterProperty.temperatureEnabled.onChange([this]() { onJupiterSettingChanged(); });
    _jupiterProperty.atmosphereEnabled.onChange([this]() { onJupiterSettingChanged(); });
    _jupiterProperty.moonsEnabled.onChange([this]() { onJupiterSettingChanged(); });

    // Saturn
    _saturnProperty.toggleAll.onChange([this]() { onSaturnAllChanged(); });
    _saturnProperty.sizeDayEnabled.onChange([this]() { onSaturnSettingChanged(); });
    _saturnProperty.gravityEnabled.onChange([this]() { onSaturnSettingChanged(); });
    _saturnProperty.temperatureEnabled.onChange([this]() { onSaturnSettingChanged(); });
    _saturnProperty.atmosphereEnabled.onChange([this]() { onSaturnSettingChanged(); });
    _saturnProperty.moonsEnabled.onChange([this]() { onSaturnSettingChanged(); });
    _saturnProperty.ringsEnabled.onChange([this]() { onSaturnSettingChanged(); });

    // Uranus
    _uranusProperty.toggleAll.onChange([this]() { onUranusAllChanged(); });
    _uranusProperty.sizeDayEnabled.onChange([this]() { onUranusSettingChanged(); });
    _uranusProperty.gravityEnabled.onChange([this]() { onUranusSettingChanged(); });
    _uranusProperty.temperatureEnabled.onChange([this]() { onUranusSettingChanged(); });
    _uranusProperty.atmosphereEnabled.onChange([this]() { onUranusSettingChanged(); });
    _uranusProperty.moonsEnabled.onChange([this]() { onUranusSettingChanged(); });

    // Neptune
    _neptuneProperty.toggleAll.onChange([this]() { onNeptuneAllChanged(); });
    _neptuneProperty.sizeDayEnabled.onChange([this]() { onNeptuneSettingChanged(); });
    _neptuneProperty.gravityEnabled.onChange([this]() { onNeptuneSettingChanged(); });
    _neptuneProperty.temperatureEnabled.onChange([this]() { onNeptuneSettingChanged(); });
    _neptuneProperty.atmosphereEnabled.onChange([this]() { onNeptuneSettingChanged(); });
    _neptuneProperty.moonsEnabled.onChange([this]() { onNeptuneSettingChanged(); });

    // Add the properties
    addProperty(_toggleAll);
    addPropertySubOwner(_mercuryProperty);
    addPropertySubOwner(_venusProperty);
    addPropertySubOwner(_earthProperty);
    addPropertySubOwner(_marsProperty);
    addPropertySubOwner(_jupiterProperty);
    addPropertySubOwner(_saturnProperty);
    addPropertySubOwner(_uranusProperty);
    addPropertySubOwner(_neptuneProperty);
}

PlanetsSonification::~PlanetsSonification() {
    stop();
}

PlanetsSonification::PlanetProperty::PlanetProperty(
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
    addProperty(toggleAll);
    addProperty(sizeDayEnabled);
    addProperty(gravityEnabled);
    addProperty(temperatureEnabled);

    // Check if any special cases should be added
    if (planetInfo.guiName != "Mercury") {
        addProperty(atmosphereEnabled);
    }
    if (planetInfo.guiName != "Mercury" && planetInfo.guiName != "Venus") {
        addProperty(moonsEnabled);
    }
    if (planetInfo.guiName == "Saturn") {
        addProperty(ringsEnabled);
    }
}

osc::Blob PlanetsSonification::createSettingsBlob(int planetIndex) const {
    bool settings[6] = { false };

    if (planetIndex == MercuryIndex) {
        // Mercury
        settings[SizeDayIndex] = _mercuryProperty.sizeDayEnabled;
        settings[GravityIndex] = _mercuryProperty.gravityEnabled;
        settings[TemperatureIndex] = _mercuryProperty.temperatureEnabled;
    }
    else if (planetIndex == VenusIndex) {
        // Venus
        settings[SizeDayIndex] = _venusProperty.sizeDayEnabled;
        settings[GravityIndex] = _venusProperty.gravityEnabled;
        settings[TemperatureIndex] = _venusProperty.temperatureEnabled;
        settings[AtmosphereIndex] = _venusProperty.atmosphereEnabled;
    }
    else if (planetIndex == EarthIndex) {
        // Earth
        settings[SizeDayIndex] = _earthProperty.sizeDayEnabled;
        settings[GravityIndex] = _earthProperty.gravityEnabled;
        settings[TemperatureIndex] = _earthProperty.temperatureEnabled;
        settings[AtmosphereIndex] = _earthProperty.atmosphereEnabled;
        settings[MoonsIndex] = _earthProperty.moonsEnabled;
    }
    else if (planetIndex == MarsIndex) {
        // Mars
        settings[SizeDayIndex] = _marsProperty.sizeDayEnabled;
        settings[GravityIndex] = _marsProperty.gravityEnabled;
        settings[TemperatureIndex] = _marsProperty.temperatureEnabled;
        settings[AtmosphereIndex] = _marsProperty.atmosphereEnabled;
        settings[MoonsIndex] = _marsProperty.moonsEnabled;
    }
    else if (planetIndex == JupiterIndex) {
        // Jupiter
        settings[SizeDayIndex] = _jupiterProperty.sizeDayEnabled;
        settings[GravityIndex] = _jupiterProperty.gravityEnabled;
        settings[TemperatureIndex] = _jupiterProperty.temperatureEnabled;
        settings[AtmosphereIndex] = _jupiterProperty.atmosphereEnabled;
        settings[MoonsIndex] = _jupiterProperty.moonsEnabled;
    }
    else if (planetIndex == SaturnIndex) {
        // Saturn
        settings[SizeDayIndex] = _saturnProperty.sizeDayEnabled;
        settings[GravityIndex] = _saturnProperty.gravityEnabled;
        settings[TemperatureIndex] = _saturnProperty.temperatureEnabled;
        settings[AtmosphereIndex] = _saturnProperty.atmosphereEnabled;
        settings[MoonsIndex] = _saturnProperty.moonsEnabled;
        settings[RingsIndex] = _saturnProperty.ringsEnabled;
    }
    else if (planetIndex == UranusIndex) {
        // Uranus
        settings[SizeDayIndex] = _uranusProperty.sizeDayEnabled;
        settings[GravityIndex] = _uranusProperty.gravityEnabled;
        settings[TemperatureIndex] = _uranusProperty.temperatureEnabled;
        settings[AtmosphereIndex] = _uranusProperty.atmosphereEnabled;
        settings[MoonsIndex] = _uranusProperty.moonsEnabled;
    }
    else if (planetIndex == NeptuneIndex) {
        // Neptune
        settings[SizeDayIndex] = _neptuneProperty.sizeDayEnabled;
        settings[GravityIndex] = _neptuneProperty.gravityEnabled;
        settings[TemperatureIndex] = _neptuneProperty.temperatureEnabled;
        settings[AtmosphereIndex] = _neptuneProperty.atmosphereEnabled;
        settings[MoonsIndex] = _neptuneProperty.moonsEnabled;
    }
    else {
        throw ghoul::MissingCaseException();
    }

    return osc::Blob(settings, 6);
}

void PlanetsSonification::sendSettings(int planetIndex) {
    if (_planets.size() <= planetIndex) {
        LWARNING(std::format("Planet list does not include index {}", planetIndex));
        return;
    }

    std::string label = "/" + _planets[planetIndex].identifier;
    std::vector<OscDataType> data;

    // Distance
    data.push_back(_planets[planetIndex].data[DistanceIndex]);

    // Horizontal Angle
    data.push_back(_planets[planetIndex].data[HAngleIndex]);

    // Vertical Angle
    data.push_back(_planets[planetIndex].data[VAngleIndex]);

    // Settings
    osc::Blob settingsBlob = createSettingsBlob(planetIndex);
    data.push_back(settingsBlob);

    // Moons
    for (size_t m = 0; m < _planets[planetIndex].moons.size(); ++m) {
        // Distance
        data.push_back(_planets[planetIndex].moons[m].second[DistanceIndex]);

        // Horizontal Angle
        data.push_back(_planets[planetIndex].moons[m].second[HAngleIndex]);

        // Vertical Angle
        data.push_back(_planets[planetIndex].moons[m].second[VAngleIndex]);
    }

    data.shrink_to_fit();
    _connection->send(label, data);
}

void PlanetsSonification::onToggleAllChanged() {
    // Set all the settings
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
    sendSettings(MercuryIndex);
}

// Venus
void PlanetsSonification::onVenusAllChanged() {
    _venusProperty.sizeDayEnabled.setValue(_venusProperty.toggleAll);
    _venusProperty.gravityEnabled.setValue(_venusProperty.toggleAll);
    _venusProperty.temperatureEnabled.setValue(_venusProperty.toggleAll);
    _venusProperty.atmosphereEnabled.setValue(_venusProperty.toggleAll);
}
void PlanetsSonification::onVenusSettingChanged() {
    sendSettings(VenusIndex);
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
    sendSettings(EarthIndex);
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
    sendSettings(MarsIndex);
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
    sendSettings(JupiterIndex);
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
    sendSettings(SaturnIndex);
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
    sendSettings(UranusIndex);
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
    sendSettings(NeptuneIndex);
}

// Extract data from the given identifier
bool PlanetsSonification::getData(const Camera* camera, int planetIndex) {
    double distance = SonificationBase::calculateDistanceTo(
        camera,
        _planets[planetIndex].identifier,
        DistanceUnit::Kilometer
    );
    double HAngle = SonificationBase::calculateAngleTo(
        camera,
        _planets[planetIndex].identifier
    );

    double VAngle = SonificationBase::calculateElevationAngleTo(
        camera,
        _planets[planetIndex].identifier
    );

    if (std::abs(distance) < std::numeric_limits<double>::epsilon()) {
        return false;
    }

    // Also calculate angle to moons if this planet is in focus
    bool updateMoons = false;
    for (int m = 0; m < _planets[planetIndex].moons.size(); ++m) {
        // Distance
        double dist = SonificationBase::calculateDistanceTo(
            camera,
            _planets[planetIndex].moons[m].first,
            DistanceUnit::Kilometer
        );

        if (std::abs(dist) < std::numeric_limits<double>::epsilon()) {
            return false;
        }

        if (std::abs(_planets[planetIndex].moons[m].second[DistanceIndex] - dist) >
            _distancePrecision)
        {
            updateMoons = true;
            _planets[planetIndex].moons[m].second[DistanceIndex] = dist;
        }

        // Horizontal angle
        double moonHAngle = SonificationBase::calculateAngleFromAToB(
            camera,
            _planets[planetIndex].identifier,
            _planets[planetIndex].moons[m].first
        );

        if (std::abs(_planets[planetIndex].moons[m].second[HAngleIndex] - moonHAngle) >
            _anglePrecision)
        {
            updateMoons = true;
            _planets[planetIndex].moons[m].second[HAngleIndex] = moonHAngle;
        }

        // Vertical angle
        double moonVAngle = SonificationBase::calculateElevationAngleFromAToB(
            camera,
            _planets[planetIndex].identifier,
            _planets[planetIndex].moons[m].first
        );

        if (std::abs(_planets[planetIndex].moons[m].second[VAngleIndex] - moonVAngle) >
            _anglePrecision)
        {
            updateMoons = true;
            _planets[planetIndex].moons[m].second[VAngleIndex] = moonVAngle;
        }
    }

    // Check if this data is new, otherwise don't send it
    double prevDistance = _planets[planetIndex].data[DistanceIndex];
    double prevHAngle = _planets[planetIndex].data[HAngleIndex];
    double prevVAngle = _planets[planetIndex].data[VAngleIndex];

    bool shouldSendData = false;
    if (std::abs(prevDistance - distance) > _distancePrecision ||
        std::abs(prevHAngle - HAngle) > _anglePrecision ||
        std::abs(prevVAngle - VAngle) > _anglePrecision ||
        updateMoons)
    {
        // Update the saved data for the planet
        _planets[planetIndex].data[DistanceIndex] = distance;
        _planets[planetIndex].data[HAngleIndex] = HAngle;
        _planets[planetIndex].data[VAngleIndex] = VAngle;
        shouldSendData = true;
    }

    return shouldSendData;
}

void PlanetsSonification::update(const Camera* camera) {
    SonificationModule* module = global::moduleEngine->module<SonificationModule>();
    if (!module) {
        LERROR("Could not find the SonificationModule");
        return;
    }
    const SonificationBase* solar = module->sonification("SolarSonification");
    if (!solar) {
        LERROR("Could not find the SolarSonification");
        return;
    }
    const SonificationBase* compare = module->sonification("CompareSonification");
    if (!compare) {
        LERROR("Could not find the CompareSonification");
        return;
    }

    bool solarEnabled = solar->enabled();
    bool compareEnabled = compare->enabled();

    if (!_enabled && !solarEnabled && !compareEnabled) {
        return;
    }

    const SceneGraphNode* focusNode =
        global::navigationHandler->orbitalNavigator().anchorNode();

    if (!focusNode) {
        return;
    }

    // Update data for all planets
    for (int i = 0; i < _planets.size(); ++i) {
        // Increase presision if the planet is in focus
        if (focusNode->identifier() == _planets[i].identifier) {
            _anglePrecision = HighAnglePrecision;
            _distancePrecision = HighDistancePrecision;
        }
        else {
            _anglePrecision = LowAnglePrecision;
            _distancePrecision = LowDistancePrecision;
        }

        bool hasNewData = getData(camera, i);

        // Only send data if something new has happened
        if (hasNewData) {
            sendSettings(i);
        }
    }
}

void PlanetsSonification::stop() {
    _toggleAll = false;
}

void PlanetsSonification::addPlanet(ghoul::Dictionary dict) {
    const Parameters p = codegen::bake<Parameters>(dict);
    Planet planet = p.name;

    if (p.moons.has_value()) {
        for (const std::string& moon : *p.moons) {
            planet.moons.push_back({ moon, std::vector<double>(NumDataItems) });
        }
    }

    _planets.push_back(planet);
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
