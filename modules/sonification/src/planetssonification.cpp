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

#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/distanceconversion.h>

namespace {
    constexpr std::string_view _loggerCat = "PlanetSonification";

    // Set the differnet levels of precision
    constexpr double LowDistancePrecision = 10000.0;
    constexpr double HighDistancePrecision = 1000.0;
    constexpr double LowAnglePrecision = 0.1;
    constexpr double HighAnglePrecision = 0.05;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        PlanetSonificationInfo =
    {
       "PlanetSonification",
       "Planet Sonification",
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
} // namespace

namespace openspace {

PlanetsSonification::PlanetsSonification(const std::string& ip, int port)
    : SonificationBase(PlanetSonificationInfo, ip, port)
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

    // Fill the _planets list
    _planets.reserve(8);
    _planets.push_back(Planet("Mercury"));
    _planets.push_back(Planet("Venus"));

    _planets.push_back(Planet("Earth"));
    _planets.back().moons.reserve(1);
    _planets.back().moons.push_back({ "Moon", 0.0 });

    _planets.push_back(Planet("Mars"));
    _planets.back().moons.reserve(2);
    _planets.back().moons.push_back({ "Phobos", 0.0 });
    _planets.back().moons.push_back({ "Deimos", 0.0 });

    _planets.push_back(Planet("Jupiter"));
    _planets.back().moons.reserve(4);
    _planets.back().moons.push_back({ "Io", 0.0 });
    _planets.back().moons.push_back({ "Europa", 0.0 });
    _planets.back().moons.push_back({ "Ganymede", 0.0 });
    _planets.back().moons.push_back({ "Callisto", 0.0 });

    _planets.push_back(Planet("Saturn"));
    _planets.back().moons.reserve(8);
    _planets.back().moons.push_back({ "Dione", 0.0 });
    _planets.back().moons.push_back({ "Enceladus", 0.0 });
    _planets.back().moons.push_back({ "Hyperion", 0.0 });
    _planets.back().moons.push_back({ "Iapetus", 0.0 });
    _planets.back().moons.push_back({ "Mimas", 0.0 });
    _planets.back().moons.push_back({ "Rhea", 0.0 });
    _planets.back().moons.push_back({ "Tethys", 0.0 });
    _planets.back().moons.push_back({ "Titan", 0.0 });

    _planets.push_back(Planet("Uranus"));
    _planets.back().moons.reserve(5);
    _planets.back().moons.push_back({ "Ariel", 0.0 });
    _planets.back().moons.push_back({ "Miranda", 0.0 });
    _planets.back().moons.push_back({ "Oberon", 0.0 });
    _planets.back().moons.push_back({ "Titania", 0.0 });
    _planets.back().moons.push_back({ "Umbriel", 0.0 });

    _planets.push_back(Planet("Neptune"));
    _planets.back().moons.reserve(1);
    _planets.back().moons.push_back({ "Triton", 0.0 });

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

    switch (planetIndex) {
        case 0:
            // Mercury
            settings[0] = _mercuryProperty.sizeDayEnabled;
            settings[1] = _mercuryProperty.gravityEnabled;
            settings[2] = _mercuryProperty.temperatureEnabled;
            break;
        case 1:
            // Venus
            settings[0] = _venusProperty.sizeDayEnabled;
            settings[1] = _venusProperty.gravityEnabled;
            settings[2] = _venusProperty.temperatureEnabled;
            settings[3] = _venusProperty.atmosphereEnabled;
            break;
        case 2:
            // Earth
            settings[0] = _earthProperty.sizeDayEnabled;
            settings[1] = _earthProperty.gravityEnabled;
            settings[2] = _earthProperty.temperatureEnabled;
            settings[3] = _earthProperty.atmosphereEnabled;
            settings[4] = _earthProperty.moonsEnabled;
            break;
        case 3:
            // Mars
            settings[0] = _marsProperty.sizeDayEnabled;
            settings[1] = _marsProperty.gravityEnabled;
            settings[2] = _marsProperty.temperatureEnabled;
            settings[3] = _marsProperty.atmosphereEnabled;
            settings[4] = _marsProperty.moonsEnabled;
            break;
        case 4:
            // Jupiter
            settings[0] = _jupiterProperty.sizeDayEnabled;
            settings[1] = _jupiterProperty.gravityEnabled;
            settings[2] = _jupiterProperty.temperatureEnabled;
            settings[3] = _jupiterProperty.atmosphereEnabled;
            settings[4] = _jupiterProperty.moonsEnabled;
            break;
        case 5:
            // Saturn
            settings[0] = _saturnProperty.sizeDayEnabled;
            settings[1] = _saturnProperty.gravityEnabled;
            settings[2] = _saturnProperty.temperatureEnabled;
            settings[3] = _saturnProperty.atmosphereEnabled;
            settings[4] = _saturnProperty.moonsEnabled;
            settings[5] = _saturnProperty.ringsEnabled;
            break;
        case 6:
            // Uranus
            settings[0] = _uranusProperty.sizeDayEnabled;
            settings[1] = _uranusProperty.gravityEnabled;
            settings[2] = _uranusProperty.temperatureEnabled;
            settings[3] = _uranusProperty.atmosphereEnabled;
            settings[4] = _uranusProperty.moonsEnabled;
            break;
        case 7:
            // Neptune
            settings[0] = _neptuneProperty.sizeDayEnabled;
            settings[1] = _neptuneProperty.gravityEnabled;
            settings[2] = _neptuneProperty.temperatureEnabled;
            settings[3] = _neptuneProperty.atmosphereEnabled;
            settings[4] = _neptuneProperty.moonsEnabled;
            break;
        default:
            throw ghoul::MissingCaseException();
    }

    return osc::Blob(settings, 6);
}

void PlanetsSonification::sendSettings(int planetIndex) {
    std::string label = "/" + _planets[planetIndex].identifier;
    std::vector<OscDataType> data;

    // Distance
    data.push_back(_planets[planetIndex].distance);

    // Angle
    data.push_back(_planets[planetIndex].angle);

    // Settings
    osc::Blob settingsBlob = createSettingsBlob(planetIndex);
    data.push_back(settingsBlob);

    // Moons
    for (size_t m = 0; m < _planets[planetIndex].moons.size(); ++m) {
        data.push_back(_planets[planetIndex].moons[m].second);
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
    sendSettings(0);
}

// Venus
void PlanetsSonification::onVenusAllChanged() {
    _venusProperty.sizeDayEnabled.setValue(_venusProperty.toggleAll);
    _venusProperty.gravityEnabled.setValue(_venusProperty.toggleAll);
    _venusProperty.temperatureEnabled.setValue(_venusProperty.toggleAll);
    _venusProperty.atmosphereEnabled.setValue(_venusProperty.toggleAll);
}
void PlanetsSonification::onVenusSettingChanged() {
    sendSettings(1);
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
    sendSettings(2);
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
    sendSettings(3);
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
    sendSettings(4);
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
    sendSettings(5);
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
    sendSettings(6);
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
    sendSettings(7);
}

// Extract data from the given identifier
bool PlanetsSonification::getData(const Camera* camera, int planetIndex) {
    double distance = SonificationBase::calculateDistanceTo(
        camera,
        _planets[planetIndex].identifier,
        DistanceUnit::Kilometer
    );
    double angle =
        SonificationBase::calculateAngleTo(camera, _planets[planetIndex].identifier);

    if (abs(distance) < std::numeric_limits<double>::epsilon()) {
        return false;
    }

    // Also calculate angle to moons if this planet is in focus
    bool updateMoons = false;
    for (int m = 0; m < _planets[planetIndex].moons.size(); ++m) {
        double moonAngle = SonificationBase::calculateAngleFromAToB(
            camera,
            _planets[planetIndex].identifier,
            _planets[planetIndex].moons[m].first
        );

        if (abs(_planets[planetIndex].moons[m].second - moonAngle) > _anglePrecision) {
            updateMoons = true;
            _planets[planetIndex].moons[m].second = moonAngle;
        }
    }

    // Check if this data is new, otherwise don't send it
    bool shouldSendData = false;
    if (abs(_planets[planetIndex].distance - distance) > _distancePrecision ||
        abs(_planets[planetIndex].angle - angle) > _anglePrecision ||
        updateMoons)
    {
        // Update the saved data for the planet
        _planets[planetIndex].distance = distance;
        _planets[planetIndex].angle = angle;
        shouldSendData = true;
    }

    return shouldSendData;
}

void PlanetsSonification::update(const Camera* camera) {
    if (!_enabled) {
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

} // namespace openspace
