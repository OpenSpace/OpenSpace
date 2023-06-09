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

#define _USE_MATH_DEFINES

#include <modules/sonification/include/planetssonification.h>

#include <modules/sonification/sonificationmodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/distanceconversion.h>
#include <math.h>

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

    constexpr openspace::properties::Property::PropertyInfo LowDistancePrecisionInfo = {
        "LowDistancePrecision",
        "Low Distance Precision",
        "The lower precision used for distances of planets that are NOT in focus, "
        "given in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo HighDistancePrecisionInfo = {
        "HighDistancePrecision",
        "High Distance Precision",
        "The higher precision used for distances of planets that are in focus, "
        "given in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo LowAnglePrecisionInfo = {
        "LowAnglePrecision",
        "Low Angle Precision",
        "The lower precision used for angles of planets that are NOT in focus, "
        "given in radians"
    };

    constexpr openspace::properties::Property::PropertyInfo HighAnglePrecisionInfo = {
        "HighAnglePrecision",
        "High Angle Precision",
        "The lower precision used for angles of planets that are in focus, "
        "given in radians"
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
    , _lowDistancePrecision(LowDistancePrecisionInfo, 1.0e4, 1.0e3, 1.0e9)
    , _highDistancePrecision(HighDistancePrecisionInfo, 1.0e3, 1.0e2, 1.0e6)
    , _lowAnglePrecision(LowAnglePrecisionInfo, 0.1, 0.1, M_PI/2.0)
    , _highAnglePrecision(HighAnglePrecisionInfo, 0.05, 0.0, M_PI/4.0)
    , _mercuryProperty(PlanetsSonification::PlanetProperty(MercuryInfo))
    , _venusProperty(PlanetsSonification::PlanetProperty(VenusInfo))
    , _earthProperty(PlanetsSonification::PlanetProperty(EarthInfo))
    , _marsProperty(PlanetsSonification::PlanetProperty(MarsInfo))
    , _jupiterProperty(PlanetsSonification::PlanetProperty(JupiterInfo))
    , _saturnProperty(PlanetsSonification::PlanetProperty(SaturnInfo))
    , _uranusProperty(PlanetsSonification::PlanetProperty(UranusInfo))
    , _neptuneProperty(PlanetsSonification::PlanetProperty(NeptuneInfo))
{
    _anglePrecision = _lowAnglePrecision;
    _distancePrecision = _lowDistancePrecision;

    // Add onChange for the properties
    _enabled.onChange([this]() { onEnabledChanged(); });
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
    addProperty(_lowDistancePrecision);
    _lowDistancePrecision.setExponent(3);
    addProperty(_highDistancePrecision);
    _highDistancePrecision.setExponent(3);
    addProperty(_lowAnglePrecision);
    addProperty(_highAnglePrecision);

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

std::vector<int> PlanetsSonification::createSettingsVector(int planetIndex) const {
    std::vector<int> settings(NumSettings, false);

    if (planetIndex == MercuryIndex) {
        // Mercury
        settings[SizeDayIndex] = static_cast<int>(_mercuryProperty.sizeDayEnabled.value());
        settings[GravityIndex] = static_cast<int>(_mercuryProperty.gravityEnabled.value());
        settings[TemperatureIndex] = static_cast<int>(_mercuryProperty.temperatureEnabled.value());
    }
    else if (planetIndex == VenusIndex) {
        // Venus
        settings[SizeDayIndex] = static_cast<int>(_venusProperty.sizeDayEnabled.value());
        settings[GravityIndex] = static_cast<int>(_venusProperty.gravityEnabled.value());
        settings[TemperatureIndex] = static_cast<int>(_venusProperty.temperatureEnabled.value());
        settings[AtmosphereIndex] = static_cast<int>(_venusProperty.atmosphereEnabled.value());
    }
    else if (planetIndex == EarthIndex) {
        // Earth
        settings[SizeDayIndex] = static_cast<int>(_earthProperty.sizeDayEnabled.value());
        settings[GravityIndex] = static_cast<int>(_earthProperty.gravityEnabled.value());
        settings[TemperatureIndex] = static_cast<int>(_earthProperty.temperatureEnabled.value());
        settings[AtmosphereIndex] = static_cast<int>(_earthProperty.atmosphereEnabled.value());
        settings[MoonsIndex] = static_cast<int>(_earthProperty.moonsEnabled.value());
    }
    else if (planetIndex == MarsIndex) {
        // Mars
        settings[SizeDayIndex] = static_cast<int>(_marsProperty.sizeDayEnabled.value());
        settings[GravityIndex] = static_cast<int>(_marsProperty.gravityEnabled.value());
        settings[TemperatureIndex] = static_cast<int>(_marsProperty.temperatureEnabled.value());
        settings[AtmosphereIndex] = static_cast<int>(_marsProperty.atmosphereEnabled.value());
        settings[MoonsIndex] = static_cast<int>(_marsProperty.moonsEnabled.value());
    }
    else if (planetIndex == JupiterIndex) {
        // Jupiter
        settings[SizeDayIndex] = static_cast<int>(_jupiterProperty.sizeDayEnabled.value());
        settings[GravityIndex] = static_cast<int>(_jupiterProperty.gravityEnabled.value());
        settings[TemperatureIndex] = static_cast<int>(_jupiterProperty.temperatureEnabled.value());
        settings[AtmosphereIndex] = static_cast<int>(_jupiterProperty.atmosphereEnabled.value());
        settings[MoonsIndex] = static_cast<int>(_jupiterProperty.moonsEnabled.value());
    }
    else if (planetIndex == SaturnIndex) {
        // Saturn
        settings[SizeDayIndex] = static_cast<int>(_saturnProperty.sizeDayEnabled.value());
        settings[GravityIndex] = static_cast<int>(_saturnProperty.gravityEnabled.value());
        settings[TemperatureIndex] = static_cast<int>(_saturnProperty.temperatureEnabled.value());
        settings[AtmosphereIndex] = static_cast<int>(_saturnProperty.atmosphereEnabled.value());
        settings[MoonsIndex] = static_cast<int>(_saturnProperty.moonsEnabled.value());
        settings[RingsIndex] = static_cast<int>(_saturnProperty.ringsEnabled.value());
    }
    else if (planetIndex == UranusIndex) {
        // Uranus
        settings[SizeDayIndex] = static_cast<int>(_uranusProperty.sizeDayEnabled.value());
        settings[GravityIndex] = static_cast<int>(_uranusProperty.gravityEnabled.value());
        settings[TemperatureIndex] = static_cast<int>(_uranusProperty.temperatureEnabled.value());
        settings[AtmosphereIndex] = static_cast<int>(_uranusProperty.atmosphereEnabled.value());
        settings[MoonsIndex] = static_cast<int>(_uranusProperty.moonsEnabled.value());
    }
    else if (planetIndex == NeptuneIndex) {
        // Neptune
        settings[SizeDayIndex] = static_cast<int>(_neptuneProperty.sizeDayEnabled.value());
        settings[GravityIndex] = static_cast<int>(_neptuneProperty.gravityEnabled.value());
        settings[TemperatureIndex] = static_cast<int>(_neptuneProperty.temperatureEnabled.value());
        settings[AtmosphereIndex] = static_cast<int>(_neptuneProperty.atmosphereEnabled.value());
        settings[MoonsIndex] = static_cast<int>(_neptuneProperty.moonsEnabled.value());
    }
    else {
        throw ghoul::MissingCaseException();
    }

    return settings;
}

void PlanetsSonification::sendSettings(int planetIndex) {
    if (_planets.size() <= planetIndex) {
        LWARNING(fmt::format("Planet list does not include index {}", planetIndex));
        return;
    }

    std::string label = "/" + _planets[planetIndex].identifier;
    std::vector<OscDataType> data;

    // Distance
    data.push_back(_planets[planetIndex].distance());

    // Horizontal Angle
    data.push_back(_planets[planetIndex].HAngle());

    // Vertical Angle
    data.push_back(_planets[planetIndex].VAngle());

    // Settings
    std::vector<int> settingsBlob = createSettingsVector(planetIndex);
    data.push_back(settingsBlob);

    // Moons
    for (size_t m = 0; m < _planets[planetIndex].moons.size(); ++m) {
        // Horizontal Angle
        data.push_back(_planets[planetIndex].moons[m].HAngle());

        // Vertical Angle
        data.push_back(_planets[planetIndex].moons[m].VAngle());
    }

    data.shrink_to_fit();
    _connection->send(label, data);
}

void PlanetsSonification::onEnabledChanged() {
    if (_enabled) {
        // Check if any other "main" sonification is already on and turn them off
        SonificationModule* module = global::moduleEngine->module<SonificationModule>();
        if (!module) {
            LERROR("Could not find the SonificationModule");
            return;
        }

        // Solar
        SonificationBase* solar = module->sonification("SolarSonification");
        if (!solar) {
            LERROR("Could not find the SolarSonification");
            return;
        }
        if (solar->enabled()) {
            solar->setEnabled(false);
            LINFO(
                "Turning off the Solar sonification in favor for the Planets sonification"
            );
        }

        // Compare
        SonificationBase* compare = module->sonification("CompareSonification");
        if (!compare) {
            LERROR("Could not find the CompareSonification");
            return;
        }
        if (compare->enabled()) {
            compare->setEnabled(false);
            LINFO(
                "Turning off the Compare sonification in favor for the Planets "
                "sonification"
            );
        }
    }
    else {
        stop();
    }
}

void PlanetsSonification::onToggleAllChanged() {
    setAll(_toggleAll);
}

// Mercury
void PlanetsSonification::onMercuryAllChanged() {
    setAllMercury(_mercuryProperty.toggleAll);
}
void PlanetsSonification::onMercurySettingChanged() {
    sendSettings(MercuryIndex);
}

// Venus
void PlanetsSonification::onVenusAllChanged() {
    setAllVenus(_venusProperty.toggleAll);
}
void PlanetsSonification::onVenusSettingChanged() {
    sendSettings(VenusIndex);
}

// Earth
void PlanetsSonification::onEarthAllChanged() {
    setAllEarth(_earthProperty.toggleAll);
}
void PlanetsSonification::onEarthSettingChanged() {
    sendSettings(EarthIndex);
}

// Mars
void PlanetsSonification::onMarsAllChanged() {
    setAllMars(_marsProperty.toggleAll);
}
void PlanetsSonification::onMarsSettingChanged() {
    sendSettings(MarsIndex);
}

// Jupiter
void PlanetsSonification::onJupiterAllChanged() {
    setAllJupiter(_jupiterProperty.toggleAll);
}
void PlanetsSonification::onJupiterSettingChanged() {
    sendSettings(JupiterIndex);
}

// Saturn
void PlanetsSonification::onSaturnAllChanged() {
    setAllSaturn(_saturnProperty.toggleAll);
}
void PlanetsSonification::onSaturnSettingChanged() {
    sendSettings(SaturnIndex);
}

// Uranus
void PlanetsSonification::onUranusAllChanged() {
    setAllUranus(_uranusProperty.toggleAll);
}
void PlanetsSonification::onUranusSettingChanged() {
    sendSettings(UranusIndex);
}

// Neptune
void PlanetsSonification::onNeptuneAllChanged() {
    setAllNeptune(_neptuneProperty.toggleAll);
}
void PlanetsSonification::onNeptuneSettingChanged() {
    sendSettings(NeptuneIndex);
}

// Set functions for all planets
void PlanetsSonification::setAll(bool value) {
    // Set all the settings
    _mercuryProperty.toggleAll.setValue(value);
    setAllMercury(value);

    _venusProperty.toggleAll.setValue(value);
    setAllVenus(value);

    _earthProperty.toggleAll.setValue(value);
    setAllEarth(value);

    _marsProperty.toggleAll.setValue(value);
    setAllMars(value);

    _jupiterProperty.toggleAll.setValue(value);
    setAllJupiter(value);

    _saturnProperty.toggleAll.setValue(value);
    setAllSaturn(value);

    _uranusProperty.toggleAll.setValue(value);
    setAllUranus(value);

    _neptuneProperty.toggleAll.setValue(value);
    setAllNeptune(value);
}

void PlanetsSonification::setAllMercury(bool value) {
    _mercuryProperty.sizeDayEnabled.setValue(value);
    _mercuryProperty.gravityEnabled.setValue(value);
    _mercuryProperty.temperatureEnabled.setValue(value);
}

void PlanetsSonification::setAllVenus(bool value) {
    _venusProperty.sizeDayEnabled.setValue(value);
    _venusProperty.gravityEnabled.setValue(value);
    _venusProperty.temperatureEnabled.setValue(value);
    _venusProperty.atmosphereEnabled.setValue(value);
}

void PlanetsSonification::setAllEarth(bool value) {
    _earthProperty.sizeDayEnabled.setValue(value);
    _earthProperty.gravityEnabled.setValue(value);
    _earthProperty.temperatureEnabled.setValue(value);
    _earthProperty.atmosphereEnabled.setValue(value);
    _earthProperty.moonsEnabled.setValue(value);
}

void PlanetsSonification::setAllMars(bool value) {
    _marsProperty.sizeDayEnabled.setValue(value);
    _marsProperty.gravityEnabled.setValue(value);
    _marsProperty.temperatureEnabled.setValue(value);
    _marsProperty.atmosphereEnabled.setValue(value);
    _marsProperty.moonsEnabled.setValue(value);
}

void PlanetsSonification::setAllJupiter(bool value) {
    _jupiterProperty.sizeDayEnabled.setValue(value);
    _jupiterProperty.gravityEnabled.setValue(value);
    _jupiterProperty.temperatureEnabled.setValue(value);
    _jupiterProperty.atmosphereEnabled.setValue(value);
    _jupiterProperty.moonsEnabled.setValue(value);
}

void PlanetsSonification::setAllSaturn(bool value) {
    _saturnProperty.sizeDayEnabled.setValue(value);
    _saturnProperty.gravityEnabled.setValue(value);
    _saturnProperty.temperatureEnabled.setValue(value);
    _saturnProperty.atmosphereEnabled.setValue(value);
    _saturnProperty.moonsEnabled.setValue(value);
    _saturnProperty.ringsEnabled.setValue(value);
}

void PlanetsSonification::setAllUranus(bool value) {
    _uranusProperty.sizeDayEnabled.setValue(value);
    _uranusProperty.gravityEnabled.setValue(value);
    _uranusProperty.temperatureEnabled.setValue(value);
    _uranusProperty.atmosphereEnabled.setValue(value);
    _uranusProperty.moonsEnabled.setValue(value);
}

void PlanetsSonification::setAllNeptune(bool value) {
    _neptuneProperty.sizeDayEnabled.setValue(value);
    _neptuneProperty.gravityEnabled.setValue(value);
    _neptuneProperty.temperatureEnabled.setValue(value);
    _neptuneProperty.atmosphereEnabled.setValue(value);
    _neptuneProperty.moonsEnabled.setValue(value);
}

// Extract data from the given identifier
bool PlanetsSonification::getData(const Camera* camera, int planetIndex) {
    SonificationModule* module = global::moduleEngine->module<SonificationModule>();
    if (!module) {
        LERROR("Could not find the SonificationModule");
        return 0.0;
    }
    SonificationModule::SurroundMode mode = module->surroundMode();

    // Distance
    double distance = SonificationBase::calculateDistanceTo(
        camera,
        _planets[planetIndex].identifier,
        DistanceUnit::Kilometer
    );

    if (std::abs(distance) < std::numeric_limits<double>::epsilon()) {
        return false;
    }

    // Angle
    double HAngle = 0.0, VAngle = 0.0;
    bool inACircularMode =
        mode == SonificationModule::SurroundMode::Circular ||
        mode == SonificationModule::SurroundMode::CircularWithElevation;
    // Don't do solar perspective when in circular surround mode
    if (_inSolarPerspective && !inACircularMode) {
        HAngle = SonificationBase::calculateAngleFromAToB(
            camera,
            "Sun",
            _planets[planetIndex].identifier
        );

        VAngle = SonificationBase::calculateElevationAngleFromAToB(
            camera,
            "Sun",
            _planets[planetIndex].identifier
        );
    }
    else {
        HAngle = SonificationBase::calculateAngleTo(
            camera,
            _planets[planetIndex].identifier
        );

        VAngle = SonificationBase::calculateElevationAngleTo(
            camera,
            _planets[planetIndex].identifier
        );
    }

    // Moons
    // Also calculate angle to moons if this planet is in focus
    bool updateMoons = false;
    for (int m = 0; m < _planets[planetIndex].moons.size(); ++m) {
        // Angles
        double moonHAngle = 0.0, moonVAngle = 0.0;
        if (mode == SonificationModule::SurroundMode::Horizontal ||
            mode == SonificationModule::SurroundMode::HorizontalWithElevation)
        {
            moonHAngle = SonificationBase::calculateAngleFromAToB(
                camera,
                _planets[planetIndex].identifier,
                _planets[planetIndex].moons[m].identifier
            );

            moonVAngle = SonificationBase::calculateElevationAngleFromAToB(
                camera,
                _planets[planetIndex].identifier,
                _planets[planetIndex].moons[m].identifier
            );
        }
        // Don't do "You are the planet"-perspective when in circular surround mode
        else if (mode == SonificationModule::SurroundMode::Circular ||
                 mode == SonificationModule::SurroundMode::CircularWithElevation)
        {
            moonHAngle = SonificationBase::calculateAngleTo(
                camera,
                _planets[planetIndex].moons[m].identifier
            );

            moonVAngle = SonificationBase::calculateElevationAngleTo(
                camera,
                _planets[planetIndex].moons[m].identifier
            );
        }

        if (std::abs(_planets[planetIndex].moons[m].HAngle() - moonHAngle) >
            _anglePrecision)
        {
            updateMoons = true;
            _planets[planetIndex].moons[m].addHAngle(moonHAngle);
        }

        if (std::abs(_planets[planetIndex].moons[m].VAngle() - moonVAngle) >
            _anglePrecision)
        {
            updateMoons = true;
            _planets[planetIndex].moons[m].addVAngle(moonVAngle);
        }
    }

    // Check if this data is new, otherwise don't send it
    double prevDistance = _planets[planetIndex].distance();
    double prevHAngle = _planets[planetIndex].HAngle();
    double prevVAngle = _planets[planetIndex].VAngle();

    bool shouldSendData = false;
    if (std::abs(prevDistance - distance) > _distancePrecision ||
        std::abs(prevHAngle - HAngle) > _anglePrecision ||
        std::abs(prevVAngle - VAngle) > _anglePrecision ||
        updateMoons)
    {
        // Update the saved data for the planet
        _planets[planetIndex].addDistance(distance);
        _planets[planetIndex].addHAngle(HAngle);
        _planets[planetIndex].addVAngle(VAngle);
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

    if (!_enabled && !solarEnabled && !compareEnabled && !_isTurningOff) {
        return;
    }
    else if (!_enabled && (solarEnabled || compareEnabled)) {
        _inSolarPerspective = true;
    }
    else {
        _inSolarPerspective = false;
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
            _anglePrecision = _highAnglePrecision;
            _distancePrecision = _highDistancePrecision;
        }
        else {
            _anglePrecision = _lowDistancePrecision;
            _distancePrecision = _lowAnglePrecision;
        }

        bool hasNewData = getData(camera, i);

        // Only send data if something new has happened
        if (hasNewData) {
            sendSettings(i);
        }
    }

    if (_isTurningOff) {
        _isTurningOff = false;
    }
}

void PlanetsSonification::stop() {
    _isTurningOff = true;
    setAll(false);
}

void PlanetsSonification::addPlanet(ghoul::Dictionary dict) {
    const Parameters p = codegen::bake<Parameters>(dict);
    Planet planet = p.name;

    if (p.moons.has_value()) {
        for (const std::string& moon : *p.moons) {
            planet.moons.push_back({ moon });
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
