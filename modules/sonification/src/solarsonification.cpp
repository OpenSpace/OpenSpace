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

#include <modules/sonification/include/solarsonification.h>

#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>

namespace {
    constexpr std::string_view _loggerCat = "SolarSonification";

    // Set the differnet levels of precision
    constexpr double DistancePrecision = 10000.0;
    constexpr double AnglePrecision = 0.1;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        SolarSonificationInfo =
    {
       "SolarSonification",
       "Solar Sonification",
       "Sonification that gives an overview of the planets in the solarsystem with "
       "simpler sounds"
    };

    constexpr openspace::properties::Property::PropertyInfo ToggleAllInfo = {
        "ToggleAll",
        "All",
        "Toggle the sonification for all planets"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableMercuryInfo = {
        "Mercury",
        "Mercury",
        "Toggle sonification for Mercury"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableVenusInfo = {
        "Venus",
        "Venus",
        "Toggle sonification for Venus"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableEarthInfo = {
        "Earth",
        "Earth",
        "Toggle sonification for Earth"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableMarsInfo = {
        "Mars",
        "Mars",
        "Toggle sonification for Mars"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableJupiterInfo = {
        "Jupiter",
        "Jupiter",
        "Toggle sonification for Jupiter"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableSaturnInfo = {
        "Saturn",
        "Saturn",
        "Toggle sonification for Saturn"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableUranusInfo = {
        "Uranus",
        "Uranus",
        "Toggle sonification for Uranus"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableNeptuneInfo = {
        "Neptune",
        "Neptune",
        "Toggle sonification for Neptune"
    };
} // namespace

namespace openspace {

SolarSonification::SolarSonification(const std::string& ip, int port)
    : SonificationBase(SolarSonificationInfo, ip, port)
    , _toggleAll(ToggleAllInfo, false)
    , _mercuryEnabled(EnableMercuryInfo, false)
    , _venusEnabled(EnableVenusInfo, false)
    , _earthEnabled(EnableEarthInfo, false)
    , _marsEnabled(EnableMarsInfo, false)
    , _jupiterEnabled(EnableJupiterInfo, false)
    , _saturnEnabled(EnableSaturnInfo, false)
    , _uranusEnabled(EnableUranusInfo, false)
    , _neptuneEnabled(EnableNeptuneInfo, false)
{
    // Fill the _planets list
    _planets.push_back(Planet("Mercury"));
    _planets.push_back(Planet("Venus"));
    _planets.push_back(Planet("Earth"));
    _planets.push_back(Planet("Mars"));
    _planets.push_back(Planet("Jupiter"));
    _planets.push_back(Planet("Saturn"));
    _planets.push_back(Planet("Uranus"));
    _planets.push_back(Planet("Neptune"));

    // Add onChange functions to the properties
    _toggleAll.onChange([this]() { onToggleAllChanged(); });
    _mercuryEnabled.onChange([this]() { sendSettings(); });
    _venusEnabled.onChange([this]() { sendSettings(); });
    _earthEnabled.onChange([this]() { sendSettings(); });
    _marsEnabled.onChange([this]() { sendSettings(); });
    _jupiterEnabled.onChange([this]() { sendSettings(); });
    _saturnEnabled.onChange([this]() { sendSettings(); });
    _uranusEnabled.onChange([this]() { sendSettings(); });
    _neptuneEnabled.onChange([this]() { sendSettings(); });

    // Add the properties
    addProperty(_toggleAll);
    addProperty(_mercuryEnabled);
    addProperty(_venusEnabled);
    addProperty(_earthEnabled);
    addProperty(_marsEnabled);
    addProperty(_jupiterEnabled);
    addProperty(_saturnEnabled);
    addProperty(_uranusEnabled);
    addProperty(_neptuneEnabled);
}

SolarSonification::~SolarSonification() {
    stop();
}

osc::Blob SolarSonification::createSettingsBlob() const {
    bool settings[8] = { false };

    settings[0] = _mercuryEnabled;
    settings[1] = _venusEnabled;
    settings[2] = _earthEnabled;
    settings[3] = _marsEnabled;
    settings[4] = _jupiterEnabled;
    settings[5] = _saturnEnabled;
    settings[6] = _uranusEnabled;
    settings[7] = _neptuneEnabled;

    return osc::Blob(settings, 8);
}

void SolarSonification::sendSettings() {
    std::string label = "/Sun";
    std::vector<OscDataType> data(1);

    data[0] = createSettingsBlob();

    _connection->send(label, data);
}

void SolarSonification::onToggleAllChanged() {
    _mercuryEnabled.setValue(_toggleAll);
    _venusEnabled.setValue(_toggleAll);
    _earthEnabled.setValue(_toggleAll);
    _marsEnabled.setValue(_toggleAll);
    _jupiterEnabled.setValue(_toggleAll);
    _saturnEnabled.setValue(_toggleAll);
    _uranusEnabled.setValue(_toggleAll);
    _neptuneEnabled.setValue(_toggleAll);
}

// Extract the data from the given identifier
bool SolarSonification::getData(const Camera* camera, Planet& planet) {
    double distance = SonificationBase::calculateDistanceTo(
        camera,
        planet.identifier,
        DistanceUnit::Kilometer
    );
    double angle =
        SonificationBase::calculateAngleFromAToB(camera, "Sun", planet.identifier);

    if (abs(distance) < std::numeric_limits<double>::epsilon()) {
        return false;
    }

    // Check if this data is new, otherwise don't send it
    bool isNewData = false;
    if (abs(planet.distance - distance) > DistancePrecision ||
        abs(planet.angle - angle) > AnglePrecision)
    {
        // Update the saved data for the planet
        planet.distance = distance;
        planet.angle = angle;
        isNewData = true;
    }
    return isNewData;
}

void SolarSonification::update(const Camera* camera) {
    if (!_enabled) {
        return;
    }

    // Update data for all planets
    for (Planet& planet : _planets) {
        bool hasDataUpdated = getData(camera, planet);

        // Only send data if something new has happened
        if (hasDataUpdated) {
            sendSettings();
        }
    }
}

void SolarSonification::stop() {
    _toggleAll = false;
}

} // namespace openspace
