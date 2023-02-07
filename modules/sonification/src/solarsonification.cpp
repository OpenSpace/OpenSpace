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
    constexpr int NumSecPerDay = 86400;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        SolarSonificationInfo =
    {
       "SolarSonification",
       "Solar Sonification",
       "The Solar Sonification"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableAllInfo = {
        "Enabled",
        "All",
        "Enable or disable all the soloar sonifications"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableMercuryInfo = {
        "EnableMercury",
        "Mercury",
        "Enable or disable sonification for Mercury"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableVenusInfo = {
        "EnableVenus",
        "Venus",
        "Enable or disable sonification for Venus"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableEarthInfo = {
        "EnableEarth",
        "Earth",
        "Enable or disable sonification for Earth"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableMarsInfo = {
        "EnableMars",
        "Mars",
        "Enable or disable sonification for Mars"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableJupiterInfo = {
        "EnableJupiter",
        "Jupiter",
        "Enable or disable sonification for Jupiter"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableSaturnInfo = {
        "EnableSaturn",
        "Saturn",
        "Enable or disable sonification for Saturn"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableUranusInfo = {
        "EnableUranus",
        "Uranus",
        "Enable or disable sonification for Uranus"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableNeptuneInfo = {
        "EnableNeptune",
        "Neptune",
        "Enable or disable sonification for Neptune"
    };
} // namespace

namespace openspace {

SolarSonification::SolarSonification(const std::string& ip, int port)
    : SonificationBase(SolarSonificationInfo, ip, port)
    , _enableAll(EnableAllInfo, false)
    , _mercuryEnabled(EnableMercuryInfo, false)
    , _venusEnabled(EnableVenusInfo, false)
    , _earthEnabled(EnableEarthInfo, false)
    , _marsEnabled(EnableMarsInfo, false)
    , _jupiterEnabled(EnableJupiterInfo, false)
    , _saturnEnabled(EnableSaturnInfo, false)
    , _uranusEnabled(EnableUranusInfo, false)
    , _neptuneEnabled(EnableNeptuneInfo, false)
{
    _anglePrecision = 0.1;
    _distancePrecision = 10000.0;

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
    _enableAll.onChange([this]() { onAllEnabledChanged(); });
    _mercuryEnabled.onChange([this]() { onSettingChanged(); });
    _venusEnabled.onChange([this]() { onSettingChanged(); });
    _earthEnabled.onChange([this]() { onSettingChanged(); });
    _marsEnabled.onChange([this]() { onSettingChanged(); });
    _jupiterEnabled.onChange([this]() { onSettingChanged(); });
    _saturnEnabled.onChange([this]() { onSettingChanged(); });
    _uranusEnabled.onChange([this]() { onSettingChanged(); });
    _neptuneEnabled.onChange([this]() { onSettingChanged(); });

    // Add the properties
    addProperty(_enableAll);
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
    _enableAll = false;
}

osc::Blob SolarSonification::createSettingsBlob() const {
    bool settings[8];

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

void SolarSonification::onAllEnabledChanged() {
    _mercuryEnabled = _enableAll;
    _venusEnabled = _enableAll;
    _earthEnabled = _enableAll;
    _marsEnabled = _enableAll;
    _jupiterEnabled = _enableAll;
    _saturnEnabled = _enableAll;
    _uranusEnabled = _enableAll;
    _neptuneEnabled = _enableAll;
}

void SolarSonification::onSettingChanged() {
    sendSettings();
}

// Extract the data from the given identifier
bool SolarSonification::extractData(const Camera* camera, const std::string& identifier,
                                    int i)
{
    double distance = SonificationBase::calculateDistanceTo(
        camera,
        identifier,
        DistanceUnit::Kilometer
    );
    double angle = SonificationBase::calculateAngleFromAToB(camera, "Sun", identifier);

    if (abs(distance) < std::numeric_limits<double>::epsilon()) {
        return false;
    }

    // Check if this data is new, otherwise don't send it
    bool shouldSendData = false;
    if (abs(_planets[i].distance - distance) > _distancePrecision ||
        abs(_planets[i].angle - angle) > _anglePrecision)
    {
        // Update the saved data for the planet
        _planets[i].distance = distance;
        _planets[i].angle = angle;
        shouldSendData = true;
    }
    return shouldSendData;
}

void SolarSonification::update(const Scene* scene, const Camera* camera) {
    const SceneGraphNode* focusNode = nullptr;
    const SceneGraphNode* previousFocusNode = nullptr;

    // Check what node is in focus
    focusNode = global::navigationHandler->orbitalNavigator().anchorNode();
    if (!focusNode) {
        return;
    }

    // Extract data from all the planets
    for (int i = 0; i < _planets.size(); ++i) {

        // Only send data if something new has happened
        // If the node is in focus, increase sensitivity
        if (focusNode->identifier() == _planets[i].identifier) {
            _anglePrecision = 0.05;
            _distancePrecision = 1000.0;
        }
        else {
            _anglePrecision = 0.1;
            _distancePrecision = 10000.0;
        }

        bool hasDataUpdated = extractData(camera, _planets[i].identifier, i);

        if (hasDataUpdated) {
            // Send the data to SuperCollider
            sendSettings();
        }
    }
}

} // namespace openspace
