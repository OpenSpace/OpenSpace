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

#include <modules/sonification/include/planetssolarsonification.h>

#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>

namespace {
    constexpr std::string_view _loggerCat = "PlanetsSolarSonification";

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        PlanetsSolarSonificationInfo =
    {
       "PlanetsSolarSonification",
       "Planets Solar Sonification",
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

PlanetsSolarSonification::PlanetsSolarSonification(const std::string& ip, int port)
    : SonificationBase(PlanetsSolarSonificationInfo, ip, port)
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

PlanetsSolarSonification::~PlanetsSolarSonification() {
    stop();
}

osc::Blob PlanetsSolarSonification::createSettingsBlob() const {
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

void PlanetsSolarSonification::sendSettings() {
    if (!_enabled) {
        return;
    }

    std::string label = "/Sun";
    std::vector<OscDataType> data(1);

    data[0] = createSettingsBlob();

    _connection->send(label, data);
}

void PlanetsSolarSonification::onToggleAllChanged() {
    _mercuryEnabled.setValue(_toggleAll);
    _venusEnabled.setValue(_toggleAll);
    _earthEnabled.setValue(_toggleAll);
    _marsEnabled.setValue(_toggleAll);
    _jupiterEnabled.setValue(_toggleAll);
    _saturnEnabled.setValue(_toggleAll);
    _uranusEnabled.setValue(_toggleAll);
    _neptuneEnabled.setValue(_toggleAll);
}

void PlanetsSolarSonification::update(const Camera*) {}

void PlanetsSolarSonification::stop() {
    _toggleAll = false;
}

} // namespace openspace
