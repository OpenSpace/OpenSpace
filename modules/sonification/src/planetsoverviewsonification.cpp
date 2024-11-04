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

#include <modules/sonification/include/planetsoverviewsonification.h>

#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/util/memorymanager.h>

namespace {
    constexpr std::string_view _loggerCat = "PlanetsOverviewSonification";

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        PlanetsOverviewSonificationInfo =
    {
        "PlanetsOverviewSonification",
        "Planets Overview Sonification",
        "Sonification that gives an overview of the planets in the overviewsystem with "
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

PlanetsOverviewSonification::PlanetsOverviewSonification(const std::string& ip, int port)
    : SonificationBase(PlanetsOverviewSonificationInfo, ip, port)
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
    _mercuryEnabled.onChange([this]() { sendData(); });
    _venusEnabled.onChange([this]() { sendData(); });
    _earthEnabled.onChange([this]() { sendData(); });
    _marsEnabled.onChange([this]() { sendData(); });
    _jupiterEnabled.onChange([this]() { sendData(); });
    _saturnEnabled.onChange([this]() { sendData(); });
    _uranusEnabled.onChange([this]() { sendData(); });
    _neptuneEnabled.onChange([this]() { sendData(); });

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

PlanetsOverviewSonification::~PlanetsOverviewSonification() {
    stop();
}

osc::Blob PlanetsOverviewSonification::createSettingsBlob() const {
    int8_t* settings = reinterpret_cast<int8_t*>(
        global::memoryManager->TemporaryMemory.allocate(NumPlanets)
    );

    settings[MercuryIndex] = _mercuryEnabled;
    settings[VenusIndex] = _venusEnabled;
    settings[EarthIndex] = _earthEnabled;
    settings[MarsIndex] = _marsEnabled;
    settings[JupiterIndex] = _jupiterEnabled;
    settings[SaturnIndex] = _saturnEnabled;
    settings[UranusIndex] = _uranusEnabled;
    settings[NeptuneIndex] = _neptuneEnabled;

    return osc::Blob(settings, NumPlanets);
}

void PlanetsOverviewSonification::sendData() {
    if (!_enabled) {
        return;
    }

    std::string label = "/Overview";
    std::vector<OscDataType> data(NumDataItems);

    data[GuiSettingsIndex] = createSettingsBlob();

    _connection->send(label, data);
}

void PlanetsOverviewSonification::onToggleAllChanged() {
    _mercuryEnabled.setValue(_toggleAll);
    _venusEnabled.setValue(_toggleAll);
    _earthEnabled.setValue(_toggleAll);
    _marsEnabled.setValue(_toggleAll);
    _jupiterEnabled.setValue(_toggleAll);
    _saturnEnabled.setValue(_toggleAll);
    _uranusEnabled.setValue(_toggleAll);
    _neptuneEnabled.setValue(_toggleAll);
}

void PlanetsOverviewSonification::update(const Camera*) {}

void PlanetsOverviewSonification::stop() {
    _toggleAll = false;
}

} // namespace openspace
