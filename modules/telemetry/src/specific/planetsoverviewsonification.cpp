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

#include <modules/telemetry/include/specific/planetsoverviewsonification.h>

#include <openspace/engine/globals.h>
#include <openspace/util/memorymanager.h>

namespace {
    // Indices for data items
    constexpr int NumDataItems = 1;
    constexpr int GuiSettingsIndex = 0;

    // Indices for the planets
    constexpr int NumPlanets = 8;
    constexpr int MercuryIndex = 0;
    constexpr int VenusIndex = 1;
    constexpr int EarthIndex = 2;
    constexpr int MarsIndex = 3;
    constexpr int JupiterIndex = 4;
    constexpr int SaturnIndex = 5;
    constexpr int UranusIndex = 6;
    constexpr int NeptuneIndex = 7;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        PlanetsOverviewSonificationInfo =
    {
        "PlanetsOverviewSonification",
        "Planets Overview Sonification",
        "Sonification that gives an overview of the planets in the Solar System with "
        "fewer audible features for each planet compared to the full planets "
        "sonification."
    };

    constexpr openspace::properties::Property::PropertyInfo ToggleAllInfo = {
        "ToggleAll",
        "Toggle All",
        "Toggle the sonification for all planets.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableMercuryInfo = {
        "Mercury",
        "Mercury",
        "Toggle sonification for Mercury.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableVenusInfo = {
        "Venus",
        "Venus",
        "Toggle sonification for Venus.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableEarthInfo = {
        "Earth",
        "Earth",
        "Toggle sonification for Earth.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableMarsInfo = {
        "Mars",
        "Mars",
        "Toggle sonification for Mars.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableJupiterInfo = {
        "Jupiter",
        "Jupiter",
        "Toggle sonification for Jupiter.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableSaturnInfo = {
        "Saturn",
        "Saturn",
        "Toggle sonification for Saturn.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableUranusInfo = {
        "Uranus",
        "Uranus",
        "Toggle sonification for Uranus.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableNeptuneInfo = {
        "Neptune",
        "Neptune",
        "Toggle sonification for Neptune.",
        openspace::properties::Property::Visibility::User
    };
} // namespace

namespace openspace {

PlanetsOverviewSonification::PlanetsOverviewSonification(const std::string& ip, int port)
    : TelemetryBase(PlanetsOverviewSonificationInfo, ip, port)
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
    _toggleAll.onChange([this]() { onToggleAllChanged(); });
    addProperty(_toggleAll);
    _mercuryEnabled.onChange([this]() { sendData(); });
    addProperty(_mercuryEnabled);
    _venusEnabled.onChange([this]() { sendData(); });
    addProperty(_venusEnabled);
    _earthEnabled.onChange([this]() { sendData(); });
    addProperty(_earthEnabled);
    _marsEnabled.onChange([this]() { sendData(); });
    addProperty(_marsEnabled);
    _jupiterEnabled.onChange([this]() { sendData(); });
    addProperty(_jupiterEnabled);
    _saturnEnabled.onChange([this]() { sendData(); });
    addProperty(_saturnEnabled);
    _uranusEnabled.onChange([this]() { sendData(); });
    addProperty(_uranusEnabled);
    _neptuneEnabled.onChange([this]() { sendData(); });
    addProperty(_neptuneEnabled);
}

void PlanetsOverviewSonification::stop() {
    _toggleAll = false;
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

bool PlanetsOverviewSonification::updateData(const Camera*) {
    return false;
}

void PlanetsOverviewSonification::sendData() {
    if (!_enabled) {
        return;
    }

    std::string label = "/Overview";
    std::vector<OpenSoundControlDataType> data(NumDataItems);

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

} // namespace openspace
