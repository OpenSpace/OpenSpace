/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/telemetry/include/specific/planetscomparesonification.h>

#include <modules/opensoundcontrol/include/opensoundcontrolconnection.h>
#include <openspace/engine/globals.h>
#include <openspace/format.h>
#include <openspace/logging/logmanager.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/memorymanager.h>
#include <osc/OscTypes.h>
#include <array>
#include <cstdint>
#include <utility>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "PlanetsCompareSonification";

    // Indices for data items
    constexpr int NumDataItems = 3;
    constexpr int FirstPlanetIndex = 0;
    constexpr int SecondPlanetIndex = 1;
    constexpr int SettingsIndex = 2;

    // Indices for the GUI settings
    constexpr int NumSettings = 6;
    constexpr int SizeDayIndex = 0;
    constexpr int GravityIndex = 1;
    constexpr int TemperatureIndex = 2;
    constexpr int AtmosphereIndex = 3;
    constexpr int MoonsIndex = 4;
    constexpr int RingsIndex = 5;

    // List of planets that can be selected
    constexpr std::array<std::string_view, 9> PlanetsOptions = {
        "Choose Planet", "Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn",
        "Uranus", "Neptune"
    };

    static const PropertyOwner::PropertyOwnerInfo PlanetsCompareSonificationInfo = {
        "PlanetsCompareSonification",
        "Planets Compare Sonification",
        "Sonification that compares two different planets to each other in a variety of "
        "aspects."
    };

    constexpr DoublePropertyInfo SelectedUpscaleInfo = {
        .identifier = "SelectedUpscale",
        .name = "Selected planet upscale multiplier",
        .description = "When a planet is selected to be compared, it is also upscaled as "
            "a visual indicator of which planets are currently being compared. This "
            "property determines how much the planet is scaled up as a multiplier of the "
            "original size.",
        .visibility = Visibility::User,
        .defaults = {
            .value = 2000.0,
            .minMax = { 0.0, 1.0e+20 }
        }
    };

    constexpr DoublePropertyInfo SelectedScaleInterpolationTimeInfo = {
        .identifier = "SelectedScaleInterpolationTimeInfo",
        .name = "Selected planet scale interpolation time",
        .description = "When a planet is selected to be compared, it is also upscaled as "
            "a visual indicator of which planets are currently being compared. This "
            "property determines over how many seconds the scaling animation should "
            "play.",
        .visibility = Visibility::User,
        .defaults = {
            .value = 1.0,
            .minMax = { 0.0, 60.0 }
        }
    };

    constexpr PropertyInfo FirstOptionInfo = {
        .identifier = "FirstOption",
        .name = "Choose a planet to compare",
        .description = "Choose a planet in the list to compare to the other selected "
            "planet.",
        .visibility = Visibility::User
    };

    constexpr PropertyInfo SecondOptionInfo = {
        .identifier = "SecondOption",
        .name = "Choose another planet to compare",
        .description = "Choose another planet in the list to compare to the other "
            "selected planet.",
        .visibility = Visibility::User
    };

    constexpr BoolPropertyInfo ToggleAllInfo = {
        .identifier = "ToggleAll",
        .name = "All",
        .description = "Toggle all comparing sonification varieties for both selected "
            "planets.",
        .visibility = Visibility::User,
        .defaults = {
            .value = false
        }
    };

    constexpr BoolPropertyInfo SizeDayInfo = {
        .identifier = "SizeDay",
        .name = "Size/day",
        .description = "Toggle size/day sonification for both selected planets.",
        .visibility = Visibility::User,
        .defaults = {
            .value = false
        }
    };

    constexpr BoolPropertyInfo GravityInfo = {
        .identifier = "Gravity",
        .name = "Gravity",
        .description = "Toggle gravity sonification for both selected planets.",
        .visibility = Visibility::User,
        .defaults = {
            .value = false
        }
    };

    constexpr BoolPropertyInfo TemperatureInfo = {
        .identifier = "Temperature",
        .name = "Temperature",
        .description = "Toggle temperature sonification for both selected planets.",
        .visibility = Visibility::User,
        .defaults = {
            .value = false
        }
    };

    constexpr BoolPropertyInfo AtmosphereInfo = {
        .identifier = "Atmosphere",
        .name = "Atmosphere",
        .description = "Toggle atmosphere sonification for both selected planets.",
        .visibility = Visibility::User,
        .defaults = {
            .value = false
        }
    };

    constexpr BoolPropertyInfo MoonsInfo = {
        .identifier = "Moons",
        .name = "Moons",
        .description = "Toggle moons sonification for both selected planets.",
        .visibility = Visibility::User,
        .defaults = {
            .value = false
        }
    };

    constexpr BoolPropertyInfo RingsInfo = {
        .identifier = "Rings",
        .name = "Rings",
        .description = "Toggle rings sonification for both selected planets.",
        .visibility = Visibility::User,
        .defaults = {
            .value = false
        }
    };
} // namespace

namespace openspace {

PlanetsCompareSonification::PlanetsCompareSonification(const std::string& ip, int port)
    : TelemetryBase(PlanetsCompareSonificationInfo, ip, port)
    , _selectedUpscale(SelectedUpscaleInfo)
    , _selectedScaleInterpolationTime(SelectedScaleInterpolationTimeInfo)
    , _firstPlanet(FirstOptionInfo)
    , _secondPlanet(SecondOptionInfo)
    , _toggleAll(ToggleAllInfo)
    , _sizeDayEnabled(SizeDayInfo)
    , _gravityEnabled(GravityInfo)
    , _temperatureEnabled(TemperatureInfo)
    , _atmosphereEnabled(AtmosphereInfo)
    , _moonsEnabled(MoonsInfo)
    , _ringsEnabled(RingsInfo)
{
    // Scaling animation properties
    _selectedUpscale.setExponent(15.f);
    _selectedUpscale.onChange([this]() { onUpscaleChanged(); });
    addProperty(_selectedUpscale);
    addProperty(_selectedScaleInterpolationTime);

    // Planet selection properties
    for (int i = 0; i < PlanetsOptions.size(); i++) {
        _firstPlanet.addOption(i, PlanetsOptions[i].data());
        _secondPlanet.addOption(i, PlanetsOptions[i].data());
    }
    _firstPlanet.onChange([this]() { onFirstChanged(); });
    addProperty(_firstPlanet);
    _secondPlanet.onChange([this]() { onSecondChanged(); });
    addProperty(_secondPlanet);

    // Add the sonifiation aspects properties
    _toggleAll.onChange([this]() { onToggleAllChanged(); });
    addProperty(_toggleAll);
    _sizeDayEnabled.onChange([this]() { sendData(); });
    addProperty(_sizeDayEnabled);
    _gravityEnabled.onChange([this]() { sendData(); });
    addProperty(_gravityEnabled);
    _temperatureEnabled.onChange([this]() { sendData(); });
    addProperty(_temperatureEnabled);
    _atmosphereEnabled.onChange([this]() { sendData(); });
    addProperty(_atmosphereEnabled);
    _moonsEnabled.onChange([this]() { sendData(); });
    addProperty(_moonsEnabled);
    _ringsEnabled.onChange([this]() { sendData(); });
    addProperty(_ringsEnabled);
}

void PlanetsCompareSonification::stop() {
    _toggleAll = false;

    _firstPlanet = 0;
    _secondPlanet = 0;
}

osc::Blob PlanetsCompareSonification::createSettingsBlob() const {
    int8_t* settings = reinterpret_cast<int8_t*>(
        global::memoryManager->TemporaryMemory.allocate(NumSettings)
    );

    settings[SizeDayIndex] = _sizeDayEnabled;
    settings[GravityIndex] = _gravityEnabled;
    settings[TemperatureIndex] = _temperatureEnabled;
    settings[AtmosphereIndex] = _atmosphereEnabled;
    settings[MoonsIndex] = _moonsEnabled;
    settings[RingsIndex] = _ringsEnabled;

    return osc::Blob(settings, NumSettings);
}

bool PlanetsCompareSonification::updateData(const Camera*) {
    return false;
}

void PlanetsCompareSonification::sendData() {
    if (!_enabled) {
        return;
    }

    std::string label = "/Compare";
    std::vector<OpenSoundControlDataType> data(NumDataItems);

    data[FirstPlanetIndex] = _firstPlanet;
    data[SecondPlanetIndex] = _secondPlanet;
    data[SettingsIndex] = createSettingsBlob();

    _connection->send(label, data);
}

void PlanetsCompareSonification::onPlanetSelectionChanged(OptionProperty& changedPlanet,
                                                          OptionProperty& otherPlanet,
                                                          std::string& prevChangedPlanet)
{
    if (changedPlanet != 0 && changedPlanet == otherPlanet) {
        LINFO("Cannot compare a planet to itself");
        changedPlanet.setValue(0);
        return;
    }

    if (!prevChangedPlanet.empty()) {
        // Reset the scale of the previously compared planet
        scalePlanet(prevChangedPlanet, 1.0, _selectedScaleInterpolationTime);
    }

    if (changedPlanet != 0) {
        scalePlanet(
            changedPlanet.getDescriptionByValue(changedPlanet.value()),
            _selectedUpscale,
            _selectedScaleInterpolationTime
        );

        prevChangedPlanet = changedPlanet.getDescriptionByValue(changedPlanet.value());
    }
    else {
        prevChangedPlanet = "";
    }

    sendData();
}

void PlanetsCompareSonification::scalePlanet(const std::string& planet, double scale,
                                             double interpolationTime)
{
    // Scale the selected planet to visually indicate which planets are currently compared
    std::string script = std::format(
        "openspace.setPropertyValueSingle('Scene.{}.Scale.Scale', {}, {});",
        planet, scale, interpolationTime
    );
    global::scriptEngine->queueScript(std::move(script));
}

void PlanetsCompareSonification::onUpscaleChanged() {
    // Update the scale value for the first planet if something is selected
    if (_firstPlanet != 0) {
        scalePlanet(
            _firstPlanet.getDescriptionByValue(_firstPlanet.value()),
            _selectedUpscale,
            0.0
        );
    }

    // Update the scale value for the second planet if something is selected
    if (_secondPlanet != 0) {
        scalePlanet(
            _secondPlanet.getDescriptionByValue(_secondPlanet.value()),
            _selectedUpscale,
            0.0
        );
    }
}

void PlanetsCompareSonification::onFirstChanged() {
    onPlanetSelectionChanged(_firstPlanet, _secondPlanet, _oldFirst);
}

void PlanetsCompareSonification::onSecondChanged() {
    onPlanetSelectionChanged(_secondPlanet, _firstPlanet, _oldSecond);
}

void PlanetsCompareSonification::onToggleAllChanged() {
    _sizeDayEnabled.setValue(_toggleAll);
    _gravityEnabled.setValue(_toggleAll);
    _temperatureEnabled.setValue(_toggleAll);
    _atmosphereEnabled.setValue(_toggleAll);
    _moonsEnabled.setValue(_toggleAll);
    _ringsEnabled.setValue(_toggleAll);
}

} // namespace openspace
