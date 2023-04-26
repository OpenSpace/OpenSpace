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

#include <modules/sonification/include/comparesonification.h>

#include <modules/sonification/sonificationmodule.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/fmt.h>

namespace {
    constexpr std::string_view _loggerCat = "CompareSonification";

    // Property info
    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        CompareSonificationInfo =
    {
       "CompareSonification",
       "Compare Sonification",
       "Sonification that compares two different planets to each other in different "
       "aspects"
    };

    constexpr openspace::properties::Property::PropertyInfo FirstOptionInfo = {
        "FirstOption",
        "Choose planet to compare",
        "Choose a planet to compare"
    };

    constexpr openspace::properties::Property::PropertyInfo SecondOptionInfo = {
        "SecondOption",
        "Choose planet to compare",
        "Choose another planet to compare"
    };

    constexpr openspace::properties::Property::PropertyInfo ToggleAllInfo = {
        "ToggleAll",
        "All",
        "Toggle all comparing sonifications for both selected planets"
    };

    constexpr openspace::properties::Property::PropertyInfo SizeDayInfo = {
        "SizeDay",
        "Size/Day",
        "Toggle size/day sonification for both selected planets"
    };

    constexpr openspace::properties::Property::PropertyInfo GravityInfo = {
        "Gravity",
        "Gravity",
        "Toggle gravity sonification for both selected planets"
    };

    constexpr openspace::properties::Property::PropertyInfo TemperatureInfo = {
        "Temperature",
        "Temperature",
        "Toggle temperature sonification for both selected planets"
    };

    constexpr openspace::properties::Property::PropertyInfo AtmosphereInfo = {
        "Atmosphere",
        "Atmosphere",
        "Toggle atmosphere sonification for both selected planets"
    };

    constexpr openspace::properties::Property::PropertyInfo MoonsInfo = {
        "Moons",
        "Moons",
        "Toggle moons sonification for both selected planets"
    };

    constexpr openspace::properties::Property::PropertyInfo RingsInfo = {
        "Rings",
        "Rings",
        "Toggle rings sonification for both selected planets"
    };
} // namespace

namespace openspace {

CompareSonification::CompareSonification(const std::string& ip, int port)
    : SonificationBase(CompareSonificationInfo, ip, port)
    , _firstPlanet(FirstOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _secondPlanet(SecondOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _toggleAll(ToggleAllInfo, false)
    , _sizeDayEnabled(SizeDayInfo, false)
    , _gravityEnabled(GravityInfo, false)
    , _temperatureEnabled(TemperatureInfo, false)
    , _atmosphereEnabled(AtmosphereInfo, false)
    , _moonsEnabled(MoonsInfo, false)
    , _ringsEnabled(RingsInfo, false)
{
    // Add options to the drop down menues
    _firstPlanet.addOptions({
        { 0, "Choose Planet" },
        { 1, "Mercury" },
        { 2, "Venus" },
        { 3, "Earth" },
        { 4, "Mars" },
        { 5, "Jupiter" },
        { 6, "Saturn" },
        { 7, "Uranus" },
        { 8, "Neptune" }
    });

    _secondPlanet.addOptions({
        { 0, "Choose Planet" },
        { 1, "Mercury" },
        { 2, "Venus" },
        { 3, "Earth" },
        { 4, "Mars" },
        { 5, "Jupiter" },
        { 6, "Saturn" },
        { 7, "Uranus" },
        { 8, "Neptune" }
    });

    // Add onChange for the properties
    _enabled.onChange([this]() { onEnabledChanged(); });
    _firstPlanet.onChange([this]() { onFirstChanged(); });
    _secondPlanet.onChange([this]() { onSecondChanged(); });
    _toggleAll.onChange([this]() { onToggleAllChanged(); });
    _sizeDayEnabled.onChange([this]() { sendSettings(); });
    _gravityEnabled.onChange([this]() { sendSettings(); });
    _temperatureEnabled.onChange([this]() { sendSettings(); });
    _atmosphereEnabled.onChange([this]() { sendSettings(); });
    _moonsEnabled.onChange([this]() { sendSettings(); });
    _ringsEnabled.onChange([this]() { sendSettings(); });

    // Add the properties
    addProperty(_firstPlanet);
    addProperty(_secondPlanet);

    addProperty(_toggleAll);
    addProperty(_sizeDayEnabled);
    addProperty(_gravityEnabled);
    addProperty(_temperatureEnabled);
    addProperty(_atmosphereEnabled);
    addProperty(_moonsEnabled);
    addProperty(_ringsEnabled);
}

CompareSonification::~CompareSonification() {
    stop();
}

std::vector<int> CompareSonification::createSettingsVector() const {
    std::vector<int> settings(NumSettings, false);

    settings[SizeDayIndex] = _sizeDayEnabled;
    settings[GravityIndex] = _gravityEnabled;
    settings[TemperatureIndex] = _temperatureEnabled;
    settings[AtmosphereIndex] = _atmosphereEnabled;
    settings[MoonsIndex] = _moonsEnabled;
    settings[RingsIndex] = _ringsEnabled;

    return settings;
}

void CompareSonification::sendSettings() {
    if (!_enabled) {
        return;
    }

    std::string label = "/Compare";
    std::vector<OscDataType> data(3);

    data[0] = _firstPlanet;
    data[1] = _secondPlanet;
    data[2] = createSettingsVector();

    _connection->send(label, data);
}

void CompareSonification::planetSelectionChanged(
                                                properties::OptionProperty& changedPlanet,
                                             properties::OptionProperty& notChangedPlanet,
                                                 std::string& prevChangedPlanet)
{
    if (changedPlanet != 0 && changedPlanet == notChangedPlanet) {
        LINFO("Cannot compare a planet to itself");
        changedPlanet.setValue(0);
        return;
    }

    if (prevChangedPlanet != "") {
        // Reset scale of previously compared planet
        std::string script = fmt::format(
            "openspace.setPropertyValueSingle('Scene.{}.Scale.Scale', {});",
            prevChangedPlanet, 1
        );
        global::scriptEngine->queueScript(
            script,
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    if (changedPlanet != 0) {
        // Scale up the planet to visually show which planets are being compared
        std::string script = fmt::format(
            "openspace.setPropertyValueSingle('Scene.{}.Scale.Scale', {});",
            changedPlanet.getDescriptionByValue(changedPlanet.value()), _focusScale
        );
        global::scriptEngine->queueScript(
            script,
            scripting::ScriptEngine::RemoteScripting::Yes
        );

        prevChangedPlanet = changedPlanet.getDescriptionByValue(changedPlanet.value());
    }
    else {
        prevChangedPlanet = "";
    }

    sendSettings();
}

void CompareSonification::onEnabledChanged() {
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
                "Turning off the Solar sonification in favor for the Compare sonification"
            );
        }

        // Planetary
        SonificationBase* planeraty = module->sonification("PlanetsSonification");
        if (!planeraty) {
            LERROR("Could not find the PlanetsSonification");
            return;
        }
        if (planeraty->enabled()) {
            planeraty->setEnabled(false);
            LINFO(
                "Turning off the Planets sonification in favor for the Compare "
                "sonification"
            );
        }
    }
}

void CompareSonification::onFirstChanged() {
    planetSelectionChanged(_firstPlanet, _secondPlanet, _oldFirst);
}

void CompareSonification::onSecondChanged() {
    planetSelectionChanged(_secondPlanet, _firstPlanet, _oldSecond);
}

void CompareSonification::onToggleAllChanged() {
    _sizeDayEnabled.setValue(_toggleAll);
    _gravityEnabled.setValue(_toggleAll);
    _temperatureEnabled.setValue(_toggleAll);
    _atmosphereEnabled.setValue(_toggleAll);
    _moonsEnabled.setValue(_toggleAll);
    _ringsEnabled.setValue(_toggleAll);
}

void CompareSonification::update(const Camera* camera) {}

void CompareSonification::stop() {
    _toggleAll = false;

    _firstPlanet = 0;
    _secondPlanet = 0;
}

} // namespace openspace
