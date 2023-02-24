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

#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/fmt.h>

namespace {
    constexpr std::string_view _loggerCat = "CompareSonification";

    // Set the differnet levels of precision
    constexpr double LowDistancePrecision = 10000.0;
    constexpr double HighDistancePrecision = 1000.0;
    constexpr double LowAnglePrecision = 0.1;
    constexpr double HighAnglePrecision = 0.05;

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
    _toggleAll = false;

    _firstPlanet = 0;
    _secondPlanet = 0;
}

osc::Blob CompareSonification::createSettingsBlob() const {
    bool settings[6] = { false };

    settings[0] = _sizeDayEnabled;
    settings[1] = _gravityEnabled;
    settings[2] = _temperatureEnabled;
    settings[3] = _atmosphereEnabled;
    settings[4] = _moonsEnabled;
    settings[5] = _ringsEnabled;

    return osc::Blob(settings, 6);
}

void CompareSonification::sendSettings() {
    std::string label = "/Compare";
    std::vector<OscDataType> data(3);

    data[0] = _firstPlanet;
    data[1] = _secondPlanet;
    data[2] = createSettingsBlob();

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

// Extract the data from the given identifier
bool CompareSonification::getData(const Camera* camera, Planet& planet) {
    double distance = SonificationBase::calculateDistanceTo(
        camera,
        planet.identifier,
        DistanceUnit::Kilometer
    );
    double angle = SonificationBase::calculateAngleTo(camera, planet.identifier);

    if (abs(distance) < std::numeric_limits<double>::epsilon()) {
        return false;
    }

    // Also calculate angle to moons
    bool updateMoons = false;
    for (std::pair<std::string, double>& moon : planet.moons) {
        double moonAngle = SonificationBase::calculateAngleFromAToB(
            camera,
            planet.identifier,
            moon.first
        );

        if (abs(moon.second - moonAngle) > _anglePrecision) {
            moon.second = moonAngle;
            updateMoons = true;
        }
    }

    // Check if this data is new, otherwise don't send it
    bool isNewData = false;
    if (abs(planet.distance - distance) > _distancePrecision ||
        abs(planet.angle - angle) > _anglePrecision || updateMoons)
    {
        // Update the saved data for the planet
        planet.distance = distance;
        planet.angle = angle;
        isNewData = true;
    }
    return isNewData;
}

void CompareSonification::update(const Camera* camera) {
    const SceneGraphNode* focusNode =
        global::navigationHandler->orbitalNavigator().anchorNode();

    if (!focusNode) {
        return;
    }

    // Update data for all planets
    for (Planet& planet : _planets) {
        // Increase presision if the planet is in focus
        if (focusNode->identifier() == planet.identifier) {
            _anglePrecision = HighAnglePrecision;
            _distancePrecision = HighDistancePrecision;
        }
        else {
            _anglePrecision = LowAnglePrecision;
            _distancePrecision = LowDistancePrecision;
        }

        bool hasDataUpdated = getData(camera, planet);

        // Only send data if something new has happened
        if (hasDataUpdated) {
            sendSettings();
        }
    }
}

} // namespace openspace
