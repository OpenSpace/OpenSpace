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

#include <openspace/camera/camera.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/timemanager.h>
#include <openspace/query/query.h>
#include <glm/gtx/projection.hpp>
#include <glm/gtx/vector_angle.hpp>

namespace {
    constexpr int NumSecPerDay = 86400;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        CompareSonificationInfo =
    {
       "CompareSonification",
       "Compare Sonification",
       "The compare sonification"
    };

    constexpr openspace::properties::Property::PropertyInfo CompareOptionsInfo = {
        "FirstOption",
        "Choose planet to compare",
        "Choose a planet to compare"
    };

    constexpr openspace::properties::Property::PropertyInfo CompareOptionsInfoT = {
        "SecondOption",
        "Choose planet to compare",
        "Choose another planet to compare"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableAllInfo = {
        "EnableAll",
        "All",
        "Enable or disable all comparing sonifications for both selected planets"
    };

    constexpr openspace::properties::Property::PropertyInfo CompareSizeDayInfo = {
        "EnableSizeDay",
        "Size/Day",
        "Enable or disable size/day sonification for both selected planets"
    };

    constexpr openspace::properties::Property::PropertyInfo CompareGravityInfo = {
        "EnableGravity",
        "Gravity",
        "Enable or disable gravity sonification for both selected planets"
    };

    constexpr openspace::properties::Property::PropertyInfo CompareTemperatureInfo = {
        "EnableTemperature",
        "Temperature",
        "Enable or disable temperature sonification for both selected planets"
    };

    constexpr openspace::properties::Property::PropertyInfo CompareAtmosphereInfo = {
        "EnableAtmosphere",
        "Atmosphere",
        "Enable or disable atmosphere sonification for both selected planets"
    };

    constexpr openspace::properties::Property::PropertyInfo CompareMoonsInfo = {
        "EnableMoons",
        "Moons",
        "Enable or disable moons sonification for both selected planets"
    };

    constexpr openspace::properties::Property::PropertyInfo CompareRingsInfo = {
        "EnableRings",
        "Rings",
        "Enable or disable rings sonification for both selected planets"
    };

} // namespace

namespace openspace {

CompareSonification::CompareSonification(const std::string& ip, int port)
    : SonificationBase(CompareSonificationInfo, ip, port)
    , _firstPlanet(CompareOptionsInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _secondPlanet(CompareOptionsInfoT, properties::OptionProperty::DisplayType::Dropdown)
    , _enableAll(EnableAllInfo, false)
    , _sizeDayEnabled(CompareSizeDayInfo, false)
    , _gravityEnabled(CompareGravityInfo, false)
    , _temperatureEnabled(CompareTemperatureInfo, false)
    , _atmosphereEnabled(CompareAtmosphereInfo, false)
    , _moonsEnabled(CompareMoonsInfo, false)
    , _ringsEnabled(CompareRingsInfo, false)
{
    _scene = global::renderEngine->scene();
    _camera = _scene? _scene->camera() : nullptr;

    _timeSpeed = 0.0;
    _timePrecision = 0.0001;

    //Fill the _planets array
    {
        _planets[0] = Planet("Mercury");
        _planets[1] = Planet("Venus");

        _planets[2] = Planet("Earth");
        _planets[2].moons.reserve(1);
        _planets[2].moons.push_back({ "Moon", 0.0 });

        _planets[3] = Planet("Mars");
        _planets[3].moons.reserve(2);
        _planets[3].moons.push_back({ "Phobos", 0.0 });
        _planets[3].moons.push_back({ "Deimos", 0.0 });

        _planets[4] = Planet("Jupiter");
        _planets[4].moons.reserve(4);
        _planets[4].moons.push_back({ "Io", 0.0 });
        _planets[4].moons.push_back({ "Europa", 0.0 });
        _planets[4].moons.push_back({ "Ganymede", 0.0 });
        _planets[4].moons.push_back({ "Callisto", 0.0 });

        _planets[5] = Planet("Saturn");
        _planets[5].moons.reserve(8);
        _planets[5].moons.push_back({ "Dione", 0.0 });
        _planets[5].moons.push_back({ "Enceladus", 0.0 });
        _planets[5].moons.push_back({ "Hyperion", 0.0 });
        _planets[5].moons.push_back({ "Iapetus", 0.0 });
        _planets[5].moons.push_back({ "Mimas", 0.0 });
        _planets[5].moons.push_back({ "Rhea", 0.0 });
        _planets[5].moons.push_back({ "Tethys", 0.0 });
        _planets[5].moons.push_back({ "Titan", 0.0 });

        _planets[6] = Planet("Uranus");
        _planets[6].moons.reserve(5);
        _planets[6].moons.push_back({ "Ariel", 0.0 });
        _planets[6].moons.push_back({ "Miranda", 0.0 });
        _planets[6].moons.push_back({ "Oberon", 0.0 });
        _planets[6].moons.push_back({ "Titania", 0.0 });
        _planets[6].moons.push_back({ "Umbriel", 0.0 });

        _planets[7] = Planet("Neptune");
        _planets[7].moons.reserve(1);
        _planets[7].moons.push_back({ "Triton", 0.0 });
    }

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

    //Add onChange for the properties
    _firstPlanet.onChange([this]() { onFirstChanged(); });
    _secondPlanet.onChange([this]() { onSecondChanged(); });
    _enableAll.onChange([this]() { onAllChanged(); });
    _sizeDayEnabled.onChange([this]() { onSettingChanged(); });
    _gravityEnabled.onChange([this]() { onSettingChanged(); });
    _temperatureEnabled.onChange([this]() { onSettingChanged(); });
    _atmosphereEnabled.onChange([this]() { onSettingChanged(); });
    _moonsEnabled.onChange([this]() { onSettingChanged(); });
    _ringsEnabled.onChange([this]() { onSettingChanged(); });

    //Add the properties
    addProperty(_firstPlanet);
    addProperty(_secondPlanet);

    addProperty(_enableAll);
    addProperty(_sizeDayEnabled);
    addProperty(_gravityEnabled);
    addProperty(_temperatureEnabled);
    addProperty(_atmosphereEnabled);
    addProperty(_moonsEnabled);
    addProperty(_ringsEnabled);
}

CompareSonification::~CompareSonification() {
    _enableAll = false;

    _firstPlanet.setValue(0);
    _secondPlanet.setValue(0);
}

osc::Blob CompareSonification::createSettingsBlob() const {
    bool settings[7];

    settings[0] = _enableAll;
    settings[1] = _sizeDayEnabled;
    settings[2] = _gravityEnabled;
    settings[3] = _temperatureEnabled;
    settings[4] = _atmosphereEnabled;
    settings[5] = _moonsEnabled;
    settings[6] = _ringsEnabled;

    return osc::Blob(settings, 8);
}

//Compare
void CompareSonification::sendSettings() {
    std::string label = "/Compare";
    std::vector<OscDataType> data(3);

    data[0] = _firstPlanet.value();

    data[1] = _secondPlanet.value();

    data[2] = createSettingsBlob();

    _connection->send(label, data);
}

void CompareSonification::onFirstChanged()
{
    if (_firstPlanet.value() != 0 && _firstPlanet == _secondPlanet)
    {
        _firstPlanet = 0;
        return;
    }

    if (_oldFirst != "") {
        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('Scene." +
            _oldFirst + ".Scale.Scale', 1);",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    if (_firstPlanet.value() != 0) {
        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('Scene." +
            _firstPlanet.description() + ".Scale.Scale', 2000);",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        _oldFirst = _firstPlanet.description();
    }
    else {
        _oldFirst = "";
    }

    sendSettings();
}

void CompareSonification::onSecondChanged()
{
    if (_secondPlanet.value() != 0 && _firstPlanet == _secondPlanet) {
        _secondPlanet = 0;
        return;
    }

    if (_oldSecond != "") {
        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('Scene." +
            _oldSecond + ".Scale.Scale', 1);",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    if (_secondPlanet.value() != 0) {
        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('Scene." +
            _secondPlanet.description() + ".Scale.Scale', 2000);",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        _oldSecond = _secondPlanet.description();
    }
    else {
        _oldSecond = "";
    }

    sendSettings();
}

void CompareSonification::onAllChanged() {
    _sizeDayEnabled = _enableAll;
    _gravityEnabled = _enableAll;
    _temperatureEnabled = _enableAll;
    _atmosphereEnabled = _enableAll;
    _moonsEnabled = _enableAll;
    _ringsEnabled = _enableAll;
}

void CompareSonification::onSettingChanged() {
    sendSettings();
}

void CompareSonification::checkTimeSpeed(double& ts) {
    double timeSpeed = global::timeManager->deltaTime() / NumSecPerDay;
    if (abs(ts - timeSpeed) > _timePrecision) {
        ts = timeSpeed;

        std::string label = "/Time";
        std::vector<OscDataType> data(1);
        data[0] = ts;

        _connection->send(label, data);
    }
}

//Extract the data from the given identifier
//NOTE: The identifier must start with capital letter,
//otherwise no match will be found
bool CompareSonification::extractData(const std::string& identifier, int i)
{
    if (_scene->isInitializing()) {
        return false;
    }

    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        return false;
    }

    glm::dvec3 nodePosition = node->worldPosition();
    if (nodePosition.length < std::numeric_limits<glm::length_t>::epsilon) {
        return false;
    }

    glm::dvec3 cameraDirection = _camera->viewDirectionWorldSpace();
    glm::dvec3 cameraUpVector = _camera->lookUpVectorWorldSpace();

    //Check the time speed in OpenSpace
    checkTimeSpeed(_timeSpeed);

    //Calculate distance to the planet from the camera, convert to km
    glm::dvec3 cameraToNode = nodePosition - _camera->positionVec3();
    double distance = glm::length(cameraToNode) / 1000.0;
    bool updateMoons = false;

    // Calculate angle from the Sun (origin) to node,
    // with camera forward vector as forward axis and camera up vector as upward axis
    // angle from Sun with respect to the camera
    double angle = glm::orientedAngle(
        glm::normalize(cameraDirection),
        glm::normalize(nodePosition - glm::proj(nodePosition, cameraUpVector)),
        glm::normalize(cameraUpVector)
    );

    // Check if this data is new, otherwise don't send it
    bool shouldSendData = false;
    if (abs(_planets[i].distance - distance) > _distancePrecision ||
        abs(_planets[i].angle - angle) > _anglePrecision ||
        updateMoons)
    {
        // Update the saved data for the planet
        _planets[i].distance = distance;
        _planets[i].angle = angle;

        shouldSendData = true;
    }
    return shouldSendData;
}

void CompareSonification::update() {
    const SceneGraphNode* focusNode = nullptr;
    const SceneGraphNode* previousFocusNode = nullptr;

    // If no scene, try to find it
    if (!_scene || _scene->isInitializing() || _scene->root()->children().size() == 0) {
        _scene = global::renderEngine->scene();
    }

    // If no camera, try to find it
    if (!_camera) {
        _camera = _scene ? _scene->camera() : nullptr;
    }

    if (!_scene || !_camera ||
        _camera->positionVec3().length < std::numeric_limits<glm::length_t>::epsilon)
    {
        return;
    }

    // Scne and camera is initialized

    //Which node is in focus?
    focusNode = global::navigationHandler->orbitalNavigator().anchorNode();
    if (!focusNode) {
        return;
    }

    //Extract data from all the planets
    for (int i = 0; i < 8; ++i) {

        //Only send data if something new has happened
        //If the node is in focus, increase sensitivity
        if (focusNode->identifier().compare(_planets[i].identifier) == 0)
        {
            _anglePrecision = 0.05;
            _distancePrecision = 1000.0;
        }
        else {
            _anglePrecision = 0.1;
            _distancePrecision = 10000.0;
        }

        bool hasDataUpdated = extractData(_planets[i].identifier, i);

        if (hasDataUpdated) {
            // Send the data to SuperCollider
            sendSettings();
        }
    }
}

} // namespace openspace
