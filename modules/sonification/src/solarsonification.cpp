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
    constexpr int MetersPerKm = 1000;

    //Solar View
    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        SolarSonificationInfo =
    {
       "SolarSonification",
       "Solar Sonification",
       "The Solar Sonification"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableAllInfo = {
        "EnableAll",
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
    _scene = global::renderEngine->scene();
    _camera = _scene? _scene->camera() : nullptr;

    _timeSpeed = 0.0;
    _timePrecision = 0.0001;

    //Fill the _planets array
    {
        _planets[0] = Planet("Mercury");
        _planets[1] = Planet("Venus");
        _planets[2] = Planet("Earth");
        _planets[3] = Planet("Mars");
        _planets[4] = Planet("Jupiter");
        _planets[5] = Planet("Saturn");
        _planets[6] = Planet("Uranus");
        _planets[7] = Planet("Neptune");
    }

    //Solar
    _enableAll.onChange([this]() { onAllEnabledChanged(); });
    _mercuryEnabled.onChange([this]() { onSettingChanged(); });
    _venusEnabled.onChange([this]() { onSettingChanged(); });
    _earthEnabled.onChange([this]() { onSettingChanged(); });
    _marsEnabled.onChange([this]() { onSettingChanged(); });
    _jupiterEnabled.onChange([this]() { onSettingChanged(); });
    _saturnEnabled.onChange([this]() { onSettingChanged(); });
    _uranusEnabled.onChange([this]() { onSettingChanged(); });
    _neptuneEnabled.onChange([this]() { onSettingChanged(); });

    //Add the properties
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

void SolarSonification::checkTimeSpeed() {
    double timeSpeed = global::timeManager->deltaTime() / NumSecPerDay;
    if (abs(_timeSpeed - timeSpeed) > _timePrecision) {
        _timeSpeed = timeSpeed;

        std::string label = "/Time";
        std::vector<OscDataType> data(1);
        data[0] = _timeSpeed;

        _connection->send(label, data);
    }
}

//Extract the data from the given identifier
//NOTE: The identifier must start with capital letter,
//otherwise no match will be found
bool SolarSonification::extractData(const std::string& identifier, int i)
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

    // Check the time speed in OpenSpace
    checkTimeSpeed();

    // Calculate distance to the planet from the camera, convert to km
    glm::dvec3 cameraToNode = nodePosition - _camera->positionVec3();
    double distance = glm::length(cameraToNode) / static_cast<double>(MetersPerKm);

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
        abs(_planets[i].angle - angle) > _anglePrecision)
    {
        // Update the saved data for the planet
        _planets[i].distance = distance;
        _planets[i].angle = angle;
        shouldSendData = true;
    }
    return shouldSendData;
}

void SolarSonification::update() {
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

    // Check what node is in focus
    focusNode = global::navigationHandler->orbitalNavigator().anchorNode();
    if (!focusNode) {
        return;
    }

    // Extract data from all the planets
    for (int i = 0; i < 8; ++i) {

        // Only send data if something new has happened
        // If the node is in focus, increase sensitivity
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
