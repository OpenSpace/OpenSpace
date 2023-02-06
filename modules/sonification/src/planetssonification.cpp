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

#include <modules/sonification/include/planetssonification.h>

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
        PlanetSonificationInfo =
    {
       "PlanetSonification",
       "Planet Sonification",
       "Sonification of the planetProperties in our solarsystem"
    };

    // Planets
    const openspace::properties::PropertyOwner::PropertyOwnerInfo MercuryInfo = {
        "MercurySonification",
        "Mercury",
        "Settings for the sonification of Mercury"
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo VenusInfo = {
        "VenusSonification",
        "Venus",
        "Settings for the sonification of Venus"
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo EarthInfo = {
        "EarthSonification",
        "Earth",
        "Settings for the sonification of Earth"
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo MarsInfo = {
        "MarsSonification",
        "Mars",
        "Settings for the sonification of Mars"
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo JupiterInfo = {
        "JupiterSonification",
        "Jupiter",
        "Settings for the sonification of Jupiter"
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo SaturnInfo = {
        "SaturnSonification",
        "Saturn",
        "Settings for the sonification of Saturn"
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo UranusInfo = {
        "UranusSonification",
        "Uranus",
        "Settings for the sonification of Uranus"
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo NeptuneInfo = {
        "NeptuneSonification",
        "Neptune",
        "Settings for the sonification of  Neptune"
    };


    // Per planet settings
    constexpr openspace::properties::Property::PropertyInfo EnableAllInfo = {
        "EnableAll",
        "All Planets",
        "Enable or disable the sonifications for all planetProperties"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableInfo = {
        "EnabledInfo",
        "All",
        "Enable or disable all sonifications for the planet"
    };

    constexpr openspace::properties::Property::PropertyInfo SizeDayInfo = {
        "SizeDayInfo",
        "Size/Day",
        "Enable or disable size/day sonification or"
    };

    constexpr openspace::properties::Property::PropertyInfo GravityInfo = {
        "GravityInfo",
        "Gravity",
        "Enable or disable gravity sonification"
    };

    constexpr openspace::properties::Property::PropertyInfo TemperatureInfo = {
        "TemperatureInfo",
        "Temperature",
        "Enable or disable temperature sonification"
    };

    constexpr openspace::properties::Property::PropertyInfo AtmosphereInfo = {
        "AtmosphereInfo",
        "Atmosphere",
        "Enable or disable atmosphere sonification"
    };

    constexpr openspace::properties::Property::PropertyInfo MoonsInfo = {
        "MoonsInfo",
        "Moons",
        "Enable or disable moons sonification"
    };

    constexpr openspace::properties::Property::PropertyInfo RingsInfo = {
        "RingsInfo",
        "Rings",
        "Enable or disable rings sonification"
    };
} // namespace

namespace openspace {

PlanetsSonification::PlanetsSonification(const std::string& ip, int port)
    : SonificationBase(PlanetSonificationInfo, ip, port)
    , _enableAll(EnableAllInfo, false)
    , _mercuryProperty(PlanetsSonification::PlanetProperty(MercuryInfo))
    , _venusProperty(PlanetsSonification::PlanetProperty(VenusInfo))
    , _earthProperty(PlanetsSonification::PlanetProperty(EarthInfo))
    , _marsProperty(PlanetsSonification::PlanetProperty(MarsInfo))
    , _jupiterProperty(PlanetsSonification::PlanetProperty(JupiterInfo))
    , _saturnProperty(PlanetsSonification::PlanetProperty(SaturnInfo))
    , _uranusProperty(PlanetsSonification::PlanetProperty(UranusInfo))
    , _neptuneProperty(PlanetsSonification::PlanetProperty(NeptuneInfo))
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

    //Add onChange for the properties
    _enableAll.onChange([this]() { onAllEnabledChanged(); });

    //Mercury
    _mercuryProperty.enabled.onChange([this]() { onMercuryEnabledChanged(); });
    _mercuryProperty.sizeDayEnabled.onChange([this]() { onMercurySettingChanged(); });
    _mercuryProperty.gravityEnabled.onChange([this]() { onMercurySettingChanged(); });
    _mercuryProperty.temperatureEnabled.onChange([this]() { onMercurySettingChanged(); });

    //Venus
    _venusProperty.enabled.onChange([this]() { onVenusEnabledChanged(); });
    _venusProperty.sizeDayEnabled.onChange([this]() { onVenusSettingChanged(); });
    _venusProperty.gravityEnabled.onChange([this]() { onVenusSettingChanged(); });
    _venusProperty.temperatureEnabled.onChange([this]() { onVenusSettingChanged(); });
    _venusProperty.atmosphereEnabled.onChange([this]() { onVenusSettingChanged(); });

    //Earth
    _earthProperty.enabled.onChange([this]() { onEarthEnabledChanged(); });
    _earthProperty.sizeDayEnabled.onChange([this]() { onEarthSettingChanged(); });
    _earthProperty.gravityEnabled.onChange([this]() { onEarthSettingChanged(); });
    _earthProperty.temperatureEnabled.onChange([this]() { onEarthSettingChanged(); });
    _earthProperty.atmosphereEnabled.onChange([this]() { onEarthSettingChanged(); });
    _earthProperty.moonsEnabled.onChange([this]() { onEarthSettingChanged(); });

    //Mars
    _marsProperty.enabled.onChange([this]() { onMarsEnabledChanged(); });
    _marsProperty.sizeDayEnabled.onChange([this]() { onMarsSettingChanged(); });
    _marsProperty.gravityEnabled.onChange([this]() { onMarsSettingChanged(); });
    _marsProperty.temperatureEnabled.onChange([this]() { onMarsSettingChanged(); });
    _marsProperty.atmosphereEnabled.onChange([this]() { onMarsSettingChanged(); });
    _marsProperty.moonsEnabled.onChange([this]() { onMarsSettingChanged(); });

    //Jupiter
    _jupiterProperty.enabled.onChange([this]() { onJupiterEnabledChanged(); });
    _jupiterProperty.sizeDayEnabled.onChange([this]() { onJupiterSettingChanged(); });
    _jupiterProperty.gravityEnabled.onChange([this]() { onJupiterSettingChanged(); });
    _jupiterProperty.temperatureEnabled.onChange([this]() { onJupiterSettingChanged(); });
    _jupiterProperty.atmosphereEnabled.onChange([this]() { onJupiterSettingChanged(); });
    _jupiterProperty.moonsEnabled.onChange([this]() { onJupiterSettingChanged(); });

    //Saturn
    _saturnProperty.enabled.onChange([this]() { onSaturnEnabledChanged(); });
    _saturnProperty.sizeDayEnabled.onChange([this]() { onSaturnSettingChanged(); });
    _saturnProperty.gravityEnabled.onChange([this]() { onSaturnSettingChanged(); });
    _saturnProperty.temperatureEnabled.onChange([this]() { onSaturnSettingChanged(); });
    _saturnProperty.atmosphereEnabled.onChange([this]() { onSaturnSettingChanged(); });
    _saturnProperty.moonsEnabled.onChange([this]() { onSaturnSettingChanged(); });
    _saturnProperty.ringsEnabled.onChange([this]() { onSaturnSettingChanged(); });

    //Uranus
    _uranusProperty.enabled.onChange([this]() { onUranusEnabledChanged(); });
    _uranusProperty.sizeDayEnabled.onChange([this]() { onUranusSettingChanged(); });
    _uranusProperty.gravityEnabled.onChange([this]() { onUranusSettingChanged(); });
    _uranusProperty.temperatureEnabled.onChange([this]() { onUranusSettingChanged(); });
    _uranusProperty.atmosphereEnabled.onChange([this]() { onUranusSettingChanged(); });
    _uranusProperty.moonsEnabled.onChange([this]() { onUranusSettingChanged(); });

    //Neptune
    _neptuneProperty.enabled.onChange([this]() { onNeptuneEnabledChanged(); });
    _neptuneProperty.sizeDayEnabled.onChange([this]() { onNeptuneSettingChanged(); });
    _neptuneProperty.gravityEnabled.onChange([this]() { onNeptuneSettingChanged(); });
    _neptuneProperty.temperatureEnabled.onChange([this]() { onNeptuneSettingChanged(); });
    _neptuneProperty.atmosphereEnabled.onChange([this]() { onNeptuneSettingChanged(); });
    _neptuneProperty.moonsEnabled.onChange([this]() { onNeptuneSettingChanged(); });

    //Add the properties
    addProperty(_enableAll);
    addPropertySubOwner(_mercuryProperty);
    addPropertySubOwner(_venusProperty);
    addPropertySubOwner(_earthProperty);
    addPropertySubOwner(_marsProperty);
    addPropertySubOwner(_jupiterProperty);
    addPropertySubOwner(_saturnProperty);
    addPropertySubOwner(_uranusProperty);
    addPropertySubOwner(_neptuneProperty);
}

PlanetsSonification::~PlanetsSonification() {
    _enableAll = false;
}

osc::Blob PlanetsSonification::createSettingsBlob(int planetIndex) const {
    std::vector<const properties::PropertyOwner*> planets(8);
    planets[0] = &_mercuryProperty;
    planets[1] = &_venusProperty;
    planets[2] = &_earthProperty;
    planets[3] = &_marsProperty;
    planets[4] = &_jupiterProperty;
    planets[5] = &_saturnProperty;
    planets[6] = &_uranusProperty;
    planets[7] = &_neptuneProperty;

    bool settings[7];

    int count = 0;
    for (properties::Property* p : planets[planetIndex]->properties()) {
        settings[count++] = p;
    }

    return osc::Blob(settings, 7);
}

void PlanetsSonification::setAllProperties(bool value) {
    std::vector<properties::PropertyOwner*> planets(8);
    planets[0] = &_mercuryProperty;
    planets[1] = &_venusProperty;
    planets[2] = &_earthProperty;
    planets[3] = &_marsProperty;
    planets[4] = &_jupiterProperty;
    planets[5] = &_saturnProperty;
    planets[6] = &_uranusProperty;
    planets[7] = &_neptuneProperty;

    for (properties::PropertyOwner* planet : planets) {
        std::vector<properties::Property*> planetProperties = planet->properties();

        for (properties::Property* p : planetProperties) {
            p->set(value);
        }
    }
}

void PlanetsSonification::sendSettings(const int planetIndex) {
    std::string label = "/" + _planets[planetIndex].identifier;
    std::vector<OscDataType> data;

    // Distance
    data.push_back(_planets[planetIndex].distance);

    // Angle
    data.push_back(_planets[planetIndex].angle);

    // Settings
    osc::Blob settingsBlob = createSettingsBlob(planetIndex);
    data.push_back(settingsBlob);

    // Moons
    for (size_t m = 0; m < _planets[planetIndex].moons.size(); ++m) {
        data.push_back(_planets[planetIndex].moons[m].second);
    }

    data.shrink_to_fit();
    _connection->send(label, data);
}

void PlanetsSonification::onAllEnabledChanged() {
    //Set all the planetary settings
    setAllProperties(_enableAll);
}

//Mercury
void PlanetsSonification::onMercuryEnabledChanged() {
    _mercuryProperty.sizeDayEnabled = _mercuryProperty.enabled;
    _mercuryProperty.gravityEnabled = _mercuryProperty.enabled;
    _mercuryProperty.temperatureEnabled = _mercuryProperty.enabled;
}

void PlanetsSonification::onMercurySettingChanged() {
    sendSettings(0);
}


//Venus
void PlanetsSonification::onVenusEnabledChanged() {
    _venusProperty.sizeDayEnabled = _venusProperty.enabled;
    _venusProperty.gravityEnabled = _venusProperty.enabled;
    _venusProperty.temperatureEnabled = _venusProperty.enabled;
    _venusProperty.atmosphereEnabled = _venusProperty.enabled;
}

void PlanetsSonification::onVenusSettingChanged() {
    sendSettings(1);
}


//Earth
void PlanetsSonification::onEarthEnabledChanged() {
    _earthProperty.sizeDayEnabled = _earthProperty.enabled;
    _earthProperty.gravityEnabled = _earthProperty.enabled;
    _earthProperty.temperatureEnabled = _earthProperty.enabled;
    _earthProperty.atmosphereEnabled = _earthProperty.enabled;
    _earthProperty.moonsEnabled = _earthProperty.enabled;
}

void PlanetsSonification::onEarthSettingChanged() {
    sendSettings(2);
}


//Mars
void PlanetsSonification::onMarsEnabledChanged() {
    _marsProperty.sizeDayEnabled = _marsProperty.enabled;
    _marsProperty.gravityEnabled = _marsProperty.enabled;
    _marsProperty.temperatureEnabled = _marsProperty.enabled;
    _marsProperty.atmosphereEnabled = _marsProperty.enabled;
    _marsProperty.moonsEnabled = _marsProperty.enabled;
}

void PlanetsSonification::onMarsSettingChanged() {
    sendSettings(3);
}


//Jupiter
void PlanetsSonification::onJupiterEnabledChanged() {
    _jupiterProperty.sizeDayEnabled = _jupiterProperty.enabled;
    _jupiterProperty.gravityEnabled = _jupiterProperty.enabled;
    _jupiterProperty.temperatureEnabled = _jupiterProperty.enabled;
    _jupiterProperty.atmosphereEnabled = _jupiterProperty.enabled;
    _jupiterProperty.moonsEnabled = _jupiterProperty.enabled;
}

void PlanetsSonification::onJupiterSettingChanged() {
    sendSettings(4);
}


//Saturn
void PlanetsSonification::onSaturnEnabledChanged() {
    _saturnProperty.sizeDayEnabled = _saturnProperty.enabled;
    _saturnProperty.gravityEnabled = _saturnProperty.enabled;
    _saturnProperty.temperatureEnabled = _saturnProperty.enabled;
    _saturnProperty.atmosphereEnabled = _saturnProperty.enabled;
    _saturnProperty.moonsEnabled = _saturnProperty.enabled;
    _saturnProperty.ringsEnabled = _saturnProperty.enabled;
}

void PlanetsSonification::onSaturnSettingChanged() {
    sendSettings(5);
}


//Uranus
void PlanetsSonification::onUranusEnabledChanged() {
    _uranusProperty.sizeDayEnabled = _uranusProperty.enabled;
    _uranusProperty.gravityEnabled = _uranusProperty.enabled;
    _uranusProperty.temperatureEnabled = _uranusProperty.enabled;
    _uranusProperty.atmosphereEnabled = _uranusProperty.enabled;
    _uranusProperty.moonsEnabled = _uranusProperty.enabled;
}

void PlanetsSonification::onUranusSettingChanged() {
    sendSettings(6);
}


//Neptune
void PlanetsSonification::onNeptuneEnabledChanged() {
    _neptuneProperty.sizeDayEnabled = _neptuneProperty.enabled;
    _neptuneProperty.gravityEnabled = _neptuneProperty.enabled;
    _neptuneProperty.temperatureEnabled = _neptuneProperty.enabled;
    _neptuneProperty.atmosphereEnabled = _neptuneProperty.enabled;
    _neptuneProperty.moonsEnabled = _neptuneProperty.enabled;
}

void PlanetsSonification::onNeptuneSettingChanged() {
    sendSettings(7);
}


PlanetsSonification::PlanetProperty::PlanetProperty(
                                  properties::PropertyOwner::PropertyOwnerInfo planetInfo)
    : properties::PropertyOwner(planetInfo)
    , enabled(EnableInfo, false)
    , sizeDayEnabled(SizeDayInfo, false)
    , gravityEnabled(GravityInfo, false)
    , temperatureEnabled(TemperatureInfo, false)
    , atmosphereEnabled(AtmosphereInfo, false)
    , moonsEnabled(MoonsInfo, false)
    , ringsEnabled(RingsInfo, false)
{
    addProperty(enabled);
    addProperty(sizeDayEnabled);
    addProperty(gravityEnabled);
    addProperty(temperatureEnabled);

    // Check if any special cases should be added
    if (planetInfo.identifier != "Mercury") {
        addProperty(atmosphereEnabled);
    }
    if (planetInfo.identifier != "Mercury" && planetInfo.identifier != "Venus") {
        addProperty(moonsEnabled);
    }
    if (planetInfo.identifier == "Saturn") {
        addProperty(ringsEnabled);
    }
}

void PlanetsSonification::checkTimeSpeed(double& ts) {
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
void PlanetsSonification::extractData(const std::string& identifier, int i)
{
    if (!_scene || !_camera || _scene->isInitializing()) {
        return;
    }

    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        return;
    }

    glm::dvec3 nodePosition = node->worldPosition();
    if (nodePosition.length < std::numeric_limits<glm::length_t>::epsilon) {
        return;
    }

    glm::dvec3 cameraDirection = _camera->viewDirectionWorldSpace();
    glm::dvec3 cameraUpVector = _camera->lookUpVectorWorldSpace();

    //Check the time speed in OpenSpace
    checkTimeSpeed(_timeSpeed);

    //Calculate distance to the planet from the camera, convert to km
    glm::dvec3 cameraToNode = nodePosition - _camera->positionVec3();
    double distance = glm::length(cameraToNode) / 1000.0;
    double angle;
    bool updateMoons = false;

    //Calculate angle from camera to the planet in the camera plane
    //Project v down to the camera plane, Pplane(v)
    //Pn(v) is v projected on the normal n of the plane
    //Pplane(v) = v - Pn(v)
    glm::dvec3 cameraToProjectedNode = cameraToNode -
        glm::proj(cameraToNode, cameraUpVector);

    angle = glm::orientedAngle(glm::normalize(cameraDirection),
        glm::normalize(cameraToProjectedNode),
        glm::normalize(cameraUpVector));

    //If this planet is in focus then calculate the angle from
    //the planet to its moons and send them too
    for (int m = 0; m < _planets[i].moons.size(); ++m) {
        // TODO: This throws an exception sometimes for Io
        SceneGraphNode* moon =
            sceneGraphNode(_planets[i].moons[m].first);
        if (moon) {
            glm::dvec3 planetToMoon = moon->worldPosition() - nodePosition;
            glm::dvec3 planetToProjectedMoon = planetToMoon -
                glm::proj(planetToMoon, cameraUpVector);

            //Angle from planet to moon with respect to camera
            //NOTE: This might not work if the camera is looking straight
            //down on the planet
            double moonAngle =
                glm::orientedAngle(glm::normalize(cameraDirection),
                    glm::normalize(planetToProjectedMoon),
                    glm::normalize(cameraUpVector));

            if (abs(_planets[i].moons[m].second - moonAngle) > _anglePrecision)
            {
                updateMoons = true;
                _planets[i].moons[m].second = moonAngle;
            }
        }
    }


    // Check if this data is new, otherwise dont send the data
    if (abs(_planets[i].distance - distance) > _distancePrecision ||
        abs(_planets[i].angle - angle) > _anglePrecision ||
        updateMoons)
    {
        // Update the saved data for the planet
        _planets[i].distance = distance;
        _planets[i].angle = angle;

        // Send the data to SuperCollider
        sendSettings(i);
    }
}

void PlanetsSonification::update() {
    const SceneGraphNode* focusNode = nullptr;
    const SceneGraphNode* previousFocusNode = nullptr;

    // If no scene, try to find it
    if (!_scene || _scene->root()->children().size() == 0) {
        _scene = global::renderEngine->scene();
    }

    if (_scene && _scene->isInitializing()) {
        return;
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

        extractData(_planets[i].identifier, i);
    }
}

} // namespace openspace
