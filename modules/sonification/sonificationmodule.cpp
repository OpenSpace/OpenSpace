/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                              *
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

#include <modules/sonification/sonificationmodule.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/camera.h>
#include <glm/gtx/vector_angle.hpp>
#include <glm/gtx/projection.hpp>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/util/timemanager.h>

 //Debug purposes
#include <iostream>

//Output to SuperCollider (SC)
#define SC_IP_ADDRESS "127.0.0.1"
#define SC_PORT 57120
#define BUFFER_SIZE 1024

namespace {
    constexpr openspace::properties::Property::PropertyInfo EnableInfo = {
        "EnabledInfo",
        "Enable all",
        "Play all sonifications for the planet or turn it off"
    };

    constexpr openspace::properties::Property::PropertyInfo SizeDayInfo = {
        "SizeDayInfo",
        "Size/Day",
        "Play Size/Day sonification or turn it off"
    };

    constexpr openspace::properties::Property::PropertyInfo GravityInfo = {
        "GravityInfo",
        "Gravity",
        "Play Gravity sonification or turn it off"
    };

    constexpr openspace::properties::Property::PropertyInfo AtmosphereInfo = {
        "AtmosphereInfo",
        "Atmosphere",
        "Play Atmosphere sonification or turn it off"
    };

    constexpr openspace::properties::Property::PropertyInfo MoonsInfo = {
        "MoonsInfo",
        "Moons",
        "Play Moons sonification or turn it off"
    };

} // namespace

namespace openspace {

SonificationModule::SonificationModule()
    : OpenSpaceModule("Sonification")
{
    //Create buffer and stream to send to SuperCollider
    _buffer = new char[BUFFER_SIZE];
    _stream = osc::OutboundPacketStream(_buffer, BUFFER_SIZE);
    _isRunning = true;
    _isPlanetaryView = true;
    _previousTimeSpeed = 0.0;
    _timePrecision = 0.001;

    if (_thread.joinable())
        _thread.join();

    //Fill the _planets array
    {
        _planets[0] = Planet("Mercury");
        _planets[1] = Planet("Venus");

        _planets[2] = Planet("Earth");
        _planets[2]._moons.reserve(1);
        _planets[2]._moons.push_back({ "Moon", 0.0 });

        _planets[3] = Planet("Mars");
        _planets[3]._moons.reserve(2);
        _planets[3]._moons.push_back({ "Phobos", 0.0 });
        _planets[3]._moons.push_back({ "Deimos", 0.0 });

        _planets[4] = Planet("Jupiter");
        _planets[4]._moons.reserve(4);
        _planets[4]._moons.push_back({ "Io", 0.0 });
        _planets[4]._moons.push_back({ "Europa", 0.0 });
        _planets[4]._moons.push_back({ "Ganymede", 0.0 });
        _planets[4]._moons.push_back({ "Callisto", 0.0 });

        _planets[5] = Planet("Saturn");
        _planets[5]._moons.reserve(8);
        _planets[5]._moons.push_back({ "Dione", 0.0 });
        _planets[5]._moons.push_back({ "Enceladus", 0.0 });
        _planets[5]._moons.push_back({ "Hyperion", 0.0 });
        _planets[5]._moons.push_back({ "Iapetus", 0.0 });
        _planets[5]._moons.push_back({ "Mimas", 0.0 });
        _planets[5]._moons.push_back({ "Rhea", 0.0 });
        _planets[5]._moons.push_back({ "Tethys", 0.0 });
        _planets[5]._moons.push_back({ "Titan", 0.0 });

        _planets[6] = Planet("Uranus");
        _planets[6]._moons.reserve(5);
        _planets[6]._moons.push_back({ "Ariel", 0.0 });
        _planets[6]._moons.push_back({ "Miranda", 0.0 });
        _planets[6]._moons.push_back({ "Oberon", 0.0 });
        _planets[6]._moons.push_back({ "Titania", 0.0 });
        _planets[6]._moons.push_back({ "Umbriel", 0.0 });

        _planets[7] = Planet("Neptune");
        _planets[7]._moons.reserve(1);
        _planets[7]._moons.push_back({ "Triton", 0.0 });
    }

    //Add onChange for the properties
    //Mercury
    _mercuryProperty.enabled.onChange([this]() { onMercuryEnabledChanged(_mercuryProperty.enabled.value()); } );
    _mercuryProperty.sizeDayEnabled.onChange([this]() { onMercurySizeDayChanged(_mercuryProperty.sizeDayEnabled.value()); } );
    _mercuryProperty.gravityEnabled.onChange([this]() { onMercuryGravityChanged(_mercuryProperty.gravityEnabled.value()); } );

    //Venus
    _venusProperty.enabled.onChange([this]() { onVenusEnabledChanged(_venusProperty.enabled.value()); });
    _venusProperty.sizeDayEnabled.onChange([this]() { onVenusSizeDayChanged(_venusProperty.sizeDayEnabled.value()); });
    _venusProperty.gravityEnabled.onChange([this]() { onVenusGravityChanged(_venusProperty.gravityEnabled.value()); });
    _venusProperty.atmosphereEnabled.onChange([this]() { onVenusAtmosphereChanged(_venusProperty.atmosphereEnabled.value()); });

    //Earth
    _earthProperty.enabled.onChange([this]() { onEarthEnabledChanged(_earthProperty.enabled.value()); });
    _earthProperty.sizeDayEnabled.onChange([this]() { onEarthSizeDayChanged(_earthProperty.sizeDayEnabled.value()); });
    _earthProperty.gravityEnabled.onChange([this]() { onEarthGravityChanged(_earthProperty.gravityEnabled.value()); });
    _earthProperty.atmosphereEnabled.onChange([this]() { onEarthAtmosphereChanged(_earthProperty.atmosphereEnabled.value()); });
    _earthProperty.moonsEnabled.onChange([this]() { onEarthMoonsChanged(_earthProperty.moonsEnabled.value()); });

    //Mars
    _marsProperty.enabled.onChange([this]() { onMarsEnabledChanged(_marsProperty.enabled.value()); });
    _marsProperty.sizeDayEnabled.onChange([this]() { onMarsSizeDayChanged(_marsProperty.sizeDayEnabled.value()); });
    _marsProperty.gravityEnabled.onChange([this]() { onMarsGravityChanged(_marsProperty.gravityEnabled.value()); });
    _marsProperty.atmosphereEnabled.onChange([this]() { onMarsAtmosphereChanged(_marsProperty.atmosphereEnabled.value()); });
    _marsProperty.moonsEnabled.onChange([this]() { onMarsMoonsChanged(_marsProperty.moonsEnabled.value()); });

    //Add the properties
    addPropertySubOwner(_mercuryProperty);
    addPropertySubOwner(_venusProperty);
    addPropertySubOwner(_earthProperty);
    addPropertySubOwner(_marsProperty);
}

//Mercury
void SonificationModule::onMercuryEnabledChanged(bool value) {
    _planets[0]._settings[0] = value;
    _planets[0]._settings[1] = value;
    _planets[0]._settings[2] = value;

    _mercuryProperty.sizeDayEnabled = value;
    _mercuryProperty.gravityEnabled = value;
    _mercuryProperty.atmosphereEnabled = value;
    _mercuryProperty.moonsEnabled = value;
    _planets[0]._update = true;
}

void SonificationModule::onMercurySizeDayChanged(bool value) {
    _planets[0]._settings[1] = value;
    _planets[0]._update = true;
}

void SonificationModule::onMercuryGravityChanged(bool value) {
    _planets[0]._settings[2] = value;
    _planets[0]._update = true;
}


//Venus
void SonificationModule::onVenusEnabledChanged(bool value) {
    _planets[1]._settings[0] = value;
    _planets[1]._settings[1] = value;
    _planets[1]._settings[2] = value;
    _planets[1]._settings[3] = value;

    _venusProperty.sizeDayEnabled = value;
    _venusProperty.gravityEnabled = value;
    _venusProperty.atmosphereEnabled = value;
    _venusProperty.moonsEnabled = value;
    _planets[1]._update = true;
}

void SonificationModule::onVenusSizeDayChanged(bool value) {
    _planets[1]._settings[1] = value;
    _planets[1]._update = true;
}

void SonificationModule::onVenusGravityChanged(bool value) {
    _planets[1]._settings[2] = value;
    _planets[1]._update = true;
}

void SonificationModule::onVenusAtmosphereChanged(bool value) {
    _planets[1]._settings[3] = value;
    _planets[1]._update = true;
}


//Earth
void SonificationModule::onEarthEnabledChanged(bool value) {
    _planets[2]._settings[0] = value;
    _planets[2]._settings[1] = value;
    _planets[2]._settings[2] = value;
    _planets[2]._settings[3] = value;
    _planets[2]._settings[4] = value;

    _earthProperty.sizeDayEnabled = value;
    _earthProperty.gravityEnabled = value;
    _earthProperty.atmosphereEnabled = value;
    _earthProperty.moonsEnabled = value;
    _planets[2]._update = true;
}

void SonificationModule::onEarthSizeDayChanged(bool value) {
    _planets[2]._settings[1] = value;
    _planets[2]._update = true;
}

void SonificationModule::onEarthGravityChanged(bool value) {
    _planets[2]._settings[2] = value;
    _planets[2]._update = true;
}

void SonificationModule::onEarthAtmosphereChanged(bool value) {
    _planets[2]._settings[3] = value;
    _planets[2]._update = true;
}

void SonificationModule::onEarthMoonsChanged(bool value) {
    _planets[2]._settings[4] = value;
    _planets[2]._update = true;
}


//Mars
void SonificationModule::onMarsEnabledChanged(bool value) {
    _planets[3]._settings[0] = value;
    _planets[3]._settings[1] = value;
    _planets[3]._settings[2] = value;
    _planets[3]._settings[3] = value;
    _planets[3]._settings[4] = value;

    _marsProperty.sizeDayEnabled = value;
    _marsProperty.gravityEnabled = value;
    _marsProperty.atmosphereEnabled = value;
    _marsProperty.moonsEnabled = value;
    _planets[3]._update = true;
}

void SonificationModule::onMarsSizeDayChanged(bool value) {
    _planets[3]._settings[1] = value;
    _planets[3]._update = true;
}

void SonificationModule::onMarsGravityChanged(bool value) {
    _planets[3]._settings[2] = value;
    _planets[3]._update = true;
}

void SonificationModule::onMarsAtmosphereChanged(bool value) {
    _planets[3]._settings[3] = value;
    _planets[3]._update = true;
}

void SonificationModule::onMarsMoonsChanged(bool value) {
    _planets[3]._settings[4] = value;
    _planets[3]._update = true;
}


SonificationModule::PlanetProperty::PlanetProperty(
    properties::PropertyOwner::PropertyOwnerInfo planetInfo)
    : properties::PropertyOwner(planetInfo),
    enabled(EnableInfo, false),
    sizeDayEnabled(SizeDayInfo, false),
    gravityEnabled(GravityInfo, false),
    atmosphereEnabled(AtmosphereInfo, false),
    moonsEnabled(MoonsInfo, false)
{
    //Common
    addProperty(enabled);
    addProperty(sizeDayEnabled);
    addProperty(gravityEnabled);

    //Unique
    if(planetInfo.identifier.compare("Mercury") != 0)
        addProperty(atmosphereEnabled);
    if(planetInfo.identifier.compare("Mercury") != 0 && planetInfo.identifier.compare("Venus") != 0)
        addProperty(moonsEnabled);
}

//Extract the data from the given identifier
//NOTE: The identifier must start with capital letter,
//otherwise no match will be found
void SonificationModule::extractData(const std::string& identifier, int i,
    const Scene * const scene, const glm::dvec3& cameraPosition,
    const glm::dvec3& cameraDirection, const glm::dvec3& cameraUpVector)
{
    SceneGraphNode* node = scene->sceneGraphNode(identifier);

    if (node) {
        glm::dvec3 nodePosition = node->worldPosition();

        if (nodePosition != glm::dvec3(0.0, 0.0, 0.0)) {
            //Calculate distance to the planet from the camera, convert to km
            glm::dvec3 cameraToNode = nodePosition - cameraPosition;
            double distance = glm::length(cameraToNode)/1000.0;
            double timeSpeed = global::timeManager.deltaTime() / NUM_SEC_PER_DAY;
            double angle;
            bool updateMoons = false;

            //Calculate angle differently if planetary view or solar view
            if (_isPlanetaryView) {
                //Calculate angle from camera to the planet in the camera plane
                //Project v down to the camera plane, Pplane(v)
                //Pn(v) is v projected on the normal n of the plane
                //Pplane(v) = v - Pn(v)
                glm::dvec3 cameraToProjectedNode = (nodePosition -
                    glm::proj(cameraToNode, cameraUpVector)) - cameraPosition;

                angle = glm::orientedAngle(glm::normalize(cameraDirection),
                    glm::normalize(cameraToProjectedNode), 
                    glm::normalize(cameraUpVector));

                //If this planet is in focus then calculate the angle from
                //the planet to its moons and send them too
                for (int m = 0; m < _planets[i]._moons.size(); ++m) {
                    SceneGraphNode* moon = scene->sceneGraphNode(_planets[i]._moons[m].first);
                    if (moon) {
                        glm::dvec3 planetToMoon = moon->worldPosition() - nodePosition;
                        glm::dvec3 planetToProjectedMoon = planetToMoon - glm::proj(planetToMoon, cameraUpVector);

                        //Easy switch between different angles
                        //Angle from planet to moon with respect to camera

                        //NOTE: This might not work if the camera is looking straight down on the planet,
                        //weired behaviour when switching from upside to downside vice versa
                        double moonAngle = glm::orientedAngle(glm::normalize(cameraDirection), glm::normalize(planetToProjectedMoon), glm::normalize(cameraUpVector));

                        //Angle from camera to the moon projected on camera plane
                        //glm::dvec3 cameraToProjectedMoon = (moon->worldPosition() - glm::proj(moon->worldPosition() - cameraPosition, cameraUpVector)) - cameraPosition;
                        //double moonAngle = glm::orientedAngle(glm::normalize(cameraDirection), glm::normalize(cameraToProjectedMoon), glm::normalize(cameraUpVector));

                        if (abs(_planets[i]._moons[m].second - moonAngle) > _anglePrecision) {
                            updateMoons = true;
                            _planets[i]._moons[m].second = moonAngle;
                        }
                    }
                }
            }
            else {
                //Solar view, calculate angle from sun (origin) to node, 
                //with x axis as forward and y axis as upwards 
                //NOTE: Does not take into accoutnt the cameras position
                //Angle from Sun
                //angle = glm::orientedAngle(glm::normalize(nodePosition), glm::normalize(glm::dvec3(1.0, 0.0, 0.0)), glm::normalize(glm::dvec3(0.0, 1.0, 0.0)));

                //angle from sun with respect to the camera
                angle = glm::orientedAngle(glm::normalize(cameraDirection), glm::normalize(nodePosition - glm::proj(nodePosition, cameraUpVector)), glm::normalize(cameraUpVector));

                //Angle from camera
                //glm::dvec3 cameraToProjectedNode = (nodePosition - glm::proj(cameraToNode, cameraUpVector)) - cameraPosition;
                //angle = glm::orientedAngle(glm::normalize(cameraDirection), glm::normalize(cameraToProjectedNode), glm::normalize(cameraUpVector));
            }
            
            //Check if this data is new, otherwise dont send the data
            if (abs(_planets[i]._distance - distance) > _distancePrecision || 
                abs(_planets[i]._angle - angle) > _anglePrecision ||
                abs(_previousTimeSpeed - timeSpeed) > _timePrecision || updateMoons ||
                _planets[i]._update)
            {
                //Update the saved data for the planet
                _planets[i].setDistance(distance);
                _planets[i].setAngle(angle);
                _previousTimeSpeed = timeSpeed;

                //Send the data to SuperCollider
                //NOTE: Socket cannot be saved in class, it does not work then,
                //dont know why. Only works if the socket is recreated
                std::string label = "/" + identifier;
                UdpTransmitSocket socket = UdpTransmitSocket(
                    IpEndpointName(SC_IP_ADDRESS, SC_PORT));
                _stream.Clear();
                osc::Blob settingsBlob = osc::Blob(_planets[i]._settings, NUM_SETTINGS);
                _stream << osc::BeginMessage(label.c_str()) << distance << angle << timeSpeed << settingsBlob;

                //Add the information of the moons if any
                for (int m = 0; m < _planets[i]._moons.size(); ++m) {
                    _stream << _planets[i]._moons[m].second;
                }

                _stream << osc::EndMessage;
                socket.Send(_stream.Data(), _stream.Size());
                _planets[i]._update = false;
            }
        }
    }
}

void SonificationModule::threadMain(std::atomic<bool>& isRunning) {
    
    Scene* scene = nullptr;
    Camera* camera = nullptr;
    glm::dvec3 cameraDirection, cameraPosition, cameraUpVector;
    const SceneGraphNode* focusNode = nullptr;
    const SceneGraphNode *previousFocusNode = nullptr;

    while (isRunning) {

        scene = global::renderEngine.scene();
        if (scene && scene->root()->children().size() > 0) {
           
            camera = scene->camera();
            
            if (camera) {
                cameraPosition = camera->positionVec3();
                cameraDirection = camera->viewDirectionWorldSpace();
                cameraUpVector = camera->lookUpVectorWorldSpace();

                //Complete scene initialized, start extracting data
                if (cameraPosition != glm::dvec3(1.0, 1.0, 1.0)) {
                    
                    //Which node is in focus?
                    focusNode = global::navigationHandler.orbitalNavigator().anchorNode();
                    if (!focusNode) continue;
                    
                    //Check if focus has changed
                    if (!previousFocusNode || previousFocusNode->identifier()
                        .compare(focusNode->identifier()) != 0) 
                    {                        
                        //Update
                        previousFocusNode = focusNode;

                        //Let the sonification know which node is in focus,
                        //is used to switch different sonifications
                        UdpTransmitSocket socket = UdpTransmitSocket(
                            IpEndpointName(SC_IP_ADDRESS, SC_PORT));
                        _stream.Clear();
                        std::string focusLabel = "/focus";
                        _stream << osc::BeginMessage(focusLabel.c_str()) << 
                            focusNode->identifier().c_str() << osc::EndMessage;
                        socket.Send(_stream.Data(), _stream.Size());

                        //If focus is on the sun, switch sonification view
                        if (focusNode->identifier().compare("Sun") == 0)
                            _isPlanetaryView = false;
                        else
                            _isPlanetaryView = true;
                    }

                    //Extract data from all the planets
                    for (int i = 0; i < NUM_PLANETS; ++i) {
                        
                        //Only send data if something new has happened
                        //If the node is in focus, increase sensitivity
                        if (focusNode->identifier().compare(_planets[i]._identifier) == 0) {
                            _anglePrecision = 0.05;
                            _distancePrecision = 1000.0;
                        }
                        else {
                            _anglePrecision = 0.1;
                            _distancePrecision = 10000.0;
                        }

                        extractData(_planets[i]._identifier, i, scene,
                            cameraPosition, cameraDirection, cameraUpVector);
                    }
                }
            }
        }
    }
}

void SonificationModule::internalInitialize(const ghoul::Dictionary&)
{
    //start a thread to extract data to the sonification
    _thread = std::thread([this]() { threadMain(std::ref(_isRunning)); });
}

void SonificationModule::internalDeinitialize() {
    //Turn off the sonification in SuperCollider
    for (int i = 0; i < NUM_PLANETS; ++i) {
        for (int s = 0; s < NUM_SETTINGS; ++s) {
            _planets[i]._settings[s] = false;
        }

        UdpTransmitSocket socket = UdpTransmitSocket(IpEndpointName(SC_IP_ADDRESS, SC_PORT));
        _stream.Clear();
        std::string label = "/" + _planets[i]._identifier;
        osc::Blob settingsBlob = osc::Blob(_planets[i]._settings, NUM_SETTINGS);
        _stream << osc::BeginMessage(label.c_str()) <<
            _planets[i]._distance << _planets[i]._angle << _previousTimeSpeed << settingsBlob;

        //Add the information of the moons if any
        for (int m = 0; m < _planets[i]._moons.size(); ++m) {
            _stream << _planets[i]._moons[m].second;
        }

        _stream << osc::EndMessage;
        socket.Send(_stream.Data(), _stream.Size());
    }

    //Clear data
    delete[] _buffer;
    _isRunning = false;
    if (_thread.joinable())
        _thread.join();
}

SonificationModule::~SonificationModule() { }

} // namespace openspace
