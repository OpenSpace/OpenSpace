/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONMODULE___H__
#define __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONMODULE___H__

#include "modules/sonification/ext/osc/ip/UdpSocket.h"
#include "modules/sonification/ext/osc/osc/OscOutboundPacketStream.h"
#include <string>
#include <thread>
#include <atomic>
#include <ghoul/glm.h>
#include <openspace/scene/scene.h>
#include <vector>
#include <utility>
#include <openspace/properties/scalar/boolproperty.h>

#define NUM_PLANETS 8
#define NUM_SEC_PER_DAY 86400.0
#define NUM_SETTINGS 5

#include <openspace/util/openspacemodule.h>

namespace openspace {

class SonificationModule : public OpenSpaceModule {
public:
    SonificationModule();
    ~SonificationModule();

    //Extract the data from the given identifier
    //NOTE: The identifier must start with capital letter,
    //otherwise no match will be found
    void extractData(const std::string& identifier, int i, const Scene * const scene,
        const glm::dvec3& cameraPosition, const glm::dvec3& cameraDirection,
        const glm::dvec3& cameraUpVector);

protected:
    void internalInitialize(const ghoul::Dictionary& dictionary) override;
    void internalDeinitialize() override;

private:
    //Main function for _thread
    void threadMain(std::atomic<bool>& isRunning);

    //On change methods for each property
    void onEarthEnabledChanged(bool value);
    void onEarthSizeDayChanged(bool value);
    void onEarthGravityChanged(bool value);
    void onEarthAtmosphereChanged(bool value);
    void onEarthMoonsChanged(bool value);

    void onMarsEnabledChanged(bool value);
    void onMarsSizeDayChanged(bool value);
    void onMarsGravityChanged(bool value);
    void onMarsAtmosphereChanged(bool value);
    void onMarsMoonsChanged(bool value);

    //Struct to hold data for all the planets
    struct Planet {
        Planet() {
            _identifier = "";
            _distance = 0.0;
            _angle = 0.0;
            _update = false;
        }

        Planet(std::string identifier) {
            _identifier = identifier;
            _distance = 0.0;
            _angle = 0.0;
            _update = false;
        }

        void setDistance(double distance) {
            _distance = distance;
        }

        void setAngle(double angle) {
            _angle = angle;
        }

        std::string _identifier;
        double _distance;
        double _angle;
        std::vector<std::pair<std::string, double>> _moons;
        bool _settings[NUM_SETTINGS] = {false, false, false, false, false};
        bool _update;
    };

    char* _buffer;
    osc::OutboundPacketStream _stream;
    std::thread _thread;
    std::atomic<bool> _isRunning;
    double _anglePrecision;
    double _distancePrecision;
    double _previousTimeSpeed;
    double _timePrecision;
    Planet _planets[NUM_PLANETS];
    bool _isPlanetaryView;

    //Properties
    struct PlanetProperty : properties::PropertyOwner {
        PlanetProperty(properties::PropertyOwner::PropertyOwnerInfo planetInfo);

        //Common
        properties::BoolProperty enabled;
        properties::BoolProperty sizeDayEnabled;
        properties::BoolProperty gravityEnabled;

        //Unique
        properties::BoolProperty atmosphereEnabled;
        properties::BoolProperty moonsEnabled;
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo _EarthInfo = {
        "Earth",
        "Earth",
        "Sonification settings for Earth"
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo _MarsInfo = {
        "Mars",
        "Mars",
        "Sonification settings for Mars"
    };

    PlanetProperty _earthProperty = PlanetProperty(_EarthInfo);
    PlanetProperty _marsProperty = PlanetProperty(_MarsInfo);
};

} // namespace openspace

#endif __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONMODULE___H__
