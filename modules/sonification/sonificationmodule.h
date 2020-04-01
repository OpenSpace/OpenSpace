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
#include <openspace/properties/optionproperty.h>

#define NUM_PLANETS 8
#define NUM_SEC_PER_DAY 86400.0
#define NUM_PLANETARY_SETTINGS 4
#define NUM_SOLAR_SETTINGS 4

#include <openspace/util/openspacemodule.h>

namespace openspace {

class SonificationModule : public OpenSpaceModule {
public:
    enum GUIMode {
        Solar,
        Planetary,
        Compare
    };

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
    //Solar
    void onSolarAllEnabledChanged(bool value);
    void onSolarMercuryEnabledChanged(bool value);
    void onSolarVenusEnabledChanged(bool value);
    void onSolarEarthEnabledChanged(bool value);
    void onSolarMarsEnabledChanged(bool value);

    //Compare
    void onFirstCompareChanged(properties::OptionProperty::Option value);
    void onSecondCompareChanged(properties::OptionProperty::Option value);

    //Planetary
    //Mercury
    void onMercuryEnabledChanged(bool value);
    void onMercurySizeDayChanged(bool value);
    void onMercuryGravityChanged(bool value);

    //Venus
    void onVenusEnabledChanged(bool value);
    void onVenusSizeDayChanged(bool value);
    void onVenusGravityChanged(bool value);
    void onVenusAtmosphereChanged(bool value);

    //Earth
    void onEarthEnabledChanged(bool value);
    void onEarthSizeDayChanged(bool value);
    void onEarthGravityChanged(bool value);
    void onEarthAtmosphereChanged(bool value);
    void onEarthMoonsChanged(bool value);

    //Mars
    void onMarsEnabledChanged(bool value);
    void onMarsSizeDayChanged(bool value);
    void onMarsGravityChanged(bool value);
    void onMarsAtmosphereChanged(bool value);
    void onMarsMoonsChanged(bool value);

    //Struct to hold data for all the planets
    struct Planet {
        Planet() {
            identifier = "";
            distance = 0.0;
            angle = 0.0;
            update = false;
        }

        Planet(std::string inIdentifier) {
            identifier = inIdentifier;
            distance = 0.0;
            angle = 0.0;
            update = false;
        }

        void setDistance(double inDistance) {
            distance = inDistance;
        }

        void setAngle(double inAngle) {
            angle = inAngle;
        }

        std::string identifier;
        double distance;
        double angle;
        std::vector<std::pair<std::string, double>> moons;
        //Settings for each planet
        //[0] size/day enabled, [1] gravity enabled, [2] atmosphere enabled, [3] moons enabled
        bool settings[NUM_PLANETARY_SETTINGS] = {false, false, false, false};
        bool update;
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
    GUIMode _GUIState;

    //Settings for each planet
    //[0] mercury enabled, [1] venus enabled, [2] earth enabled, [3] mars enabled
    bool _solarSettings[NUM_SOLAR_SETTINGS] = { false, false, false, false};

    //Properties
    //Planetary View
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

    struct PlanetHeadProperty : properties::PropertyOwner {
        PlanetHeadProperty(properties::PropertyOwner::PropertyOwnerInfo planetHeadInfo,
            properties::PropertyOwner::PropertyOwnerInfo inMercuryInfo,
            properties::PropertyOwner::PropertyOwnerInfo inVenusInfo,
            properties::PropertyOwner::PropertyOwnerInfo inEarthInfo,
            properties::PropertyOwner::PropertyOwnerInfo inMarsInfo);

        PlanetProperty mercuryProperty;
        PlanetProperty venusProperty;
        PlanetProperty earthProperty;
        PlanetProperty marsProperty;
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo _PlanetsInfo = {
        "Planetary View",
        "Planets Sonification",
        "Sonification settings for the planets. Only works if the sun is NOT in focus."
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo _MercuryInfo = {
        "Mercury",
        "Mercury Sonification",
        "Sonification settings for Mercury. Only works if the sun is NOT in focus."
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo _VenusInfo = {
        "Venus",
        "Venus Sonification",
        "Sonification settings for Venus. Only works if the sun is NOT in focus."
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo _EarthInfo = {
        "Earth",
        "Earth Sonification",
        "Sonification settings for Earth. Only works if the sun is NOT in focus."
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo _MarsInfo = {
        "Mars",
        "Mars Sonification",
        "Sonification settings for Mars. Only works if the sun is NOT in focus."
    };

    PlanetHeadProperty _planetsProperty = PlanetHeadProperty(_PlanetsInfo, _MercuryInfo,
        _VenusInfo, _EarthInfo, _MarsInfo);

    //Solar View
    struct SolarProperty : properties::PropertyOwner {
        SolarProperty();

        properties::BoolProperty allEnabled;
        properties::BoolProperty mercuryEnabled;
        properties::BoolProperty venusEnabled;
        properties::BoolProperty earthEnabled;
        properties::BoolProperty marsEnabled;
    };

    SolarProperty _solarProperty = SolarProperty();

    //Compare View
    struct CompareProperty : properties::PropertyOwner {
        CompareProperty();

        properties::OptionProperty firstPlanet;
        properties::OptionProperty secondPlanet;
    };

    CompareProperty _compareProperty = CompareProperty();
};

} // namespace openspace

#endif __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONMODULE___H__
