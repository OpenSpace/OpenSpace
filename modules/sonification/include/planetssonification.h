/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_SONIFICATION___PLANETSSONIFICATION___H__
#define __OPENSPACE_MODULE_SONIFICATION___PLANETSSONIFICATION___H__

#include <modules/sonification/include/sonificationbase.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/scene/scene.h>

namespace {
    constexpr int NumPlanets = 8;
    constexpr int NumPlanetarySettings = 6;
} // namespace

namespace openspace {

class PlanetsSonification : public SonificationBase {
public:
    enum class GUIMode {
        Solar = 0,
        Planetary,
        Compare
    };

    PlanetsSonification(const std::string& ip, int port);
    virtual ~PlanetsSonification() override;

    virtual void update() override;

private:
    Scene* _scene = nullptr;
    Camera* _camera = nullptr;

    //Extract the data from the given identifier
    void extractData(const std::string& identifier, int i);

    //Property functions
    void setAllSolarProperties(bool value);
    void setAllPlanetaryProperties(bool value);

    //Check the speed of the simulated time
    void checkTimeSpeed(double& ts);

    //On change methods for each property
    void onEverythingChanged(bool value);

    //Solar
    void sendSolarSettings();
    void onSolarAllEnabledChanged(bool value);
    void onSolarMercuryEnabledChanged(bool value);
    void onSolarVenusEnabledChanged(bool value);
    void onSolarEarthEnabledChanged(bool value);
    void onSolarMarsEnabledChanged(bool value);
    void onSolarJupiterEnabledChanged(bool value);
    void onSolarSaturnEnabledChanged(bool value);
    void onSolarUranusEnabledChanged(bool value);
    void onSolarNeptuneEnabledChanged(bool value);

    //Compare
    void sendCompareSettings();
    void onFirstCompareChanged(properties::OptionProperty::Option value);
    void onSecondCompareChanged(properties::OptionProperty::Option value);
    void onCompareAllChanged(bool value);
    void onCompareSizeDayChanged(bool value);
    void onCompareGravityChanged(bool value);
    void onCompareTemperatureChanged(bool value);
    void onCompareAtmosphereChanged(bool value);
    void onCompareMoonsChanged(bool value);
    void onCompareRingsChanged(bool value);

    //Planetary
    void sendPlanetarySettings(const int planetIndex);
    void onAllEnabledChanged(bool value);

    //Mercury
    void onMercuryEnabledChanged(bool value);
    void onMercurySizeDayChanged(bool value);
    void onMercuryGravityChanged(bool value);
    void onMercuryTemperatureChanged(bool value);

    //Venus
    void onVenusEnabledChanged(bool value);
    void onVenusSizeDayChanged(bool value);
    void onVenusGravityChanged(bool value);
    void onVenusTemperatureChanged(bool value);
    void onVenusAtmosphereChanged(bool value);

    //Earth
    void onEarthEnabledChanged(bool value);
    void onEarthSizeDayChanged(bool value);
    void onEarthGravityChanged(bool value);
    void onEarthTemperatureChanged(bool value);
    void onEarthAtmosphereChanged(bool value);
    void onEarthMoonsChanged(bool value);

    //Mars
    void onMarsEnabledChanged(bool value);
    void onMarsSizeDayChanged(bool value);
    void onMarsGravityChanged(bool value);
    void onMarsTemperatureChanged(bool value);
    void onMarsAtmosphereChanged(bool value);
    void onMarsMoonsChanged(bool value);

    //Jupiter
    void onJupiterEnabledChanged(bool value);
    void onJupiterSizeDayChanged(bool value);
    void onJupiterGravityChanged(bool value);
    void onJupiterTemperatureChanged(bool value);
    void onJupiterAtmosphereChanged(bool value);
    void onJupiterMoonsChanged(bool value);

    //Saturn
    void onSaturnEnabledChanged(bool value);
    void onSaturnSizeDayChanged(bool value);
    void onSaturnGravityChanged(bool value);
    void onSaturnTemperatureChanged(bool value);
    void onSaturnAtmosphereChanged(bool value);
    void onSaturnMoonsChanged(bool value);
    void onSaturnRingsChanged(bool value);

    //Uranus
    void onUranusEnabledChanged(bool value);
    void onUranusSizeDayChanged(bool value);
    void onUranusGravityChanged(bool value);
    void onUranusTemperatureChanged(bool value);
    void onUranusAtmosphereChanged(bool value);
    void onUranusMoonsChanged(bool value);

    //Neptune
    void onNeptuneEnabledChanged(bool value);
    void onNeptuneSizeDayChanged(bool value);
    void onNeptuneGravityChanged(bool value);
    void onNeptuneTemperatureChanged(bool value);
    void onNeptuneAtmosphereChanged(bool value);
    void onNeptuneMoonsChanged(bool value);

    //Struct to hold data for all the planets
    struct Planet {
        Planet() {
            identifier = "";
            distance = 0.0;
            angle = 0.0;
            shouldUpdate = false;
        }

        Planet(std::string inIdentifier) {
            identifier = inIdentifier;
            distance = 0.0;
            angle = 0.0;
            shouldUpdate = false;
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
        //Sonification settings for each planet
        //[0] size/day enabled, [1] gravity enabled, [2] temperature enabled,
        //[3] atmosphere enabled, [4] moons enabled, [5] rings enabled
        bool settings[NumPlanetarySettings] =
        { false, false, false, false, false, false };
        bool shouldUpdate;
    };

    double _anglePrecision;
    double _distancePrecision;
    double _timeSpeed;
    double _timePrecision;
    Planet _planets[NumPlanets];
    GUIMode _GUIState;
    std::string _oldCompareFirst;
    std::string _oldCompareSecond;

    //Settings for each planet
    //[0] mercury enabled, [1] venus enabled, [2] earth enabled, [3] mars enabled,
    //[4] jupiter enabled, [5] saturn enabled, [6] uranus enabled, [7] neptuen enabled
    bool _solarSettings[NumPlanets] =
    { false, false, false, false, false, false, false, false };

    //Settings of which parameters to compare between the selected planets
    //[0] size/day, [1] gravity, [2] temperature,
    //[3] atmosphere, [4] moons, [5] rings
    bool _compareSettings[NumPlanetarySettings] =
    { false, false, false, false, false, false };

    //Properties
    //Planetary View
    struct PlanetProperty : properties::PropertyOwner {
        PlanetProperty(properties::PropertyOwner::PropertyOwnerInfo planetInfo);

        //Common
        properties::BoolProperty enabled;
        properties::BoolProperty sizeDayEnabled;
        properties::BoolProperty gravityEnabled;
        properties::BoolProperty temperatureEnabled;

        //Unique
        properties::BoolProperty atmosphereEnabled;
        properties::BoolProperty moonsEnabled;
        properties::BoolProperty ringsEnabled;
    };

    struct PlanetHeadProperty : properties::PropertyOwner {
        PlanetHeadProperty(
            properties::PropertyOwner::PropertyOwnerInfo planetHeadInfo,
            properties::PropertyOwner::PropertyOwnerInfo inMercuryInfo,
            properties::PropertyOwner::PropertyOwnerInfo inVenusInfo,
            properties::PropertyOwner::PropertyOwnerInfo inEarthInfo,
            properties::PropertyOwner::PropertyOwnerInfo inMarsInfo,
            properties::PropertyOwner::PropertyOwnerInfo inJupiterInfo,
            properties::PropertyOwner::PropertyOwnerInfo inSaturnInfo,
            properties::PropertyOwner::PropertyOwnerInfo inUranusInfo,
            properties::PropertyOwner::PropertyOwnerInfo inNeptuneInfo);

        properties::BoolProperty allEnabled;
        PlanetProperty mercuryProperty;
        PlanetProperty venusProperty;
        PlanetProperty earthProperty;
        PlanetProperty marsProperty;
        PlanetProperty jupiterProperty;
        PlanetProperty saturnProperty;
        PlanetProperty uranusProperty;
        PlanetProperty neptuneProperty;
    };

    const openspace::properties::Property::PropertyInfo _EverythingInfo = {
        "EverythingInfo",
        "Enable all",
        "Enable all the planet and solar sonifications, NOT the compare sonifications"
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

    const openspace::properties::PropertyOwner::PropertyOwnerInfo _JupiterInfo = {
        "Jupiter",
        "Jupiter Sonification",
        "Sonification settings for Jupiter. Only works if the sun is NOT in focus."
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo _SaturnInfo = {
        "Saturn",
        "Saturn Sonification",
        "Sonification settings for Saturn. Only works if the sun is NOT in focus."
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo _UranusInfo = {
        "Uranus",
        "Uranus Sonification",
        "Sonification settings for Uranus. Only works if the sun is NOT in focus."
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo _NeptuneInfo = {
        "Neptune",
        "Neptune Sonification",
        "Sonification settings for Neptune. Only works if the sun is NOT in focus."
    };

    PlanetHeadProperty _planetsProperty = PlanetHeadProperty(_PlanetsInfo, _MercuryInfo,
        _VenusInfo, _EarthInfo, _MarsInfo, _JupiterInfo, _SaturnInfo, _UranusInfo,
        _NeptuneInfo);

    //Solar View
    struct SolarProperty : properties::PropertyOwner {
        SolarProperty();

        properties::BoolProperty allEnabled;
        properties::BoolProperty mercuryEnabled;
        properties::BoolProperty venusEnabled;
        properties::BoolProperty earthEnabled;
        properties::BoolProperty marsEnabled;
        properties::BoolProperty jupiterEnabled;
        properties::BoolProperty saturnEnabled;
        properties::BoolProperty uranusEnabled;
        properties::BoolProperty neptuneEnabled;
    };

    SolarProperty _solarProperty = SolarProperty();

    //Compare View
    struct CompareProperty : properties::PropertyOwner {
        CompareProperty();

        properties::OptionProperty firstPlanet;
        properties::OptionProperty secondPlanet;

        properties::BoolProperty allEnabled;
        properties::BoolProperty sizeDayEnabled;
        properties::BoolProperty gravityEnabled;
        properties::BoolProperty temperatureEnabled;
        properties::BoolProperty atmosphereEnabled;
        properties::BoolProperty moonsEnabled;
        properties::BoolProperty ringsEnabled;
    };

    CompareProperty _compareProperty = CompareProperty();
    properties::BoolProperty _everythingEnabled =
        properties::BoolProperty(_EverythingInfo, false);

};

} // namespace openspace

#endif __OPENSPACE_MODULE_SONIFICATION___PLANETSSONIFICATION___H__
