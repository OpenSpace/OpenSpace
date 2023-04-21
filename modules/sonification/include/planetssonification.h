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

#ifndef __OPENSPACE_MODULE_SONIFICATION___PLANETSSONIFICATION___H__
#define __OPENSPACE_MODULE_SONIFICATION___PLANETSSONIFICATION___H__

#include <modules/sonification/include/sonificationbase.h>

#include <openspace/properties/scalar/boolproperty.h>

namespace openspace {

namespace scripting { struct LuaLibrary; }

class PlanetsSonification : public SonificationBase {
public:
    PlanetsSonification(const std::string& ip, int port);
    virtual ~PlanetsSonification() override;

    /**
     * Main update function for the sonification
     *
     * \param camera pointer to the camera in the scene
     */
    virtual void update(const Camera* camera) override;

    /**
     * Function to stop the sonification
     */
    virtual void stop() override;

    /**
    * Add the given planet to the list of planets and moons
    *
    * \param dict the planet that should be added
    */
    void addPlanet(ghoul::Dictionary dict);

    /**
     * Returns the Lua library that contains all Lua functions available to change the
     * planets sonification.
     * \return The Lua library that contains all Lua functions available to change the
     * planets sonification
     */
    static scripting::LuaLibrary luaLibrary();

private:
    // Indices for data items
    const int NumDataItems = 3;
    const int DistanceIndex = 0;
    const int HAngleIndex = 1;
    const int VAngleIndex = 2;
    const int MoonHAngleIndex = 0;
    const int MoonVAngleIndex = 1;

    // Set the differnet levels of precision
    const double LowDistancePrecision = 10000.0;
    const double HighDistancePrecision = 1000.0;
    const double LowAnglePrecision = 0.1;
    const double HighAnglePrecision = 0.05;

    // Indices for the planets
    const int MercuryIndex = 0;
    const int VenusIndex = 1;
    const int EarthIndex = 2;
    const int MarsIndex = 3;
    const int JupiterIndex = 4;
    const int SaturnIndex = 5;
    const int UranusIndex = 6;
    const int NeptuneIndex = 7;

    // Indices for the settings for the planets
    const int SizeDayIndex = 0;
    const int GravityIndex = 1;
    const int TemperatureIndex = 2;
    const int AtmosphereIndex = 3;
    const int MoonsIndex = 4;
    const int RingsIndex = 5;

    // Struct to hold data for all the planets
    struct Planet {
        Planet(std::string id = "") {
            identifier = id;
        }

        std::string identifier;

        // Distance, horizontal angle, vertical angle
        std::vector<double> data = std::vector<double>(3);

        // <name of moon, <horizontal angle, vertical angle>>
        std::vector<std::pair<std::string, std::vector<double>>> moons;
    };

    /**
     * Update distance and angle data for the given planet
     *
     * \param camera pointer to the camera in the scene. Used to calculated the data for
     *               the planet
     * \param planetIndex index to the internally stored planet data that should be
     *                    updated
     *
     * \return true if the data is new compared to before, otherwise false
     */
    bool getData(const Camera* camera, int planetIndex);

    /**
     * Create a osc::Blob object with current sonification settings for the indicated
     * planet. Order of settings: size/day, gravity, temperature, (atmosphere, moons,
     * rings).
     *
     * \param planetIndex indicates which planet to create the settings blob for
     *
     * \return a osc::Blob object with current sonificaiton settings
     */
    osc::Blob createSettingsBlob(int planetIndex) const;

    /**
     * Send current sonification settings for the indicated planet over the osc connection
     * Order of data: distance, angle, settings, moon angles
     */
    void sendSettings(int planetIndex);

    // Properties onChange
    void onToggleAllChanged();

    //Mercury
    void onMercuryAllChanged();
    void onMercurySettingChanged();

    //Venus
    void onVenusAllChanged();
    void onVenusSettingChanged();

    //Earth
    void onEarthAllChanged();
    void onEarthSettingChanged();

    //Mars
    void onMarsAllChanged();
    void onMarsSettingChanged();

    //Jupiter
    void onJupiterAllChanged();
    void onJupiterSettingChanged();

    //Saturn
    void onSaturnAllChanged();
    void onSaturnSettingChanged();

    //Uranus
    void onUranusAllChanged();
    void onUranusSettingChanged();

    //Neptune
    void onNeptuneAllChanged();
    void onNeptuneSettingChanged();

    double _anglePrecision;
    double _distancePrecision;
    std::vector<Planet> _planets;

    // Properties
    struct PlanetProperty : properties::PropertyOwner {
        PlanetProperty(properties::PropertyOwner::PropertyOwnerInfo planetInfo);

        // All planets have these
        properties::BoolProperty toggleAll;
        properties::BoolProperty sizeDayEnabled;
        properties::BoolProperty gravityEnabled;
        properties::BoolProperty temperatureEnabled;

        // Some planets have these
        properties::BoolProperty atmosphereEnabled;
        properties::BoolProperty moonsEnabled;
        properties::BoolProperty ringsEnabled;
    };

    properties::BoolProperty _toggleAll;
    PlanetProperty _mercuryProperty;
    PlanetProperty _venusProperty;
    PlanetProperty _earthProperty;
    PlanetProperty _marsProperty;
    PlanetProperty _jupiterProperty;
    PlanetProperty _saturnProperty;
    PlanetProperty _uranusProperty;
    PlanetProperty _neptuneProperty;
};

} // namespace openspace

#endif __OPENSPACE_MODULE_SONIFICATION___PLANETSSONIFICATION___H__