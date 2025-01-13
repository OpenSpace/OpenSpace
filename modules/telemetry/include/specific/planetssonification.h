/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_TELEMETRY___PLANETSSONIFICATION___H__
#define __OPENSPACE_MODULE_TELEMETRY___PLANETSSONIFICATION___H__

#include <modules/telemetry/include/telemetrybase.h>

#include <modules/telemetry/telemetrymodule.h>
#include <openspace/properties/scalar/doubleproperty.h>

namespace openspace {

namespace scripting { struct LuaLibrary; }

class PlanetsSonification : public TelemetryBase {
public:
    PlanetsSonification(const std::string& ip, int port);
    virtual ~PlanetsSonification() override;

    /**
     * Main update function to gather planets telemetry information (distance, horizontal
     * angle, vertical angle, distance to moons, horizontal angle to moons, and vertical
     * angle to moons) for the planets sonificaiton and send it via the osc connection.
     *
     * \param camera The camera in the scene
     */
    virtual void update(const Camera* camera) override;

    /**
     * Function to stop the sonification.
     */
    virtual void stop() override;

    /**
     * Add the given planet information to the list of planets and their moons.
     *
     * \param dict The planet that should be added
     */
    void addPlanet(ghoul::Dictionary dict);

    /**
     * Returns the Lua library that contains all Lua functions available to change the
     * planets sonification.
     *
     * \return The Lua library that contains all Lua functions available to change the
     *         planets sonification
     */
    static scripting::LuaLibrary luaLibrary();

private:
    // Number of data items for planets and moons, which is used to calculate the total
    // size of the data vector sent over the osc connection
    static constexpr int NumDataItemsPlanet = 4;
    static constexpr int NumDataItemsMoon = 3;

    // Indices for the planets
    static constexpr int MercuryIndex = 0;
    static constexpr int VenusIndex = 1;
    static constexpr int EarthIndex = 2;
    static constexpr int MarsIndex = 3;
    static constexpr int JupiterIndex = 4;
    static constexpr int SaturnIndex = 5;
    static constexpr int UranusIndex = 6;
    static constexpr int NeptuneIndex = 7;

    // Indices for the settings for the planets
    static constexpr int NumSettings = 6;
    static constexpr int SizeDayIndex = 0;
    static constexpr int GravityIndex = 1;
    static constexpr int TemperatureIndex = 2;
    static constexpr int AtmosphereIndex = 3;
    static constexpr int MoonsIndex = 4;
    static constexpr int RingsIndex = 5;

    // To hold all the data for a planet or a moon, including a list of data for the
    // moons of the planet. However, in the case of a moon, then the list of moons
    // is empty
    struct DataBody {
        DataBody(std::string inName = "") {
            name = inName;
        }

        std::string name;
        double distance = 0.0;
        double horizontalAngle = 0.0;
        double verticalAngle = 0.0;

        // List of moons that orbit this planet (in the case that this is a moon, then
        // this list is empty)
        std::vector<DataBody> moons;
    };

    /**
     * Update the distance and angle information for the given planet.
     *
     * \param camera The camera in the scene
     * \param planetIndex The index to the internally stored planet data that should be
     *        updated
     * \param angleCalculationMode The angle calculation mode to use. This determines
     *        which method to use when calculating the angle.
     * \param includeElevation Whether the additional elevation angle should be calculated
     *
     * \return `true` if the data is new compared to before, otherwise `false`
     */
    bool getData(const Camera* camera, int planetIndex,
        TelemetryModule::AngleCalculationMode angleCalculationMode,
        bool includeElevation);

    /**
     * Create an osc::Blob object with the current sonification settings for the indicated
     * planet.
     * Order of settings: Size/day, gravity, temperature, and optionaly atmosphere, moons,
     *                    and rings.
     *
     * \param planetIndex The index of the planet to create the settings blob for
     *
     * \return An osc::Blob object with current sonificaiton settings for the indicated
     *         planet
     */
    osc::Blob createSettingsBlob(int planetIndex) const;

    /**
     * Send the current sonification settings for the indicated planet over the osc
     * connection.
     * Order of data: distance, horizontal angle, vertical angle, settings, data for each
     *                moon (distance, horizontal angle, and vertical angle)
     */
    void sendPlanetData(int planetIndex);

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

    struct PlanetProperty : properties::PropertyOwner {
        PlanetProperty(properties::PropertyOwner::PropertyOwnerInfo planetInfo);

        // All planets have these settings
        properties::BoolProperty toggleAll;
        properties::BoolProperty sizeDayEnabled;
        properties::BoolProperty gravityEnabled;
        properties::BoolProperty temperatureEnabled;

        // Only some planets have some of these settings
        properties::BoolProperty atmosphereEnabled;
        properties::BoolProperty moonsEnabled;
        properties::BoolProperty ringsEnabled;
    };

    struct PrecisionProperty : properties::PropertyOwner {
        PrecisionProperty(properties::PropertyOwner::PropertyOwnerInfo precisionInfo);

        // The low and high precision values are used in different situations. When the
        // planet is the current focus node, then the high precision value is used. This
        // is due to the sonification and planet being in the current focus and should
        // therefore have better precision. If the planet is not the current focus node,
        // then the low precision value is used to save performance, both on the
        // OpenSpace side and the receiving side.
        properties::DoubleProperty lowDistancePrecision;
        properties::DoubleProperty highDistancePrecision;
        properties::DoubleProperty lowAnglePrecision;
        properties::DoubleProperty highAnglePrecision;
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
    PrecisionProperty _precisionProperty;

    std::vector<DataBody> _planets;

    double _anglePrecision;
    double _distancePrecision;
};

} // namespace openspace

#endif __OPENSPACE_MODULE_TELEMETRY___PLANETSSONIFICATION___H__
