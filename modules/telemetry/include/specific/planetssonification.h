/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
    ~PlanetsSonification() override = default;

    /**
     * Main update function to gather planets telemetry information (distance, horizontal
     * angle, vertical angle, distance to moons, horizontal angle to moons, and vertical
     * angle to moons) for the planets sonification and send it to the Open Sound Control
     * receiver.
     *
     * \param camera The camera in the scene
     */
    void update(const Camera* camera) override;

    /**
     * Function to stop the sonification.
     */
    void stop() override;

    /**
     * Add the given planet information to the list of planets and their moons.
     *
     * \param dict The planet that should be added
     */
    void addPlanet(const ghoul::Dictionary& dict);

    /**
     * Returns the Lua library that contains all Lua functions available to change the
     * planets sonification.
     *
     * \return The Lua library that contains all Lua functions available to change the
     *         planets sonification
     */
    static scripting::LuaLibrary luaLibrary();

private:
    // To hold all the data for a planet or a moon, including a list of data for the
    // moons of the planet. However, in the case of a moon, then the list of moons
    // is empty
    struct DataBody {
        DataBody(std::string inName);

        std::string name;
        double distance = 0.0;
        double horizontalAngle = 0.0;
        double verticalAngle = 0.0;

        // List of moons that orbit this planet (in the case that this is a moon, then
        // this list is empty)
        std::vector<DataBody> moons;
    };

    /**
     * Create an osc::Blob object with the current sonification settings for the indicated
     * planet.
     * Order of settings: Size/day, gravity, temperature, and optionally atmosphere,
     *                    moons, and rings.
     *
     * \param planetIndex The index of the planet to create the settings blob for
     * \return An osc::Blob object with current sonification settings for the indicated
     *         planet
     */
    osc::Blob createSettingsBlob(int planetIndex) const;

    /**
     * For this sonification, a more advanced custom updateData function is needed with
     * additional arguments. Therefore, this implementation is left empty and the update
     * function is overriden to use the custom updateData function instead.
     *
     * \param camera The camera in the scene (not used in this case)
     * \return Always return `false` (this function is empty)
     */
    bool updateData(const Camera* camera) override;

    /**
     * For this sonification, a more advanced custom sendData function is needed with
     * additional arguments. Therefore, this implementation is left empty and the update
     * function is overriden to use the custom updateData function instead.
     */
    void sendData() override;

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
    bool updateData(const Camera* camera, int planetIndex,
        TelemetryModule::AngleCalculationMode angleCalculationMode,
        bool includeElevation);

    /**
     * Send the current sonification settings for the indicated planet to the Open Sound
     * Control receiver. The order of sent data is as follows: distance, horizontal angle,
     * vertical angle, settings, and data for each moon in order of distance to the planet
     * (distance, horizontal angle, and vertical angle).
     */
    void sendData(int planetIndex);

    void onToggleAllChanged();
    void onMercuryAllChanged();
    void onMercurySettingChanged();
    void onVenusAllChanged();
    void onVenusSettingChanged();
    void onEarthAllChanged();
    void onEarthSettingChanged();
    void onMarsAllChanged();
    void onMarsSettingChanged();
    void onJupiterAllChanged();
    void onJupiterSettingChanged();
    void onSaturnAllChanged();
    void onSaturnSettingChanged();
    void onUranusAllChanged();
    void onUranusSettingChanged();
    void onNeptuneAllChanged();
    void onNeptuneSettingChanged();

    struct PlanetProperties : properties::PropertyOwner {
        PlanetProperties(properties::PropertyOwner::PropertyOwnerInfo planetInfo);

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

    struct PrecisionProperties : properties::PropertyOwner {
        PrecisionProperties(properties::PropertyOwner::PropertyOwnerInfo precisionInfo);

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
    PlanetProperties _mercuryProperty;
    PlanetProperties _venusProperty;
    PlanetProperties _earthProperty;
    PlanetProperties _marsProperty;
    PlanetProperties _jupiterProperty;
    PlanetProperties _saturnProperty;
    PlanetProperties _uranusProperty;
    PlanetProperties _neptuneProperty;
    PrecisionProperties _precisionProperties;

    std::vector<DataBody> _planets;

    double _anglePrecision;
    double _distancePrecision;
    bool _overviewEnabled = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_TELEMETRY___PLANETSSONIFICATION___H__
