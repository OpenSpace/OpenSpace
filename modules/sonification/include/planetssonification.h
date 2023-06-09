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
    static const int NumSettings = 6;

    // Set the differnet levels of precision
    const double LowDistancePrecision = 10000.0;
    const double HighDistancePrecision = 1000.0;
    const double LowAnglePrecision = 0.1;
    const double HighAnglePrecision = 0.05;

    // Indices for the planets
    static const int MercuryIndex = 0;
    static const int VenusIndex = 1;
    static const int EarthIndex = 2;
    static const int MarsIndex = 3;
    static const int JupiterIndex = 4;
    static const int SaturnIndex = 5;
    static const int UranusIndex = 6;
    static const int NeptuneIndex = 7;

    // Indices for the settings for the planets
    static const int SizeDayIndex = 0;
    static const int GravityIndex = 1;
    static const int TemperatureIndex = 2;
    static const int AtmosphereIndex = 3;
    static const int MoonsIndex = 4;
    static const int RingsIndex = 5;

    // Number of samples to smooth out the data for the sonification
    static const int NumSamples = 60;

    // Struct to hold data for all the planets and moons
    struct Moon {
    public:
        Moon(std::string id = "") {
            identifier = id;
        }

        // Get a smoothed out value from the data
        double HAngle() const {
            return SonificationBase::calcMedian(horizontalAngles);
        }
        double VAngle() const {
            return SonificationBase::calcMedian(verticalAngles);
        }

        // Add value to ring buffers
        void addHAngle(double angle) {
            SonificationBase::addValueToRingBuffer(
                horizontalAngles,
                ringBufferIndex,
                NumSamples,
                angle
            );
        }
        void addVAngle(double angle) {
            SonificationBase::addValueToRingBuffer(
                verticalAngles,
                ringBufferIndex,
                NumSamples,
                angle
            );
        }

        std::string identifier;

    private:
        // Horizontal and vertical angle
        std::vector<double> horizontalAngles = std::vector<double>(NumSamples, 0.0);
        std::vector<double> verticalAngles = std::vector<double>(NumSamples, 0.0);

        // The first "empty" slot in the ring buffer order. The "oldest" value
        int ringBufferIndex = 0;
    };

    struct Planet {
    public:
        Planet(std::string id = "") {
            identifier = id;
        }

        // Get a smoothed out value from the data
        double distance() const {
            return SonificationBase::calcMedian(distances);
        }
        double HAngle() const {
            return SonificationBase::calcMedian(horizontalAngles);
        }
        double VAngle() const {
            return SonificationBase::calcMedian(verticalAngles);
        }

        // Add values to ring buffers
        void addDistance(const double distance) {
            SonificationBase::addValueToRingBuffer(
                distances,
                indexDistances,
                NumSamples,
                distance
            );
        }
        void addHAngle(const double angle) {
            SonificationBase::addValueToRingBuffer(
                horizontalAngles,
                indexHAngles,
                NumSamples,
                angle
            );
        }
        void addVAngle(const double angle) {
            SonificationBase::addValueToRingBuffer(
                verticalAngles,
                indexVAngles,
                NumSamples,
                angle
            );
        }

        std::string identifier;
        std::vector<Moon> moons;

    private:
        // Distance, horizontal angle, vertical angle
        std::vector<double> distances = std::vector<double>(NumSamples, 0.0);
        std::vector<double> horizontalAngles = std::vector<double>(NumSamples, 0.0);
        std::vector<double> verticalAngles = std::vector<double>(NumSamples, 0.0);

        // The first "empty" slot in the ring buffer order. The "oldest" value
        int indexDistances = 0;
        int indexHAngles = 0;
        int indexVAngles = 0;
    };

    /**
     * Set all settings for all planets to the given value
     */
    void setAll(bool value);

    /**
     * Set all settings for Mercury to the given value
     */
    void setAllMercury(bool value);

    /**
     * Set all settings for Mercury to the given value
     */
    void setAllVenus(bool value);

    /**
     * Set all settings for Mercury to the given value
     */
    void setAllEarth(bool value);

    /**
     * Set all settings for Mercury to the given value
     */
    void setAllMars(bool value);

    /**
     * Set all settings for Mercury to the given value
     */
    void setAllJupiter(bool value);

    /**
     * Set all settings for Mercury to the given value
     */
    void setAllSaturn(bool value);

    /**
     * Set all settings for Mercury to the given value
     */
    void setAllUranus(bool value);

    /**
     * Set all settings for Mercury to the given value
     */
    void setAllNeptune(bool value);

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
     * Create a vector with current sonification settings for the indicated
     * planet. Order of settings: size/day, gravity, temperature, (atmosphere, moons,
     * rings).
     *
     * \param planetIndex indicates which planet to create the settings blob for
     *
     * \return a osc::Blob object with current sonificaiton settings
     */
    std::vector<int> createSettingsVector(int planetIndex) const;

    /**
     * Send current sonification settings for the indicated planet over the osc connection
     * Order of data: distance, angle, settings, moon angles
     */
    void sendSettings(int planetIndex);

    // Properties onChange
    void onEnabledChanged();
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
    bool _inSolarPerspective = false;

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
