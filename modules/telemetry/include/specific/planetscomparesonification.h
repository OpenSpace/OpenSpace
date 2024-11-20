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

#ifndef __OPENSPACE_MODULE_TELEMETRY___PLANETSCOMPARESONIFICATION___H__
#define __OPENSPACE_MODULE_TELEMETRY___PLANETSCOMPARESONIFICATION___H__

#include <modules/telemetry/include/telemetrybase.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/optionproperty.h>

namespace openspace {

class PlanetsCompareSonification : public TelemetryBase {
public:
    PlanetsCompareSonification(const std::string& ip, int port);
    virtual ~PlanetsCompareSonification() override;

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

private:
    // Indices for data items
    static constexpr int NumDataItems = 3;
    static constexpr int FirstPlanetIndex = 0;
    static constexpr int SecondPlanetIndex = 1;
    static constexpr int SettingsIndex = 2;

    // Indicies for the gui settings
    static constexpr int NumSettings = 6;
    static constexpr int SizeDayIndex = 0;
    static constexpr int GravityIndex = 1;
    static constexpr int TemperatureIndex = 2;
    static constexpr int AtmosphereIndex = 3;
    static constexpr int MoonsIndex = 4;
    static constexpr int RingsIndex = 5;

    /**
     * Create a osc::Blob object with current sonification settings.
     * Order of settings: size/day, gravity, temperature, atmosphere, moons, rings
     *
     * \return a osc::Blob object with current sonificaiton settings
     */
    osc::Blob createSettingsBlob() const;

    /**
     * Send current sonification settings over the osc connection
     * Order of data: name of first planet, name of second planet, settings
     */
    void sendSettings();

    /**
     * Function that gets called when either the first or second planet selection
     * was changed
     *
     * \param changedPlanet the planet that was recently changed
     * \param notChangedPlanet the planet that was NOT changed
     * \param prevChangedPlanet the previous value of the planet that was recently changed
     */
    void planetSelectionChanged(properties::OptionProperty& changedPlanet,
        properties::OptionProperty& notChangedPlanet, std::string& prevChangedPlanet);

    // Properties onChange
    void onFirstChanged();
    void onSecondChanged();
    void onToggleAllChanged();

    float _focusScale = 2000.f;
    std::string _oldFirst;
    std::string _oldSecond;

    // Properties
    properties::OptionProperty _firstPlanet;
    properties::OptionProperty _secondPlanet;

    properties::BoolProperty _toggleAll;
    properties::BoolProperty _sizeDayEnabled;
    properties::BoolProperty _gravityEnabled;
    properties::BoolProperty _temperatureEnabled;
    properties::BoolProperty _atmosphereEnabled;
    properties::BoolProperty _moonsEnabled;
    properties::BoolProperty _ringsEnabled;
};

} // namespace openspace

#endif __OPENSPACE_MODULE_SONIFICATION___PLANETSCOMPARESONIFICATION___H__
