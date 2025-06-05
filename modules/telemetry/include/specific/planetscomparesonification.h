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

#ifndef __OPENSPACE_MODULE_TELEMETRY___PLANETSCOMPARESONIFICATION___H__
#define __OPENSPACE_MODULE_TELEMETRY___PLANETSCOMPARESONIFICATION___H__

#include <modules/telemetry/include/telemetrybase.h>

#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/misc/optionproperty.h>

namespace openspace {

class PlanetsCompareSonification : public TelemetryBase {
public:
    PlanetsCompareSonification(const std::string& ip, int port);
    ~PlanetsCompareSonification() override = default;

    /**
     * Function to stop the sonification.
     */
    void stop() override;

private:
    /**
     * Create an osc::Blob object with the current compare planets sonification settings.
     * Order of settings: Size/day, gravity, temperature, atmosphere, moons, rings.
     *
     * \return An osc::Blob object with the current compare planets sonification settings
     */
    osc::Blob createSettingsBlob() const;

    /**
     * For this sonification, this implementiation is empty since the GUI properties keep
     * track of tha data with the `onChange` function.
     *
     * \param camera The camera in the scene (not used in this case)
     * \return `true` if the data was updated, otherwise `false`
     */
    bool updateData(const Camera* camera) override;

    /**
     * Send current compare planets sonification settings to the Open Sound Control
     * receiver. The order of sent data is as follows: Name of the first selected planet,
     * name of the second selected planet, and compare planets settings.
     */
    void sendData() override;

    /**
     * Function that gets called when either the first or second planet selection
     * was changed with the GUI.
     *
     * \param changedPlanet The planet selection that was recently changed
     * \param otherPlanet The other planet selection that was NOT changed
     * \param prevChangedPlanet The previous value of the planet that was changed
     */
    void onPlanetSelectionChanged(properties::OptionProperty& changedPlanet,
        properties::OptionProperty& otherPlanet, std::string& prevChangedPlanet);

    /**
     * Function that scales the given planet by the given amount over the given amount of
     * seconds.
     *
     * \param planet The identifer of the planet that should be scaled
     * \param scale The amount that the planet should be scaled with as a multiplier of
     *        the original size
     * \param interpolationTime The amount of time in seconds to interpolate to the new
     *        scale for the planet
     */
    void scalePlanet(const std::string& planet, double scale,
        double interpolationTime);

    void onUpscaleChanged();
    void onFirstChanged();
    void onSecondChanged();
    void onToggleAllChanged();

    properties::DoubleProperty _selectedUpscale;
    properties::DoubleProperty _selectedScaleInterpolationTime;
    properties::OptionProperty _firstPlanet;
    properties::OptionProperty _secondPlanet;

    properties::BoolProperty _toggleAll;
    properties::BoolProperty _sizeDayEnabled;
    properties::BoolProperty _gravityEnabled;
    properties::BoolProperty _temperatureEnabled;
    properties::BoolProperty _atmosphereEnabled;
    properties::BoolProperty _moonsEnabled;
    properties::BoolProperty _ringsEnabled;

    std::string _oldFirst;
    std::string _oldSecond;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_TELEMETRY___PLANETSCOMPARESONIFICATION___H__
