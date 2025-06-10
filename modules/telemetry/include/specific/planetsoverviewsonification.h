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

#ifndef __OPENSPACE_MODULE_TELEMETRY___PLANETSOVERVIEWSONIFICATION___H__
#define __OPENSPACE_MODULE_TELEMETRY___PLANETSOVERVIEWSONIFICATION___H__

#include <modules/telemetry/include/telemetrybase.h>

namespace openspace {

class PlanetsOverviewSonification : public TelemetryBase {
public:
    PlanetsOverviewSonification(const std::string& ip, int port);
    ~PlanetsOverviewSonification() override = default;

    /**
     * Function to stop the sonification.
     */
    void stop() override;

private:
    /**
     * Create an osc::Blob object with the current planets overview sonification settings.
     * Order of settings: Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune.
     *
     * \return An osc::Blob object with the current planets overview sonification settings
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
     * Send current planets overview sonification settings to the Open Sound Control
     * receiver. The order of sent data is as follows: Planets overview settings.
     */
    void sendData() override;

    void onToggleAllChanged();

    properties::BoolProperty _toggleAll;
    properties::BoolProperty _mercuryEnabled;
    properties::BoolProperty _venusEnabled;
    properties::BoolProperty _earthEnabled;
    properties::BoolProperty _marsEnabled;
    properties::BoolProperty _jupiterEnabled;
    properties::BoolProperty _saturnEnabled;
    properties::BoolProperty _uranusEnabled;
    properties::BoolProperty _neptuneEnabled;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_TELEMETRY___PLANETSOVERVIEWSONIFICATION___H__
