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

#ifndef __OPENSPACE_MODULE_SONIFICATION___SOLARSONIFICATION___H__
#define __OPENSPACE_MODULE_SONIFICATION___SOLARSONIFICATION___H__

#include <modules/sonification/include/sonificationbase.h>

namespace openspace {

class SolarSonification : public SonificationBase {
public:
    SolarSonification(const std::string& ip, int port);
    virtual ~SolarSonification() override;

    /**
     * Main update function for the sonification
     *
     * \param camera pointer to the camera in the scene
     */
    virtual void update(const Camera* camera) override;

private:
    // Struct to hold data for all the planets
    struct Planet {
        Planet(std::string id = "") {
            identifier = id;
        }

        std::string identifier;
        double distance = 0.0;
        double angle = 0.0;
    };

    /**
     * Update distance and angle data for the given planet
     *
     * \param camera pointer to the camera in the scene. Used to calculated the data for
     *               the planet
     * \param planet a reference to the internally stored planet data that should be
     *               updated
     *
     * \return true if the data is new compared to before, otherwise false
     */
    bool getData(const Camera* camera, Planet& planet);

    /**
     * Create a osc::Blob object with current sonification settings.
     * Order of settings: Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune
     *
     * \return a osc::Blob object with current sonificaiton settings
     */
    osc::Blob createSettingsBlob() const;

    /**
     * Send current sonification settings over the osc connection
     */
    void sendSettings();

    // Properties onChange
    void onToggleAllChanged();

    std::vector<Planet> _planets;

    // Properties
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

#endif __OPENSPACE_MODULE_SONIFICATION___SOLARSONIFICATION___H__
