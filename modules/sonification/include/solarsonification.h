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

#include <openspace/scene/scene.h>

namespace openspace {

class SolarSonification : public SonificationBase {
public:
    SolarSonification(const std::string& ip, int port);
    virtual ~SolarSonification() override;

    virtual void update() override;

private:
    Scene* _scene = nullptr;
    Camera* _camera = nullptr;

    /**
     * Extracts data from the given identifier
     * \param identifier of the scene graph node to extract data from
     * \param i index in internal list of planets that corresponds to the identifier
     *
     * \return boolean if the data extracted is new or not
     */
    bool extractData(const std::string& identifier, int i);

    // Check the speed of the simulated time
    void checkTimeSpeed();

    osc::Blob createSettingsBlob() const;
    void sendSettings();
    void onAllEnabledChanged();
    void onSettingChanged();

    //Struct to hold data for all the planets
    struct Planet {
        Planet(std::string id = "") {
            identifier = id;
        }

        std::string identifier;
        double distance = 0.0;
        double angle = 0.0;
    };

    double _anglePrecision;
    double _distancePrecision;
    double _timeSpeed;
    double _timePrecision;
    Planet _planets[8];

    //Properties
    properties::BoolProperty _enableAll;
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
