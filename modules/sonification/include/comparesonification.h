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

#ifndef __OPENSPACE_MODULE_SONIFICATION___COMPARESONIFICATION___H__
#define __OPENSPACE_MODULE_SONIFICATION___COMPARESONIFICATION___H__

#include <modules/sonification/include/sonificationbase.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/scene/scene.h>

namespace openspace {

class CompareSonification : public SonificationBase {
public:

    CompareSonification(const std::string& ip, int port);
    virtual ~CompareSonification() override;

    virtual void update() override;

private:
    Scene* _scene = nullptr;
    Camera* _camera = nullptr;

    //Extract the data from the given identifier
    bool extractData(const std::string& identifier, int i);

    //Property functions
    void setAllPlanetaryProperties(bool value);

    //Check the speed of the simulated time
    void checkTimeSpeed(double& ts);

    //Compare
    osc::Blob createSettingsBlob() const;
    void sendSettings();
    void onFirstChanged();
    void onSecondChanged();
    void onAllChanged();
    void onSettingChanged();

    //Struct to hold data for all the planets
    struct Planet {
        Planet(std::string id = "") {
            identifier = id;
        }

        std::string identifier;
        double distance = 0.0;
        double angle = 0.0;
        std::vector<std::pair<std::string, double>> moons;
    };

    double _anglePrecision;
    double _distancePrecision;
    double _timeSpeed;
    double _timePrecision;
    Planet _planets[8];
    std::string _oldFirst;
    std::string _oldSecond;

    //Properties

    //Compare View
    properties::OptionProperty _firstPlanet;
    properties::OptionProperty _secondPlanet;

    properties::BoolProperty _enableAll;
    properties::BoolProperty _sizeDayEnabled;
    properties::BoolProperty _gravityEnabled;
    properties::BoolProperty _temperatureEnabled;
    properties::BoolProperty _atmosphereEnabled;
    properties::BoolProperty _moonsEnabled;
    properties::BoolProperty _ringsEnabled;
};

} // namespace openspace

#endif __OPENSPACE_MODULE_SONIFICATION___COMPARESONIFICATION___H__
