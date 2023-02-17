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

#include <openspace/properties/optionproperty.h>

namespace openspace {

class PlanetsSonification : public SonificationBase {
public:
    PlanetsSonification(const std::string& ip, int port);
    virtual ~PlanetsSonification() override;

    virtual void update(const Scene* scene, const Camera* camera) override;

private:
    //Extract the data from the given identifier
    bool extractData(const Camera* camera, const std::string& identifier, int i);

    //Property functions
    osc::Blob createSettingsBlob(int planetIndex) const;
    void sendSettings(const int planetIndex);
    void onAllEnabledChanged();

    //Mercury
    void onMercuryEnabledChanged();
    void onMercurySettingChanged();

    //Venus
    void onVenusEnabledChanged();
    void onVenusSettingChanged();

    //Earth
    void onEarthEnabledChanged();
    void onEarthSettingChanged();

    //Mars
    void onMarsEnabledChanged();
    void onMarsSettingChanged();

    //Jupiter
    void onJupiterEnabledChanged();
    void onJupiterSettingChanged();

    //Saturn
    void onSaturnEnabledChanged();
    void onSaturnSettingChanged();

    //Uranus
    void onUranusEnabledChanged();
    void onUranusSettingChanged();

    //Neptune
    void onNeptuneEnabledChanged();
    void onNeptuneSettingChanged();

    //Struct to hold data for all the planets and moons
    struct Planet {
        Planet(std::string id = "") {
            identifier = id;
        }

        std::string identifier;
        double distance = 0.0;
        double angle = 0.0;

        // std::pair<name of moon, latset calculated angle to it>
        std::vector<std::pair<std::string, double>> moons;
    };

    double _anglePrecision;
    double _distancePrecision;
    std::vector<Planet> _planets;

    //Properties
    struct PlanetProperty : properties::PropertyOwner {
        PlanetProperty(properties::PropertyOwner::PropertyOwnerInfo planetInfo);

        // All planets have these
        properties::BoolProperty enabled;
        properties::BoolProperty sizeDayEnabled;
        properties::BoolProperty gravityEnabled;
        properties::BoolProperty temperatureEnabled;

        // Some planets have these
        properties::BoolProperty atmosphereEnabled;
        properties::BoolProperty moonsEnabled;
        properties::BoolProperty ringsEnabled;
    };

    properties::BoolProperty _enableAll;
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
