/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <modules/sonification/include/planetssonification.h>

#include <openspace/camera/camera.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/timemanager.h>
#include <glm/gtx/projection.hpp>
#include <glm/gtx/vector_angle.hpp>

namespace {
    constexpr int NumSecPerDay = 86400;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        PlanetsSonificationInfo =
    {
       "PlanetsSonification",
       "Planets Sonification",
       "Sonification of the planets in our solarsystem"
    };

    //Solar View
    static const openspace::properties::PropertyOwner::PropertyOwnerInfo SolarInfo = {
       "Solar View",
       "Solar View Sonification",
       "Sonification settings for the solar view. Only works if the sun IS in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo EnableAllInfo = {
        "EnabledallInfo",
        "All",
        "Play sonification for all planets in the solar view. "
        "Only works if the sun IS in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo EnableMercuryInfo = {
        "EnabledMercuryInfo",
        "Mercury",
        "Play sonification for Mercury. Only works if the sun IS in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo EnableVenusInfo = {
        "EnabledVenusInfo",
        "Venus",
        "Play sonification for Venus. Only works if the sun IS in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo EnableEarthInfo = {
        "EnabledEarthInfo",
        "Earth",
        "Play sonification for Earth. Only works if the sun IS in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo EnableMarsInfo = {
        "EnabledMarsInfo",
        "Mars",
        "Play sonification for Mars. Only works if the sun IS in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo EnableJupiterInfo = {
        "EnabledJupiterInfo",
        "Jupiter",
        "Play sonification for Jupiter. Only works if the sun IS in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo EnableSaturnInfo = {
        "EnabledSaturnInfo",
        "Saturn",
        "Play sonification for Saturn. Only works if the sun IS in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo EnableUranusInfo = {
        "EnabledUranusInfo",
        "Uranus",
        "Play sonification for Uranus. Only works if the sun IS in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo EnableNeptuneInfo = {
        "EnabledNeptuneInfo",
        "Neptune",
        "Play sonification for Neptune. Only works if the sun IS in focus."
    };


    //Compare View
    static const openspace::properties::PropertyOwner::PropertyOwnerInfo CompareInfo = {
       "Compare View",
       "Compare Sonifications",
       "Sonification settings for the compare view."
    };

    constexpr openspace::properties::Property::PropertyInfo CompareOptionsInfo = {
        "CompareOptions",
        "Choose planet to compare",
        "Chooses what planets to compare"
    };

    constexpr openspace::properties::Property::PropertyInfo CompareOptionsInfoT = {
        "CompareOptionsT",
        "Choose planet to compare",
        "Chooses what planets to compare"
    };

    constexpr openspace::properties::Property::PropertyInfo CompareAllInfo = {
        "CompareAllInfo",
        "All",
        "Play all sonifications for all the selected planets or turn it off. "
        "Only works if the sun IS in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo CompareSizeDayInfo = {
        "CompareSizeDayInfo",
        "Size/Day",
        "Play Size/Day sonification for all the selected planets or turn it off. "
        "Only works if the sun IS in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo CompareGravityInfo = {
        "CompareGravityInfo",
        "Gravity",
        "Play Gravity sonification for all the selected planets or turn it off. "
        "Only works if the sun IS in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo CompareTemperatureInfo = {
        "CompareTemperatureInfo",
        "Temperature",
        "Play Temperature sonification for all the selected planets or turn it off. "
        "Only works if the sun IS in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo CompareAtmosphereInfo = {
        "CompareAtmosphereInfo",
        "Atmosphere",
        "Play Atmosphere sonification for all the selected planets or turn it off. "
        "Only works if the sun IS in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo CompareMoonsInfo = {
        "CompareMoonsInfo",
        "Moons",
        "Play Moons sonification for all the selected planets or turn it off. "
        "Only works if the sun IS in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo CompareRingsInfo = {
        "CompareRingssInfo",
        "Rings",
        "Play Rings sonification for all the selected planets or turn it off. "
        "Only works if the sun IS in focus."
    };


    //Planetary View
    constexpr openspace::properties::Property::PropertyInfo EnableAllPlanetsInfo = {
        "EnableAllPlanetsInfo",
        "All",
        "Play all sonifications for all the planets or turn it off. "
        "Only works if the sun is NOT in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo EnableInfo = {
        "EnabledInfo",
        "All",
        "Play all sonifications for the planet or turn it off. "
        "Only works if the sun is NOT in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeDayInfo = {
        "SizeDayInfo",
        "Size/Day",
        "Play Size/Day sonification or turn it off. "
        "Only works if the sun is NOT in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo GravityInfo = {
        "GravityInfo",
        "Gravity",
        "Play Gravity sonification or turn it off. "
        "Only works if the sun is NOT in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo TemperatureInfo = {
        "TemperatureInfo",
        "Temperature",
        "Play Temperature sonification or turn it off. "
        "Only works if the sun is NOT in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo AtmosphereInfo = {
        "AtmosphereInfo",
        "Atmosphere",
        "Play Atmosphere sonification or turn it off. "
        "Only works if the sun is NOT in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo MoonsInfo = {
        "MoonsInfo",
        "Moons",
        "Play Moons sonification or turn it off. Only works if the sun is NOT in focus."
    };

    constexpr openspace::properties::Property::PropertyInfo RingsInfo = {
        "RingsInfo",
        "Rings",
        "Play Rings sonification or turn it off. Only works if the sun is NOT in focus."
    };
} // namespace

namespace openspace {

PlanetsSonification::PlanetsSonification()
    : SonificationBase(PlanetsSonificationInfo)
{
    _scene = global::renderEngine->scene();
    _camera = _scene? _scene->camera() : nullptr;

    _GUIState = PlanetsSonification::GUIMode::Planetary;
    _timeSpeed = 0.0;
    _timePrecision = 0.0001;

    //Fill the _planets array
    {
        _planets[0] = Planet("Mercury");
        _planets[1] = Planet("Venus");

        _planets[2] = Planet("Earth");
        _planets[2].moons.reserve(1);
        _planets[2].moons.push_back({ "Moon", 0.0 });

        _planets[3] = Planet("Mars");
        _planets[3].moons.reserve(2);
        _planets[3].moons.push_back({ "Phobos", 0.0 });
        _planets[3].moons.push_back({ "Deimos", 0.0 });

        _planets[4] = Planet("Jupiter");
        _planets[4].moons.reserve(4);
        _planets[4].moons.push_back({ "Io", 0.0 });
        _planets[4].moons.push_back({ "Europa", 0.0 });
        _planets[4].moons.push_back({ "Ganymede", 0.0 });
        _planets[4].moons.push_back({ "Callisto", 0.0 });

        _planets[5] = Planet("Saturn");
        _planets[5].moons.reserve(8);
        _planets[5].moons.push_back({ "Dione", 0.0 });
        _planets[5].moons.push_back({ "Enceladus", 0.0 });
        _planets[5].moons.push_back({ "Hyperion", 0.0 });
        _planets[5].moons.push_back({ "Iapetus", 0.0 });
        _planets[5].moons.push_back({ "Mimas", 0.0 });
        _planets[5].moons.push_back({ "Rhea", 0.0 });
        _planets[5].moons.push_back({ "Tethys", 0.0 });
        _planets[5].moons.push_back({ "Titan", 0.0 });

        _planets[6] = Planet("Uranus");
        _planets[6].moons.reserve(5);
        _planets[6].moons.push_back({ "Ariel", 0.0 });
        _planets[6].moons.push_back({ "Miranda", 0.0 });
        _planets[6].moons.push_back({ "Oberon", 0.0 });
        _planets[6].moons.push_back({ "Titania", 0.0 });
        _planets[6].moons.push_back({ "Umbriel", 0.0 });

        _planets[7] = Planet("Neptune");
        _planets[7].moons.reserve(1);
        _planets[7].moons.push_back({ "Triton", 0.0 });
    }

    //Add onChange for the properties
    _everythingEnabled.onChange([this]() {
        onEverythingChanged(_everythingEnabled.value()); });

    //Solar
    _solarProperty.allEnabled.onChange([this]() {
        onSolarAllEnabledChanged(_solarProperty.allEnabled.value()); });
    _solarProperty.mercuryEnabled.onChange([this]() {
        onSolarMercuryEnabledChanged(_solarProperty.mercuryEnabled.value()); });
    _solarProperty.venusEnabled.onChange([this]() {
        onSolarVenusEnabledChanged(_solarProperty.venusEnabled.value()); });
    _solarProperty.earthEnabled.onChange([this]() {
        onSolarEarthEnabledChanged(_solarProperty.earthEnabled.value()); });
    _solarProperty.marsEnabled.onChange([this]() {
        onSolarMarsEnabledChanged(_solarProperty.marsEnabled.value()); });
    _solarProperty.jupiterEnabled.onChange([this]() {
        onSolarJupiterEnabledChanged(_solarProperty.jupiterEnabled.value()); });
    _solarProperty.saturnEnabled.onChange([this]() {
        onSolarSaturnEnabledChanged(_solarProperty.saturnEnabled.value()); });
    _solarProperty.uranusEnabled.onChange([this]() {
        onSolarUranusEnabledChanged(_solarProperty.uranusEnabled.value()); });
    _solarProperty.neptuneEnabled.onChange([this]() {
        onSolarNeptuneEnabledChanged(_solarProperty.neptuneEnabled.value()); });

    //Compare
    _compareProperty.firstPlanet.onChange([this]() {
        onFirstCompareChanged(_compareProperty.firstPlanet.option()); });
    _compareProperty.secondPlanet.onChange([this]() {
        onSecondCompareChanged(_compareProperty.secondPlanet.option()); });
    _compareProperty.allEnabled.onChange([this]() {
        onCompareAllChanged(_compareProperty.allEnabled.value()); });
    _compareProperty.sizeDayEnabled.onChange([this]() {
        onCompareSizeDayChanged(_compareProperty.sizeDayEnabled.value()); });
    _compareProperty.gravityEnabled.onChange([this]() {
        onCompareGravityChanged(_compareProperty.gravityEnabled.value()); });
    _compareProperty.temperatureEnabled.onChange([this]() {
        onCompareTemperatureChanged(_compareProperty.temperatureEnabled.value()); });
    _compareProperty.atmosphereEnabled.onChange([this]() {
        onCompareAtmosphereChanged(_compareProperty.atmosphereEnabled.value()); });
    _compareProperty.moonsEnabled.onChange([this]() {
        onCompareMoonsChanged(_compareProperty.moonsEnabled.value()); });
    _compareProperty.ringsEnabled.onChange([this]() {
        onCompareRingsChanged(_compareProperty.ringsEnabled.value()); });

    //Planetary View
    _planetsProperty.allEnabled.onChange([this]() {
        onAllEnabledChanged(_planetsProperty.allEnabled.value()); });

    //Mercury
    _planetsProperty.mercuryProperty.enabled.onChange([this]() {
        onMercuryEnabledChanged(_planetsProperty.mercuryProperty.enabled.value()); });
    _planetsProperty.mercuryProperty.sizeDayEnabled.onChange([this]() {
        onMercurySizeDayChanged(_planetsProperty.mercuryProperty.sizeDayEnabled.value()); });
    _planetsProperty.mercuryProperty.gravityEnabled.onChange([this]() {
        onMercuryGravityChanged(_planetsProperty.mercuryProperty.gravityEnabled.value()); });
    _planetsProperty.mercuryProperty.temperatureEnabled.onChange([this]() {
        onMercuryTemperatureChanged(_planetsProperty.mercuryProperty.temperatureEnabled.value()); });

    //Venus
    _planetsProperty.venusProperty.enabled.onChange([this]() {
        onVenusEnabledChanged(_planetsProperty.venusProperty.enabled.value()); });
    _planetsProperty.venusProperty.sizeDayEnabled.onChange([this]() {
        onVenusSizeDayChanged(_planetsProperty.venusProperty.sizeDayEnabled.value()); });
    _planetsProperty.venusProperty.gravityEnabled.onChange([this]() {
        onVenusGravityChanged(_planetsProperty.venusProperty.gravityEnabled.value()); });
    _planetsProperty.venusProperty.temperatureEnabled.onChange([this]() {
        onVenusTemperatureChanged(_planetsProperty.venusProperty.temperatureEnabled.value()); });
    _planetsProperty.venusProperty.atmosphereEnabled.onChange([this]() {
        onVenusAtmosphereChanged(_planetsProperty.venusProperty.atmosphereEnabled.value()); });

    //Earth
    _planetsProperty.earthProperty.enabled.onChange([this]() {
        onEarthEnabledChanged(_planetsProperty.earthProperty.enabled.value()); });
    _planetsProperty.earthProperty.sizeDayEnabled.onChange([this]() {
        onEarthSizeDayChanged(_planetsProperty.earthProperty.sizeDayEnabled.value()); });
    _planetsProperty.earthProperty.gravityEnabled.onChange([this]() {
        onEarthGravityChanged(_planetsProperty.earthProperty.gravityEnabled.value()); });
    _planetsProperty.earthProperty.temperatureEnabled.onChange([this]() {
        onEarthTemperatureChanged(_planetsProperty.earthProperty.temperatureEnabled.value()); });
    _planetsProperty.earthProperty.atmosphereEnabled.onChange([this]() {
        onEarthAtmosphereChanged(_planetsProperty.earthProperty.atmosphereEnabled.value()); });
    _planetsProperty.earthProperty.moonsEnabled.onChange([this]() {
        onEarthMoonsChanged(_planetsProperty.earthProperty.moonsEnabled.value()); });

    //Mars
    _planetsProperty.marsProperty.enabled.onChange([this]() {
        onMarsEnabledChanged(_planetsProperty.marsProperty.enabled.value()); });
    _planetsProperty.marsProperty.sizeDayEnabled.onChange([this]() {
        onMarsSizeDayChanged(_planetsProperty.marsProperty.sizeDayEnabled.value()); });
    _planetsProperty.marsProperty.gravityEnabled.onChange([this]() {
        onMarsGravityChanged(_planetsProperty.marsProperty.gravityEnabled.value()); });
    _planetsProperty.marsProperty.temperatureEnabled.onChange([this]() {
        onMarsTemperatureChanged(_planetsProperty.marsProperty.temperatureEnabled.value()); });
    _planetsProperty.marsProperty.atmosphereEnabled.onChange([this]() {
        onMarsAtmosphereChanged(_planetsProperty.marsProperty.atmosphereEnabled.value()); });
    _planetsProperty.marsProperty.moonsEnabled.onChange([this]() {
        onMarsMoonsChanged(_planetsProperty.marsProperty.moonsEnabled.value()); });

    //Jupiter
    _planetsProperty.jupiterProperty.enabled.onChange([this]() {
        onJupiterEnabledChanged(_planetsProperty.jupiterProperty.enabled.value()); });
    _planetsProperty.jupiterProperty.sizeDayEnabled.onChange([this]() {
        onJupiterSizeDayChanged(_planetsProperty.jupiterProperty.sizeDayEnabled.value()); });
    _planetsProperty.jupiterProperty.gravityEnabled.onChange([this]() {
        onJupiterGravityChanged(_planetsProperty.jupiterProperty.gravityEnabled.value()); });
    _planetsProperty.jupiterProperty.temperatureEnabled.onChange([this]() {
        onJupiterTemperatureChanged(_planetsProperty.jupiterProperty.temperatureEnabled.value()); });
    _planetsProperty.jupiterProperty.atmosphereEnabled.onChange([this]() {
        onJupiterAtmosphereChanged(_planetsProperty.jupiterProperty.atmosphereEnabled.value()); });
    _planetsProperty.jupiterProperty.moonsEnabled.onChange([this]() {
        onJupiterMoonsChanged(_planetsProperty.jupiterProperty.moonsEnabled.value()); });

    //Saturn
    _planetsProperty.saturnProperty.enabled.onChange([this]() {
        onSaturnEnabledChanged(_planetsProperty.saturnProperty.enabled.value()); });
    _planetsProperty.saturnProperty.sizeDayEnabled.onChange([this]() {
        onSaturnSizeDayChanged(_planetsProperty.saturnProperty.sizeDayEnabled.value()); });
    _planetsProperty.saturnProperty.gravityEnabled.onChange([this]() {
        onSaturnGravityChanged(_planetsProperty.saturnProperty.gravityEnabled.value()); });
    _planetsProperty.saturnProperty.temperatureEnabled.onChange([this]() {
        onSaturnTemperatureChanged(_planetsProperty.saturnProperty.temperatureEnabled.value()); });
    _planetsProperty.saturnProperty.atmosphereEnabled.onChange([this]() {
        onSaturnAtmosphereChanged(_planetsProperty.saturnProperty.atmosphereEnabled.value()); });
    _planetsProperty.saturnProperty.moonsEnabled.onChange([this]() {
        onSaturnMoonsChanged(_planetsProperty.saturnProperty.moonsEnabled.value()); });
    _planetsProperty.saturnProperty.ringsEnabled.onChange([this]() {
        onSaturnRingsChanged(_planetsProperty.saturnProperty.ringsEnabled.value()); });

    //Uranus
    _planetsProperty.uranusProperty.enabled.onChange([this]() {
        onUranusEnabledChanged(_planetsProperty.uranusProperty.enabled.value()); });
    _planetsProperty.uranusProperty.sizeDayEnabled.onChange([this]() {
        onUranusSizeDayChanged(_planetsProperty.uranusProperty.sizeDayEnabled.value()); });
    _planetsProperty.uranusProperty.gravityEnabled.onChange([this]() {
        onUranusGravityChanged(_planetsProperty.uranusProperty.gravityEnabled.value()); });
    _planetsProperty.uranusProperty.temperatureEnabled.onChange([this]() {
        onUranusTemperatureChanged(_planetsProperty.uranusProperty.temperatureEnabled.value()); });
    _planetsProperty.uranusProperty.atmosphereEnabled.onChange([this]() {
        onUranusAtmosphereChanged(_planetsProperty.uranusProperty.atmosphereEnabled.value()); });
    _planetsProperty.uranusProperty.moonsEnabled.onChange([this]() {
        onUranusMoonsChanged(_planetsProperty.uranusProperty.moonsEnabled.value()); });

    //Neptune
    _planetsProperty.neptuneProperty.enabled.onChange([this]() {
        onNeptuneEnabledChanged(_planetsProperty.neptuneProperty.enabled.value()); });
    _planetsProperty.neptuneProperty.sizeDayEnabled.onChange([this]() {
        onNeptuneSizeDayChanged(_planetsProperty.neptuneProperty.sizeDayEnabled.value()); });
    _planetsProperty.neptuneProperty.gravityEnabled.onChange([this]() {
        onNeptuneGravityChanged(_planetsProperty.neptuneProperty.gravityEnabled.value()); });
    _planetsProperty.neptuneProperty.temperatureEnabled.onChange([this]() {
        onNeptuneTemperatureChanged(_planetsProperty.neptuneProperty.temperatureEnabled.value()); });
    _planetsProperty.neptuneProperty.atmosphereEnabled.onChange([this]() {
        onNeptuneAtmosphereChanged(_planetsProperty.neptuneProperty.atmosphereEnabled.value()); });
    _planetsProperty.neptuneProperty.moonsEnabled.onChange([this]() {
        onNeptuneMoonsChanged(_planetsProperty.neptuneProperty.moonsEnabled.value()); });

    //Add the properties
    addProperty(_everythingEnabled);
    addPropertySubOwner(_planetsProperty);
    addPropertySubOwner(_solarProperty);
    addPropertySubOwner(_compareProperty);
}


PlanetsSonification::~PlanetsSonification() {
    //Turn off the sonification in SuperCollider
    std::string label = "";

    for (int i = 0; i < NumPlanets; ++i) {
        for (int s = 0; s < NumPlanetarySettings; ++s) {
            _planets[i].settings[s] = false;
            _compareSettings[s] = false;
        }

        sendPlanetarySettings(i);
    }

    for (int s = 0; s < NumPlanets; ++s) {
        _solarSettings[s] = false;
    }

    sendSolarSettings();

    _compareProperty.firstPlanet.setValue(0);
    _compareProperty.secondPlanet.setValue(0);

    sendCompareSettings();
}

//Turn on/off everything
void PlanetsSonification::onEverythingChanged(bool value) {

    if (_GUIState == PlanetsSonification::GUIMode::Solar) {
        //Set all the solar settings
        setAllSolarProperties(value);

        _compareProperty.firstPlanet.setValue(0);
        _compareProperty.secondPlanet.setValue(0);
        _compareProperty.allEnabled = false;
    }

    else if (_GUIState == PlanetsSonification::GUIMode::Compare) {
        //Set all the compare settings
        _compareProperty.allEnabled = value;
    }

    else {
        //Set all the planetary settings
        setAllPlanetaryProperties(value);
    }
}

//Solar View
void PlanetsSonification::sendSolarSettings() {
    std::string label = "/Sun";
    std::vector<OscEngine::OscDataEntry> data(1);

    osc::Blob settingsBlob = osc::Blob(_solarSettings, NumPlanets);
    data[0].type = OscEngine::OscDataType::Blob;
    data[0].blobValue = settingsBlob;

    _sonificationModule->engine()->send(label, data);
}

void PlanetsSonification::onSolarAllEnabledChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _solarProperty.allEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Compare) {
        _compareProperty.firstPlanet.setValue(0);
        _compareProperty.secondPlanet.setValue(0);

        _GUIState = PlanetsSonification::GUIMode::Solar;
        if (_everythingEnabled.value()) {
            setAllSolarProperties(true);
            _compareProperty.allEnabled = false;
        }
    }

    _solarSettings[0] = value;
    _solarSettings[1] = value;
    _solarSettings[2] = value;
    _solarSettings[3] = value;
    _solarSettings[4] = value;
    _solarSettings[5] = value;
    _solarSettings[6] = value;
    _solarSettings[7] = value;

    _solarProperty.mercuryEnabled = value;
    _solarProperty.venusEnabled = value;
    _solarProperty.earthEnabled = value;
    _solarProperty.marsEnabled = value;
    _solarProperty.jupiterEnabled = value;
    _solarProperty.saturnEnabled = value;
    _solarProperty.uranusEnabled = value;
    _solarProperty.neptuneEnabled = value;
}

void PlanetsSonification::onSolarMercuryEnabledChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _solarProperty.mercuryEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Compare) {
        _compareProperty.firstPlanet.setValue(0);
        _compareProperty.secondPlanet.setValue(0);

        _GUIState = PlanetsSonification::GUIMode::Solar;
        if (_everythingEnabled.value()) {
            setAllSolarProperties(true);
            _compareProperty.allEnabled = false;
        }
    }

    _solarSettings[0] = value;
    sendSolarSettings();
}

void PlanetsSonification::onSolarVenusEnabledChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _solarProperty.venusEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Compare) {
        _compareProperty.firstPlanet.setValue(0);
        _compareProperty.secondPlanet.setValue(0);

        _GUIState = PlanetsSonification::GUIMode::Solar;
        if (_everythingEnabled.value()) {
            setAllSolarProperties(true);
            _compareProperty.allEnabled = false;
        }
    }

    _solarSettings[1] = value;
    sendSolarSettings();
}

void PlanetsSonification::onSolarEarthEnabledChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _solarProperty.earthEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Compare) {
        _compareProperty.firstPlanet.setValue(0);
        _compareProperty.secondPlanet.setValue(0);

        _GUIState = PlanetsSonification::GUIMode::Solar;
        if (_everythingEnabled.value()) {
            setAllSolarProperties(true);
            _compareProperty.allEnabled = false;
        }
    }

    _solarSettings[2] = value;
    sendSolarSettings();
}

void PlanetsSonification::onSolarMarsEnabledChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _solarProperty.marsEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Compare) {
        _compareProperty.firstPlanet.setValue(0);
        _compareProperty.secondPlanet.setValue(0);

        _GUIState = PlanetsSonification::GUIMode::Solar;
        if (_everythingEnabled.value()) {
            setAllSolarProperties(true);
            _compareProperty.allEnabled = false;
        }
    }

    _solarSettings[3] = value;
    sendSolarSettings();
}

void PlanetsSonification::onSolarJupiterEnabledChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _solarProperty.jupiterEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Compare) {
        _compareProperty.firstPlanet.setValue(0);
        _compareProperty.secondPlanet.setValue(0);

        _GUIState = PlanetsSonification::GUIMode::Solar;
        if (_everythingEnabled.value()) {
            setAllSolarProperties(true);
            _compareProperty.allEnabled = false;
        }
    }

    _solarSettings[4] = value;
    sendSolarSettings();
}

void PlanetsSonification::onSolarSaturnEnabledChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _solarProperty.saturnEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Compare) {
        _compareProperty.firstPlanet.setValue(0);
        _compareProperty.secondPlanet.setValue(0);

        _GUIState = PlanetsSonification::GUIMode::Solar;
        if (_everythingEnabled.value()) {
            setAllSolarProperties(true);
            _compareProperty.allEnabled = false;
        }
    }

    _solarSettings[5] = value;
    sendSolarSettings();
}

void PlanetsSonification::onSolarUranusEnabledChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _solarProperty.uranusEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Compare) {
        _compareProperty.firstPlanet.setValue(0);
        _compareProperty.secondPlanet.setValue(0);

        _GUIState = PlanetsSonification::GUIMode::Solar;
        if (_everythingEnabled.value()) {
            setAllSolarProperties(true);
            _compareProperty.allEnabled = false;
        }
    }

    _solarSettings[6] = value;
    sendSolarSettings();
}

void PlanetsSonification::onSolarNeptuneEnabledChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _solarProperty.neptuneEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Compare) {
        _compareProperty.firstPlanet.setValue(0);
        _compareProperty.secondPlanet.setValue(0);

        _GUIState = PlanetsSonification::GUIMode::Solar;
        if (_everythingEnabled.value()) {
            setAllSolarProperties(true);
            _compareProperty.allEnabled = false;
        }
    }

    _solarSettings[7] = value;
    sendSolarSettings();
}


//Compare
void PlanetsSonification::sendCompareSettings() {
    std::string label = "/Compare";
    std::vector<OscEngine::OscDataEntry> data(3);

    data[0].type = OscEngine::OscDataType::Int;
    data[0].intValue = _compareProperty.firstPlanet.value();

    data[1].type = OscEngine::OscDataType::Int;
    data[1].intValue = _compareProperty.secondPlanet.value();

    osc::Blob settingsBlob = osc::Blob(_compareSettings, NumPlanetarySettings);
    data[2].type = OscEngine::OscDataType::Blob;
    data[2].blobValue = settingsBlob;

    _sonificationModule->engine()->send(label, data);
}

void PlanetsSonification::onFirstCompareChanged(properties::OptionProperty::Option value)
{
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value.value != 0) {
        _compareProperty.firstPlanet.setValue(0);
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Solar) {
        setAllSolarProperties(false);

        _GUIState = PlanetsSonification::GUIMode::Compare;
        if (_everythingEnabled.value()) {
            _compareProperty.allEnabled = true;
        }
    }

    if (value.value != 0 && value.value == _compareProperty.secondPlanet.option().value)
    {
        _compareProperty.firstPlanet.setValue(0);
        return;
    }

    if (_oldCompareFirst != "") {
        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('Scene." +
            _oldCompareFirst + ".Scale.Scale', 1);",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    if (value.value != 0) {
        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('Scene." +
            value.description + ".Scale.Scale', 2000);",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        _oldCompareFirst = value.description;
    }
    else
        _oldCompareFirst = "";

    sendCompareSettings();
}

void PlanetsSonification::onSecondCompareChanged(properties::OptionProperty::Option value)
{
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value.value != 0) {
        _compareProperty.secondPlanet.setValue(0);
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Solar) {
        setAllSolarProperties(false);

        _GUIState = PlanetsSonification::GUIMode::Compare;
        if (_everythingEnabled.value()) {
            _compareProperty.allEnabled = true;
        }
    }

    if (value.value != 0 && value.value == _compareProperty.firstPlanet.option().value) {
        _compareProperty.secondPlanet.setValue(0);
        return;
    }

    if (_oldCompareSecond != "") {
        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('Scene." +
            _oldCompareSecond + ".Scale.Scale', 1);",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    if (value.value != 0) {
        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('Scene." +
            value.description + ".Scale.Scale', 2000);",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        _oldCompareSecond = value.description;
    }
    else
        _oldCompareSecond = "";

    sendCompareSettings();
}

void PlanetsSonification::onCompareAllChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _compareProperty.allEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Solar) {
        setAllSolarProperties(false);

        _GUIState = PlanetsSonification::GUIMode::Compare;
    }

    //Set the array of settings
    _compareSettings[0] = value;
    _compareSettings[1] = value;
    _compareSettings[2] = value;
    _compareSettings[3] = value;
    _compareSettings[4] = value;
    _compareSettings[5] = value;

    _compareProperty.sizeDayEnabled = value;
    _compareProperty.gravityEnabled = value;
    _compareProperty.temperatureEnabled = value;
    _compareProperty.atmosphereEnabled = value;
    _compareProperty.moonsEnabled = value;
    _compareProperty.ringsEnabled = value;
}

void PlanetsSonification::onCompareSizeDayChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _compareProperty.sizeDayEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Solar) {
        setAllSolarProperties(false);

        _GUIState = PlanetsSonification::GUIMode::Compare;
    }

    //Set the array of settings
    _compareSettings[0] = value;

    sendCompareSettings();
}

void PlanetsSonification::onCompareGravityChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _compareProperty.gravityEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Solar) {
        setAllSolarProperties(false);

        _GUIState = PlanetsSonification::GUIMode::Compare;
    }

    //Set the array of settings
    _compareSettings[1] = value;

    sendCompareSettings();
}

void PlanetsSonification::onCompareTemperatureChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _compareProperty.temperatureEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Solar) {
        setAllSolarProperties(false);

        _GUIState = PlanetsSonification::GUIMode::Compare;
    }

    //Set the array of settings
    _compareSettings[2] = value;

    sendCompareSettings();
}

void PlanetsSonification::onCompareAtmosphereChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _compareProperty.atmosphereEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Solar) {
        setAllSolarProperties(false);

        _GUIState = PlanetsSonification::GUIMode::Compare;
    }

    //Set the array of settings
    _compareSettings[3] = value;

    sendCompareSettings();
}

void PlanetsSonification::onCompareMoonsChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _compareProperty.moonsEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Solar) {
        setAllSolarProperties(false);

        _GUIState = PlanetsSonification::GUIMode::Compare;
    }

    //Set the array of settings
    _compareSettings[4] = value;

    sendCompareSettings();
}

void PlanetsSonification::onCompareRingsChanged(bool value) {
    if (_GUIState == PlanetsSonification::GUIMode::Planetary && value) {
        _compareProperty.ringsEnabled = false;
        return;
    }

    if (_GUIState == PlanetsSonification::GUIMode::Solar) {
        setAllSolarProperties(false);

        _GUIState = PlanetsSonification::GUIMode::Compare;
    }

    //Set the array of settings
    _compareSettings[5] = value;

    sendCompareSettings();
}


//Planetary View
void PlanetsSonification::sendPlanetarySettings(const int planetIndex) {
    std::string label = "/" + _planets[planetIndex].identifier;
    std::vector<OscEngine::OscDataEntry> data;

    // Distance
    OscEngine::OscDataEntry distanceData;
    distanceData.type = OscEngine::OscDataType::Double;
    distanceData.doubleValue = _planets[planetIndex].distance;
    data.push_back(distanceData);

    // Angle
    OscEngine::OscDataEntry angleData;
    angleData.type = OscEngine::OscDataType::Double;
    angleData.doubleValue = _planets[planetIndex].angle;
    data.push_back(angleData);

    // Settings
    OscEngine::OscDataEntry SettingsData;
    osc::Blob settingsBlob =
        osc::Blob(_planets[planetIndex].settings, NumPlanetarySettings);
    SettingsData.type = OscEngine::OscDataType::Blob;
    SettingsData.blobValue = settingsBlob;
    data.push_back(SettingsData);

    // Moons
    for (size_t m = 0; m < _planets[planetIndex].moons.size(); ++m) {
        OscEngine::OscDataEntry moonData;
        moonData.type = OscEngine::OscDataType::Double;
        moonData.doubleValue = _planets[planetIndex].moons[m].second;
        data.push_back(moonData);
    }

    data.shrink_to_fit();
    _sonificationModule->engine()->send(label, data);
}

void PlanetsSonification::onAllEnabledChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.allEnabled = false;
        return;
    }

    //Set all the planetary settings
    setAllPlanetaryProperties(value);
}

//Mercury
void PlanetsSonification::onMercuryEnabledChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.mercuryProperty.enabled = false;
        return;
    }

    _planets[0].settings[0] = value;
    _planets[0].settings[1] = value;
    _planets[0].settings[2] = value;

    _planetsProperty.mercuryProperty.sizeDayEnabled = value;
    _planetsProperty.mercuryProperty.gravityEnabled = value;
    _planetsProperty.mercuryProperty.temperatureEnabled = value;
    _planets[0].shouldUpdate = true;
}

void PlanetsSonification::onMercurySizeDayChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.mercuryProperty.sizeDayEnabled = false;
        return;
    }

    _planets[0].settings[0] = value;
    _planets[0].shouldUpdate = true;
}

void PlanetsSonification::onMercuryGravityChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.mercuryProperty.gravityEnabled = false;
        return;
    }

    _planets[0].settings[1] = value;
    _planets[0].shouldUpdate = true;
}

void PlanetsSonification::onMercuryTemperatureChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.mercuryProperty.temperatureEnabled = false;
        return;
    }

    _planets[0].settings[2] = value;
    _planets[0].shouldUpdate = true;
}

//Venus
void PlanetsSonification::onVenusEnabledChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.venusProperty.enabled = false;
        return;
    }

    _planets[1].settings[0] = value;
    _planets[1].settings[1] = value;
    _planets[1].settings[2] = value;
    _planets[1].settings[3] = value;

    _planetsProperty.venusProperty.sizeDayEnabled = value;
    _planetsProperty.venusProperty.gravityEnabled = value;
    _planetsProperty.venusProperty.temperatureEnabled = value;
    _planetsProperty.venusProperty.atmosphereEnabled = value;
    _planets[1].shouldUpdate = true;
}

void PlanetsSonification::onVenusSizeDayChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.venusProperty.sizeDayEnabled = false;
        return;
    }

    _planets[1].settings[0] = value;
    _planets[1].shouldUpdate = true;
}

void PlanetsSonification::onVenusGravityChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.venusProperty.gravityEnabled = false;
        return;
    }

    _planets[1].settings[1] = value;
    _planets[1].shouldUpdate = true;
}

void PlanetsSonification::onVenusTemperatureChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.venusProperty.temperatureEnabled = false;
        return;
    }

    _planets[1].settings[2] = value;
    _planets[1].shouldUpdate = true;
}

void PlanetsSonification::onVenusAtmosphereChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.venusProperty.atmosphereEnabled = false;
        return;
    }

    _planets[1].settings[3] = value;
    _planets[1].shouldUpdate = true;
}


//Earth
void PlanetsSonification::onEarthEnabledChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.earthProperty.enabled = false;
        return;
    }

    _planets[2].settings[0] = value;
    _planets[2].settings[1] = value;
    _planets[2].settings[2] = value;
    _planets[2].settings[3] = value;
    _planets[2].settings[4] = value;

    _planetsProperty.earthProperty.sizeDayEnabled = value;
    _planetsProperty.earthProperty.gravityEnabled = value;
    _planetsProperty.earthProperty.temperatureEnabled = value;
    _planetsProperty.earthProperty.atmosphereEnabled = value;
    _planetsProperty.earthProperty.moonsEnabled = value;
    _planets[2].shouldUpdate = true;
}

void PlanetsSonification::onEarthSizeDayChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.earthProperty.sizeDayEnabled = false;
        return;
    }

    _planets[2].settings[0] = value;
    _planets[2].shouldUpdate = true;
}

void PlanetsSonification::onEarthGravityChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.earthProperty.gravityEnabled = false;
        return;
    }

    _planets[2].settings[1] = value;
    _planets[2].shouldUpdate = true;
}

void PlanetsSonification::onEarthTemperatureChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.earthProperty.temperatureEnabled = false;
        return;
    }

    _planets[2].settings[2] = value;
    _planets[2].shouldUpdate = true;
}

void PlanetsSonification::onEarthAtmosphereChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.earthProperty.atmosphereEnabled = false;
        return;
    }

    _planets[2].settings[3] = value;
    _planets[2].shouldUpdate = true;
}

void PlanetsSonification::onEarthMoonsChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.earthProperty.moonsEnabled = false;
        return;
    }

    _planets[2].settings[4] = value;
    _planets[2].shouldUpdate = true;
}


//Mars
void PlanetsSonification::onMarsEnabledChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.marsProperty.enabled = false;
        return;
    }

    _planets[3].settings[0] = value;
    _planets[3].settings[1] = value;
    _planets[3].settings[2] = value;
    _planets[3].settings[3] = value;
    _planets[3].settings[4] = value;

    _planetsProperty.marsProperty.sizeDayEnabled = value;
    _planetsProperty.marsProperty.gravityEnabled = value;
    _planetsProperty.marsProperty.temperatureEnabled = value;
    _planetsProperty.marsProperty.atmosphereEnabled = value;
    _planetsProperty.marsProperty.moonsEnabled = value;
    _planets[3].shouldUpdate = true;
}

void PlanetsSonification::onMarsSizeDayChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.marsProperty.sizeDayEnabled = false;
        return;
    }

    _planets[3].settings[0] = value;
    _planets[3].shouldUpdate = true;
}

void PlanetsSonification::onMarsGravityChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.marsProperty.gravityEnabled = false;
        return;
    }

    _planets[3].settings[1] = value;
    _planets[3].shouldUpdate = true;
}

void PlanetsSonification::onMarsTemperatureChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.marsProperty.temperatureEnabled = false;
        return;
    }

    _planets[3].settings[2] = value;
    _planets[3].shouldUpdate = true;
}

void PlanetsSonification::onMarsAtmosphereChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.marsProperty.atmosphereEnabled = false;
        return;
    }

    _planets[3].settings[3] = value;
    _planets[3].shouldUpdate = true;
}

void PlanetsSonification::onMarsMoonsChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.marsProperty.moonsEnabled = false;
        return;
    }

    _planets[3].settings[4] = value;
    _planets[3].shouldUpdate = true;
}


//Jupiter
void PlanetsSonification::onJupiterEnabledChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.jupiterProperty.enabled = false;
        return;
    }

    _planets[4].settings[0] = value;
    _planets[4].settings[1] = value;
    _planets[4].settings[2] = value;
    _planets[4].settings[3] = value;
    _planets[4].settings[4] = value;

    _planetsProperty.jupiterProperty.sizeDayEnabled = value;
    _planetsProperty.jupiterProperty.gravityEnabled = value;
    _planetsProperty.jupiterProperty.temperatureEnabled = value;
    _planetsProperty.jupiterProperty.atmosphereEnabled = value;
    _planetsProperty.jupiterProperty.moonsEnabled = value;
    _planets[4].shouldUpdate = true;
}

void PlanetsSonification::onJupiterSizeDayChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.jupiterProperty.sizeDayEnabled = false;
        return;
    }

    _planets[4].settings[0] = value;
    _planets[4].shouldUpdate = true;
}

void PlanetsSonification::onJupiterGravityChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.jupiterProperty.gravityEnabled = false;
        return;
    }

    _planets[4].settings[1] = value;
    _planets[4].shouldUpdate = true;
}

void PlanetsSonification::onJupiterTemperatureChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.jupiterProperty.temperatureEnabled = false;
        return;
    }

    _planets[4].settings[2] = value;
    _planets[4].shouldUpdate = true;
}

void PlanetsSonification::onJupiterAtmosphereChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.jupiterProperty.atmosphereEnabled = false;
        return;
    }

    _planets[4].settings[3] = value;
    _planets[4].shouldUpdate = true;
}

void PlanetsSonification::onJupiterMoonsChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.jupiterProperty.moonsEnabled = false;
        return;
    }

    _planets[4].settings[4] = value;
    _planets[4].shouldUpdate = true;
}


//Saturn
void PlanetsSonification::onSaturnEnabledChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.saturnProperty.enabled = false;
        return;
    }

    _planets[5].settings[0] = value;
    _planets[5].settings[1] = value;
    _planets[5].settings[2] = value;
    _planets[5].settings[3] = value;
    _planets[5].settings[4] = value;
    _planets[5].settings[5] = value;

    _planetsProperty.saturnProperty.sizeDayEnabled = value;
    _planetsProperty.saturnProperty.gravityEnabled = value;
    _planetsProperty.saturnProperty.temperatureEnabled = value;
    _planetsProperty.saturnProperty.atmosphereEnabled = value;
    _planetsProperty.saturnProperty.moonsEnabled = value;
    _planetsProperty.saturnProperty.ringsEnabled = value;
    _planets[5].shouldUpdate = true;
}

void PlanetsSonification::onSaturnSizeDayChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.saturnProperty.sizeDayEnabled = false;
        return;
    }

    _planets[5].settings[0] = value;
    _planets[5].shouldUpdate = true;
}

void PlanetsSonification::onSaturnGravityChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.saturnProperty.gravityEnabled = false;
        return;
    }

    _planets[5].settings[1] = value;
    _planets[5].shouldUpdate = true;
}

void PlanetsSonification::onSaturnTemperatureChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.saturnProperty.temperatureEnabled = false;
        return;
    }

    _planets[5].settings[2] = value;
    _planets[5].shouldUpdate = true;
}

void PlanetsSonification::onSaturnAtmosphereChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.saturnProperty.atmosphereEnabled = false;
        return;
    }

    _planets[5].settings[3] = value;
    _planets[5].shouldUpdate = true;
}

void PlanetsSonification::onSaturnMoonsChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.saturnProperty.moonsEnabled = false;
        return;
    }

    _planets[5].settings[4] = value;
    _planets[5].shouldUpdate = true;
}

void PlanetsSonification::onSaturnRingsChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.saturnProperty.ringsEnabled = false;
        return;
    }

    _planets[5].settings[5] = value;
    _planets[5].shouldUpdate = true;
}


//Uranus
void PlanetsSonification::onUranusEnabledChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.uranusProperty.enabled = false;
        return;
    }

    _planets[6].settings[0] = value;
    _planets[6].settings[1] = value;
    _planets[6].settings[2] = value;
    _planets[6].settings[3] = value;
    _planets[6].settings[4] = value;

    _planetsProperty.uranusProperty.sizeDayEnabled = value;
    _planetsProperty.uranusProperty.gravityEnabled = value;
    _planetsProperty.uranusProperty.temperatureEnabled = value;
    _planetsProperty.uranusProperty.atmosphereEnabled = value;
    _planetsProperty.uranusProperty.moonsEnabled = value;
    _planets[6].shouldUpdate = true;
}

void PlanetsSonification::onUranusSizeDayChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.uranusProperty.sizeDayEnabled = false;
        return;
    }

    _planets[6].settings[0] = value;
    _planets[6].shouldUpdate = true;
}

void PlanetsSonification::onUranusGravityChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.uranusProperty.gravityEnabled = false;
        return;
    }

    _planets[6].settings[1] = value;
    _planets[6].shouldUpdate = true;
}

void PlanetsSonification::onUranusTemperatureChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.uranusProperty.temperatureEnabled = false;
        return;
    }

    _planets[6].settings[2] = value;
    _planets[6].shouldUpdate = true;
}

void PlanetsSonification::onUranusAtmosphereChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.uranusProperty.atmosphereEnabled = false;
        return;
    }

    _planets[6].settings[3] = value;
    _planets[6].shouldUpdate = true;
}

void PlanetsSonification::onUranusMoonsChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.uranusProperty.moonsEnabled = false;
        return;
    }

    _planets[6].settings[4] = value;
    _planets[6].shouldUpdate = true;
}


//Neptune
void PlanetsSonification::onNeptuneEnabledChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.neptuneProperty.enabled = false;
        return;
    }

    _planets[7].settings[0] = value;
    _planets[7].settings[1] = value;
    _planets[7].settings[2] = value;
    _planets[7].settings[3] = value;
    _planets[7].settings[4] = value;

    _planetsProperty.neptuneProperty.sizeDayEnabled = value;
    _planetsProperty.neptuneProperty.gravityEnabled = value;
    _planetsProperty.neptuneProperty.temperatureEnabled = value;
    _planetsProperty.neptuneProperty.atmosphereEnabled = value;
    _planetsProperty.neptuneProperty.moonsEnabled = value;
    _planets[7].shouldUpdate = true;
}

void PlanetsSonification::onNeptuneSizeDayChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.neptuneProperty.sizeDayEnabled = false;
        return;
    }

    _planets[7].settings[0] = value;
    _planets[7].shouldUpdate = true;
}

void PlanetsSonification::onNeptuneGravityChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.neptuneProperty.gravityEnabled = false;
        return;
    }

    _planets[7].settings[1] = value;
    _planets[7].shouldUpdate = true;
}

void PlanetsSonification::onNeptuneTemperatureChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.neptuneProperty.temperatureEnabled = false;
        return;
    }

    _planets[7].settings[2] = value;
    _planets[7].shouldUpdate = true;
}

void PlanetsSonification::onNeptuneAtmosphereChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.neptuneProperty.atmosphereEnabled = false;
        return;
    }

    _planets[7].settings[3] = value;
    _planets[7].shouldUpdate = true;
}

void PlanetsSonification::onNeptuneMoonsChanged(bool value) {
    if (_GUIState != PlanetsSonification::GUIMode::Planetary && value) {
        _planetsProperty.neptuneProperty.moonsEnabled = false;
        return;
    }

    _planets[7].settings[4] = value;
    _planets[7].shouldUpdate = true;
}


PlanetsSonification::PlanetProperty::PlanetProperty(
    properties::PropertyOwner::PropertyOwnerInfo planetInfo)
    : properties::PropertyOwner(planetInfo),
    enabled(EnableInfo, false),
    sizeDayEnabled(SizeDayInfo, false),
    gravityEnabled(GravityInfo, false),
    temperatureEnabled(TemperatureInfo, false),
    atmosphereEnabled(AtmosphereInfo, false),
    moonsEnabled(MoonsInfo, false),
    ringsEnabled(RingsInfo, false)
{
    //Common
    addProperty(enabled);
    addProperty(sizeDayEnabled);
    addProperty(gravityEnabled);
    addProperty(temperatureEnabled);

    //Unique
    if (planetInfo.identifier.compare("Mercury") != 0)
        addProperty(atmosphereEnabled);
    if (planetInfo.identifier.compare("Mercury") != 0
        && planetInfo.identifier.compare("Venus") != 0)
        addProperty(moonsEnabled);
    if (planetInfo.identifier.compare("Saturn") == 0)
        addProperty(ringsEnabled);
}


PlanetsSonification::SolarProperty::SolarProperty()
    : properties::PropertyOwner(SolarInfo),
    allEnabled(EnableAllInfo, false),
    mercuryEnabled(EnableMercuryInfo, false),
    venusEnabled(EnableVenusInfo, false),
    earthEnabled(EnableEarthInfo, false),
    marsEnabled(EnableMarsInfo, false),
    jupiterEnabled(EnableJupiterInfo, false),
    saturnEnabled(EnableSaturnInfo, false),
    uranusEnabled(EnableUranusInfo, false),
    neptuneEnabled(EnableNeptuneInfo, false)
{
    addProperty(allEnabled);
    addProperty(mercuryEnabled);
    addProperty(venusEnabled);
    addProperty(earthEnabled);
    addProperty(marsEnabled);
    addProperty(jupiterEnabled);
    addProperty(saturnEnabled);
    addProperty(uranusEnabled);
    addProperty(neptuneEnabled);
}


PlanetsSonification::CompareProperty::CompareProperty()
    : properties::PropertyOwner(CompareInfo),
    firstPlanet(CompareOptionsInfo, properties::OptionProperty::DisplayType::Dropdown),
    secondPlanet(CompareOptionsInfoT, properties::OptionProperty::DisplayType::Dropdown),
    allEnabled(CompareAllInfo, false),
    sizeDayEnabled(CompareSizeDayInfo, false),
    gravityEnabled(CompareGravityInfo, false),
    temperatureEnabled(CompareTemperatureInfo, false),
    atmosphereEnabled(CompareAtmosphereInfo, false),
    moonsEnabled(CompareMoonsInfo, false),
    ringsEnabled(CompareRingsInfo, false)
{
    firstPlanet.addOptions({
        { 0, "Choose Planet" },
        { 1, "Mercury" },
        { 2, "Venus" },
        { 3, "Earth" },
        { 4, "Mars" },
        { 5, "Jupiter" },
        { 6, "Saturn" },
        { 7, "Uranus" },
        { 8, "Neptune" }
        });

    secondPlanet.addOptions({
        { 0, "Choose Planet" },
        { 1, "Mercury" },
        { 2, "Venus" },
        { 3, "Earth" },
        { 4, "Mars" },
        { 5, "Jupiter" },
        { 6, "Saturn" },
        { 7, "Uranus" },
        { 8, "Neptune" }
        });

    addProperty(firstPlanet);
    addProperty(secondPlanet);

    addProperty(allEnabled);
    addProperty(sizeDayEnabled);
    addProperty(gravityEnabled);
    addProperty(temperatureEnabled);
    addProperty(atmosphereEnabled);
    addProperty(moonsEnabled);
    addProperty(ringsEnabled);
}


PlanetsSonification::PlanetHeadProperty::PlanetHeadProperty(
    properties::PropertyOwner::PropertyOwnerInfo planetHeadInfo,
    properties::PropertyOwner::PropertyOwnerInfo mercuryInfo,
    properties::PropertyOwner::PropertyOwnerInfo venusInfo,
    properties::PropertyOwner::PropertyOwnerInfo earthInfo,
    properties::PropertyOwner::PropertyOwnerInfo marsInfo,
    properties::PropertyOwner::PropertyOwnerInfo jupiterInfo,
    properties::PropertyOwner::PropertyOwnerInfo saturnInfo,
    properties::PropertyOwner::PropertyOwnerInfo uranusInfo,
    properties::PropertyOwner::PropertyOwnerInfo neptuneInfo)
    : properties::PropertyOwner(planetHeadInfo)
    , allEnabled(EnableAllPlanetsInfo, false)
    , mercuryProperty(PlanetsSonification::PlanetProperty(mercuryInfo))
    , venusProperty(PlanetsSonification::PlanetProperty(venusInfo))
    , earthProperty(PlanetsSonification::PlanetProperty(earthInfo))
    , marsProperty(PlanetsSonification::PlanetProperty(marsInfo))
    , jupiterProperty(PlanetsSonification::PlanetProperty(jupiterInfo))
    , saturnProperty(PlanetsSonification::PlanetProperty(saturnInfo))
    , uranusProperty(PlanetsSonification::PlanetProperty(uranusInfo))
    , neptuneProperty(PlanetsSonification::PlanetProperty(neptuneInfo))
{
    addProperty(allEnabled);
    addPropertySubOwner(mercuryProperty);
    addPropertySubOwner(venusProperty);
    addPropertySubOwner(earthProperty);
    addPropertySubOwner(marsProperty);
    addPropertySubOwner(jupiterProperty);
    addPropertySubOwner(saturnProperty);
    addPropertySubOwner(uranusProperty);
    addPropertySubOwner(neptuneProperty);
}

void PlanetsSonification::setAllSolarProperties(bool value) {
    std::vector<properties::Property*> solarProperties = _solarProperty.properties();

    for (std::vector<properties::Property*>::iterator i = solarProperties.begin();
        i < solarProperties.end(); ++i) {
        (*i)->set(value);
    }
}

void PlanetsSonification::setAllPlanetaryProperties(bool value) {
    std::vector<properties::PropertyOwner*> planetOwners =
        _planetsProperty.propertySubOwners();

    for (std::vector<properties::PropertyOwner*>::iterator owner = planetOwners.begin();
        owner < planetOwners.end(); ++owner) {
        std::vector<properties::Property*> planetProperties = (*owner)->properties();

        for (std::vector<properties::Property*>::iterator i = planetProperties.begin();
            i < planetProperties.end(); ++i) {
            (*i)->set(value);
        }
    }
}

void PlanetsSonification::checkTimeSpeed(double& ts) {
    double timeSpeed = global::timeManager->deltaTime() / NumSecPerDay;
    if (abs(ts - timeSpeed) > _timePrecision) {
        ts = timeSpeed;

        std::string label = "/Time";
        std::vector<OscEngine::OscDataEntry> data(1);

        data[0].type = OscEngine::OscDataType::Double;
        data[0].doubleValue = ts;

        _sonificationModule->engine()->send(label, data);
    }
}

//Extract the data from the given identifier
//NOTE: The identifier must start with capital letter,
//otherwise no match will be found
void PlanetsSonification::extractData(const std::string& identifier, int i)
{
    if (!_scene || !_camera) {
        return;
    }

    SceneGraphNode* node = _scene->sceneGraphNode(identifier);
    if (!node) {
        return;
    }

    glm::dvec3 nodePosition = node->worldPosition();
    if (nodePosition.length < std::numeric_limits<glm::length_t>::epsilon) {
        return;
    }

    glm::dvec3 cameraDirection = _camera->viewDirectionWorldSpace();
    glm::dvec3 cameraUpVector = _camera->lookUpVectorWorldSpace();

    //Check the time speed in OpenSpace
    checkTimeSpeed(_timeSpeed);

    //Calculate distance to the planet from the camera, convert to km
    glm::dvec3 cameraToNode = nodePosition - _camera->positionVec3();
    double distance = glm::length(cameraToNode) / 1000.0;
    double angle;
    bool updateMoons = false;

    //Calculate angle differently if planetary view or solar view
    if (_GUIState == PlanetsSonification::GUIMode::Planetary) {
        //Calculate angle from camera to the planet in the camera plane
        //Project v down to the camera plane, Pplane(v)
        //Pn(v) is v projected on the normal n of the plane
        //Pplane(v) = v - Pn(v)
        glm::dvec3 cameraToProjectedNode = cameraToNode -
            glm::proj(cameraToNode, cameraUpVector);

        angle = glm::orientedAngle(glm::normalize(cameraDirection),
            glm::normalize(cameraToProjectedNode),
            glm::normalize(cameraUpVector));

        //If this planet is in focus then calculate the angle from
        //the planet to its moons and send them too
        for (int m = 0; m < _planets[i].moons.size(); ++m) {
            // TODO: This throws an exception sometimes for Io
            SceneGraphNode* moon =
                _scene->sceneGraphNode(_planets[i].moons[m].first);
            if (moon) {
                glm::dvec3 planetToMoon = moon->worldPosition() - nodePosition;
                glm::dvec3 planetToProjectedMoon = planetToMoon -
                    glm::proj(planetToMoon, cameraUpVector);

                //Angle from planet to moon with respect to camera
                //NOTE: This might not work if the camera is looking straight
                //down on the planet
                double moonAngle =
                    glm::orientedAngle(glm::normalize(cameraDirection),
                        glm::normalize(planetToProjectedMoon),
                        glm::normalize(cameraUpVector));

                if (abs(_planets[i].moons[m].second - moonAngle) > _anglePrecision)
                {
                    updateMoons = true;
                    _planets[i].moons[m].second = moonAngle;
                }
            }
        }
    }
    else {
        //Solar view, calculate angle from sun (origin) to node,
        //with camera forward as forward and camera up axis as upwards
        //angle from sun with respect to the camera
        angle = glm::orientedAngle(glm::normalize(cameraDirection),
            glm::normalize(nodePosition -
                glm::proj(nodePosition, cameraUpVector)),
            glm::normalize(cameraUpVector));
    }

    // Check if this data is new, otherwise dont send the data
    if (abs(_planets[i].distance - distance) > _distancePrecision ||
        abs(_planets[i].angle - angle) > _anglePrecision ||
        updateMoons || _planets[i].shouldUpdate)
    {
        // Update the saved data for the planet
        _planets[i].setDistance(distance);
        _planets[i].setAngle(angle);

        // Send the data to SuperCollider
        sendPlanetarySettings(i);
        _planets[i].shouldUpdate = false;
    }
}

void PlanetsSonification::update() {
    const SceneGraphNode* focusNode = nullptr;
    const SceneGraphNode* previousFocusNode = nullptr;

    // If no scene, try to find it
    if (!_scene || _scene->root()->children().size() == 0) {
        _scene = global::renderEngine->scene();
    }

    // If no camera, try to find it
    if (!_camera) {
        _camera = _scene ? _scene->camera() : nullptr;
    }

    if (!_scene || !_camera ||
        _camera->positionVec3().length < std::numeric_limits<glm::length_t>::epsilon)
    {
        return;
    }

    // Scne and camera is initialized

    //Which node is in focus?
    focusNode = global::navigationHandler->orbitalNavigator().anchorNode();
    if (!focusNode) {
        return;
    }

    //Check if focus has changed
    if (!previousFocusNode ||
        previousFocusNode->identifier().compare(focusNode->identifier()) != 0)
    {
        //Update
        previousFocusNode = focusNode;

        //If focus is on the sun, switch sonification view
        if (focusNode->identifier().compare("Sun") == 0) {
            _GUIState = PlanetsSonification::GUIMode::Solar;

            //Clear the planetary settings
            setAllPlanetaryProperties(false);

            if (_everythingEnabled.value()) {
                setAllSolarProperties(true);
                _compareProperty.allEnabled = false;
            }
        }
        else {
            _GUIState = PlanetsSonification::GUIMode::Planetary;

            //Clear the solar settings
            setAllSolarProperties(false);

            _compareProperty.firstPlanet.setValue(0);
            _compareProperty.secondPlanet.setValue(0);

            if (_everythingEnabled.value()) {
                setAllPlanetaryProperties(true);
            }
        }
    }

    //Extract data from all the planets
    for (int i = 0; i < NumPlanets; ++i) {

        //Only send data if something new has happened
        //If the node is in focus, increase sensitivity
        if (focusNode->identifier().compare(_planets[i].identifier) == 0)
        {
            _anglePrecision = 0.05;
            _distancePrecision = 1000.0;
        }
        else {
            _anglePrecision = 0.1;
            _distancePrecision = 10000.0;
        }

        extractData(_planets[i].identifier, i);
    }
}


} // namespace openspace

