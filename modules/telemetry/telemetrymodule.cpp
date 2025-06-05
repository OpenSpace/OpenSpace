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

#include <modules/telemetry/telemetrymodule.h>

#include <modules/telemetry/include/general/anglemodetelemetry.h>
#include <modules/telemetry/include/general/cameratelemetry.h>
#include <modules/telemetry/include/general/focustelemetry.h>
#include <modules/telemetry/include/general/nodestelemetry.h>
#include <modules/telemetry/include/general/timetelemetry.h>
#include <modules/telemetry/include/specific/planetscomparesonification.h>
#include <modules/telemetry/include/specific/planetsoverviewsonification.h>
#include <modules/telemetry/include/specific/planetssonification.h>
#include <openspace/camera/camera.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/lualibrary.h>
#include <ghoul/logging/logmanager.h>

namespace {
    // The default Open Sound Control receiver is SuperCollider with these default values.
    // However, the user can define any receiver in the openspace.cfg file as the
    // ModuleConfiguration for the Telemetry module.
    constexpr std::string_view DefaultSuperColliderIp = "127.0.0.1";
    constexpr int DefaultSuperColliderPort = 57120;

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "Enable or disable all gathering of telemetry information.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo IpAddressInfo = {
        "IpAddress",
        "IP Address",
        "The network IP address that the telemetry Open Sound Control messages is sent "
        "to.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo PortInfo = {
        "Port",
        "Port",
        "The network port that the telemetry Open Sound Control messages are sent to.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo AngleCalculationModeInfo = {
        "AngleCalculationMode",
        "Angle Calculation Mode",
        "This setting changes the method used to calculate any angles in the "
        "telemetries. The Horizontal mode, generally works well for flat displays or "
        "forward facing immersive environments. The Circular mode, generally works well "
        "for centered fisheye displays or omnidirectional immersive environments. For "
        "more information, see the pages \"Angle Calculations\" and \"Surround Sound "
        "Configurations\"on the OpenSpace documentation page.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo IncludeElevationAngleInfo = {
        "IncludeElevationAngle",
        "Include Elevation Angle",
        "This setting determines if an additional elevation angle should be calculated "
        "for the telemetries that calculate angles. This angle determines where the "
        "object is placed within a vertical plane of reference in relation to the "
        "camera, i.e the height in relation to the horizontal plane of reference. The "
        "method used for this calculation also depends on the angle calculation mode. "
        "For more information, see the page \"Angle Calculations\" on the OpenSpace "
        "documentation page.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(TelemetryModule)]] Parameters {
        // [[codegen::verbatim(IpAddressInfo.description)]]
        std::optional<std::string> ipAddress;

        // [[codegen::verbatim(PortInfo.description)]]
        std::optional<int> port;

        enum class
        [[codegen::map(openspace::TelemetryModule::AngleCalculationMode)]]
        AngleCalculationMode {
            Horizontal,
            Circular
        };

        // [[codegen::verbatim(AngleCalculationModeInfo.description)]]
        std::optional<AngleCalculationMode> angleCalculationMode;

        // [[codegen::verbatim(IncludeElevationAngleInfo.description)]]
        std::optional<bool> includeElevationAngle;
    };
#include "telemetrymodule_codegen.cpp"
} // namespace

namespace openspace {

TelemetryModule::TelemetryModule()
    : OpenSpaceModule("Telemetry")
    , _enabled(EnabledInfo, false)
    , _ipAddress(IpAddressInfo, DefaultSuperColliderIp.data())
    , _port(PortInfo, DefaultSuperColliderPort, 1025, 65536)
    , _modeOptions(AngleCalculationModeInfo)
    , _includeElevationAngle(IncludeElevationAngleInfo, false)
{
    addProperty(_enabled);

    _ipAddress.setReadOnly(true);
    addProperty(_ipAddress);

    _port.setReadOnly(true);
    addProperty(_port);

    // Add options to the drop down menu
    _modeOptions.addOptions({
        { 0, "Horizontal" },
        { 1, "Circular" }
    });
    _modeOptions.onChange([this]() { guiOnChangeAngleCalculationMode(); });

    // Select Horizontal angle calculation mode as the default
    _modeOptions.setValue(static_cast<int>(AngleCalculationMode::Horizontal));
    addProperty(_modeOptions);

    addProperty(_includeElevationAngle);
}

TelemetryModule::~TelemetryModule() {
    // Clear the telemetries list
    for (TelemetryBase* telemetry : _telemetries) {
        delete telemetry;
    }
}

void TelemetryModule::guiOnChangeAngleCalculationMode() {
    _angleCalculationMode = static_cast<AngleCalculationMode>(_modeOptions.value());
}

void TelemetryModule::internalInitialize(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _ipAddress = p.ipAddress.value_or(_ipAddress);
    _port = p.port.value_or(_port);

    if (p.angleCalculationMode.has_value()) {
        Parameters::AngleCalculationMode mode =
            Parameters::AngleCalculationMode(*p.angleCalculationMode);
        _angleCalculationMode = codegen::map<AngleCalculationMode>(mode);
    }

    // Fill telemetry list
    TelemetryBase* telemetry = new AngleModeTelemetry(_ipAddress, _port);
    addTelemetry(telemetry);

    telemetry = new CameraTelemetry(_ipAddress, _port);
    addTelemetry(telemetry);

    telemetry = new FocusTelemetry(_ipAddress, _port);
    addTelemetry(telemetry);

    telemetry = new TimeTelemetry(_ipAddress, _port);
    addTelemetry(telemetry);

    telemetry = new NodesTelemetry(_ipAddress, _port);
    addTelemetry(telemetry);

    telemetry = new PlanetsCompareSonification(_ipAddress, _port);
    addTelemetry(telemetry);

    telemetry = new PlanetsOverviewSonification(_ipAddress, _port);
    addTelemetry(telemetry);

    telemetry = new PlanetsSonification(_ipAddress, _port);
    addTelemetry(telemetry);

    // Only the master runs the TelemetryModule update thread
    if (global::windowDelegate->isMaster()) {
        _isRunning = true;
        _updateThread = std::thread([this]() { update(std::ref(_isRunning)); });

        // Make sure the telemetry thread is synced with the main thread
        global::callback::postSyncPreDraw->emplace_back([this]() {
            // Tell the telemetry thread that a new frame is starting
            syncToMain.notify_one();
        });

        // When the program shuts down, make sure this module turns itself off.
        // If the module is turned on while the scene is being destroyed, then it will
        // crash
        global::callback::deinitialize->emplace_back([this]() {
            _enabled = false;
        });
    }
}

void TelemetryModule::addTelemetry(TelemetryBase* telemetry) {
    _telemetries.push_back(telemetry);
    addPropertySubOwner(telemetry);
}

void TelemetryModule::internalDeinitialize() {
    // Stop the loop and tell the thread it is ok to run the last itteration
    _isRunning = false;
    syncToMain.notify_one();

    _updateThread.join();
}

const std::vector<TelemetryBase*>& TelemetryModule::telemetries() const {
    return _telemetries;
}

const TelemetryBase* TelemetryModule::telemetry(const std::string_view& id) const {
    for (const TelemetryBase* t : _telemetries) {
        if (t->identifier() == id) {
            return t;
        }
    }
    return nullptr;
}

TelemetryBase* TelemetryModule::telemetry(const std::string_view& id) {
    for (TelemetryBase* t : _telemetries) {
        if (t->identifier() == id) {
            return t;
        }
    }
    return nullptr;
}

TelemetryModule::AngleCalculationMode TelemetryModule::angleCalculationMode() const {
    return _angleCalculationMode;
}

bool TelemetryModule::includeElevationAngle() const {
    return _includeElevationAngle;
}

void TelemetryModule::update(std::atomic<bool>& isRunning) {
    Scene* scene = nullptr;
    Camera* camera = nullptr;
    bool isInitialized = false;

    while (isRunning) {
        // Wait for the main thread
        std::unique_lock<std::mutex> lk(mutexLock);
        syncToMain.wait(lk);

        // No need to update if the module isn't currently enabled
        if (!_enabled) {
            std::this_thread::sleep_for(std::chrono::seconds(1));
            continue;
        }

        // Initialize the scena and camera information if that has not already been done
        if (!isInitialized) {
            // Find the scene
            if (!scene) {
                scene = global::renderEngine->scene();
            }

            // Find the camera in the scene
            if (!camera) {
                camera = scene ? scene->camera() : nullptr;
            }

            // Check status
            isInitialized = scene && !scene->isInitializing() &&
                !scene->root()->children().empty() && camera &&
                glm::length(camera->positionVec3()) >
                std::numeric_limits<double>::epsilon();
        }

        // Process the telemetries
        if (isInitialized) {
            for (TelemetryBase* telemetry : _telemetries) {
                if (telemetry) {
                    telemetry->update(camera);
                }
            }
        }
    }
}

std::vector<scripting::LuaLibrary> TelemetryModule::luaLibraries() const {
    return {
        NodesTelemetry::luaLibrary(),
        PlanetsSonification::luaLibrary()
    };
}

} // namespace openspace
