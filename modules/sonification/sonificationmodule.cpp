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

#include <modules/sonification/sonificationmodule.h>

#include <modules/sonification/include/camerasonification.h>
#include <modules/sonification/include/focussonification.h>
#include <modules/sonification/include/nodessonification.h>
#include <modules/sonification/include/planetscomparesonification.h>
#include <modules/sonification/include/planetsoverviewsonification.h>
#include <modules/sonification/include/planetssonification.h>
#include <modules/sonification/include/timesonification.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "SonificationModule";

    //Output to SuperCollider
    constexpr std::string_view DefaultSuperColliderIp = "127.0.0.1";
    constexpr int DefaultSuperColliderPort = 57120;

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "Enable or disable all sonifications"
    };

    constexpr openspace::properties::Property::PropertyInfo IpAddressInfo = {
        "IpAddress",
        "Ip address",
        "The network ip address that the sonification osc messages will be sent to"
    };

    constexpr openspace::properties::Property::PropertyInfo PortInfo = {
        "Port",
        "Port",
        "The network port that the sonification osc messages will be sent to"
    };

    constexpr openspace::properties::Property::PropertyInfo SurroundModeInfo = {
        "SurroundMode",
        "Surround Mode",
        "The type of surround system that the sonification is played on"
    };

    struct [[codegen::Dictionary(SonificationModule)]] Parameters {
        // [[codegen::verbatim(IpAddressInfo.description)]]
        std::optional<std::string> ipAddress;

        // [[codegen::verbatim(PortInfo.description)]]
        std::optional<int> port;

        enum class [[codegen::map(openspace::SonificationModule::SurroundMode)]] SurroundMode {
            Horizontal,
            HorizontalWithElevation,
            Circular,
            CircularWithElevation,
            None
        };

        // [[codegen::verbatim(SurroundModeInfo.description)]]
        std::optional<SurroundMode> surroundMode;
    };
#include "sonificationmodule_codegen.cpp"
} // namespace

namespace openspace {

bool SonificationModule::isMainDone = false;

SonificationModule::SonificationModule()
    : OpenSpaceModule("Sonification")
    , _enabled(EnabledInfo, false)
    , _ipAddress(IpAddressInfo, DefaultSuperColliderIp.data())
    , _port(PortInfo, DefaultSuperColliderPort, 1025, 65536)
    , _mode(
        SurroundModeInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
{
    addProperty(_enabled);

    _ipAddress.setReadOnly(true);
    addProperty(_ipAddress);

    _port.setReadOnly(true);
    addProperty(_port);

    // Add options to the drop down menues
    _mode.addOptions({
        { 0, "Horizontal" },
        { 1, "Horizontal With Elevation" },
        { 2, "Circular" },
        { 3, "Circular With Elevation" },
        { 4, "None" }
    });
    _mode.onChange([this]() { guiOnChangeSurroundMode(); });
    addProperty(_mode);
}

SonificationModule::~SonificationModule() {
    // Clear the sonifications list
    for (SonificationBase* sonification : _sonifications) {
        delete sonification;
    }
}

void SonificationModule::guiOnChangeSurroundMode() {
    _surroundMode = static_cast<SurroundMode>(_mode.value());
}

void SonificationModule::internalInitialize(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _ipAddress = p.ipAddress.value_or(_ipAddress);
    _port = p.port.value_or(_port);

    if (p.surroundMode.has_value()) {
        Parameters::SurroundMode mode = Parameters::SurroundMode(*p.surroundMode);
        _surroundMode = codegen::map<SurroundMode>(mode);
    }

    // Fill sonification list
    SonificationBase* sonification = new FocusSonification(_ipAddress, _port);
    addSonification(sonification);

    sonification = new CameraSonification(_ipAddress, _port);
    addSonification(sonification);

    sonification = new TimeSonification(_ipAddress, _port);
    addSonification(sonification);

    sonification = new NodesSonification(_ipAddress, _port);
    addSonification(sonification);

    sonification = new PlanetsSonification(_ipAddress, _port);
    addSonification(sonification);

    sonification = new PlanetsOverviewSonification(_ipAddress, _port);
    addSonification(sonification);

    sonification = new PlanetsCompareSonification(_ipAddress, _port);
    addSonification(sonification);

    // Only the master runs the SonificationModule
    if (global::windowDelegate->isMaster()) {
        _isRunning = true;
        _updateThread = std::thread([this]() { update(std::ref(_isRunning)); });

        // Make sure the sonification thread is synced with the main thread
        global::callback::postSyncPreDraw->emplace_back([this]() {
            // Send data to the sonification thread
            {
                LINFO("Main signals sonificaiton thread");
                std::lock_guard lg(mutexLock);
                isMainDone = true;
            }
            syncToMain.notify_one();
        });
    }
}

void SonificationModule::addSonification(SonificationBase* sonification) {
    _sonifications.push_back(sonification);
    addPropertySubOwner(sonification);
}

void SonificationModule::internalDeinitialize() {
    // Join the thread
    _isRunning = false;

    // Wait before joining the thread
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
    _updateThread.join();
}

const std::vector<SonificationBase*>& SonificationModule::sonifications() const {
    return _sonifications;
}

const SonificationBase* SonificationModule::sonification(std::string id) const {
    for (const SonificationBase* s : _sonifications) {
        if (s->identifier() == id) {
            return s;
        }
    }
    return nullptr;
}

SonificationBase* SonificationModule::sonification(std::string id) {
    for (SonificationBase* s : _sonifications) {
        if (s->identifier() == id) {
            return s;
        }
    }
    return nullptr;
}

SonificationModule::SurroundMode SonificationModule::surroundMode() const {
    return _surroundMode;
}

void SonificationModule::update(std::atomic<bool>& isRunning) {
    Scene* scene = nullptr;
    Camera* camera = nullptr;
    bool isInitialized = false;

    while (isRunning) {
        // Wait for the main thread
        LINFO("Sonificaiton is waiting for the main thread");
        std::unique_lock lg(mutexLock);
        syncToMain.wait(lg, [] { return isMainDone; });
        LINFO("Sonificaiton doing work");

        // Check if the sonificaiton is even enabled
        if (_enabled) {
            // Initialize the scena and camera information
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
                if (!scene || scene->isInitializing() || scene->root()->children().empty() ||
                    !camera ||glm::length(camera->positionVec3()) <
                    std::numeric_limits<glm::f64>::epsilon())
                {
                    isInitialized = false;
                }
                else {
                    isInitialized = true;
                }
            }

            if (isInitialized && scene && camera) {
                // Process the sonifications
                for (SonificationBase* sonification : _sonifications) {
                    if (!sonification) {
                        continue;
                    }
                    sonification->update(camera);
                }
            }
        }
        else {
            std::this_thread::sleep_for(std::chrono::seconds(1));
        }

        // Reset
        isMainDone = false;

        // Manually unlock before notifying, this is to avoid waking up
        // the waiting thread only to block it again (see notify_one for details)
        lg.unlock();
        syncToMain.notify_one();
    }
}

std::vector<scripting::LuaLibrary> SonificationModule::luaLibraries() const {
    return {
        PlanetsSonification::luaLibrary()
    };
}

} // namespace openspace
