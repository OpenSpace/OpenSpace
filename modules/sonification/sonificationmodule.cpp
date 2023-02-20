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

#include <modules/sonification/include/comparesonification.h>
#include <modules/sonification/include/planetssonification.h>
#include <modules/sonification/include/solarsonification.h>
#include <modules/sonification/include/timesonification.h>
#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>

namespace {
    constexpr std::string_view _loggerCat = "SonificationModule";

    //Output to SuperCollider
    constexpr std::string_view SuperColliderIp = "127.0.0.1";
    constexpr int SuperColliderPort = 57120;

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "Enable or disable all sonifications"
    };
} // namespace

namespace openspace {

SonificationModule::SonificationModule()
    : OpenSpaceModule("Sonification")
    , _enabled(EnabledInfo, false)
{
    addProperty(_enabled);
}

SonificationModule::~SonificationModule() {
    // Clear the sonifications list
    for (SonificationBase* sonification : _sonifications) {
        delete sonification;
    }
}

void SonificationModule::internalInitialize(const ghoul::Dictionary&) {
    // Fill sonification list
    _sonifications.push_back(
        new CompareSonification(SuperColliderIp.data(), SuperColliderPort)
    );
    addPropertySubOwner(_sonifications.back());

    _sonifications.push_back(
        new PlanetsSonification(SuperColliderIp.data(), SuperColliderPort)
    );
    addPropertySubOwner(_sonifications.back());

    _sonifications.push_back(
        new SolarSonification(SuperColliderIp.data(), SuperColliderPort)
    );
    addPropertySubOwner(_sonifications.back());

    _sonifications.push_back(
        new TimeSonification(SuperColliderIp.data(), SuperColliderPort)
    );
    addPropertySubOwner(_sonifications.back());

    // Only the master runs the SonificationModule
    if (global::windowDelegate->isMaster()) {
        _isRunning = true;
        _updateThread = std::thread([this]() { update(std::ref(_isRunning)); });
    }
}

void SonificationModule::internalDeinitialize() {
    // Join the thread
    _isRunning = false;

    // Wait before joining the thread
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
    _updateThread.join();
}

void SonificationModule::update(std::atomic<bool>& isRunning) {
    Scene* scene = nullptr;
    Camera* camera = nullptr;
    bool isInitialized = false;

    while (isRunning) {
        // Wait for the scene to initialize, then user can trigger enabled
        if (!_enabled) {
            std::this_thread::sleep_for(std::chrono::seconds(1));
            continue;
        }

        if (!isInitialized) {
            // Find scene
            if (!scene) {
                scene = global::renderEngine->scene();
            }

            // Find camera
            if (!camera) {
                camera = scene ? scene->camera() : nullptr;
            }

            // Check status
            if (!scene || scene->isInitializing() || scene->root()->children().empty() ||
                !camera || glm::length(camera->positionVec3()) <
                std::numeric_limits<glm::f64>::epsilon())
            {
                continue;
            }

            isInitialized = true;
        }

        for (SonificationBase* sonification : _sonifications) {
            if (!sonification) {
                continue;
            }

            sonification->update(camera);
        }
    }
}

} // namespace openspace
