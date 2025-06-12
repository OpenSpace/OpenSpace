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

#ifndef __OPENSPACE_MODULE_TELEMETRY___TELEMETRYMODULE___H__
#define __OPENSPACE_MODULE_TELEMETRY___TELEMETRYMODULE___H__

#include "openspace/util/openspacemodule.h"

#include <modules/telemetry/include/telemetrybase.h>
#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/misc/stringproperty.h>
#include <ghoul/misc/boolean.h>
#include <atomic>
#include <condition_variable>

namespace openspace {

class TelemetryBase;

class TelemetryModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "Telemetry";

    /**
     * This mode only affects telemetries that send angle information. For example, the
     * NodesTelemetry and the PlanetsSonification. More documentation for the elevation
     * part of the angles is located further below.
     *
     * Horizontal: This angle calculation mode is suitable for flat displays or
     *             forward-facing immersive environments. For more information about
     *             surround sound configurations, see "Surround Sound Configurations" on
     *             the OpenSpace documentation page. This angle determines where the
     *             object is placed within a horizontal plane of reference in relation to
     *             the camera.
     *
     * Circular: This angle calculation mode is suitable for centered fisheye displays or
     *           omnidirectional immersive environments. For more information about
     *           surround sound configurations, see "Surround Sound Configurations" on
     *           the OpenSpace documentation page. The computed angle determines the
     *           object's position in a circular space around the center of the screen.
     *           Compared to the horizontal mode, this mode calculates the angles in a
     *           circular (or radial) manner around the center point instead of the
     *           deviation from the camera view direction.
     */
    enum class AngleCalculationMode {
        Horizontal = 0,
        Circular
    };

    TelemetryModule();
    ~TelemetryModule();

    /**
     * Returns the Lua libraries for all telemetries available in the telemetry module.
     *
     * \return The Lua libraries for all telemetries available in the telemetry module
     */
    std::vector<scripting::LuaLibrary> luaLibraries() const override;

    virtual void internalInitialize(const ghoul::Dictionary& dictionary) override;
    virtual void internalDeinitialize() override;

    /**
     * Get the list of telemetries that are currently registered in the module.
     *
     * \return A list of all registered telemetries
     */
    const std::vector<TelemetryBase*>& telemetries() const;

    /**
     * Get a specified telemetry from the list of registered telemetries in the module.
     *
     * \param id The identifier of the telemetry to fetch
     *
     * \return The requested telemetry
     */
    const TelemetryBase* telemetry(const std::string_view& id) const;
    TelemetryBase* telemetry(const std::string_view& id);

    /**
     * Get the current angle calculation mode used in the telemetry module.
     *
     * \return The angle calculation mode
     */
    AngleCalculationMode angleCalculationMode() const;

    /**
     * Return whether any elevation angles are being caclulated and sent to the Open Sound
     * Control receiver or not.
     *
     * \return `true` if elevation angles are being calculated and sent to the Open Sound
     * Control receiver, `false` otherwise
     */
    bool includeElevationAngle() const;

private:
    /**
     * Main update function that keeps track of all telemetries and keeps the update
     * thread running and synced to the OpenSpace main thread.
     *
     * \param isRunning Whether the thread should be kept running or shut down and joined
     */
    void update(std::atomic<bool>& isRunning);

    /**
     * Add a given telemetry to the list of registered telemetries in the module.
     *
     * \param telemetry The telemetry to register in the module
     */
    void addTelemetry(TelemetryBase* telemetry);

    /**
     * Function that gets called when the angle calculation mode is changed in the GUI.
     */
    void guiOnChangeAngleCalculationMode();

    // To sync the sonification thread with the main thread
    std::mutex mutexLock;
    std::condition_variable syncToMain;

    properties::BoolProperty _enabled;
    properties::StringProperty _ipAddress;
    properties::IntProperty _port;
    properties::OptionProperty _modeOptions;

    /**
     * This setting only affects telemetries that send angle information. For example, the
     * NodesTelemetry and the PlanetsSonification.
     *
     * `true`: This angle determines where the object is placed within a vertical plane of
     *         reference in relation to the camera, i.e the height in relation to the
     *         horizontal plane of reference.
     *
     * `false`: The elevation angle sent to the Open Sound Control receiver is always set
     *          to 0.0
    */
    properties::BoolProperty _includeElevationAngle;

    std::thread _updateThread;
    std::atomic<bool> _isRunning = false;
    std::vector<TelemetryBase*> _telemetries;
    AngleCalculationMode _angleCalculationMode = AngleCalculationMode::Horizontal;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_TELEMETRY___TELEMETRYMODULE___H__
