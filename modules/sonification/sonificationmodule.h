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

#ifndef __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONMODULE___H__
#define __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONMODULE___H__

#include "openspace/util/openspacemodule.h"

#include <modules/sonification/include/sonificationbase.h>
#include <openspace/camera/camera.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/scene/scene.h>
#include <atomic>

namespace openspace {

class SonificationBase;

class SonificationModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "Sonification";

    /**
    * Horizontal: Calculate the angle to the object in the horizontal direction of
    *             the camera view plane. Where straight forwards in the camera view
    *             direction is zero degrees. TODO: Where is + and -?. (Norrköping dome)
    *
    * HorizontalWithElevation: Same as Horizontal with an additional angle in the vertical
    *                          direction of the camera view plane. Where the angle goes
    *                          from -pi/2 to pi/2 and zero is straight forwards in the
    *                          camera view direction. Positive elevation angles in the up
    *                          direction of the camera.
    *
    * Circular: Calculate the angle to the object in circular space around the center
    *           point in the camera view plane. Where the angle goes from -pi to pi and
    *           zero degrees would be straig up in the camera up direction. Negative
    *           angles in the right direction of the camrea.
    *
    * CircularWithElevation: Smae as Circular with an additional angle in the vertical
    *                        direction of the camera view plane (same as the elevation
    *                        angle in HorizontalWithElevation). Where the angle goes
    *                        from -pi/2 to pi/2 and zero is straight forwards in the
    *                        camera view direction. Positive elevation angles in the up
    *                        direction of the camera. (Hayden planetarium)
    *
    * None: Mono. No directional information at all.
    */
    enum class SurroundMode {
        Horizontal = 0,
        HorizontalWithElevation,
        Circular,
        CircularWithElevation,
        None
    };

    SonificationModule();
    ~SonificationModule();

    /**
     * Returns the Lua library that contains all Lua functions available to for the
     * sonification module.
     *
     * \return The Lua library that contains all Lua functions available to for the
     * sonification module
     */
    std::vector<scripting::LuaLibrary> luaLibraries() const override;

    virtual void internalInitialize(const ghoul::Dictionary& dictionary) override;
    virtual void internalDeinitialize() override;

    /**
     * Get the list of sonifications that are currently registered in the sonification
     * module
     *
     * \return the list of registered sonifications
     */
    const std::vector<SonificationBase*>& sonifications() const;

    /**
     * Get a specified sonification from the list of registered sonifications in the
     * sonification module
     *
     * \param id the identifier for the sonification to get
     *
     * \return the requested sonification
     */
    const SonificationBase* sonification(std::string id) const;
    SonificationBase* sonification(std::string id);

    /**
     * Get the current surround mode used in the sonification module
     *
     * \return the current surround mode
     */
    SurroundMode surroundMode() const;

    // For syncing the sonification thread with the main thread
    static bool isMainDone;

private:
    /**
     * Main update function that keeps track of all sonificaitons and keeps the thread
     * running
     *
     * \param isRunning whether the thread should be kept running or not
     */
    void update(std::atomic<bool>& isRunning);

    /**
     * Initializs the data required for the sonificaiton
     *
     * \param scene pointer to the scene, should be set by this function if the scene has
     *        already been initialized
     * \param camera pointer to the camera, should be set by this function if the scene
     *        and camera has already been initialized
     *
     * \return whether the sonification was successfully initialized or not
     */
    bool initialize(Scene* scene, Camera* camera);

    /**
     * Add the a specified sonification to the list of registered sonifications in the
     * sonification module
     *
     * \param sonification the sonification to add
     */
    void addSonification(SonificationBase* sonification);

    /**
     * Function that gets called when the surround mode is changed in the GUI
     */
    void guiOnChangeSurroundMode();

    // To sync the sonificaiton thread with the main thread
    std::mutex mutexLock;
    std::condition_variable syncToMain;

    // Properties
    properties::BoolProperty _enabled;
    properties::StringProperty _ipAddress;
    properties::IntProperty _port;
    properties::OptionProperty _mode;

    // Variables
    std::thread _updateThread;
    std::atomic<bool> _isRunning = false;
    std::vector<SonificationBase*> _sonifications;
    SurroundMode _surroundMode = SurroundMode::Horizontal;
};

} // namespace openspace

#endif __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONMODULE___H__
