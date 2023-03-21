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
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/stringproperty.h>
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

    virtual void internalInitialize(const ghoul::Dictionary& dictionary) override;
    virtual void internalDeinitialize() override;

    SurroundMode surroundMode() const;

private:
    void update(std::atomic<bool>& isRunning);
    void addSonification(SonificationBase* sonification);
    void guiChangeSurroundMode();

    properties::BoolProperty _enabled;
    properties::StringProperty _ipAddress;
    properties::IntProperty _port;
    properties::OptionProperty _mode;

    std::thread _updateThread;
    std::atomic<bool> _isRunning = false;
    std::vector<SonificationBase*> _sonifications;
    SurroundMode _surroundMode = SurroundMode::Horizontal;
};

} // namespace openspace

#endif __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONMODULE___H__
