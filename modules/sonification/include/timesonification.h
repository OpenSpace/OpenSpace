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

#ifndef __OPENSPACE_MODULE_SONIFICATION___TIMESONIFICATION___H__
#define __OPENSPACE_MODULE_SONIFICATION___TIMESONIFICATION___H__

#include <modules/sonification/include/sonificationbase.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/util/timeconversion.h>

namespace openspace {

class TimeSonification : public SonificationBase {
public:
    TimeSonification(const std::string& ip, int port);
    virtual ~TimeSonification() override = default;

    /**
     * Main update function for the sonification. Checks the current delta time and sends
     * it via the osc connection in the unit of days/second.
     *
     * \param camera pointer to the camera in the scene (not used in this sonification)
     */
    virtual void update(const Camera*) override;

    /**
     * Function to stop the sonification
     */
    virtual void stop() override;

private:
    // Indices for data items
    static constexpr int NumDataItems = 3;
    static constexpr int TimeSpeedIndex = 0;
    static constexpr int TimeSpeedUnitIndex = 1;
    static constexpr int CurrentTimeIndex = 2;

    /**
     * Update the sonification data
     *
     * \return true if the data is new compared to before, otherwise false
     */
    bool getData();

    /**
     * Send current sonification data over the osc connection
     * Order of data: time speed, unit of time speed, current simulation time in J2000
     * number of seconds.
     */
    void sendData();

    // Properties
    properties::OptionProperty _timeUnitOption;

    // Variables
    double _timeSpeed = 0.0;
    double _currentTime = 0.0;
};

} // namespace openspace

#endif __OPENSPACE_MODULE_SONIFICATION___TIMESONIFICATION___H__
