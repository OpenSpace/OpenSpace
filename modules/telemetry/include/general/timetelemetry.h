/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_TELEMETRY___TIMETELEMETRY___H__
#define __OPENSPACE_MODULE_TELEMETRY___TIMETELEMETRY___H__

#include <modules/telemetry/include/telemetrybase.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>

namespace openspace {

class TimeTelemetry : public TelemetryBase {
public:
    TimeTelemetry(const std::string& ip, int port);
    virtual ~TimeTelemetry() override = default;

    /**
     * Main update function to gather time telemetry information (current deltatime, and current
     * simulation time in J2000 seconds) and send it via the osc connection.
     *
     * \param camera The camera in the scene (not used in this case)
     */
    virtual void update(const Camera*) override;

    /**
     * Function to stop the gathering of time telemetry data.
     */
    virtual void stop() override;

private:
    // Indices for data items
    static constexpr int NumDataItems = 3;
    static constexpr int TimeSpeedIndex = 0;
    static constexpr int TimeSpeedUnitIndex = 1;
    static constexpr int CurrentTimeIndex = 2;

    /**
     * Gather time telemetry information (speed, and current time)
     *
     * \return True if the data is new compared to before, otherwise false
     */
    bool getData();

    /**
     * Send the current time telemetry information over the osc connection.
     * Order of data: Time speed, unit of time speed, current simulation time in J2000
     *                number of seconds.
     */
    void sendData();

    struct PrecisionProperty : properties::PropertyOwner {
        PrecisionProperty(properties::PropertyOwner::PropertyOwnerInfo precisionInfo);

        properties::DoubleProperty timePrecision;
    };

    properties::OptionProperty _timeUnitOption;
    PrecisionProperty _precisionProperty;

    double _timeSpeed = 0.0;
    double _currentTime = 0.0;
};

} // namespace openspace

#endif __OPENSPACE_MODULE_TELEMETRY___TIMETELEMETRY___H__
