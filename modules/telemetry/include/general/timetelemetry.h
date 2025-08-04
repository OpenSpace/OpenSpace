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

#ifndef __OPENSPACE_MODULE_TELEMETRY___TIMETELEMETRY___H__
#define __OPENSPACE_MODULE_TELEMETRY___TIMETELEMETRY___H__

#include <modules/telemetry/include/telemetrybase.h>

#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>

namespace openspace {

class TimeTelemetry : public TelemetryBase {
public:
    TimeTelemetry(const std::string& ip, int port);
    ~TimeTelemetry() override = default;

private:
    /**
     * Gather time telemetry information (speed, and current time).
     *
     * \param camera The camera in the scene (not used in this case)
     * \return `true` if the data is new compared to before, otherwise `false`
     */
    bool updateData(const Camera* camera) override;

    /**
     * Send the current time telemetry information to the Open Sound Control receiver. The
     * order of sent data is as follows: Time speed, unit of time speed, and current
     * simulation time in J2000 number of seconds.
     */
    void sendData() override;

    struct PrecisionProperties : properties::PropertyOwner {
        PrecisionProperties(properties::PropertyOwner::PropertyOwnerInfo precisionInfo);

        properties::DoubleProperty timePrecision;
    };

    properties::OptionProperty _timeUnitOption;
    PrecisionProperties _precisionProperties;

    double _timeSpeed = 0.0;
    double _currentTime = 0.0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_TELEMETRY___TIMETELEMETRY___H__
