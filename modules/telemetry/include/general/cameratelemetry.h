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

#ifndef __OPENSPACE_MODULE_TELEMETRY___CAMERATELEMETRY___H__
#define __OPENSPACE_MODULE_TELEMETRY___CAMERATELEMETRY___H__

#include <modules/telemetry/include/telemetrybase.h>

#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>

namespace openspace {

class CameraTelemetry : public TelemetryBase {
public:
    CameraTelemetry(const std::string& ip, int port);
    ~CameraTelemetry() override = default;

private:
    /**
     * Gather camera telemetry information (position, rotation, and speed).
     *
     * \param camera The camera in the scene
     * \return `true` if the data is new compared to before, otherwise `false`
     */
    bool updateData(const Camera* camera) override;

    /**
     * Send the current camera telemetry information to the Open Sound Control
     * receiver. The order of sent data is as follows: Camera position (x, y, z), rotation
     * quaternion (w, x, y z), speed, and speed distance unit.
     */
    void sendData() override;

    struct PrecisionProperties : properties::PropertyOwner {
        PrecisionProperties(properties::PropertyOwner::PropertyOwnerInfo precisionInfo);

        properties::DoubleProperty positionPrecision;
        properties::DoubleProperty rotationPrecision;
        properties::DoubleProperty speedPrecision;
    };

    properties::OptionProperty _cameraSpeedDistanceUnitOption;
    PrecisionProperties _precisionProperties;

    glm::dvec3 _cameraPosition = glm::dvec3(0.0);
    glm::dquat _cameraRotation = glm::dquat(0.0, 0.0, 0.0, 0.0);
    double _cameraSpeed = 0.0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_TELEMETRY___CAMERATELEMETRY___H__
