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

#ifndef __OPENSPACE_MODULE_TELEMETRY___CAMERATELEMETRY___H__
#define __OPENSPACE_MODULE_TELEMETRY___CAMERATELEMETRY___H__

#include <modules/telemetry/include/telemetrybase.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>

namespace openspace {

class CameraTelemetry : public TelemetryBase {
public:
    CameraTelemetry(const std::string& ip, int port);
    virtual ~CameraTelemetry() override = default;

    /**
     * Main update function to gather camera telemetry information (position, rotation,
     * and speed) and send it via the osc connection.
     *
     * \param camera The camera in the scene
     */
    virtual void update(const Camera* camera) override;

    /**
     * Function to stop the gathering of camera telemetry data
     */
    virtual void stop() override;

private:
    // Indices for data items
    static constexpr int NumDataItems = 9;
    static constexpr int CameraPosXIndex = 0;
    static constexpr int CameraPosYIndex = 1;
    static constexpr int CameraPosZIndex = 2;
    static constexpr int CameraQuatRotWIndex = 3;
    static constexpr int CameraQuatRotXIndex = 4;
    static constexpr int CameraQuatRotYIndex = 5;
    static constexpr int CameraQuatRotZIndex = 6;
    static constexpr int CameraSpeedIndex = 7;
    static constexpr int CameraSpeedUnitIndex = 8;

    /**
     * Gather camera telemetry information (position, rotation, and speed)
     *
     * \param camera The camera in the scene
     *
     * \return True if the data is new compared to before, otherwise false
     */
    bool getData(const Camera* camera);

    /**
     * Send the current camera telemetry information over the osc connection
     * Order of data: Camera position (x, y, z), rotation quaternion (w, x, y z), speed,
     *                and speed distance unit.
     */
    void sendData();

    // Properties
    struct PrecisionProperty : properties::PropertyOwner {
        PrecisionProperty(properties::PropertyOwner::PropertyOwnerInfo precisionInfo);

        properties::DoubleProperty positionPrecision;
        properties::DoubleProperty rotationPrecision;
        properties::DoubleProperty speedPrecision;
    };

    properties::OptionProperty _cameraSpeedDistanceUnitOption;
    PrecisionProperty _precisionProperty;

    // Variables
    glm::dvec3 _cameraPosition = glm::dvec3(0.0);
    glm::dquat _cameraRotation = glm::dquat(0.0, 0.0, 0.0, 0.0);
    double _cameraSpeed = 0.0;
};

} // namespace openspace

#endif __OPENSPACE_MODULE_TELEMETRY___CAMERATELEMETRY___H__
