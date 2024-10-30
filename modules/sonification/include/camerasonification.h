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

#ifndef __OPENSPACE_MODULE_SONIFICATION___CAMERASONIFICATION___H__
#define __OPENSPACE_MODULE_SONIFICATION___CAMERASONIFICATION___H__

#include <modules/sonification/include/sonificationbase.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>

namespace openspace {

class CameraSonification : public SonificationBase {
public:
    CameraSonification(const std::string& ip, int port);
    virtual ~CameraSonification() override = default;

    /**
     * Main update function for the sonification. Checks the current camera information
     * (position, rotation, and speed) and sends it via the osc connection.
     *
     * \param camera pointer to the camera in the scene
     */
    virtual void update(const Camera* camera) override;

    /**
     * Function to stop the sonification
     */
    virtual void stop() override;

private:
    // Indices for data items
    static constexpr int NumDataItems = 4;
    static constexpr int CameraPosIndex = 0;
    static constexpr int CameraQuatRotIndex = 1;
    static constexpr int CameraSpeedIndex = 2;
    static constexpr int CameraSpeedUnitIndex = 3;

    /**
     * Update the sonification data
     *
     * \param camera pointer to the camera in the scene
     *
     * \return true if the data is new compared to before, otherwise false
     */
    bool getData(const Camera* camera);

    /**
     * Send current sonification data over the osc connection
     * Order of data: Camera position, rotation, speed, and speed distance unit.
     */
    void sendData();

    osc::Blob createBlob(glm::dvec3 data);
    osc::Blob createBlob(glm::dquat data);

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

#endif __OPENSPACE_MODULE_SONIFICATION___CAMERASONIFICATION___H__
