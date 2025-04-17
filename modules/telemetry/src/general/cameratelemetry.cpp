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

#include <modules/telemetry/include/general/cameratelemetry.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/util/distanceconversion.h>

namespace {
    // Indices for data items
    constexpr int NumDataItems = 9;
    constexpr int CameraPosXIndex = 0;
    constexpr int CameraPosYIndex = 1;
    constexpr int CameraPosZIndex = 2;
    constexpr int CameraQuatRotWIndex = 3;
    constexpr int CameraQuatRotXIndex = 4;
    constexpr int CameraQuatRotYIndex = 5;
    constexpr int CameraQuatRotZIndex = 6;
    constexpr int CameraSpeedIndex = 7;
    constexpr int CameraSpeedUnitIndex = 8;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        CameraTelemetryInfo =
    {
        "CameraTelemetryInfo",
        "Camera Telemetry",
        "Telemetry that sends out camera information to the Open Sound Control receiver."
    };

    constexpr openspace::properties::Property::PropertyInfo CameraSpeedDistanceUnitInfo =
    {
        "CameraSpeedDistanceUnit",
        "Camera Speed Unit (Distance)",
        "Choose a distance unit that is used for the camera speed. "
        "For example, if the distacne unit 'Kilometer' is chosen, then the unit used for "
        "the camera speed will be kilometers per second.",
        openspace::properties::Property::Visibility::User
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo PrecisionInfo = {
        "Precision",
        "Precision",
        "Settings for the precision of the camera telemetry information."
    };

    constexpr openspace::properties::Property::PropertyInfo PositionPrecisionInfo = {
        "PositionPrecision",
        "Position Precision",
        "The precision in meters used to determin when to send updated camera positional "
        "data to the Open Sound Control receiver.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo RotationPrecisionInfo = {
        "RotationPrecision",
        "Rotation Precision",
        "The precision used to determin when to send updated camera rotational "
        "data to the Open Sound Control receiver.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SpeedPrecisionInfo = {
        "SpeedPrecision",
        "Speed Precision",
        "The precision in meters per second used to determin when to send updated camera "
        "speed data to the Open Sound Control receiver.",
        openspace::properties::Property::Visibility::User
    };
} // namespace

namespace openspace {

CameraTelemetry::CameraTelemetry(const std::string& ip, int port)
    : TelemetryBase(CameraTelemetryInfo, ip, port)
    , _cameraSpeedDistanceUnitOption(CameraSpeedDistanceUnitInfo)
    , _precisionProperties(CameraTelemetry::PrecisionProperties(PrecisionInfo))
{
    for (int i = 0; i < DistanceUnitNames.size(); ++i) {
        _cameraSpeedDistanceUnitOption.addOption(i, DistanceUnitNames[i].singular.data());
    }

    _cameraSpeedDistanceUnitOption.setValue(static_cast<int>(DistanceUnit::Kilometer));
    addProperty(_cameraSpeedDistanceUnitOption);

    addPropertySubOwner(_precisionProperties);
}

CameraTelemetry::PrecisionProperties::PrecisionProperties(
                               properties::PropertyOwner::PropertyOwnerInfo precisionInfo)
    : properties::PropertyOwner(precisionInfo)
    , positionPrecision(PositionPrecisionInfo, 1000.0, 0.0, 1.0e+25)
    , rotationPrecision(RotationPrecisionInfo, 0.05, 0.0, 10.0)
    , speedPrecision(SpeedPrecisionInfo, 1000.0, 0.0, std::numeric_limits<double>::max())
{
    positionPrecision.setExponent(20.f);
    addProperty(positionPrecision);

    addProperty(rotationPrecision);

    speedPrecision.setExponent(100.f);
    addProperty(speedPrecision);
}

bool CameraTelemetry::updateData(const Camera* camera) {
    const glm::dvec3 cameraPosition = camera->positionVec3();
    const double distanceMoved = glm::length(_cameraPosition - cameraPosition);

    const glm::dquat cameraRotation = camera->rotationQuaternion();
    // To check if the rotation has changed above the precision threshold, check the
    // angle and axis of the quaternion seperatly
    const double rotationAngleDifference = std::abs(_cameraRotation.w - cameraRotation.w);
    const double rotationAxisDifference = glm::length(
        glm::dvec3(_cameraRotation.x, _cameraRotation.y, _cameraRotation.z) -
        glm::dvec3(cameraRotation.x, cameraRotation.y, cameraRotation.z)
    );

    double frameTime = global::windowDelegate->deltaTime();
    bool hasFps = false;
    double cameraSpeed = 0.0;
    // Avoid division by 0 by first checking the current frame time
    if (std::abs(frameTime) > std::numeric_limits<double>::epsilon()) {
        hasFps = true;
        double distanceMovedInUnit = convertMeters(
            distanceMoved,
            DistanceUnits[_cameraSpeedDistanceUnitOption]
        );
        cameraSpeed = distanceMovedInUnit / frameTime;
    }

    // Check if this data is new, otherwise don't send it
    double prevCameraSpeed = _cameraSpeed;
    bool dataWasUpdated = false;

    if (distanceMoved > _precisionProperties.positionPrecision) {
        _cameraPosition = cameraPosition;
        dataWasUpdated = true;
    }

    if (rotationAngleDifference > _precisionProperties.rotationPrecision ||
        rotationAxisDifference > _precisionProperties.rotationPrecision)
    {
        _cameraRotation = cameraRotation;
        dataWasUpdated = true;
    }

    if (hasFps &&
        abs(prevCameraSpeed - cameraSpeed) > _precisionProperties.speedPrecision)
    {
        _cameraSpeed = cameraSpeed;
        dataWasUpdated = true;
    }

    return dataWasUpdated;
}

void CameraTelemetry::sendData() {
    std::string label = "/Camera";
    std::vector<OpenSoundControlDataType> data(NumDataItems);

    data[CameraPosXIndex] = _cameraPosition.x;
    data[CameraPosYIndex] = _cameraPosition.y;
    data[CameraPosZIndex] = _cameraPosition.z;

    data[CameraQuatRotWIndex] = _cameraRotation.w;
    data[CameraQuatRotXIndex] = _cameraRotation.x;
    data[CameraQuatRotYIndex] = _cameraRotation.y;
    data[CameraQuatRotZIndex] = _cameraRotation.z;

    data[CameraSpeedIndex] = _cameraSpeed;
    data[CameraSpeedUnitIndex] =_cameraSpeedDistanceUnitOption.getDescriptionByValue(
        _cameraSpeedDistanceUnitOption.value()
    );

    _connection->send(label, data);
}

} // namespace openspace
