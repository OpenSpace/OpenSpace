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

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        CameraTelemetryInfo =
    {
        "CameraTelemetryInfo",
        "Camera Telemetry",
        "Telemetry that sends out camera information over the OSC connection."
    };

    constexpr openspace::properties::Property::PropertyInfo CameraSpeedDistanceUnitInfo =
    {
        "CameraSpeedDistanceUnit",
        "Camera Speed Unit (Distance)",
        "Choose a distance unit that is used for the camera speed. "
        "For example, if the distacne unit 'Kilometer' is chosen, then the unit used for "
        "the camera speed will be kilometers per second."
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
        "data over the OSC connection.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo RotationPrecisionInfo = {
        "RotationPrecision",
        "Rotation Precision",
        "The precision used to determin when to send updated camera rotational "
        "data over the OSC connection.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SpeedPrecisionInfo = {
        "SpeedPrecision",
        "Speed Precision",
        "The precision in meters per second used to determin when to send updated camera "
        "speed data over the OSC connection.",
        openspace::properties::Property::Visibility::User
    };
} // namespace

namespace openspace {

CameraTelemetry::CameraTelemetry(const std::string& ip, int port)
    : TelemetryBase(CameraTelemetryInfo, ip, port)
    , _cameraSpeedDistanceUnitOption(
        CameraSpeedDistanceUnitInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _precisionProperty(CameraTelemetry::PrecisionProperty(PrecisionInfo))
{
    // Add all distance units as options in the camera speed unit option drop down menu
    for (int i = 0; i < DistanceUnitNamesSingular.size(); ++i) {
        _cameraSpeedDistanceUnitOption.addOption(i, DistanceUnitNamesSingular[i].data());
    }

    // Set kilometers as default camera speed distance unit
    _cameraSpeedDistanceUnitOption.setValue(static_cast<int>(DistanceUnit::Kilometer));
    addProperty(_cameraSpeedDistanceUnitOption);

    addPropertySubOwner(_precisionProperty);
}

void CameraTelemetry::update(const Camera* camera) {
    if (!_enabled) {
        return;
    }

    bool hasNewData = getData(camera);

    // Only send data if something new has happened
    if (hasNewData) {
        sendData();
    }
}

void CameraTelemetry::stop() {}

CameraTelemetry::PrecisionProperty::PrecisionProperty(
                               properties::PropertyOwner::PropertyOwnerInfo precisionInfo)
    : properties::PropertyOwner(precisionInfo)
    , positionPrecision(PositionPrecisionInfo, 1000.0, 0, 1e+25)
    , rotationPrecision(RotationPrecisionInfo, 0.05, 0, 10)
    , speedPrecision(SpeedPrecisionInfo, 1000.0, 0, std::numeric_limits<double>::max())
{
    positionPrecision.setExponent(20.f);
    addProperty(positionPrecision);

    addProperty(rotationPrecision);

    speedPrecision.setExponent(100.f);
    addProperty(speedPrecision);
}

bool CameraTelemetry::getData(const Camera* camera) {
    // Position
    const glm::dvec3 cameraPosition = camera->positionVec3();
    const double distanceMoved = glm::length(_cameraPosition - cameraPosition);

    // Rotation
    const glm::dquat cameraRotation = camera->rotationQuaternion();
    // To check if the rotation has changed above the precision threshold, check the
    // angle and axis of the quaternion seperatly
    const double rotationAngleDifference = std::abs(_cameraRotation.w - cameraRotation.w);
    const double rotationAxisDifference = glm::length(
        glm::dvec3(_cameraRotation.x, _cameraRotation.y, _cameraRotation.z) -
        glm::dvec3(cameraRotation.x, cameraRotation.y, cameraRotation.z)
    );

    // Speed
    double frameTime = global::windowDelegate->deltaTime();
    bool hasFps = false;
    double cameraSpeed = 0.0;
    if (std::abs(frameTime) > std::numeric_limits<double>::epsilon()) {
        // Avoid division by 0 by first checking the current frame time
        hasFps = true;
        double distanceMovedInUnit = convertMeters(
            distanceMoved,
            DistanceUnits[_cameraSpeedDistanceUnitOption]
        );
        cameraSpeed = distanceMovedInUnit / frameTime;
    }

    // Check if this data is new, otherwise don't send it
    double prevCameraSpeed = _cameraSpeed;
    bool shouldSendData = false;

    if (distanceMoved > _precisionProperty.positionPrecision) {
        _cameraPosition = cameraPosition;
        shouldSendData = true;
    }

    if (rotationAngleDifference > _precisionProperty.rotationPrecision ||
        rotationAxisDifference > _precisionProperty.rotationPrecision)
    {
        _cameraRotation = cameraRotation;
        shouldSendData = true;
    }

    if (hasFps && abs(prevCameraSpeed - cameraSpeed) > _precisionProperty.speedPrecision)
    {
        _cameraSpeed = cameraSpeed;
        shouldSendData = true;
    }

    return shouldSendData;
}

void CameraTelemetry::sendData() {
    std::string label = "/Camera";
    std::vector<OpenSoundControlDataType> data(NumDataItems);

    // Position
    data[CameraPosXIndex] = _cameraPosition.x;
    data[CameraPosYIndex] = _cameraPosition.y;
    data[CameraPosZIndex] = _cameraPosition.z;

    // Rotation
    data[CameraQuatRotWIndex] = _cameraRotation.w;
    data[CameraQuatRotXIndex] = _cameraRotation.x;
    data[CameraQuatRotYIndex] = _cameraRotation.y;
    data[CameraQuatRotZIndex] = _cameraRotation.z;

    // Speed
    data[CameraSpeedIndex] = _cameraSpeed;

    // Speed distance unit
    data[CameraSpeedUnitIndex] =_cameraSpeedDistanceUnitOption.getDescriptionByValue(
        _cameraSpeedDistanceUnitOption.value()
    );

    _connection->send(label, data);
}

} // namespace openspace
