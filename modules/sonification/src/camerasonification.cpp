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

#include <modules/sonification/include/camerasonification.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>

namespace {
    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        CameraSonificationInfo =
    {
        "CameraSonification",
        "Camera Sonification",
        "Sonification that sends out camera information over the OSC connection"
    };

    constexpr openspace::properties::Property::PropertyInfo CmaeraSpeedDistanceUnitInfo = {
        "CmaeraSpeedDistanceUnit",
        "Cmaera Speed Unit (Distance)",
        "Choose a unit that the sonification should use for the camera speed distance. "
        "For example, if the distacne unit 'Kilometer' is chosen, then the unit used for "
        "the camera speed in the sonificaiton would be kilometers per second."
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo PrecisionInfo = {
        "Precision",
        "Precision",
        "Settings for the precision of the sonification"
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

CameraSonification::CameraSonification(const std::string& ip, int port)
    : SonificationBase(CameraSonificationInfo, ip, port)
    , _cameraSpeedDistanceUnitOption(
        CmaeraSpeedDistanceUnitInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _precisionProperty(CameraSonification::PrecisionProperty(PrecisionInfo))
{
    // Add all camera units as options in the drop down menu
    for (int i = 0; i < DistanceUnitNamesSingular.size(); ++i) {
        _cameraSpeedDistanceUnitOption.addOption(i, DistanceUnitNamesSingular[i].data());
    }

    // Set kilometers as default camera speed distance unit
    _cameraSpeedDistanceUnitOption.setValue(static_cast<int>(DistanceUnit::Kilometer));
    addProperty(_cameraSpeedDistanceUnitOption);

    // Precision
    addPropertySubOwner(_precisionProperty);
}

void CameraSonification::update(const Camera* camera) {
    if (!_enabled) {
        return;
    }

    bool hasNewData = getData(camera);

    // Only send data if something new has happened
    if (hasNewData) {
        sendData();
    }
}

void CameraSonification::stop() {}

CameraSonification::PrecisionProperty::PrecisionProperty(
                               properties::PropertyOwner::PropertyOwnerInfo precisionInfo)
    : properties::PropertyOwner(precisionInfo)
    , positionPrecision(
        PositionPrecisionInfo,
        1000.0,
        0,
        1e+25
    )
    , rotationPrecision(
        RotationPrecisionInfo,
        0.05,
        0,
        1e+25
    )
    , speedPrecision(
        SpeedPrecisionInfo,
        1000.0,
        0,
        std::numeric_limits<double>::max()
    )
{
    addProperty(positionPrecision);
    positionPrecision.setExponent(20.f);

    addProperty(rotationPrecision);
    rotationPrecision.setExponent(20.f);

    addProperty(speedPrecision);
    speedPrecision.setExponent(100.f);
}

bool CameraSonification::getData(const Camera* camera) {
    // Position
    const glm::dvec3 cameraPosition = camera->positionVec3();
    const double distanceMoved = glm::length(_cameraPosition - cameraPosition);

    // Rotation
    const glm::dquat cameraRotation = camera->rotationQuaternion();
    // The difference between two quaternions q and k is defined as diff = q * inverse(k).
    // Multiplying quaternions is done with cross protuct (x), not dot product (*).
    // To get the magnitude of rotational change, the lenght of the resulting difference
    // quaternion is used.
    const double rotationDifference =
        glm::length(glm::cross(_cameraRotation, glm::inverse(cameraRotation)));

    // Speed
    double averageFrameTime = global::windowDelegate->averageDeltaTime();
    bool hasFps = false;
    double cameraSpeed = 0.0;
    if (std::abs(averageFrameTime) > std::numeric_limits<double>::epsilon()) {
        // Avoid division by 0 by first checking the average frame time
        hasFps = true;
        double distanceMovedInUnit = convertMeters(
            distanceMoved,
            DistanceUnits[_cameraSpeedDistanceUnitOption]
        );
        cameraSpeed = distanceMovedInUnit / averageFrameTime;
    }

    // Check if this data is new, otherwise don't send it
    double prevCameraSpeed = _cameraSpeed;
    bool shouldSendData = false;

    if (distanceMoved > _precisionProperty.positionPrecision) {
        _cameraPosition = cameraPosition;
        shouldSendData = true;
    }

    if (rotationDifference > _precisionProperty.rotationPrecision) {
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

void CameraSonification::sendData() {
    std::string label = "/Camera";
    std::vector<OscDataType> data(NumDataItems);

    // Position
    data[CameraPosIndex] = createBlob(_cameraPosition);

    // Rotation
    data[CameraQuatRotIndex] = createBlob(_cameraRotation);

    // Speed
    data[CameraSpeedIndex] = _cameraSpeed;

    // Speed distance unit
    data[CameraSpeedUnitIndex] =_cameraSpeedDistanceUnitOption.getDescriptionByValue(
        _cameraSpeedDistanceUnitOption.value()
    );

    _connection->send(label, data);
}

osc::Blob CameraSonification::createBlob(glm::dvec3 data) {
    constexpr int Size = 3;
    double blob[Size] = { 0.0 };

    blob[0] = data.x;
    blob[1] = data.y;
    blob[2] = data.z;

    return osc::Blob(blob, Size);
}

osc::Blob CameraSonification::createBlob(glm::dquat data) {
    constexpr int Size = 4;
    double blob[Size] = { 0.0 };

    blob[0] = data.w;
    blob[1] = data.x;
    blob[2] = data.y;
    blob[3] = data.z;

    return osc::Blob(blob, Size);
}

} // namespace openspace
