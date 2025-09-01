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

#ifndef __OPENSPACE_MODULE_TELEMETRY___UTIL___H__
#define __OPENSPACE_MODULE_TELEMETRY___UTIL___H__

#include <modules/telemetry/telemetrymodule.h>
#include <openspace/camera/camera.h>
#include <openspace/util/distanceconversion.h>

namespace openspace {
/**
* Calculate the distance from the camera to the node with the given identifier, in the
* given distance unit.
*
* \param camera Pointer to the camera in the scene that the distance should be calculated
*        from
* \param nodeIdentifier The identifier of the node that the distance should be calculated
*        to
* \param unit The distance unit that the answer should be in, the default is meters
*
* \return The distance from the camera to the node with the given identifier in the given
*         distance unit
*/
double calculateDistanceTo(const Camera* camera, const std::string& nodeIdentifier,
    DistanceUnit unit = DistanceUnit::Meter);

/**
* Calculate the distance from the camera to the node with the given position, in the given
* distance unit.
*
* \param camera Pointer to the camera in the scene that the distance should be calculated
*        from
* \param nodePosition The world position of the node that the distance should be
*        calculated to
* \param unit The distance unit that the answer should be in, the default is meters
*
* \return The distance from the camera to the node with the given position in the given
*         distance unit
*/
double calculateDistanceTo(const Camera* camera, glm::dvec3 nodePosition,
    DistanceUnit unit = DistanceUnit::Meter);

/**
* Calculate the angle in radians from the camera to the node with the given identifier.
*
* \param camera Pointer to the camera in the scene that the angle should be calculated
*        from
* \param nodeIdentifier The identifier of the node, that the angle should be calculated to
* \param angleCalculationMode The angle calculation mode to use. This determines which
*        method to use when calculating the angle.
*
* \return The angle in radians from the camera to the node with the given identifier
*/
double calculateAngleTo(const Camera* camera, const std::string& nodeIdentifier,
    TelemetryModule::AngleCalculationMode angleCalculationMode);

/**
* Calculate the angle in radians from the camera to the node with the given position.
*
* \param camera Pointer to the camera in the scene that the angle should be calculated
*        from
* \param nodePosition The position of the node, that the angle should be calculated to
* \param angleCalculationMode The angle calculation mode to use. This determines which
*        method to use when calculating the angle.
*
* \return The angle in radians from the camera to the node with the given position
*/
double calculateAngleTo(const Camera* camera, glm::dvec3 nodePosition,
    TelemetryModule::AngleCalculationMode angleCalculationMode);

/**
* Calculate the angle in radians from the first node (A) with the given identifier to the
* second node (B) with the given identifier.
*
* \param camera Pointer to the camera in the scene
* \param nodeIdentifierA The identifier of the first node (A) that the angle should be
*        calculated from
* \param nodeIdentifierB The identifier of the second node (B) that the angle should be
*        calculated to
* \param angleCalculationMode The angle calculation mode to use. This determines which
*        method to use when calculating the angle.
*
* \return The angle in radians from the first node (A) with the given identifier to the
*         second node (B) with the given identifier
*/
double calculateAngleFromAToB(const Camera* camera,
    const std::string& nodeIdentifierA, const std::string& nodeIdentifierB,
    TelemetryModule::AngleCalculationMode angleCalculationMode);

/**
* Calculate the angle in radians from the first node (A) with the given position to the
* second node (B) with the given position.
*
* \param camera Pointer to the camera in the scene
* \param nodePositionA The position of the first node (A) that the angle should be
*        calculated from
* \param nodePositionB The position of the second node (B) that the angle should be
*        calculated to
* \param angleCalculationMode The angle calculation mode to use. This determines which
*        method to use when calculating the angle.
*
* \return The angle in radians from the first node (A) with the given position to the
*         second node (B) with the given position
*/
double calculateAngleFromAToB(const Camera* camera, glm::dvec3 nodePositionA,
    glm::dvec3 nodePositionB,
    TelemetryModule::AngleCalculationMode angleCalculationMode);

/**
* Calculate the elevation angle in radians from the camera to the node with the given
* identifier.
*
* \param camera Pointer to the camera in the scene that the elevation angle should be
*        calculated from
* \param nodeIdentifier The identifier of the node that the elevation angle should be
*        calculated to
* \param angleCalculationMode The angle calculation mode to use. This determines which
*        method to use when calculating the angle.
*
* \return The elevation angle in radians from the camera to the node with the given
*         identifier
*/
double calculateElevationAngleTo(const Camera* camera,
    const std::string& nodeIdentifier,
    TelemetryModule::AngleCalculationMode angleCalculationMode);

/**
* Calculate the elevation angle in radians from the camera to the node with the given
* position.
*
* \param camera Pointer to the camera in the scene that the elevation angle should be
*        calculated from
* \param nodePosition The position of the node that the elevation angle should be
*        calculated to
* \param angleCalculationMode The angle calculation mode to use. This determines which
*        method to use when calculating the angle.
*
* \return The elevation angle in radians from the camera to the node with the given
*         position
*/
double calculateElevationAngleTo(const Camera* camera, glm::dvec3 nodePosition,
    TelemetryModule::AngleCalculationMode angleCalculationMode);

/**
* Calculate the elevation angle in radians from the first node (A) with the given
* identifier to the second node (B) with the given identifier.
*
* \param camera Pointer to the camera in the scene
* \param nodeIdentifierA The identifier of the first node (A) that the elevation angle
*        should be calculated from
* \param nodeIdentifierB The identifier of the second node (B) that the elevation angle
*        should be calculated to
* \param angleCalculationMode The angle calculation mode to use. This determines which
*        method to use when calculating the angle.
*
* \return The elevation angle in radians from the first node (A) with the given identifier
*         to the second node (B) with the given identifier
*/
double calculateElevationAngleFromAToB(const Camera* camera,
    const std::string& nodeIdentifierA, const std::string& nodeIdentifierB,
    TelemetryModule::AngleCalculationMode angleCalculationMode);

/**
* Calculate the elevation angle in radians from the first node (A) with the given position
* to the second node (B) with the given position.
*
* \param camera Pointer to the camera in the scene
* \param nodePositionA The position of the first node (A) that the elevation angle should
*        be calculated from
* \param nodePositionB The position of the second node (B) that the elevation angle should
*        be calculated to
* \param angleCalculationMode The angle calculation mode to use. This determines which
*        method to use when calculating the angle.
*
* \return The elevation angle in radians from the first node (A) with the given position
*         to the second node (B) with the given position
*/
double calculateElevationAngleFromAToB(const Camera* camera,
    glm::dvec3 nodePositionA, glm::dvec3 nodePositionB,
    TelemetryModule::AngleCalculationMode angleCalculationMode);

} // namespace openspace

#endif // __OPENSPACE_MODULE_TELEMETRY___UTIL___H__
