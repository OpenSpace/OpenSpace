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

#ifndef __OPENSPACE_MODULE_TELEMETRY___UTIL___H__
#define __OPENSPACE_MODULE_TELEMETRY___UTIL___H__

#include <openspace/camera/camera.h>
#include <openspace/util/distanceconversion.h>
#include <variant>

namespace openspace {

    /**
     * Calculate the distance from the camera to the node with the given identifier, in
     * the given distance unit
     *
     * \param camera pointer to the camera in the scene that the distance should be
     *        calculated from
     * \param nodeIdOrPos either the identifier or the position of the node, that the
     *        distance should be calculate to
     * \param unit the distance unit the answer should be in, default is meter
     *
     * \return distance from the camera to the node with the given identifier in the
     *         given distance unit
     */
    double calculateDistanceTo(const Camera* camera,
        std::variant<std::string, glm::dvec3> nodeIdOrPos,
        DistanceUnit unit = DistanceUnit::Meter);

    /**
     * Calculate the angle from the camera to the node with the given identifier,
     * in radians
     *
     * \param camera pointer to the camera in the scene that the angle should be
     *        calculated from
     * \param nodeIdOrPos either the identifier or the position of the node, that the
     *        angle should be calculate to
     *
     * \return angle from the camera to the node with the given identifier in radians
     */
    double calculateAngleTo(const Camera* camera,
        std::variant<std::string, glm::dvec3> nodeIdOrPos);

    /**
     * Calculate the angle from the first node with the given identifier to the second
     * node with the given identifier, in radians
     *
     * \param camera pointer to the camera in the scene
     * \param nodeIdOrPosA either the identifier or the position of the first node that
     *        the angle should be calculated from
     * \param nodeIdOrPosB either the identifier or the position of the second node that
     *        the angle should be calculated to
     *
     * \return angle from the first node with the given identifier to the second node with
     *         the given identifier in radians
     */
    double calculateAngleFromAToB(const Camera* camera,
        std::variant<std::string, glm::dvec3> nodeIdOrPosA,
        std::variant<std::string, glm::dvec3> nodeIdOrPosB);

    /**
     * Calculate the elevation angle from the camera to the node with the given
     * identifier, in radians
     *
     * \param camera pointer to the camera in the scene that the elevation angle should be
     *        calculated from
     * \param nodeIdOrPos either the identifier or the position of the node, that the
     *        elevation angle should be calculate to
     *
     * \return elevation angle from the camera to the node with the given identifier in
     *         radians
     */
    double calculateElevationAngleTo(const Camera* camera,
        std::variant<std::string, glm::dvec3> nodeIdOrPos);

    /**
     * Calculate the elevation angle from the first node with the given identifier to the
     * second node with the given identifier, in radians
     *
     * \param camera pointer to the camera in the scene
     * \param nodeIdOrPosA either the identifier or the position of the first node that
     *        the elevation angle should be calculated from
     * \param nodeIdOrPosB either the identifier or the position of the second node that
     *        the elevation angle should be calculated to
     *
     * \return elevation angle from the first node with the given identifier to the second
     *         node with the given identifier in radians
     */
    double calculateElevationAngleFromAToB(const Camera* camera,
        std::variant<std::string, glm::dvec3> nodeIdOrPosA,
        std::variant<std::string, glm::dvec3> nodeIdOrPosB);

} // namespace openspace

#endif __OPENSPACE_MODULE_TELEMETRY___UTIL___H__
