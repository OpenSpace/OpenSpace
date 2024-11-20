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

#include <modules/telemetry/include/util.h>

#include <modules/telemetry/telemetrymodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <glm/gtx/projection.hpp>
#include <glm/gtx/vector_angle.hpp>

namespace {
    constexpr std::string_view _loggerCat = "TelemetryUtil";

    /**
     * Get the position of the node given by the input variant
     *
     * \param nodeIdOrPos either the identifier or the position of the node to get the
     *        position for
     *
     * \return the position of the node
     */
    glm::dvec3 getNodePosition(std::variant<std::string, glm::dvec3> nodeIdOrPos) {
        if (std::holds_alternative<std::string>(nodeIdOrPos)) {
            std::string identifier = std::get<std::string>(nodeIdOrPos);

            if (identifier.empty()) {
                return glm::dvec3(0.0);
            }

            // Find the node
            openspace::SceneGraphNode* node = openspace::sceneGraphNode(identifier);
            if (!node) {
                return glm::dvec3(0.0);
            }

            return node->worldPosition();
        }
        else {
            return std::get<glm::dvec3>(nodeIdOrPos);
        }
    }
} // namespace

namespace openspace {

    double calculateDistanceTo(const Camera* camera,
        std::variant<std::string, glm::dvec3> nodeIdOrPos,
        DistanceUnit unit)
    {
        glm::dvec3 nodePosition = getNodePosition(nodeIdOrPos);
        if (glm::length(nodePosition) < std::numeric_limits<glm::f64>::epsilon()) {
            return 0.0;
        }

        // Calculate distance to the node from the camera
        glm::dvec3 cameraToNode = nodePosition - camera->positionVec3();
        double distance = glm::length(cameraToNode);

        // Convert from meters to desired unit
        return convertMeters(distance, unit);
    }

    // Horizontal angle
    double calculateAngleTo(const Camera* camera,
        std::variant<std::string,
        glm::dvec3> nodeIdOrPos)
    {
        glm::dvec3 nodePosition = getNodePosition(nodeIdOrPos);
        if (glm::length(nodePosition) < std::numeric_limits<glm::f64>::epsilon()) {
            return 0.0;
        }

        // Camera state
        glm::dvec3 cameraUpVector = camera->lookUpVectorWorldSpace();
        glm::dvec3 cameraViewVector = camera->viewDirectionWorldSpace();

        // Calculate the horizontal angle depending on the surround mode
        TelemetryModule* module = global::moduleEngine->module<TelemetryModule>();
        if (!module) {
            LERROR("Could not find the SonificationModule");
            return 0.0;
        }
        TelemetryModule::SurroundMode mode = module->surroundMode();
        if (mode == TelemetryModule::SurroundMode::Horizontal ||
            mode == TelemetryModule::SurroundMode::HorizontalWithElevation)
        {
            // Calculate angle from camera to the node in the camera plane
            // Pplane(v) is v projected down to the camera plane,
            // Pn(v) is v projected on the normal n of the plane ->
            // Pplane(v) = v - Pn(v)
            glm::dvec3 cameraToNode = nodePosition - camera->positionVec3();
            glm::dvec3 cameraToProjectedNode =
                cameraToNode - glm::proj(cameraToNode, cameraUpVector);

            return glm::orientedAngle(
                glm::normalize(cameraViewVector),
                glm::normalize(cameraToProjectedNode),
                glm::normalize(cameraUpVector)
            );
        }
        else if (mode == TelemetryModule::SurroundMode::Circular ||
            mode == TelemetryModule::SurroundMode::CircularWithElevation)
        {
            // Calculate angle from camera to the node in the camera view plane
            // Pvplane(v) is v projected onto the camera view plane,
            // Pview(v) is v projected on the camera view vectos, which is the normal
            // of the camera view plane ->
            // Pvplane(v) = v - Pview(v)
            glm::dvec3 cameraToNode = nodePosition - camera->positionVec3();
            glm::dvec3 cameraToProjectedNode =
                cameraToNode - glm::proj(cameraToNode, cameraViewVector);

            return glm::orientedAngle(
                glm::normalize(cameraUpVector),
                glm::normalize(cameraToProjectedNode),
                glm::normalize(-cameraViewVector)
            );
        }
        // None, i.e. Mono sound
        else {
            // 0.0 if the angle straigt forward
            return 0.0;
        }

        // TODO: Implement this
        return 0.0;
    }

    double calculateAngleFromAToB(const Camera* camera,
        std::variant<std::string,
        glm::dvec3> nodeIdOrPosA,
        std::variant<std::string,
        glm::dvec3> nodeIdOrPosB)
    {
        glm::dvec3 nodeAPos = getNodePosition(nodeIdOrPosA);
        glm::dvec3 nodeBPos = getNodePosition(nodeIdOrPosB);
        if (glm::length(nodeAPos) < std::numeric_limits<glm::f64>::epsilon() ||
            glm::length(nodeBPos) < std::numeric_limits<glm::f64>::epsilon())
        {
            return 0.0;
        }

        // Camera state
        glm::dvec3 cameraUpVector = camera->lookUpVectorWorldSpace();
        glm::dvec3 cameraViewVector = camera->viewDirectionWorldSpace();

        // Calculate the horizontal angle depending on the surround mode
        TelemetryModule* module = global::moduleEngine->module<TelemetryModule>();
        if (!module) {
            LERROR("Could not find the SonificationModule");
            return 0.0;
        }
        TelemetryModule::SurroundMode mode = module->surroundMode();
        if (mode == TelemetryModule::SurroundMode::Horizontal ||
            mode == TelemetryModule::SurroundMode::HorizontalWithElevation)
        {
            // Calculate vector from A to B in the camera plane
            // Pplane(v) is v projected down to the camera plane,
            // Pn(v) is v projected on the normal n of the plane ->
            // Pplane(v) = v - Pn(v)
            glm::dvec3 AToB = nodeBPos - nodeAPos;
            glm::dvec3 AToProjectedB = AToB - glm::proj(AToB, cameraUpVector);

            // Angle from A to B with respect to the camera
            // NOTE (malej 2023-FEB-06): This might not work if the camera is looking straight
            // down on node A
            return glm::orientedAngle(
                glm::normalize(cameraViewVector),
                glm::normalize(AToProjectedB),
                glm::normalize(cameraUpVector)
            );
        }
        else if (mode == TelemetryModule::SurroundMode::Circular ||
            mode == TelemetryModule::SurroundMode::CircularWithElevation)
        {
            // Calculate angle from node A to the node B in the camera view plane
            // Pvplane(v) is v projected onto the camera view plane,
            // Pview(v) is v projected on the camera view vectos, which is the normal
            // of the camera view plane ->
            // Pvplane(v) = v - Pview(v)
            glm::dvec3 AToB = nodeBPos - nodeAPos;
            glm::dvec3 AToProjectedB = AToB - glm::proj(AToB, cameraViewVector);

            // Angle from A to B with respect to the camera
            // NOTE (malej 2023-FEB-06): This might not work if the camera is looking straight
            // down on node A
            return glm::orientedAngle(
                glm::normalize(cameraUpVector),
                glm::normalize(AToProjectedB),
                glm::normalize(-cameraViewVector)
            );
        }
        // None, i.e. Mono sound
        else {
            // 0.0 if the angle straigt forward
            return 0.0;
        }
    }

    // Elevation angle
    double calculateElevationAngleTo(const Camera* camera,
        std::variant<std::string,
        glm::dvec3> nodeIdOrPos)
    {
        glm::dvec3 nodePosition = getNodePosition(nodeIdOrPos);
        if (glm::length(nodePosition) < std::numeric_limits<glm::f64>::epsilon()) {
            return 0.0;
        }

        // Camera state
        glm::dvec3 cameraUpVector = camera->lookUpVectorWorldSpace();
        glm::dvec3 cameraViewVector = camera->viewDirectionWorldSpace();
        glm::dvec3 cameraRightVector = glm::cross(cameraViewVector, cameraUpVector);

        TelemetryModule* module = global::moduleEngine->module<TelemetryModule>();
        if (!module) {
            LERROR("Could not find the SonificationModule");
            return 0.0;
        }
        TelemetryModule::SurroundMode mode = module->surroundMode();
        if (mode == TelemetryModule::SurroundMode::HorizontalWithElevation) {
            // Pvupplane(v) is v projected on the camrea view + up plane
            // Pright(v) is v projected onto the cameras right vector, which is the normal
            // of the camera view + up plane ->
            // Pvupplane(v) = v - Pright(v)
            glm::dvec3 cameraToNode = nodePosition - camera->positionVec3();
            glm::dvec3 cameraToProjectedNode =
                cameraToNode - glm::proj(cameraToNode, cameraRightVector);

            return glm::orientedAngle(
                glm::normalize(cameraViewVector),
                glm::normalize(cameraToProjectedNode),
                glm::normalize(cameraRightVector)
            );
        }
        else if (mode == TelemetryModule::SurroundMode::CircularWithElevation) {
            glm::dvec3 cameraToNode = nodePosition - camera->positionVec3();

            // Project the node onto the view-right plane
            // Pvplane(v) is v projected onto the camera view plane,
            // Pview(v) is v projected on the camera view vectos, which is the normal
            // of the camera view plane ->
            // Pvplane(v) = v - Pview(v)
            glm::dvec3 cameraToProjectedNode =
                cameraToNode - glm::proj(cameraToNode, cameraViewVector);

            double polarAngle = glm::orientedAngle(
                glm::normalize(cameraUpVector),
                glm::normalize(cameraToProjectedNode),
                glm::normalize(-cameraViewVector)
            );

            // First we counter-rotate the circular angle to make the cameraToNode vector be
            // inside the view-up plane
            glm::dvec3 rotatedVector =
                glm::rotate(cameraToNode, polarAngle, glm::normalize(cameraViewVector));

            // Then we calculate the elavation angle in the same way as the horizontal
            // surround mode
            return std::abs(glm::orientedAngle(
                glm::normalize(cameraViewVector),
                glm::normalize(rotatedVector),
                glm::normalize(cameraRightVector)
            ));
        }
        // None, i.e. Mono sound
        else {
            // 0.0 if the angle straigt forward
            return 0.0;
        }
    }

    double calculateElevationAngleFromAToB(const Camera* camera,
        std::variant<std::string,
        glm::dvec3> nodeIdOrPosA,
        std::variant<std::string,
        glm::dvec3> nodeIdOrPosB)
    {
        glm::dvec3 nodeAPos = getNodePosition(nodeIdOrPosA);
        glm::dvec3 nodeBPos = getNodePosition(nodeIdOrPosB);
        if (glm::length(nodeAPos) < std::numeric_limits<glm::f64>::epsilon() ||
            glm::length(nodeBPos) < std::numeric_limits<glm::f64>::epsilon())
        {
            return 0.0;
        }

        // Camera state
        glm::dvec3 cameraUpVector = camera->lookUpVectorWorldSpace();
        glm::dvec3 cameraViewVector = camera->viewDirectionWorldSpace();
        glm::dvec3 cameraRightVector = glm::cross(cameraViewVector, cameraUpVector);

        TelemetryModule* module = global::moduleEngine->module<TelemetryModule>();
        if (!module) {
            LERROR("Could not find the SonificationModule");
            return 0.0;
        }
        TelemetryModule::SurroundMode mode = module->surroundMode();
        if (mode == TelemetryModule::SurroundMode::HorizontalWithElevation) {
            // Pvupplane(v) is v projected on the camrea view + up plane
            // Pright(v) is v projected onto the cameras right vector, which is the normal
            // of the camera view + up plane ->
            // Pvupplane(v) = v - Pright(v)
            glm::dvec3 AToB = nodeBPos - nodeAPos;
            glm::dvec3 AToProjectedB = AToB - glm::proj(AToB, cameraRightVector);

            return glm::orientedAngle(
                glm::normalize(cameraViewVector),
                glm::normalize(AToProjectedB),
                glm::normalize(cameraRightVector)
            );
        }
        else if (mode == TelemetryModule::SurroundMode::CircularWithElevation) {
            glm::dvec3 AToB = nodeBPos - nodeAPos;

            // Project the node onto the view-right plane
            // Pvplane(v) is v projected onto the camera view plane,
            // Pview(v) is v projected on the camera view vectos, which is the normal
            // of the camera view plane ->
            // Pvplane(v) = v - Pview(v)
            glm::dvec3 AToProjectedB =
                AToB - glm::proj(AToB, cameraViewVector);

            double polarAngle = glm::orientedAngle(
                glm::normalize(cameraUpVector),
                glm::normalize(AToProjectedB),
                glm::normalize(-cameraViewVector)
            );

            // First we counter-rotate the circular angle to make the cameraToNode vector be
            // inside the view-up plane
            glm::dvec3 rotatedVector =
                glm::rotate(AToB, polarAngle, glm::normalize(cameraViewVector));

            // Then we calculate the elavation angle in the same way as the horizontal
            // surround mode
            return std::abs(glm::orientedAngle(
                glm::normalize(cameraViewVector),
                glm::normalize(rotatedVector),
                glm::normalize(cameraRightVector)
            ));
        }
        // None, i.e. Mono sound
        else {
            // 0.0 if the angle straigt forward
            return 0.0;
        }
    }

} // namespace openspace
