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

#include <modules/telemetry/include/util.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <glm/gtx/projection.hpp>
#include <glm/gtx/vector_angle.hpp>

namespace {
    /**
     * Get the position of the node with the given identifier.
     *
     * \param nodeIdentifier The identifier of the node to get the position for
     *
     * \return The position of the node with the given identifier
     */
    glm::dvec3 nodePosition(const std::string& nodeIdentifier) {
        if (nodeIdentifier.empty()) {
            return glm::dvec3(0.0);
        }

        // Find the node
        openspace::SceneGraphNode* node = openspace::sceneGraphNode(nodeIdentifier);
        return node ? node->worldPosition() : glm::dvec3(0.0);
    }
} // namespace

namespace openspace {

// Distances
double calculateDistanceTo(const Camera* camera, const std::string& nodeIdentifier,
                           DistanceUnit unit)
{
    glm::dvec3 nodePos = nodePosition(nodeIdentifier);
    return calculateDistanceTo(camera, nodePos, unit);
}

double calculateDistanceTo(const Camera* camera, glm::dvec3 nodePosition,
                           DistanceUnit unit)
{
    if (glm::length(nodePosition) < std::numeric_limits<double>::epsilon()) {
        return 0.0;
    }

    // Calculate distance to the node from the camera
    double distance = glm::distance(nodePosition, camera->positionVec3());

    // Convert from meters to desired unit
    return convertMeters(distance, unit);
}

// Horizontal angles
double calculateAngleTo(const Camera* camera, const std::string& nodeIdentifier,
                        TelemetryModule::AngleCalculationMode angleCalculationMode)
{
    glm::dvec3 nodePos = nodePosition(nodeIdentifier);
    return calculateAngleTo(camera, nodePos, angleCalculationMode);
}

double calculateAngleTo(const Camera* camera, glm::dvec3 nodePosition,
                        TelemetryModule::AngleCalculationMode angleCalculationMode)
{
    if (glm::length(nodePosition) < std::numeric_limits<double>::epsilon()) {
        return 0.0;
    }

    // Camera state
    glm::dvec3 cameraUpVector = camera->lookUpVectorWorldSpace();
    glm::dvec3 cameraViewVector = camera->viewDirectionWorldSpace();

    // Get the vector from the camera to the node
    glm::dvec3 cameraToNode = nodePosition - camera->positionVec3();

    // Calculate the horizontal angle depending on the surround mode
    if (angleCalculationMode == TelemetryModule::AngleCalculationMode::Horizontal) {
        // Calculate the angle from the camera, to the node, within the camera plane. The
        // camera plane is the plane of the camera view direction + camera left direction
        // (i.e. the negative camera right direction), with the camera up direction as
        // the normal.
        //
        // Pplane(v) is v projected to the camera plane,
        // Pup(v) is v projected on camera up direction (which is the normal of the camera
        // plane). This gives the formula:
        // Pplane(v) = v - Pup(v)
        glm::dvec3 cameraToNodeProjected =
            cameraToNode - glm::proj(cameraToNode, cameraUpVector);

        // Get the angle between the camera view direction, and the vector from the camera
        // to the node projected to the camera plane, with the camera up vector as
        // reference axis (i.e. the normal of the camera plane). When the node is located
        // towards the left relative the center of the screen, then the angle will be a
        // positive value. If instead, the node is located towards the right, the angle
        // will become negative.
        return glm::orientedAngle(
            glm::normalize(cameraViewVector),
            glm::normalize(cameraToNodeProjected),
            glm::normalize(cameraUpVector)
        );
    }
    else if (angleCalculationMode == TelemetryModule::AngleCalculationMode::Circular) {
        // Calculate the angle from the camera, to the node, within the camera view plane.
        // The camera view plane is the plane of the camera up direction + camera left
        // direction (i.e. the negative camera right direction), with the negative camera
        // view direction as the normal.
        //
        // Pvplane(v) is v projected onto the camera view plane,
        // Pview(v) is v projected on the camera view direction (which is the negative
        // normal of the camera view plane). This gives the formula:
        // Pvplane(v) = v - Pview(v)
        glm::dvec3 cameraToNodeProjected =
            cameraToNode - glm::proj(cameraToNode, cameraViewVector);

        // Get the angle between the camera up direction, and the vector from the camera
        // to the node projected to the camera view plane, with the negative camera view
        // direction as reference axis (i.e. the normal of the camera view plane). When
        // the node is located towards the left relative the center of the screen, then
        // the angle will be a positive value. If instead, the node is located towards the
        // right, the angle will become negative.
        return glm::orientedAngle(
            glm::normalize(cameraUpVector),
            glm::normalize(cameraToNodeProjected),
            glm::normalize(-cameraViewVector)
        );
    }

    // 0.0 is the angle straight forward, in the camera view direction
    return 0.0;
}

double calculateAngleFromAToB(const Camera* camera,
                              const std::string& nodeIdentifierA,
                              const std::string& nodeIdentifierB,
                              TelemetryModule::AngleCalculationMode angleCalculationMode)
{
    glm::dvec3 nodeAPosition = nodePosition(nodeIdentifierA);
    glm::dvec3 nodeBPosition = nodePosition(nodeIdentifierB);

    return calculateAngleFromAToB(
        camera,
        nodeAPosition,
        nodeBPosition,
        angleCalculationMode
    );
}

double calculateAngleFromAToB(const Camera* camera, glm::dvec3 nodePositionA,
                              glm::dvec3 nodePositionB,
                              TelemetryModule::AngleCalculationMode angleCalculationMode)
{
    if (glm::length(nodePositionA) < std::numeric_limits<double>::epsilon() ||
        glm::length(nodePositionB) < std::numeric_limits<double>::epsilon())
    {
        return 0.0;
    }

    // Camera state
    glm::dvec3 cameraUpVector = camera->lookUpVectorWorldSpace();
    glm::dvec3 cameraViewVector = camera->viewDirectionWorldSpace();

    // Get the vector from the first node (A) to the second node (B)
    glm::dvec3 AToB = nodePositionB - nodePositionA;

    // Calculate the horizontal angle depending on the surround mode
    if (angleCalculationMode == TelemetryModule::AngleCalculationMode::Horizontal) {
        // Calculate the angle from node A, to node B, within the camera plane. The camera
        // plane is the plane of the camera view direction + camera left direction
        // (i.e. the negative camera right direction), with the camera up direction as
        // the normal.
        //
        // Pplane(v) is v projected to the camera plane,
        // Pup(v) is v projected on camera up direction (which is the normal of the camera
        // plane). This gives the formula:
        // Pplane(v) = v - Pup(v)
        glm::dvec3 AToBProjected = AToB - glm::proj(AToB, cameraUpVector);

        // Get the angle between the camera view direction, and the vector from node A to
        // node B projected to the camera plane, with the camera up vector as reference
        // axis (i.e. the normal of the camera plane). When the vector from A to B, points
        // to the left relative the camera view direction, then the angle will be a
        // positive value. If instead, the vector points towards the right, the angle will
        // become negative.
        // @NOTE (malej 2023-02-06): This might not work if the camera is looking straight
        // up/down on node A.
        return glm::orientedAngle(
            glm::normalize(cameraViewVector),
            glm::normalize(AToBProjected),
            glm::normalize(cameraUpVector)
        );
    }
    else if (angleCalculationMode == TelemetryModule::AngleCalculationMode::Circular) {
        // Calculate the angle from node A to the node B within the camera view plane. The
        // camera view plane is the plane of the camera up direction + camera left
        // direction (i.e. the negative camera right direction), with the negative camera
        // view direction as the normal.
        //
        // Pvplane(v) is v projected onto the camera view plane,
        // Pview(v) is v projected on the camera view direction (which is the negative
        // normal of the camera view plane). This gives the formula:
        // Pvplane(v) = v - Pview(v)
        glm::dvec3 AToBProjected = AToB - glm::proj(AToB, cameraViewVector);

        // Get the angle between the camera up direction, and the vector from node A to
        // node B projected to the camera view plane, with the negative camera view vector
        // as reference axis (i.e. the normal of the camera view plane). When the vector
        // from A to B, points to the left relative the camera up direction, then the
        // angle will be a positive value. If instead, the vector points towards the
        // right, the angle will become negative.
        // @NOTE (malej 2023-02-06): This might not work if the camera is looking straight
        // up/down on node A.
        return glm::orientedAngle(
            glm::normalize(cameraUpVector),
            glm::normalize(AToBProjected),
            glm::normalize(-cameraViewVector)
        );
    }

    // 0.0 is the angle straight forward, in the camera view direction
    return 0.0;
}

// Elevation angles
double calculateElevationAngleTo(const Camera* camera,
                                 const std::string& nodeIdentifier,
                               TelemetryModule::AngleCalculationMode angleCalculationMode)
{
    glm::dvec3 nodePos = nodePosition(nodeIdentifier);
    return calculateElevationAngleTo(camera, nodePos, angleCalculationMode);
}

double calculateElevationAngleTo(const Camera* camera, glm::dvec3 nodePosition,
                               TelemetryModule::AngleCalculationMode angleCalculationMode)
{
    if (glm::length(nodePosition) < std::numeric_limits<double>::epsilon()) {
        return 0.0;
    }

    // Camera state
    glm::dvec3 cameraUpVector = camera->lookUpVectorWorldSpace();
    glm::dvec3 cameraViewVector = camera->viewDirectionWorldSpace();
    glm::dvec3 cameraRightVector = glm::cross(cameraViewVector, cameraUpVector);

    // Get the vector from the camera to the node
    glm::dvec3 cameraToNode = nodePosition - camera->positionVec3();

    // Calculate the elevation angle depending on the surround mode
    if (angleCalculationMode == TelemetryModule::AngleCalculationMode::Horizontal) {
        // Calculate the elevation angle from the camera, to the node, within the camera
        // view + up plane. The camera view + up plane is the plane of the camera view
        // direction + camera up direction, with the camera right direction as the normal.
        //
        // Pvupplane(v) is v projected on the camrea view + up plane
        // Pright(v) is v projected onto the camera right vector (which is the normal of
        // the camera view + up plane). This gives the formula:
        // Pvupplane(v) = v - Pright(v)
        glm::dvec3 cameraToNodeProjected =
            cameraToNode - glm::proj(cameraToNode, cameraRightVector);

        // Get the angle between the camera view direction, and the vector from the camera
        // to the node projected to the camera view + up plane, with the camera right
        // vector as reference axis (i.e. the normal of the camera view + up plane). When
        // the node is located above the center of the screen, then the angle will be a
        // positive value. If instead, the node is located below, the angle will become
        // negative.
        return glm::orientedAngle(
            glm::normalize(cameraViewVector),
            glm::normalize(cameraToNodeProjected),
            glm::normalize(cameraRightVector)
        );
    }
    else if (angleCalculationMode == TelemetryModule::AngleCalculationMode::Circular) {
        // Calculate the elevation angle from the camera, to the node, within the camera
        // view + up plane. The camera view + up plane is the plane of the camera view
        // direction + camera up direction, with the camera right direction as the normal.
        // In case of the circular surround mode, this is done in several steps.

        // In the first step, the vector from the camera to the node is projected to the
        // camera view plane. The camera view plane is the plane of the camera up
        // direction + camera left direction (i.e. the negative camera right direction),
        // with the negative camera view direction as the normal.
        //
        // Pvplane(v) is v projected onto the camera view plane,
        // Pview(v) is v projected on the camera view direction (which is the normal of
        // the camera view plane). This gives the formula:
        // Pvplane(v) = v - Pview(v)
        glm::dvec3 cameraToNodeProjected =
            cameraToNode - glm::proj(cameraToNode, cameraViewVector);

        // In the next step, get the angle between the camera up direction, and the vector
        // from the camera to the node projected to the camera view plane, with the
        // negative camera view direction as reference axis (i.e. the normal of the camera
        // view plane). When the node is located towards the left relative the center of
        // the screen, then the angle will be a positive value. If instead, the node is
        // located towards the right, the angle will become negative.
        double rotationAngle = glm::orientedAngle(
            glm::normalize(cameraUpVector),
            glm::normalize(cameraToNodeProjected),
            glm::normalize(-cameraViewVector)
        );

        // Then we counter-rotate with the angle from the previous step, so the projected
        // vector to the node (from the first step) is inside the camera view + up plane.
        glm::dvec3 rotatedVector = glm::rotate(
            cameraToNode,
            rotationAngle,
            glm::normalize(cameraViewVector)
        );

        // Lastly, we calculate the elavation angle in the same way as in the horizontal
        // with elevation surround mode above.
        return std::abs(glm::orientedAngle(
            glm::normalize(cameraViewVector),
            glm::normalize(rotatedVector),
            glm::normalize(cameraRightVector)
        ));
    }

    // 0.0 is the angle straight forward, in the camera view direction
    return 0.0;
}

double calculateElevationAngleFromAToB(const Camera* camera,
                                       const std::string& nodeIdentifierA,
                                       const std::string& nodeIdentifierB,
                               TelemetryModule::AngleCalculationMode angleCalculationMode)
{
    glm::dvec3 nodeAPosition = nodePosition(nodeIdentifierA);
    glm::dvec3 nodeBPosition = nodePosition(nodeIdentifierB);

    return calculateElevationAngleFromAToB(
        camera,
        nodeAPosition,
        nodeBPosition,
        angleCalculationMode
    );
}

double calculateElevationAngleFromAToB(const Camera* camera, glm::dvec3 nodePositionA,
                                       glm::dvec3 nodePositionB,
                               TelemetryModule::AngleCalculationMode angleCalculationMode)
{
    if (glm::length(nodePositionA) < std::numeric_limits<double>::epsilon() ||
        glm::length(nodePositionB) < std::numeric_limits<double>::epsilon())
    {
        return 0.0;
    }

    // Camera state
    glm::dvec3 cameraUpVector = camera->lookUpVectorWorldSpace();
    glm::dvec3 cameraViewVector = camera->viewDirectionWorldSpace();
    glm::dvec3 cameraRightVector = glm::cross(cameraViewVector, cameraUpVector);

    // Get the vector from the first node (A) to the second node (B)
    glm::dvec3 AToB = nodePositionB - nodePositionA;

    // Calculate the elevation angle depending on the surround mode
    if (angleCalculationMode == TelemetryModule::AngleCalculationMode::Horizontal) {
        // Calculate the elevation angle from node A, to node B, within the camera view +
        // up plane. The camera view + up plane is the plane of the camera view direction
        // + camera up direction, with the camera right direction as the normal.
        //
        // Pvupplane(v) is v projected on the camrea view + up plane,
        // Pright(v) is v projected onto the camera right vector (which is the normal of
        // the camera view + up plane). This gives the formula:
        // Pvupplane(v) = v - Pright(v)
        glm::dvec3 AToProjectedB = AToB - glm::proj(AToB, cameraRightVector);


        // Get the angle between the camera view direction, and the vector from the camera
        // to the node projected to the camera view + up plane, with the camera  right
        // vector as reference axis (i.e. the normal of the camera view + up plane). When
        // the node is located above the center of the screen, then the angle will be a
        // positive value. If instead, the node is located below, the angle will become
        // negative.

        // Get the angle between the camera view direction, and the vector from node A to
        // node B projected to the camera view + up plane, with the camera right vector as
        // reference axis (i.e. the normal of the camera view + up plane). When the vector
        // from A to B, points upwards relative to the camera view direction, then the
        // angle will be a positive value. If instead, the vector points downwards, the
        // angle will become negative.
        // @NOTE (malej 2023-02-06): This might not work if the camera is looking straight
        // up/down on node A.
        return glm::orientedAngle(
            glm::normalize(cameraViewVector),
            glm::normalize(AToProjectedB),
            glm::normalize(cameraRightVector)
        );
    }
    else if (angleCalculationMode == TelemetryModule::AngleCalculationMode::Circular) {
        // Calculate the elevation angle from node A, to node B, within the camera view +
        // up plane. The camera view + up plane is the plane of the camera view direction
        // + camera up direction, with the camera right direction as the normal. In case
        // of the circular surround mode, this is done in several steps.

        // In the first step, the vector from the camera to the node is projected to the
        // camera view plane. The camera view plane is the plane of the camera up
        // direction + camera left direction (i.e. the negative camera right direction),
        // with the negative camera view direction as the normal.
        //
        // Pvplane(v) is v projected onto the camera view plane,
        // Pview(v) is v projected on the camera view direction (which is the normal of
        // the camera view plane). This gives the formula:
        // Pvplane(v) = v - Pview(v)
        glm::dvec3 AToProjectedB = AToB - glm::proj(AToB, cameraViewVector);

        // Then we counter-rotate with the angle from the previous step, so the projected
        // vector to the node (from the first step) is inside the camera view + up plane.
        double rotationAngle = glm::orientedAngle(
            glm::normalize(cameraUpVector),
            glm::normalize(AToProjectedB),
            glm::normalize(-cameraViewVector)
        );

        // First we counter-rotate the circular angle to make the cameraToNode vector be
        // inside the view-up plane
        glm::dvec3 rotatedVector =
            glm::rotate(AToB, rotationAngle, glm::normalize(cameraViewVector));

        // Lastly, we calculate the elavation angle in the same way as in the horizontal
        // with elevation surround mode above.
        return std::abs(glm::orientedAngle(
            glm::normalize(cameraViewVector),
            glm::normalize(rotatedVector),
            glm::normalize(cameraRightVector)
        ));
    }

    // 0.0 is the angle straight forward, in the camera view direction
    return 0.0;
}

} // namespace openspace
