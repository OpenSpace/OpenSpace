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

#include <modules/sonification/include/sonificationbase.h>

#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <glm/gtx/projection.hpp>
#include <glm/gtx/vector_angle.hpp>

namespace openspace {

SonificationBase::SonificationBase(properties::PropertyOwner::PropertyOwnerInfo info,
                                   const std::string& ip, int port)
    : properties::PropertyOwner(info)
{
    _connection = new OscConnection(ip, port);
}

SonificationBase::~SonificationBase() {
    delete _connection;
}

double SonificationBase::calculateDistanceTo(const Camera* camera,
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

double SonificationBase::calculateAngleTo(const Camera* camera,
                                        std::variant<std::string, glm::dvec3> nodeIdOrPos)
{
    glm::dvec3 nodePosition = getNodePosition(nodeIdOrPos);
    if (glm::length(nodePosition) < std::numeric_limits<glm::f64>::epsilon()) {
        return 0.0;
    }

    // Calculate angle from camera to the node in the camera plane
    // Pplane(v) is v projected down to the camera plane,
    // Pn(v) is v projected on the normal n of the plane ->
    // Pplane(v) = v - Pn(v)
    glm::dvec3 cameraUpVector = camera->lookUpVectorWorldSpace();
    glm::dvec3 cameraToNode = nodePosition - camera->positionVec3();
    glm::dvec3 cameraToProjectedNode =
        cameraToNode - glm::proj(cameraToNode, cameraUpVector);

    return glm::orientedAngle(
        glm::normalize(camera->viewDirectionWorldSpace()),
        glm::normalize(cameraToProjectedNode),
        glm::normalize(cameraUpVector)
    );
}

double SonificationBase::calculateAngleFromAToB(const Camera* camera,
                                       std::variant<std::string, glm::dvec3> nodeIdOrPosA,
                                       std::variant<std::string, glm::dvec3> nodeIdOrPosB)
{
    glm::dvec3 nodeAPos = getNodePosition(nodeIdOrPosA);
    glm::dvec3 nodeBPos = getNodePosition(nodeIdOrPosB);
    if (glm::length(nodeAPos) < std::numeric_limits<glm::f64>::epsilon() ||
        glm::length(nodeBPos) < std::numeric_limits<glm::f64>::epsilon())
    {
        return 0.0;
    }

    // Calculate vector from A to B in the camera plane
    // Pplane(v) is v projected down to the camera plane,
    // Pn(v) is v projected on the normal n of the plane ->
    // Pplane(v) = v - Pn(v)
    glm::dvec3 cameraUpVector = camera->lookUpVectorWorldSpace();
    glm::dvec3 AToB = nodeBPos - nodeAPos;
    glm::dvec3 AToProjectedB = AToB - glm::proj(AToB, cameraUpVector);

    // Angle from A to B with respect to the camera
    // NOTE (malej 2023-FEB-06): This might not work if the camera is looking straight
    // down on node A
    return glm::orientedAngle(
        glm::normalize(camera->viewDirectionWorldSpace()),
        glm::normalize(AToProjectedB),
        glm::normalize(cameraUpVector)
    );
}

glm::dvec3 SonificationBase::getNodePosition(std::variant<std::string,
                                             glm::dvec3> nodeIdOrPos)
{
    if (std::holds_alternative<std::string>(nodeIdOrPos)) {
        std::string identifier = std::get<std::string>(nodeIdOrPos);

        if (identifier.empty()) {
            return glm::dvec3(0.0);
        }

        // Find the node
        SceneGraphNode* node = sceneGraphNode(identifier);
        if (!node) {
            return glm::dvec3(0.0);
        }

        return node->worldPosition();
    }
    else {
        return std::get<glm::dvec3>(nodeIdOrPos);
    }
}

} // namespace openspace
