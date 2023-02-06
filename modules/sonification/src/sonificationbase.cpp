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
                                             const std::string& identifier,
                                             DistanceUnit unit)
{
    if (identifier.empty()) {
        return 0.0;
    }

    // Find the node
    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        return 0.0;
    }
    glm::dvec3 nodePosition = node->worldPosition();

    // Calculate distance to the node from the camera
    glm::dvec3 cameraToNode = nodePosition - camera->positionVec3();
    double distance = glm::length(cameraToNode);

    // Convert from meters to desired unit
    return convertMeters(distance, unit);
}

double SonificationBase::calculateAngleTo(const Camera* camera,
                                          const std::string& identifier)
{
    if (identifier.empty()) {
        return 0.0;
    }

    // Find the node
    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        return 0.0;
    }
    glm::dvec3 nodePosition = node->worldPosition();

    // Calculate angle from camera to the node in the camera plane.
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
                                                const std::string& idA,
                                                const std::string& idB)
{
    if (idA.empty() || idB.empty()) {
        return 0.0;
    }

    // Find the nodes
    SceneGraphNode* nodeA = sceneGraphNode(idA);
    SceneGraphNode* nodeB = sceneGraphNode(idB);
    if (!nodeA || !nodeB) {
        return 0.0;
    }
    glm::dvec3 nodeAPos = nodeA->worldPosition();
    glm::dvec3 nodeBPos = nodeB->worldPosition();

    // Calculate vector from A to the B in the camera plane.
    // Pplane(v) is v projected down to the camera plane,
    // Pn(v) is v projected on the normal n of the plane ->
    // Pplane(v) = v - Pn(v)
    glm::dvec3 cameraUpVector = camera->lookUpVectorWorldSpace();
    glm::dvec3 AToB = nodeBPos - nodeAPos;
    glm::dvec3 AToProjectedB = AToB - glm::proj(AToB, cameraUpVector);

    // Angle from A to B with respect to the camera
    // NOTE (malej 2023-FEB-06): This might not work if the camera is looking straight
    // down on the planet
    return glm::orientedAngle(
        glm::normalize(camera->viewDirectionWorldSpace()),
        glm::normalize(AToProjectedB),
        glm::normalize(cameraUpVector)
    );
}

} // namespace openspace

