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

#include <modules/sonification/sonificationmodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <glm/gtx/projection.hpp>
#include <glm/gtx/vector_angle.hpp>

namespace {
    constexpr std::string_view _loggerCat = "SonificationBase";

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Is Enabled",
        "This setting determines whether this sonification will be enabled or not"
    };
} // namespace

namespace openspace {

SonificationBase::SonificationBase(properties::PropertyOwner::PropertyOwnerInfo info,
                                   const std::string& ip, int port)
    : properties::PropertyOwner(info)
    , _enabled(EnabledInfo, false)
{
    _identifier = info.identifier;
    _connection = new OscConnection(ip, port);

    addProperty(_enabled);
    _enabled.onChange([this]() {
        if (!_enabled) {
            // Disable sending of data
            stop();
        }
    });
}

SonificationBase::~SonificationBase() {
    delete _connection;
}

std::string SonificationBase::identifier() const {
    return _identifier;
}

bool SonificationBase::enabled() const {
    return _enabled;
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

// Horizontal angle
double SonificationBase::calculateAngleTo(const Camera* camera,
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
    SonificationModule* module = global::moduleEngine->module<SonificationModule>();
    if (!module) {
        LERROR("Could not find the SonificationModule");
        return 0.0;
    }
    SonificationModule::SurroundMode mode = module->surroundMode();
    if (mode == SonificationModule::SurroundMode::Horizontal ||
        mode == SonificationModule::SurroundMode::HorizontalWithElevation)
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
    else if (mode == SonificationModule::SurroundMode::Circular ||
             mode == SonificationModule::SurroundMode::CircularWithElevation)
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

double SonificationBase::calculateAngleFromAToB(const Camera* camera,
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
    SonificationModule* module = global::moduleEngine->module<SonificationModule>();
    if (!module) {
        LERROR("Could not find the SonificationModule");
        return 0.0;
    }
    SonificationModule::SurroundMode mode = module->surroundMode();
    if (mode == SonificationModule::SurroundMode::Horizontal ||
        mode == SonificationModule::SurroundMode::HorizontalWithElevation)
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
    else if (mode == SonificationModule::SurroundMode::Circular ||
             mode == SonificationModule::SurroundMode::CircularWithElevation)
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
double SonificationBase::calculateElevationAngleTo(const Camera* camera,
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

    SonificationModule* module = global::moduleEngine->module<SonificationModule>();
    if (!module) {
        LERROR("Could not find the SonificationModule");
        return 0.0;
    }
    SonificationModule::SurroundMode mode = module->surroundMode();
    if (mode == SonificationModule::SurroundMode::HorizontalWithElevation) {
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
    else if (mode == SonificationModule::SurroundMode::CircularWithElevation) {
        // TODO: Improve!

        // Calculate angle from camera to the node in the camera view plane
        // Pvplane(v) is v projected onto the camera view plane,
        // Pview(v) is v projected on the camera view vectos, which is the normal
        // of the camera view plane ->
        // Pvplane(v) = v - Pview(v)
        glm::dvec3 cameraToNode = nodePosition - camera->positionVec3();
        glm::dvec3 cameraToProjectedNode =
            cameraToNode - glm::proj(cameraToNode, cameraViewVector);

        return glm::length(cameraToProjectedNode);
    }
    // None, i.e. Mono sound
    else {
        // 0.0 if the angle straigt forward
        return 0.0;
    }
}

double SonificationBase::calculateElevationAngleFromAToB(const Camera* camera,
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

    SonificationModule* module = global::moduleEngine->module<SonificationModule>();
    if (!module) {
        LERROR("Could not find the SonificationModule");
        return 0.0;
    }
    SonificationModule::SurroundMode mode = module->surroundMode();
    if (mode == SonificationModule::SurroundMode::HorizontalWithElevation) {
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
    else if (mode == SonificationModule::SurroundMode::CircularWithElevation) {
        // Calculate angle from node A to the node B in the camera view plane
        // Pvplane(v) is v projected onto the camera view plane,
        // Pview(v) is v projected on the camera view vectos, which is the normal
        // of the camera view plane ->
        // Pvplane(v) = v - Pview(v)
        glm::dvec3 AToB = nodeBPos - nodeAPos;
        glm::dvec3 AToProjectedB = AToB - glm::proj(AToB, cameraViewVector);

        return glm::length(AToProjectedB);
    }
    // None, i.e. Mono sound
    else {
        // 0.0 if the angle straigt forward
        return 0.0;
    }
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
