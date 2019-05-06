/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2017                                                               *
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

#include <modules/roverterrainrenderer/renderable/lodmodelswitch.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>

#include <modules/globebrowsing/geometry/geodetic2.h>
#include <openspace/engine/globals.h>

#include <ghoul/logging/logmanager.h>

#include <glm/gtx/matrix_decompose.hpp>

namespace {
 const std::string _loggerCat = "LodModelSwitch";
}

namespace openspace {

LodModelSwitch::LodModelSwitch()
{}

void LodModelSwitch::initialize(RenderableGlobe * owner) {
    _owner = owner;

    //TODO: Rickard fix this!!!
    std::string planetName = "Mars";
    _parent = global::renderEngine.scene()->sceneGraphNode(planetName)->parent();
    _ellipsoidShrinkTerm = 10000.0;// _owner->interactionDepthBelowEllipsoid();
}

LodModelSwitch::Mode LodModelSwitch::getLevel(const RenderData& data) {
    glm::dvec3 center = _parent->worldPosition();
    glm::dmat4 globeModelTransform = _owner->modelTransform();
    glm::dmat4 globeModelInverseTransform = _owner->inverseModelTransform();
    glm::dvec3 cameraPos = data.camera.positionVec3();
    glm::dvec4 cameraPositionModelSpace = globeModelInverseTransform * glm::dvec4(cameraPos, 1.0);

    glm::dvec3 directionFromSurfaceToCameraModelSpace =
    _owner->ellipsoid().geodeticSurfaceNormal(
    _owner->ellipsoid().cartesianToGeodetic2(cameraPositionModelSpace));

    glm::dvec3 centerToEllipsoidSurface = glm::dmat3(globeModelTransform)  * (_owner->projectOnEllipsoid(cameraPositionModelSpace) -
    directionFromSurfaceToCameraModelSpace * _ellipsoidShrinkTerm);

    glm::dvec3 ellipsoidSurfaceToCamera = cameraPos - (center + centerToEllipsoidSurface);
    double distFromEllipsoidSurfaceToCamera = glm::length(ellipsoidSurfaceToCamera);

    double heightToSurface =
    _owner->getHeight(cameraPositionModelSpace) + _ellipsoidShrinkTerm;

    double distFromSurfaceToCamera = glm::abs(heightToSurface - distFromEllipsoidSurfaceToCamera);

    if (distFromSurfaceToCamera > 100 && distFromSurfaceToCamera < 800) {
        return Mode::Low;
    } else if (distFromSurfaceToCamera >= 800 && distFromSurfaceToCamera < 8000) {
        return Mode::High;
    } else if(distFromSurfaceToCamera <= 100) {
        return Mode::Close;
    }

    return Mode::Far;
}

} // namespace openspace
