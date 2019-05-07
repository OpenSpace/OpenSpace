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

#include <modules/roverterrainrenderer/model/singlemodelprovider.h>

#include <modules/globebrowsing/src/renderableglobe.h>
#include <modules/globebrowsing/src/basictypes.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/glm.h>

namespace {
 const std::string _loggerCat = "SingleModelProvider";
}

namespace openspace {
SingleModelProvider::SingleModelProvider(const ghoul::Dictionary& dictionary)
    : ModelProvider(dictionary)
{}

std::vector<std::shared_ptr<Subsite>> SingleModelProvider::calculate(
    const std::vector<std::vector<std::shared_ptr<Subsite>>> subsites,
    const RenderData& data,
    const SceneGraphNode* parent)
{
    std::vector<std::shared_ptr<Subsite>> mostModelsInsideRadius;
    std::vector<std::shared_ptr<Subsite>> subsitesInsideRadius;
    std::shared_ptr<Subsite> smallest = std::make_shared<Subsite>();

    globebrowsing::RenderableGlobe* rg = (globebrowsing::RenderableGlobe*)parent->renderable();

    glm::dvec3 center = parent->worldPosition();
    glm::dmat4 globeModelTransform = rg->modelTransform();
    glm::dmat4 globeModelInverseTransform = glm::inverse(globeModelTransform);
    glm::dvec3 cameraPos = data.camera.positionVec3();
    glm::dvec4 cameraPositionModelSpace = globeModelInverseTransform * glm::dvec4(cameraPos, 1.0);
    glm::dvec3 cameraPositionProjected = rg->ellipsoid().geodeticSurfaceProjection(cameraPositionModelSpace);
    double smallestDistance = 100.0;

    for (auto s : subsites) {
        for (auto s1 : s) {
            glm::dvec3 temp = rg->ellipsoid().cartesianPosition({ s1->geodetic , 0 });
            double distanceToCamera = glm::distance(cameraPositionProjected, temp);
            if (distanceToCamera < smallestDistance) {
                smallest = s1;
                smallestDistance = distanceToCamera;
            }
        }
    }
    if (smallest->site != "" && smallest->drive != "") {
        mostModelsInsideRadius.push_back(smallest);
    }
    return mostModelsInsideRadius;
}

void SingleModelProvider::initialize() {
    return;
}
} // namespace openspace
