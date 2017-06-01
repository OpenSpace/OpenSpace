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

#include <modules/globebrowsing/chunk/culling/frustumculler.h>

#include <modules/globebrowsing/chunk/chunk.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <openspace/util/updatestructures.h>

namespace openspace {
namespace globebrowsing {
namespace culling {

FrustumCuller::FrustumCuller(AABB3 viewFrustum)
    : _viewFrustum(std::move(viewFrustum))
{}

bool FrustumCuller::isCullable(const Chunk& chunk, const RenderData& data) {
    // Calculate the MVP matrix
    glm::dmat4 modelTransform = chunk.owner().modelTransform();
    glm::dmat4 viewTransform = glm::dmat4(data.camera.combinedViewMatrix());
    glm::dmat4 modelViewProjectionTransform = glm::dmat4(data.camera.projectionMatrix())
        * viewTransform * modelTransform;

    const std::vector<glm::dvec4>& corners = chunk.getBoundingPolyhedronCorners();
        
    // Create a bounding box that fits the patch corners
    AABB3 bounds; // in screen space
    std::vector<glm::vec4> clippingSpaceCorners(8);
    for (size_t i = 0; i < 8; ++i) {
        glm::dvec4 cornerClippingSpace = modelViewProjectionTransform * corners[i];
        clippingSpaceCorners[i] = cornerClippingSpace;

        glm::dvec3 ndc = (1.0f / glm::abs(cornerClippingSpace.w)) * cornerClippingSpace;
        bounds.expand(ndc);
    }
        
    return !(_viewFrustum.intersects(bounds));
}

} // namespace culling
} // namespace globebrowsing
} // namespace openspace
