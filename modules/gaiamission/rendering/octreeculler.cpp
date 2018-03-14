/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/gaiamission/rendering/octreeculler.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/glm.h>

namespace {
    constexpr const char* _loggerCat = "OctreeCuller";
} // namespace

namespace openspace {


OctreeCuller::OctreeCuller(globebrowsing::AABB3 viewFrustum)
    : _viewFrustum(std::move(viewFrustum))
    , _nodeBounds(globebrowsing::AABB3())
{   }

OctreeCuller::~OctreeCuller() {   }

// Returns true if any part of the node is visible in the current view. 
bool OctreeCuller::isVisible(std::vector<glm::dvec4> corners, const glm::mat4 mvp) {
    
    // Create a bounding box in screen space from node boundaries.
    _nodeBounds = globebrowsing::AABB3();

    for (size_t i = 0; i < 8; ++i) {
        glm::dvec4 cornerClippingSpace = mvp * corners[i];

        glm::dvec3 ndc = glm::dvec3(
            (1.f / glm::abs(cornerClippingSpace.w)) * cornerClippingSpace
        );
        _nodeBounds.expand(ndc);
    }

    return _viewFrustum.intersects(_nodeBounds);
}

 //Returns the size [in pixels] of the node in screen space. 
glm::vec2 OctreeCuller::getNodeSizeInPixels(const glm::vec2 screenSize) {
    // Use the same AABB as before {created in isVisible()}. 
    // Screen space is mapped to [-1, 1] so divide by 2 and multiply with screen size.
    glm::vec3 size = _nodeBounds.size() / 2.f;
    return glm::vec2(size.x * screenSize.x, size.y * screenSize.y);
}

} // namespace openspace
