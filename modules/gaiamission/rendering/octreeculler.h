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

#ifndef __OPENSPACE_MODULE_GAIAMISSION___OCTREECULLER___H__
#define __OPENSPACE_MODULE_GAIAMISSION___OCTREECULLER___H__

#include <modules/globebrowsing/geometry/aabb.h>
#include <vector>

namespace openspace {

/**
* Culls all octree nodes that are completely outside the view frustum.
*
* The frustum culling uses a 2D axis aligned bounding box for the OctreeNode in
* screen space. 
* 
* TODO (adaal): Move /geometry/* to libOpenSpace so as not to depend on globebrowsing.
*/

class OctreeCuller {
public:
 
    /**
     * \param viewFrustum is the view space in normalized device coordinates space.
     * Hence it is an axis aligned bounding box and not a real frustum.
     */
    OctreeCuller(globebrowsing::AABB3 viewFrustum);
    ~OctreeCuller();

    bool isVisible(std::vector<glm::dvec4> corners, const glm::mat4 mvp);
    float getNodeSizeInPixels();

private:
    const globebrowsing::AABB3 _viewFrustum;
    globebrowsing::AABB3 _nodeBounds;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GAIAMISSION___OCTREECULLER___H__
