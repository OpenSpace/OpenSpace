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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___HORIZONCULLER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___HORIZONCULLER___H__

#include <modules/globebrowsing/chunk/culling/chunkculler.h>

#include <ghoul/glm.h>

namespace openspace {
namespace globebrowsing {
namespace culling {

/**
 * In this implementation of the horizon culling, the closer the ellipsoid is to a
 * sphere, the better this will make the culling. Using the minimum radius to
 * be safe. This means that if the ellipsoid has high difference between radii,
 * splitting might accur even though it may not be needed.
 */
class HorizonCuller : public ChunkCuller {
public:
    virtual ~HorizonCuller() override = default;
    bool isCullable(const Chunk& chunk, const RenderData& renderData) override;

private:
    bool isCullable(const glm::dvec3& cameraPosition, const glm::dvec3& globePosition,
        const glm::dvec3& objectPosition, double objectBoundingSphereRadius,
        double minimumGlobeRadius);
};

} // namespace culling
} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___HORIZONCULLER___H__
