/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SPACECAST___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SPACECAST___H__

#include <modules/exoplanetsexperttool/castselection/castselection.h>

#include <modules/exoplanetsexperttool/castselection/lasso.h>

namespace openspace::exoplanets {

/**
 * SpaceCast selects the disjoint dense volume inside the lasso frustum whose projection
 * best matches the lasso, preferring closer volumes with a similar overlap.
 */
class SpaceCast : public CastSelection {
public:
    SpaceCast(const DensityVolume& volume, const CastInput& input);

    CastResult select(float exponent) const override;

private:
    Lasso _lasso;

    std::vector<glm::vec2> _nodeScreen;
    // Clip space w, i.e. the distance to the eye; <= 0 behind the camera
    std::vector<float> _nodeDepth;
    // Signed lasso distance divided by the lasso diameter
    std::vector<float> _nodeDelta;
    std::vector<uint8_t> _particleInside;

    // Approximate on-screen size of a grid cell is _cellFootprint / depth pixels
    float _cellFootprint = 0.f;

    float _densityStdDev = 1.f;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SPACECAST___H__
