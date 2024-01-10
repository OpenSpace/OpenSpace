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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLEINTERPOLATEDPOINTS___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLEINTERPOLATEDPOINTS___H__

#include <modules/base/rendering/pointcloud/renderablepointcloud.h>

#include <openspace/properties/scalar/uintproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/triggerproperty.h>

namespace ghoul::opengl { class Texture; }

namespace openspace {

namespace documentation { struct Documentation; }

/**
 * A specialization of the RenderablePointCloud that supports interpolation beetween
 * different positions for the points.
 */
class RenderableInterpolatedPoints : public RenderablePointCloud {
public:
    explicit RenderableInterpolatedPoints(const ghoul::Dictionary& dictionary);
    ~RenderableInterpolatedPoints() override = default;

    static documentation::Documentation Documentation();

protected:
    /**
     * Create the data slice to use for rendering the points. Compared to the regular
     * point cloud, the data slice for an interpolated set of points will have to be
     * recreated when the interpolation value changes, and will only include a subset of
     * the points in the entire dataset
     *
     * \return The dataslice to use for rendering the points
     */
    std::vector<float> createDataSlice() override;

private:
    struct Interpolation : properties::PropertyOwner {
        Interpolation();
        properties::FloatProperty value;
        properties::UIntProperty nSteps;

        //properties::TriggerProperty goToNextStep;
        //properties::TriggerProperty interpolateToNextStep;
        //properties::FloatProperty timeForNextStepInterpolation;
    };
    Interpolation _interpolation;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLEINTERPOLATEDPOINTS___H__
