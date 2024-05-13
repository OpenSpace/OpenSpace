/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/uintproperty.h>
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
    void initialize() override;
    void initializeShadersAndGlExtras() override;
    void deinitializeShaders() override;
    void setExtraUniforms() override;
    void preUpdate() override;

    int nAttributesPerPoint() const override;

    bool useSplineInterpolation() const;

    /**
     * Create the rendering data for the positions for the point with the given index
     * and append that to the result. Compared to the base class, this class may require
     * 2-4 positions, depending on if spline interpolation is used or not.
     *
     * The values are computed based on the current interpolation value.
     *
     * Also, compute the maxRadius to use for setting the bounding sphere.
     */
    void addPositionDataForPoint(unsigned int index, std::vector<float>& result,
        double& maxRadius) const override;

    /**
     * Create the rendering data for the color and size data for the point with the given
     * index and append that to the result. Compared to the base class, this class require
     * 2 values per data value, to use for interpolation.
     *
     * The values are computed based on the current interpolation value.
     */
    void addColorAndSizeDataForPoint(unsigned int index,
        std::vector<float>& result) const override;

    void addOrientationDataForPoint(unsigned int index,
        std::vector<float>& result) const override;

    void initializeBufferData();
    void updateBufferData() override;

private:
    bool isAtKnot() const;
    float computeCurrentLowerValue() const;
    float computeCurrentUpperValue() const;
    std::pair<size_t, size_t> interpolationIndices(unsigned int index) const;

    struct Interpolation : public properties::PropertyOwner {
        Interpolation();
        properties::FloatProperty value;
        properties::UIntProperty nSteps;

        properties::TriggerProperty goToNextStep;
        properties::TriggerProperty goToPrevStep;
        properties::TriggerProperty interpolateToNextStep;
        properties::TriggerProperty interpolateToPrevStep;
        properties::TriggerProperty interpolateToEnd;
        properties::TriggerProperty interpolateToStart;
        properties::FloatProperty speed;

        properties::BoolProperty useSpline;
    };
    Interpolation _interpolation;

    float _prevInterpolationValue = 0.f;
    bool _shouldReinitializeBufferdata = false;

    unsigned int _nObjectsInDataset = 0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLEINTERPOLATEDPOINTS___H__
