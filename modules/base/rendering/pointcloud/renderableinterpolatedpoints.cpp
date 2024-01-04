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

#include <modules/base/rendering/pointcloud/renderableinterpolatedpoints.h>

#include <openspace/documentation/documentation.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <optional>

namespace {
    constexpr std::string_view _loggerCat = "RenderableInterpolatedPoints";

    constexpr openspace::properties::Property::PropertyInfo InterpolationValueInfo = {
        "InterpolationValue",
        "Interpolation Value",
        "TODO",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo StepsInfo = {
        "NumberOfSteps",
        "Number of Steps",
        "TODO",
        openspace::properties::Property::Visibility::User
    };

    // A RenderableInterpolatedPoints ... TODO
    struct [[codegen::Dictionary(RenderableInterpolatedPoints)]] Parameters {
        // TODO
        int numberOfObjects [[codegen::greaterequal(1)]];
    };

#include "renderableinterpolatedpoints_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation RenderableInterpolatedPoints::Documentation() {
    return codegen::doc<Parameters>(
        "base_renderableinterpolatedpoints",
        RenderablePointCloud::Documentation()
    );
}

RenderableInterpolatedPoints::RenderableInterpolatedPoints(
    const ghoul::Dictionary& dictionary)
    : RenderablePointCloud(dictionary)
    , _interpolationValue(InterpolationValueInfo, 0.f, 0.f, 1.f)
    , _nSteps(StepsInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _nObjects = static_cast<unsigned int>(p.numberOfObjects);

    // At this point, the dataset has been loaded and the number of points computed. We
    // need to recompute them and compute how many steps the number of points
    // corresponded to

    if (_nDataPoints % _nObjects != 0) {
        LERROR(fmt::format(
            "Mismatch between provided number of data entries and the specified number "
            "of points. Expected the number of entries in the data file {} to be evenly"
            "divisible by the number of points", _dataFile
        ));
    }

    _nSteps = _nDataPoints / _nObjects;
    _interpolationValue.setMaxValue(static_cast<float>(_nSteps));

    _interpolationValue.onChange([this]() { _dataIsDirty = true; });

    // This is just for show in the UI
    _nDataPoints = _nObjects;

    addProperty(_interpolationValue);
    _nSteps.setReadOnly(true);
    addProperty(_nSteps);
}

std::vector<float> RenderableInterpolatedPoints::createDataSlice() {
    ZoneScoped;

    if (_dataset.entries.empty()) {
        return std::vector<float>();
    }

    std::vector<float> result;
    result.reserve(nAttributesPerPoint() * _nObjects);

    // Find the information we need for the interpolation and to identify the points
    float t0 = glm::floor(_interpolationValue);
    float t1 = glm::ceil(_interpolationValue);
    float t = t1 - t0;
    unsigned int firstStartIndex = static_cast<unsigned int>(t0) * _nDataPoints;
    unsigned int lastStartIndex = static_cast<unsigned int>(t1) * _nDataPoints;

    // What datavar is in use for the index color
    int colorParamIndex = currentColorParameterIndex();

    // What datavar is in use for the size scaling (if present)
    int sizeParamIndex = currentSizeParameterIndex();

    double maxRadius = 0.0;

    // TODO: verify that we have enough entries fopr this computation

    for (unsigned int i = 0; i < _nObjects; i++) {
        const dataloader::Dataset::Entry& e0 = _dataset.entries[firstStartIndex + i];
        const dataloader::Dataset::Entry& e1 = _dataset.entries[lastStartIndex + i];

        // TODO::
        // OBS!! Should we rather include two points and do the intrpolation in the shader??
        // Let's see what the performance becomes... Will have to do this update whenever we
        // pass the interpolation parameter anyways.. Can we buffer the points piece by piece
        // in any clever way?

        // Compute interpolated values
        glm::dvec3 interpolatedPosition = glm::dvec3(
            glm::mix(e0.position, e1.position, t)
        );

        const double unitMeter = toMeter(_unit);
        glm::dvec4 position = glm::dvec4(interpolatedPosition * unitMeter, 1.0);
        position = _transformationMatrix * position;

        const double r = glm::length(position);
        maxRadius = std::max(maxRadius, r);

        // Positions
        for (int j = 0; j < 4; ++j) {
            result.push_back(static_cast<float>(position[j]));
        }

        // Colors
        if (_hasColorMapFile) {
            float value0 = e0.data[colorParamIndex];
            float value1 = e1.data[colorParamIndex];
            result.push_back(glm::mix(value0, value1, t));
        }

        // Size data
        if (_hasDatavarSize) {
            // @TODO: Consider more detailed control over the scaling. Currently the value
            // is multiplied with the value as is. Should have similar mapping properties
            // as the color mapping
            float value0 = e0.data[sizeParamIndex];
            float value1 = e1.data[sizeParamIndex];
            result.push_back(glm::mix(value0, value1, t));
        }
    }
    setBoundingSphere(maxRadius);
    return result;
}

} // namespace openspace
