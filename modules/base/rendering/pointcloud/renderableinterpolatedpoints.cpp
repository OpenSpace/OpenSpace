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
#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/interpolator.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <optional>

namespace {
    constexpr std::string_view _loggerCat = "RenderableInterpolatedPoints";

    constexpr openspace::properties::Property::PropertyInfo InterpolationValueInfo = {
        "Value",
        "Value",
        "The value to use for interpolation. The max value is set from the number of "
        "interpolation steps, so a step of one corresponds will correspond to doing one "
        "step in the dataset.", // TODO: imrove this description..
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo StepsInfo = {
        "NumberOfSteps",
        "Number of Steps",
        "The number of steps available in the dataset, including the initial positions.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo JumpToNextInfo = {
        "JumpToNext",
        "Jump to Next",
        "Immediately set the interpolation value to correspond to the next set of point "
        "positions compared to the current.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo JumpToPrevInfo = {
        "JumpToPrevious",
        "Jump to Previous",
        "Immediately set the interpolation value to correspond to the previous set of "
        "point positions compared to the current.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo InterpolateToNextInfo = {
        "InterpolateToNext",
        "Interpolate to Next",
        "Trigger an interpolation to the next set of point positions. The duration of "
        "the interpolation is set based on the Interpolaton Speed property.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo InterpolateToPrevInfo = {
        "InterpolateToPrevious",
        "Interpolate to Previous",
        "Trigger an interpolation to the previous set of point positions. The duration "
        "of the interpolation is set based on the Interpolaton Speed property.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo InterpolateToEndInfo = {
        "InterpolateToEnd",
        "Interpolate to End",
        "Trigger an interpolation all the way to the final set of positions. The "
        "duration of the interpolation is set based on the Interpolaton Speed property.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo InterpolateToStartInfo = {
        "InterpolateToStart",
        "Interpolate to Start",
        "Trigger an inverted interpolation to the initial set of positions. The duration "
        "of the interpolation is set based on the Interpolaton Speed property.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo InterpolationSpeedInfo = {
        "Speed",
        "Interpolation Speed",
        "Affects how long the interpolation takes when triggered using one of the "
        "trigger properties. A value of 1 means that a step takes 1 second.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo UseSplineInfo = {
        "UseSplineInterpolation",
        "Use Spline Interpolation",
        "If true, the points will be interpolated using a Catmull-Rom spline instead of "
        "linearly. This leads to a smoother transition at the breakpoints, i.e. between "
        "each step.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    // RenderableInterpolatedPoints is a version of the RenderablePointCloud class, where
    // the dataset may contain multiple time steps that can be interpolated between. It
    // supports interpolation of both of positions and data values used for color mapping
    // or size.
    //
    // The dataset should be structured in a way so that the first N rows correspond to
    // the first set of positions for the objects, the next N rows to the second set of
    // positions, and so on. The number of objects in the dataset must be specified in the
    // asset.
    struct [[codegen::Dictionary(RenderableInterpolatedPoints)]] Parameters {
        // The number of objects to read from the dataset. Every N:th datapoint will
        // be interpreted as the same point, but at a different step in the interpolation
        int numberOfObjects [[codegen::greaterequal(1)]];

        struct Interpolation {
            // [[codegen::verbatim(InterpolationValueInfo.description)]]
            std::optional<double> value;

            // [[codegen::verbatim(InterpolationSpeedInfo.description)]]
            std::optional<double> speed;

            // [[codegen::verbatim(UseSplineInfo.description)]]
            std::optional<bool> useSplineInterpolation;
        };
        // Initial settings for the interpolation
        std::optional<Interpolation> interpolation;
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

RenderableInterpolatedPoints::Interpolation::Interpolation()
    : properties::PropertyOwner({ "Interpolation", "Interpolation", "" })
    , value(InterpolationValueInfo, 0.f, 0.f, 1.f)
    , nSteps(StepsInfo)
    , goToNextStep(JumpToNextInfo)
    , goToPrevStep(JumpToPrevInfo)
    , interpolateToNextStep(InterpolateToNextInfo)
    , interpolateToPrevStep(InterpolateToPrevInfo)
    , interpolateToEnd(InterpolateToEndInfo)
    , interpolateToStart(InterpolateToStartInfo)
    , speed(InterpolationSpeedInfo, 1.f, 0.01f, 10.f)
    , useSpline(UseSplineInfo, false)
{
    addProperty(value);

    auto triggerInterpolation = [](std::string_view identifier, float v, float d) {
        std::string script = fmt::format(
            "openspace.setPropertyValueSingle(\"{}\", {}, {})",
            identifier, v, d
        );
        // No syncing, as this was triggered from a property change (which happened
        // based on an already synced script)
        global::scriptEngine->queueScript(
            script,
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    };

    interpolateToEnd.onChange([triggerInterpolation, this]() {
        float remaining = value.maxValue() - value;
        float duration = remaining / speed;
        triggerInterpolation(
            value.fullyQualifiedIdentifier(),
            value.maxValue(),
            duration
        );
    });

    interpolateToStart.onChange([triggerInterpolation, this]() {
        float duration = value / speed;
        triggerInterpolation(value.fullyQualifiedIdentifier(), 0.f, duration);
    });

    interpolateToNextStep.onChange([triggerInterpolation, this]() {
        float prevValue = glm::floor(value);
        float newValue = glm::min(prevValue + 1.f, value.maxValue());
        float duration = 1.f / speed;
        triggerInterpolation(value.fullyQualifiedIdentifier(), newValue, duration);
    });

    interpolateToPrevStep.onChange([triggerInterpolation, this]() {
        float prevValue = glm::ceil(value);
        float newValue = glm::max(prevValue - 1.f, value.minValue());
        float duration = 1.f / speed;
        triggerInterpolation(value.fullyQualifiedIdentifier(), newValue, duration);
    });

    addProperty(interpolateToEnd);
    addProperty(interpolateToStart);
    addProperty(interpolateToNextStep);
    addProperty(interpolateToPrevStep);
    addProperty(speed);

    goToNextStep.onChange([this]() {
        float prevValue = glm::floor(value);
        value = glm::min(prevValue + 1.f, value.maxValue());
        });

    goToPrevStep.onChange([this]() {
        float prevValue = glm::ceil(value);
        value = glm::max(prevValue - 1.f, value.minValue());
    });

    addProperty(goToNextStep);
    addProperty(goToPrevStep);

    nSteps.setReadOnly(true);
    addProperty(nSteps);

    addProperty(useSpline);
}

RenderableInterpolatedPoints::RenderableInterpolatedPoints(
    const ghoul::Dictionary& dictionary)
    : RenderablePointCloud(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addPropertySubOwner(_interpolation);

    if (p.interpolation.has_value()) {
        _interpolation.value = static_cast<float>(
            p.interpolation->value.value_or(_interpolation.value)
        );
        _interpolation.speed = static_cast<float>(
            p.interpolation->speed.value_or(_interpolation.speed)
        );
        _interpolation.useSpline = p.interpolation->useSplineInterpolation.value_or(
            _interpolation.useSpline
        );
    }

    unsigned int nObjects = static_cast<unsigned int>(p.numberOfObjects);

    // At this point, the dataset has been loaded and the number of points computed. We
    // need to recompute them and compute how many steps the number of points
    // corresponded to

    if (_nDataPoints % nObjects != 0) {
        LERROR(fmt::format(
            "Mismatch between provided number of data entries and the specified number "
            "of points. Expected the number of entries in the data file {} to be evenly "
            "divisible by the number of points", _dataFile
        ));
    }

    _interpolation.nSteps = _nDataPoints / nObjects;
    _interpolation.value.setMaxValue(static_cast<float>(_interpolation.nSteps - 1));

    _interpolation.value.onChange([this]() { _dataIsDirty = true; });

    // This property is mostly for show in the UI, but also used to tell how many points
    // shoudl be rendered. So make sure it is updated once we know the number of
    // interpolation steps
    _nDataPoints = nObjects;
}

std::vector<float> RenderableInterpolatedPoints::createDataSlice() {
    ZoneScoped;

    if (_dataset.entries.empty()) {
        return std::vector<float>();
    }

    std::vector<float> result;
    result.reserve(nAttributesPerPoint() * _nDataPoints);

    const float maxTValue = _interpolation.value.maxValue();

    // Find the information we need for the interpolation and to identify the points,
    // and make sure these result in valid indices in all cases
    float t0 = glm::floor(_interpolation.value);
    float maxAllowedT0 = glm::max(maxTValue - 1.f, 0.f);
    t0 = glm::clamp(t0, 0.f, maxAllowedT0);

    float t = glm::clamp(_interpolation.value - t0, 0.f, 1.f);

    float t1 = t0 + 1.f;
    t1 = glm::clamp(t1, 0.f, maxTValue);

    // What datavar is in use for the index color
    int colorParamIndex = currentColorParameterIndex();

    // What datavar is in use for the size scaling (if present)
    int sizeParamIndex = currentSizeParameterIndex();

    double maxRadius = 0.0;
    unsigned int t0Index = static_cast<unsigned int>(t0);
    unsigned int t1Index = static_cast<unsigned int>(t1);

    for (unsigned int i = 0; i < _nDataPoints; i++) {
        using namespace dataloader;
        const Dataset::Entry& e0 = _dataset.entries[t0Index * _nDataPoints + i];
        const Dataset::Entry& e1 = _dataset.entries[t1Index * _nDataPoints + i];

        // TODO (emmbr): Is this too naive?
        // Should we rather include two points and do the interpolation in the shader?
        // Let's see what the performance becomes for larger datasets. Maybe we can use
        // threading or some other approach update the buffered datasets more cleverly,
        // to reduce the runtime performance cost of recomputing the buffered data
        // (Although, note that this would require updating the rendering code as well
        // => more complexity)

        // Compute interpolated values
        const glm::dvec3 start = glm::dvec3(e0.position);
        const glm::dvec3 end = glm::dvec3(e1.position);

        glm::dvec3 interpolatedPosition;
        if (!_interpolation.useSpline || _interpolation.nSteps < 2) {
            interpolatedPosition = ghoul::interpolateLinear(t, start, end);
        }
        else {
            // Compute the extra positions, before and after the interpolated ones
            unsigned int beforeIndex = static_cast<unsigned int>(
                glm::max(t0 - 1.f, 0.f)
            );
            unsigned int afterIndex = static_cast<unsigned int>(
                glm::min(t1 + 1.f, maxTValue - 1.f)
            );

            const Dataset::Entry& e00 = _dataset.entries[beforeIndex * _nDataPoints + i];
            const Dataset::Entry& e11 = _dataset.entries[afterIndex * _nDataPoints + i];
            const glm::dvec3 before = glm::dvec3(e00.position);
            const glm::dvec3 after = glm::dvec3(e11.position);

            interpolatedPosition =
                ghoul::interpolateCatmullRom(t, before, start, end, after);
        }

        const double unitMeter = toMeter(_unit);
        glm::dvec4 position = glm::dvec4(interpolatedPosition * unitMeter, 1.0);
        position = _transformationMatrix * position;

        const double r = glm::length(position);
        maxRadius = std::max(maxRadius, r);

        // Positions
        for (int j = 0; j < 4; ++j) {
            result.push_back(static_cast<float>(position[j]));
        }

        // @TODO: Also need to update label positions, if we have created labels from the dataset
        // And make sure these are created from only the first set of points..

        // @TODO: make sure interpolation handles missing values for data parameters!

        // Colors
        if (_hasColorMapFile) {
            float value0 = e0.data[colorParamIndex];
            float value1 = e1.data[colorParamIndex];
            float value = ghoul::interpolateLinear(t, value0, value1);
            result.push_back(value);
        }

        // Size data
        if (_hasDatavarSize) {
            // @TODO: Consider more detailed control over the scaling. Currently the value
            // is multiplied with the value as is. Should have similar mapping properties
            // as the color mapping
            float value0 = e0.data[sizeParamIndex];
            float value1 = e1.data[sizeParamIndex];
            float value = ghoul::interpolateLinear(t, value0, value1);
            result.push_back(value);
        }
    }
    setBoundingSphere(maxRadius);
    return result;
}

} // namespace openspace
