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

#include <modules/base/rendering/pointcloud/renderableinterpolatedpoints.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
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
        "The value used for interpolation. The max value is set from the number of "
        "steps in the dataset, so a step of one corresponds to one step in the dataset "
        "and values in-between will be determined using interpolation.",
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
    //
    // MultiTexture:
    // Note that if using multiple textures for the points based on values in the dataset,
    // the used texture will be decided based on the first N set of points.
    struct [[codegen::Dictionary(RenderableInterpolatedPoints)]] Parameters {
        // The number of objects to read from the dataset. Every N:th datapoint will
        // be interpreted as the same point, but at a different step in the interpolation.
        int numberOfObjects [[codegen::greaterequal(1)]];

        struct Interpolation {
            // [[codegen::verbatim(InterpolationValueInfo.description)]]
            std::optional<double> value;

            // [[codegen::verbatim(InterpolationSpeedInfo.description)]]
            std::optional<double> speed;

            // [[codegen::verbatim(UseSplineInfo.description)]]
            std::optional<bool> useSplineInterpolation;
        };
        // Initial settings for the interpolation.
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
    , nSteps(StepsInfo, 1)
    , goToNextStep(JumpToNextInfo)
    , goToPrevStep(JumpToPrevInfo)
    , interpolateToNextStep(InterpolateToNextInfo)
    , interpolateToPrevStep(InterpolateToPrevInfo)
    , interpolateToEnd(InterpolateToEndInfo)
    , interpolateToStart(InterpolateToStartInfo)
    , speed(InterpolationSpeedInfo, 1.f, 0.01f, 100.f)
    , useSpline(UseSplineInfo, false)
{
    addProperty(value);

    auto triggerInterpolation = [](std::string_view identifier, float v, float d) {
        std::string script = std::format(
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
            value.uri(),
            value.maxValue(),
            duration
        );
    });

    interpolateToStart.onChange([triggerInterpolation, this]() {
        float duration = value / speed;
        triggerInterpolation(value.uri(), 0.f, duration);
    });

    interpolateToNextStep.onChange([triggerInterpolation, this]() {
        float prevValue = glm::floor(value);
        float newValue = glm::min(prevValue + 1.f, value.maxValue());
        float duration = 1.f / speed;
        triggerInterpolation(value.uri(), newValue, duration);
    });

    interpolateToPrevStep.onChange([triggerInterpolation, this]() {
        float prevValue = glm::ceil(value);
        float newValue = glm::max(prevValue - 1.f, value.minValue());
        float duration = 1.f / speed;
        triggerInterpolation(value.uri(), newValue, duration);
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

    _interpolation.value.onChange([this]() {
        bool passedAKnot =
            glm::ceil(_interpolation.value) != glm::ceil(_prevInterpolationValue);

        if (passedAKnot) {
            _dataIsDirty = true;
        }
        _prevInterpolationValue = _interpolation.value;
    });

    _interpolation.useSpline.onChange([this]() {
        _dataIsDirty = true;
        _shouldReinitializeBufferdata = true;
    });

    _nObjectsInDataset = static_cast<unsigned int>(p.numberOfObjects);

    if (_skipFirstDataPoint) {
        LWARNING(
            "Found setting to skip first data point in asset. This is not supported for "
            "interpolated point clouds. Ignoring"
        );
        _skipFirstDataPoint = false;
    }
}

void RenderableInterpolatedPoints::initialize() {
    RenderablePointCloud::initialize();

    // At this point, the dataset has been loaded and we know how many data points it
    // contains => we can compute the number of interpolation steps
    if (_nDataPoints % _nObjectsInDataset != 0) {
        LERROR(std::format(
            "Mismatch between provided number of data entries and the specified number "
            "of points. Expected the number of entries in the data file '{}' to be "
            "evenly divisible by the number of objects", _dataFile
        ));
    }

    if (_nObjectsInDataset > 0) {
        _interpolation.nSteps = _nDataPoints / _nObjectsInDataset;
    }
    _interpolation.value.setMaxValue(static_cast<float>(_interpolation.nSteps - 1));

    // This is the property that is shown in the user interface, so update it so the user
    // can get an idea of how many points will be rendered
    _nDataPoints = _nObjectsInDataset;
}

void RenderableInterpolatedPoints::initializeShadersAndGlExtras() {
    _program = BaseModule::ProgramObjectManager.request(
        "RenderablePointCloud_Interpolated",
        []() {
            std::filesystem::path path = absPath("${MODULE_BASE}/shaders/pointcloud");
            return global::renderEngine->buildRenderProgram(
                "RenderablePointCloud_Interpolated",
                path / "pointcloud_interpolated_vs.glsl",
                path / "pointcloud_fs.glsl",
                path / "pointcloud_gs.glsl"
            );
        }
    );

    initializeBufferData();
}

void RenderableInterpolatedPoints::deinitializeShaders() {
    BaseModule::ProgramObjectManager.release(
        "RenderablePointCloud_Interpolated",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _program = nullptr;
}

void RenderableInterpolatedPoints::setExtraUniforms() {
    float t0 = computeCurrentLowerValue();
    float t = glm::clamp(_interpolation.value - t0, 0.f, 1.f);

    _program->setUniform("interpolationValue", t);
    _program->setUniform("useSpline", useSplineInterpolation());
}

void RenderableInterpolatedPoints::preUpdate() {
    if (_shouldReinitializeBufferdata) {
        initializeBufferData();
        _shouldReinitializeBufferdata = false;
    }
}

int RenderableInterpolatedPoints::nAttributesPerPoint() const {
    int n = RenderablePointCloud::nAttributesPerPoint();

    // Always at least three extra position values (xyz)
    n += 3;
    if (useSplineInterpolation()) {
        // Use two more positions (xyz)
        n += 2 * 3;
    }
    if (useOrientationData()) {
        // Use one more orientation quaternion (wxyz)
        n += 4;
    }
    // And potentially some more color and size data
    n += hasColorData() ? 1 : 0;
    n += hasSizeData() ? 1 : 0;

    return n;
}

bool RenderableInterpolatedPoints::useSplineInterpolation() const {
    return _interpolation.useSpline && _interpolation.nSteps > 1;
}

void RenderableInterpolatedPoints::addPositionDataForPoint(unsigned int index,
                                                           std::vector<float>& result,
                                                           double& maxRadius) const
{
    using namespace dataloader;
    auto [firstIndex, secondIndex] = interpolationIndices(index);

    const Dataset::Entry& e0 = _dataset.entries[firstIndex];
    const Dataset::Entry& e1 = _dataset.entries[secondIndex];

    glm::dvec3 position0 = transformedPosition(e0);
    glm::dvec3 position1 = transformedPosition(e1);

    const double r = glm::max(glm::length(position0), glm::length(position1));
    maxRadius = glm::max(maxRadius, r);

    for (int j = 0; j < 3; ++j) {
        result.push_back(static_cast<float>(position0[j]));
    }

    for (int j = 0; j < 3; ++j) {
        result.push_back(static_cast<float>(position1[j]));
    }

    if (useSplineInterpolation()) {
        // Compute the extra positions, before and after the other ones. But make sure
        // we do not overflow the allowed bound for the current interpolation step
        int beforeIndex = glm::max(static_cast<int>(firstIndex - _nDataPoints), 0);
        int maxT = static_cast<int>(_interpolation.value.maxValue() - 1.f);
        int maxAllowedindex = maxT * _nDataPoints + index;
        int afterIndex = glm::min(
            static_cast<int>(secondIndex + _nDataPoints),
            maxAllowedindex
        );

        const Dataset::Entry& e00 = _dataset.entries[beforeIndex];
        const Dataset::Entry& e11 = _dataset.entries[afterIndex];
        glm::dvec3 positionBefore = transformedPosition(e00);
        glm::dvec3 positionAfter = transformedPosition(e11);

        for (int j = 0; j < 3; ++j) {
            result.push_back(static_cast<float>(positionBefore[j]));
        }

        for (int j = 0; j < 3; ++j) {
            result.push_back(static_cast<float>(positionAfter[j]));
        }
    }
}

void RenderableInterpolatedPoints::addColorAndSizeDataForPoint(unsigned int index,
                                                        std::vector<float>& result) const
{
    using namespace dataloader;
    auto [firstIndex, secondIndex] = interpolationIndices(index);
    const Dataset::Entry& e0 = _dataset.entries[firstIndex];
    const Dataset::Entry& e1 = _dataset.entries[secondIndex];

    if (hasColorData()) {
        const int colorParamIndex = currentColorParameterIndex();
        result.push_back(e0.data[colorParamIndex]);
        result.push_back(e1.data[colorParamIndex]);
    }

    if (hasSizeData()) {
        const int sizeParamIndex = currentSizeParameterIndex();
        // @TODO: Consider more detailed control over the scaling. Currently the value
        // is multiplied with the value as is. Should have similar mapping properties
        // as the color mapping

        // Convert to diameter if data is given as radius
        float multiplier = _sizeSettings.sizeMapping->isRadius ? 2.f : 1.f;
        result.push_back(multiplier * e0.data[sizeParamIndex]);
        result.push_back(multiplier * e1.data[sizeParamIndex]);
    }
}

void RenderableInterpolatedPoints::addOrientationDataForPoint(unsigned int index,
                                                        std::vector<float>& result) const
{
    using namespace dataloader;
    auto [firstIndex, secondIndex] = interpolationIndices(index);
    const Dataset::Entry& e0 = _dataset.entries[firstIndex];
    const Dataset::Entry& e1 = _dataset.entries[secondIndex];

    glm::quat q0 = orientationQuaternion(e0);
    glm::quat q1 = orientationQuaternion(e1);

    result.push_back(q0.x);
    result.push_back(q0.y);
    result.push_back(q0.z);
    result.push_back(q0.w);

    result.push_back(q1.x);
    result.push_back(q1.y);
    result.push_back(q1.z);
    result.push_back(q1.w);
}

void RenderableInterpolatedPoints::initializeBufferData() {
    if (_vao == 0) {
        glGenVertexArrays(1, &_vao);
        LDEBUG(std::format("Generating Vertex Array id '{}'", _vao));
    }
    if (_vbo == 0) {
        glGenBuffers(1, &_vbo);
        LDEBUG(std::format("Generating Vertex Buffer Object id '{}'", _vbo));
    }

    const int attibsPerPoint = nAttributesPerPoint();
    const unsigned int bufferSize = attibsPerPoint * _nDataPoints * sizeof(float);

    // Allocate the memory for the buffer (we will want to upload the data quite often)
    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(GL_ARRAY_BUFFER, bufferSize, nullptr, GL_DYNAMIC_DRAW);

    int offset = 0;

    offset = bufferVertexAttribute("in_position0", 3, attibsPerPoint, offset);
    offset = bufferVertexAttribute("in_position1", 3, attibsPerPoint, offset);

    if (useSplineInterpolation()) {
        offset = bufferVertexAttribute("in_position_before", 3, attibsPerPoint, offset);
        offset = bufferVertexAttribute("in_position_after", 3, attibsPerPoint, offset);
    }

    if (hasColorData()) {
        offset = bufferVertexAttribute("in_colorParameter0", 1, attibsPerPoint, offset);
        offset = bufferVertexAttribute("in_colorParameter1", 1, attibsPerPoint, offset);
    }

    if (hasSizeData()) {
        offset = bufferVertexAttribute("in_scalingParameter0", 1, attibsPerPoint, offset);
        offset = bufferVertexAttribute("in_scalingParameter1", 1, attibsPerPoint, offset);
    }

    if (useOrientationData()) {
        offset = bufferVertexAttribute("in_orientation0", 4, attibsPerPoint, offset);
        offset = bufferVertexAttribute("in_orientation1", 4, attibsPerPoint, offset);
    }

    if (_hasSpriteTexture) {
        offset = bufferVertexAttribute("in_textureLayer", 1, attibsPerPoint, offset);
    }

    glBindVertexArray(0);
}

void RenderableInterpolatedPoints::updateBufferData() {
    if (!_hasDataFile || _dataset.entries.empty()) {
        return;
    }

    ZoneScopedN("Data dirty");
    TracyGpuZone("Data dirty");
    LDEBUG("Regenerating data");

    // Regenerate data and update buffer
    std::vector<float> slice = createDataSlice();

    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferSubData(GL_ARRAY_BUFFER, 0, slice.size() * sizeof(float), slice.data());

    glBindVertexArray(0);

    _dataIsDirty = false;
}

bool RenderableInterpolatedPoints::isAtKnot() const {
    float v = _interpolation.value;
    return (v - glm::floor(v)) < std::numeric_limits<float>::epsilon();
}

float RenderableInterpolatedPoints::computeCurrentLowerValue() const {
    float t0 = glm::floor(_interpolation.value);

    if (isAtKnot()) {
        t0 = t0 - 1.f;
    }

    const float maxTValue = _interpolation.value.maxValue();
    float maxAllowedT0 = glm::max(maxTValue - 1.f, 0.f);
    t0 = glm::clamp(t0, 0.f, maxAllowedT0);
    return t0;
}

float RenderableInterpolatedPoints::computeCurrentUpperValue() const {
    float t0 = computeCurrentLowerValue();
    float t1 = t0 + 1.f;
    t1 = glm::clamp(t1, 0.f, _interpolation.value.maxValue());
    return t1;
}

std::pair<size_t, size_t>
RenderableInterpolatedPoints::interpolationIndices(unsigned int index) const
{
    float t0 = computeCurrentLowerValue();
    float t1 = computeCurrentUpperValue();
    unsigned int t0Index = static_cast<unsigned int>(t0);
    unsigned int t1Index = static_cast<unsigned int>(t1);

    size_t lower = size_t(t0Index * _nDataPoints + index);
    size_t upper = size_t(t1Index * _nDataPoints + index);

    return { lower, upper };
}

} // namespace openspace
