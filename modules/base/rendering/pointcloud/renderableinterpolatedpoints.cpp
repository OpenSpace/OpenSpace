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

    // This property is mostly for show in the UI, but also used to tell how many points
    // shoudl be rendered. So make sure it is updated once we know the number of
    // interpolation steps
    _nDataPoints = nObjects;
}

void RenderableInterpolatedPoints::initializeShadersAndGlExtras() {
    _program = BaseModule::ProgramObjectManager.request(
        "RenderablePointCloud_Interpolated",
        []() {
            return global::renderEngine->buildRenderProgram(
                "RenderablePointCloud_Interpolated",
                absPath("${MODULE_BASE}/shaders/pointcloud/billboardpoint_interpolated_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/pointcloud/billboardpoint_fs.glsl"),
                absPath("${MODULE_BASE}/shaders/pointcloud/billboardpoint_gs.glsl")
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

void RenderableInterpolatedPoints::bindDataForPointRendering() {
    RenderablePointCloud::bindDataForPointRendering();

    float t0 = computeCurrentLowerValue();
    float t = glm::clamp(_interpolation.value - t0, 0.f, 1.f);
    _program->setUniform("interpolationValue", t);
    _program->setUniform("useSpline", _interpolation.useSpline);
}

void RenderableInterpolatedPoints::preUpdate() {
    if (_shouldReinitializeBufferdata) {
        initializeBufferData();
        _shouldReinitializeBufferdata = false;
    }
}

int RenderableInterpolatedPoints::nAttributesPerPoint() const {
    int n = RenderablePointCloud::nAttributesPerPoint();
    // Need twice as much information as the regular points
    n *= 2;
    if (_interpolation.useSpline) {
        // Use two more positions
        n += 8;
    }
    return n;
}

std::vector<float> RenderableInterpolatedPoints::createDataSlice() {
    ZoneScoped;

    if (_dataset.entries.empty()) {
        return std::vector<float>();
    }

    std::vector<float> result;
    result.reserve(nAttributesPerPoint() * _nDataPoints);

    // Find the information we need for the interpolation and to identify the points,
    // and make sure these result in valid indices in all cases
    float t0 = computeCurrentLowerValue();
    float t1 = t0 + 1.f;
    t1 = glm::clamp(t1, 0.f, _interpolation.value.maxValue());
    unsigned int t0Index = static_cast<unsigned int>(t0);
    unsigned int t1Index = static_cast<unsigned int>(t1);

    // What datavar is in use for the index color
    int colorParamIndex = currentColorParameterIndex();

    // What datavar is in use for the size scaling (if present)
    int sizeParamIndex = currentSizeParameterIndex();

    double maxRadius = 0.0;

    for (unsigned int i = 0; i < _nDataPoints; i++) {
        using namespace dataloader;
        const Dataset::Entry& e0 = _dataset.entries[t0Index * _nDataPoints + i];
        const Dataset::Entry& e1 = _dataset.entries[t1Index * _nDataPoints + i];

        const double unitMeter = toMeter(_unit);
        glm::dvec4 position0 = glm::dvec4(glm::dvec3(e0.position) * unitMeter, 1.0);
        position0 = _transformationMatrix * position0;

        glm::dvec4 position1 = glm::dvec4(glm::dvec3(e1.position) * unitMeter, 1.0);
        position1 = _transformationMatrix * position1;

        // @TODO
        const double r = glm::length(position0);
        maxRadius = std::max(maxRadius, r);

        // Positions
        for (int j = 0; j < 4; ++j) {
            result.push_back(static_cast<float>(position0[j]));
        }

        for (int j = 0; j < 4; ++j) {
            result.push_back(static_cast<float>(position1[j]));
        }

        if (_interpolation.useSpline && _interpolation.nSteps > 1) {
            // Compute the extra positions, before and after the other ones
            unsigned int beforeIndex = static_cast<unsigned int>(
                glm::max(t0 - 1.f, 0.f)
            );
            unsigned int afterIndex = static_cast<unsigned int>(
                glm::min(t1 + 1.f, _interpolation.value.maxValue() - 1.f)
            );

            const Dataset::Entry& e00 = _dataset.entries[beforeIndex * _nDataPoints + i];
            const Dataset::Entry& e11 = _dataset.entries[afterIndex * _nDataPoints + i];
            const glm::dvec3 before = glm::dvec3(e00.position);
            const glm::dvec3 after = glm::dvec3(e11.position);

            const double unitMeter = toMeter(_unit);
            glm::dvec4 positionBefore = glm::dvec4(before * unitMeter, 1.0);
            positionBefore = _transformationMatrix * positionBefore;

            glm::dvec4 positionAfter = glm::dvec4(after * unitMeter, 1.0);
            positionAfter = _transformationMatrix * positionAfter;

            for (int j = 0; j < 4; ++j) {
                result.push_back(static_cast<float>(positionBefore[j]));
            }

            for (int j = 0; j < 4; ++j) {
                result.push_back(static_cast<float>(positionAfter[j]));
            }
        }

        // Colors
        if (_hasColorMapFile) {
            result.push_back(e0.data[colorParamIndex]);
            result.push_back(e1.data[colorParamIndex]);
        }

        // Size data
        if (_hasDatavarSize) {
            // @TODO: Consider more detailed control over the scaling. Currently the value
            // is multiplied with the value as is. Should have similar mapping properties
            // as the color mapping
            result.push_back(e0.data[sizeParamIndex]);
            result.push_back(e1.data[sizeParamIndex]);
        }

        // @TODO: Also need to update label positions, if we have created labels from the dataset
        // And make sure these are created from only the first set of points..
    }
    setBoundingSphere(maxRadius);
    return result;
}

void RenderableInterpolatedPoints::initializeBufferData() {
    if (_vao == 0) {
        glGenVertexArrays(1, &_vao);
        LDEBUG(fmt::format("Generating Vertex Array id '{}'", _vao));
    }
    if (_vbo == 0) {
        glGenBuffers(1, &_vbo);
        LDEBUG(fmt::format("Generating Vertex Buffer Object id '{}'", _vbo));
    }

    const int attibutesPerPoint = nAttributesPerPoint();
    const unsigned int bufferSize = attibutesPerPoint * _nDataPoints * sizeof(float);

    // Allocate the memory for the buffer (we will want to upload the data quite often)
    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(GL_ARRAY_BUFFER, bufferSize, nullptr, GL_DYNAMIC_DRAW);

    int attributeOffset = 0;

    auto addFloatAttribute = [&](const std::string& name, GLint nValues) {
        GLint attrib = _program->attributeLocation(name);
        glEnableVertexAttribArray(attrib);
        glVertexAttribPointer(
            attrib,
            nValues,
            GL_FLOAT,
            GL_FALSE,
            attibutesPerPoint * sizeof(float),
            (attributeOffset > 0) ?
                reinterpret_cast<void*>(attributeOffset * sizeof(float)) :
                nullptr
        );
        attributeOffset += nValues;
    };

    addFloatAttribute("in_position0", 4);
    addFloatAttribute("in_position1", 4);

    if (_interpolation.useSpline) {
        addFloatAttribute("in_position_before", 4);
        addFloatAttribute("in_position_after", 4);
    }

    if (_hasColorMapFile) {
        addFloatAttribute("in_colorParameter0", 1);
        addFloatAttribute("in_colorParameter1", 1);
    }

    if (_hasDatavarSize) {
        addFloatAttribute("in_scalingParameter0", 1);
        addFloatAttribute("in_scalingParameter1", 1);
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
        t0 = t0 - 1.0;
    }

    const float maxTValue = _interpolation.value.maxValue();
    float maxAllowedT0 = glm::max(maxTValue - 1.f, 0.f);
    t0 = glm::clamp(t0, 0.f, maxAllowedT0);
    return t0;
}


} // namespace openspace
