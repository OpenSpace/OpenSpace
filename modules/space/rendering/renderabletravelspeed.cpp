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

#include <modules/space/rendering/renderabletravelspeed.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/filesystem/filesystem.h>
#include <openspace/format.h>
#include <openspace/logging/logmanager.h>
#include <openspace/misc/assert.h>
#include <openspace/misc/dictionary.h>
#include <openspace/misc/exception.h>
#include <openspace/opengl/programobject.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <memory>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "RenderableTravelSpeed";

    struct VertexPositions {
        glm::vec3 endOfFade;
        glm::vec3 betweenLightAndFade;
        glm::vec3 headOfLight;
    };

    constexpr DoublePropertyInfo SpeedInfo = {
        .identifier = "TravelSpeed",
        .name = "Speed of travel",
        .description = "A value for how fast the speed indicator should travel, in "
            "meters per second.",
        .visibility = Visibility::NoviceUser,
        .defaults = {
            .value = distanceconstants::LightSecond,
            .minMax = { 1.0, distanceconstants::LightSecond }
        }
    };

    constexpr StringPropertyInfo TargetInfo = {
        .identifier = "TargetNode",
        .name = "Target object",
        .description = "The identifier of the scene graph node to target with the speed "
            "indicator. The speed indicator will travel from the parent node to this "
            "scene graph node.",
        .visibility = Visibility::AdvancedUser
    };

    constexpr TriggerPropertyInfo ResetLineInfo = {
        .identifier = "ResetLine",
        .name = "Reset Line",
        .description = "When triggered, the line will start again from the beginning.",
        .visibility = Visibility::AdvancedUser
    };

    constexpr DoublePropertyInfo EpochInfo = {
        .identifier = "Epoch",
        .name = "Epoch",
        .description = "The epoch for the indicator, that is the time modulo the travel "
            "time in which the length of the indicator is exactly 0. Due to the way the "
            "indicator is calculated, this value will stay the same even as the "
            "indicator finishes each trip. So for a trip duration of 1s the epochs of "
            "2.5, 3.5, 6.5, 10.5 are all equivalent.",
        .visibility = Visibility::AdvancedUser,
        .defaults = {
            .value = -1.0
        }
    };

    constexpr Vec3PropertyInfo LineColorInfo = {
        .identifier = "Color",
        .name = "Color",
        .description = "An RGB color for the line.",
        .visibility = Visibility::NoviceUser,
        .defaults = {
            .value = glm::vec3(1.f),
            .minMax = { glm::vec3(0.f), glm::vec3(1.f) }
        }
    };

    constexpr FloatPropertyInfo LineWidthInfo = {
        .identifier = "LineWidth",
        .name = "Line width",
        .description = "This value specifies the line width.",
        .visibility = Visibility::NoviceUser,
        .defaults = {
            .value = 2.f,
            .minMax = { 1.f, 20.f }
        }
    };

    constexpr FloatPropertyInfo IndicatorLengthInfo = {
        .identifier = "IndicatorLength",
        .name = "Indicator length",
        .description = "The length of the speed indicator line, given in seconds. The "
            "length will be computed as the speed times this value. For example, a value "
            "of 1 will make it as long as the distance it would travel over one second "
            "with the specified speed.",
        .visibility = Visibility::User,
        .defaults = {
            .value = 1.f,
            .minMax = { 0.f, 360.f }
        }
    };

    constexpr FloatPropertyInfo FadeLengthInfo = {
        .identifier = "FadeLength",
        .name = "Fade length",
        .description = "The length of the faded tail of the speed indicator, given in "
            "seconds. The length of the tail will be computed as the speed times this "
            "value. For example, a value of 1 will make it as long as the distance it "
            "would travel over one second. A linear fade will be applied over this "
            "distance to create the tail.",
        .visibility = Visibility::User,
        .defaults = {
            .value = 0.f,
            .minMax = { 0.f, 360.f }
        }
    };

    // Visualizes a certain travel speed using a line that moves at the provided speed
    // from a start object to a target. The start position will be set from the `Parent`
    // of this scene graph node, and the end position is set from the provided `Target`
    // scene graph node. Per default, the speed is set to the speed of light.
    //
    // The length of the travelling line is set based on the travel speed and can be used
    // to show more information related to the distance traveled. For example, a length
    // of 1 shows how far an object would move over a duration of one second based on the
    // selected speed.
    //
    // The indicator will move from the parent to the target node based on the currently
    // set simulation time and the set `TravelSpeed`. That is, increasing the simulation
    // speed makes the indicator move faster.
    struct [[codegen::Dictionary(RenderableTravelSpeed)]] Parameters {
        std::string target [[codegen::Info(TargetInfo), codegen::identifier()]];

        std::optional<double> travelSpeed
            [[codegen::Info(SpeedInfo), codegen::greater(0.f)]];

        std::optional<glm::vec3> color [[codegen::Info(LineColorInfo), codegen::color()]];

        std::optional<float> lineWidth
            [[codegen::Info(LineWidthInfo), codegen::greater(0.f)]];

        std::optional<float> indicatorLength
            [[codegen::Info(IndicatorLengthInfo), codegen::greater(0.f)]];

        std::optional<float> fadeLength
            [[codegen::Info(FadeLengthInfo), codegen::greater(0.f)]];
    };
} // namespace
#include "renderabletravelspeed_codegen.cpp"

namespace openspace {

Documentation RenderableTravelSpeed::Documentation() {
    return codegen::doc<Parameters>("space_renderable_travelspeed");
}

RenderableTravelSpeed::RenderableTravelSpeed(const Dictionary& dictionary)
    : Renderable(dictionary)
    , _targetIdentifier(TargetInfo)
    , _resetLine(ResetLineInfo)
    , _epoch(EpochInfo)
    , _travelSpeed(SpeedInfo)
    , _indicatorLength(IndicatorLengthInfo)
    , _fadeLength(FadeLengthInfo)
    , _lineWidth(LineWidthInfo)
    , _lineColor(LineColorInfo)
{
    addProperty(Fadeable::_opacity);

    const Parameters p = codegen::bake<Parameters>(dictionary);
    setRenderBin(RenderBin::Overlay);

    _epoch.onChange([this]() { reinitiateTravel(_epoch); });
    addProperty(_epoch);

    _resetLine.onChange([this]() {
        reinitiateTravel(global::timeManager->time().j2000Seconds());
    });
    addProperty(_resetLine);

    _lineColor = p.color.value_or(_lineColor);
    _lineColor.setViewOption(Property::ViewOptions::Color);
    addProperty(_lineColor);

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    _indicatorLength = p.indicatorLength.value_or(_indicatorLength);
    addProperty(_indicatorLength);

    _fadeLength = p.fadeLength.value_or(_fadeLength);
    addProperty(_fadeLength);

    _targetIdentifier = p.target;
    addProperty(_targetIdentifier);
    _targetIdentifier.onChange([this]() {
        if (SceneGraphNode* n = sceneGraphNode(_targetIdentifier);  n) {
            _targetNode = n;
            reinitiateTravel();
        }
        else {
            LERROR(std::format(
                "Could not find target node with identifier '{}'. Resetting to '{}'",
                _targetIdentifier.value(), _targetNode->identifier()
            ));
            _targetIdentifier = _targetNode->identifier();
        }
    });

    _travelSpeed = p.travelSpeed.value_or(_travelSpeed);
    addProperty(_travelSpeed);
    _travelSpeed.onChange([this]() { reinitiateTravel(); });
}

void RenderableTravelSpeed::initialize() {
    _targetNode = sceneGraphNode(_targetIdentifier);
    if (!_targetNode) {
        throw RuntimeError("Could not find Target Node");
    }
}

void RenderableTravelSpeed::initializeGL() {
    _shaderProgram = BaseModule::ProgramObjectManager.request(
        "Travelspeed",
        []() -> std::unique_ptr<opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "Travelspeed",
                absPath("${MODULE_SPACE}/shaders/travelspeed_vs.glsl"),
                absPath("${MODULE_SPACE}/shaders/travelspeed_fs.glsl")
            );
        }
    );

    glCreateBuffers(1, &_vbo);
    glNamedBufferStorage(_vbo, sizeof(VertexPositions), nullptr, GL_DYNAMIC_STORAGE_BIT);

    glCreateVertexArrays(1, &_vao);
    glVertexArrayVertexBuffer(_vao, 0, _vbo, 0, sizeof(glm::vec3));

    glEnableVertexArrayAttrib(_vao, 0);
    glVertexArrayAttribFormat(_vao, 0, 3, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_vao, 0, 0);

    opengl::updateUniformLocations(*_shaderProgram, _uniformCache);
}

void RenderableTravelSpeed::deinitializeGL() {
    BaseModule::ProgramObjectManager.release(
        "Travelspeed",
        [](opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    glDeleteVertexArrays(1, &_vao);
    glDeleteBuffers(1, &_vbo);
}

void RenderableTravelSpeed::updateVertexData() {
    VertexPositions positions;

    // 3: start of light, 2: start of fade, 1: end of fade
    positions.headOfLight = _travelSpeed * _timeSinceStart * _directionVector;

    // This if statement is there to not start the line from behind the source node
    if (_timeSinceStart < _indicatorLength) {
        positions.betweenLightAndFade = glm::vec3(0.0, 0.0, 0.0);
    }
    else {
        positions.betweenLightAndFade =
            _travelSpeed * (_timeSinceStart - _indicatorLength) * _directionVector;
    }

    // This if statement is there to not start the line from behind the source node
    if (_timeSinceStart < (_indicatorLength + _fadeLength)) {
        positions.endOfFade = glm::vec3(0.0, 0.0, 0.0);
    }
    else {
        positions.endOfFade = _travelSpeed *
            (_timeSinceStart - _indicatorLength - _fadeLength) * _directionVector;
    }

    glNamedBufferSubData(_vbo, 0, sizeof(positions), &positions);
}

void RenderableTravelSpeed::reinitiateTravel(std::optional<double> time) {
    if (time.has_value()) {
        _epoch = *time;
        _initiationTime = _epoch;
    }
    else {
        _initiationTime = global::timeManager->time().j2000Seconds();
    }
    _arrivalTime = _initiationTime + _travelTime;
}

void RenderableTravelSpeed::update(const UpdateData& data) {
    if (_epoch == -1.0) {
        _epoch = data.time.j2000Seconds();
        _initiationTime = _epoch;
    }

    SceneGraphNode* sourceNode = parent();
    assert_msg(sourceNode, "Renderable have to be owned by scene graph node");

    // Target position, in the reference frame of the source node (to correctly inherit
    // parent transform)
    glm::dvec3 targetPosition = glm::dvec3(
        glm::inverse(sourceNode->modelTransform()) *
        glm::dvec4(_targetNode->worldPosition(), 1.0)
    );

    // Assumes source position is at origin, i.e. the parent node position
    _travelTime = glm::length(targetPosition) / _travelSpeed;
    _directionVector = glm::normalize(targetPosition);

    // Keep arrival time in sync with travel time
    _arrivalTime = _initiationTime + _travelTime;

    const double currentTime = data.time.j2000Seconds();

    if (currentTime >= _arrivalTime || currentTime < _initiationTime) {
        // Wrapped past the target (or time went backwards); restart
        reinitiateTravel();
    }

    _timeSinceStart = glm::clamp(
        currentTime - _initiationTime, 0.0, _travelTime
    );
    updateVertexData();
}

void RenderableTravelSpeed::render(const RenderData& data, RendererTasks&) {
    if (!_enabled) {
        return;
    }
    _shaderProgram->activate();

    _shaderProgram->setUniform(
        "modelViewTransform",
        glm::mat4(calcModelViewTransform(data))
    );
    _shaderProgram->setUniform("projectionTransform", data.camera.projectionMatrix());

    _shaderProgram->setUniform("lineColor", _lineColor);
    _shaderProgram->setUniform("opacity", opacity());

    glLineWidth(_lineWidth);
    glBindVertexArray(_vao);
    glDrawArrays(GL_LINE_STRIP, 0, 3);
    glBindVertexArray(0);

    _shaderProgram->deactivate();
}

} // namespace openspace
