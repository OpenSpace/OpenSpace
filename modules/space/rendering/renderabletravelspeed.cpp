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

#include <modules/space/rendering/renderabletravelspeed.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <optional>

namespace {
    constexpr std::array<const char*, 2> UniformNames = { "lineColor", "opacity" };

    constexpr openspace::properties::Property::PropertyInfo SpeedInfo = {
        "TravelSpeed",
        "Speed of Travel",
        "A value for how fast the speed indicator should travel, in meters per second. "
        "The default value is the speed of light.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo TargetInfo = {
        "TargetNode",
        "Target Object",
        "The identifier of the scene graph node to target with the speed indicator. The "
        "speed indicator will travel from the parent node to this scene graph node.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo LineColorInfo = {
        "Color",
        "Color",
        "An RGB color for the line.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo IndicatorLengthInfo = {
        "IndicatorLength",
        "Indicator Length",
        "The length of the speed indicator line, given in seconds. The length will be "
        "computed as the speed times this value. For example, a value of 1 will make it "
        "as long as the distance it would travel over one second with the specified "
        "speed.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FadeLengthInfo = {
        "FadeLength",
        "Fade Length",
        "The length of the faded tail of the speed indicator, given in seconds. The "
        "length of the tail will be computed as the speed times this value. For example, "
        "a value of 1 will make it as long as the distance it would travel over one "
        "second. A linear fade will be applied over this distance to create the tail.",
        openspace::properties::Property::Visibility::User
    };

    // This renderable can be used to visualize a certain travel speed using a line that
    // moves at the provided speed from a start object to a target. The start position
    // will be set from the `Parent` of this scene graph node, and the end position is
    // set from the provided `Target` scene graph node. Per default, the speed is set to
    // the speed of light.
    //
    // The length of the traveling line is set based on the travel speed and can be used
    // to show more information related to the distance traveled. For example, a length
    // of 1 shows how far an object would move over a duration of one second based on the
    // selected speed.
    struct [[codegen::Dictionary(RenderableTravelSpeed)]] Parameters {
        // [[codegen::verbatim(TargetInfo.description)]]
        std::string target [[codegen::identifier()]];;

        // [[codegen::verbatim(SpeedInfo.description)]]
        std::optional<double> travelSpeed [[codegen::greater(0.f)]];

        // [[codegen::verbatim(LineColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth [[codegen::greater(0.f)]];

        // [[codegen::verbatim(IndicatorLengthInfo.description)]]
        std::optional<float> indicatorLength [[codegen::greater(0.f)]];

        // [[codegen::verbatim(FadeLengthInfo.description)]]
        std::optional<float> fadeLength [[codegen::greater(0.f)]];
    };
#include "renderabletravelspeed_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableTravelSpeed::Documentation() {
    return codegen::doc<Parameters>("base_renderable_renderabletravelspeed");
}

RenderableTravelSpeed::RenderableTravelSpeed(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _targetName(TargetInfo)
    , _travelSpeed(
        SpeedInfo,
        distanceconstants::LightSecond,
        1.0,
        distanceconstants::LightSecond
      )
    , _indicatorLength(IndicatorLengthInfo, 1.f, 0.f, 360.f)
    , _fadeLength(FadeLengthInfo, 0.f, 0.f, 360.f)
    , _lineWidth(LineWidthInfo, 2.f, 1.f, 20.f)
    , _lineColor(LineColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
{
    addProperty(Fadeable::_opacity);

    const Parameters p = codegen::bake<Parameters>(dictionary);
    setRenderBin(RenderBin::Overlay);

    _lineColor = p.color.value_or(_lineColor);
    _lineColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_lineColor);

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    _indicatorLength = p.indicatorLength.value_or(_indicatorLength);
    addProperty(_indicatorLength);

    _fadeLength = p.fadeLength.value_or(_fadeLength);
    addProperty(_fadeLength);

    _targetName = p.target;
    addProperty(_targetName);
    _targetName.onChange([this]() {
        if (SceneGraphNode* n = sceneGraphNode(_targetName);  n) {
            _targetNode = n;
            _targetPosition = _targetNode->worldPosition();
            _travelTime = glm::distance(_targetPosition, _sourcePosition) / _travelSpeed;
            calculateDirectionVector();
            reinitiateTravel();
        }
    });

    _travelSpeed = p.travelSpeed.value_or(_travelSpeed);
    addProperty(_travelSpeed);
    _travelSpeed.onChange([this]() { reinitiateTravel(); });
}

void RenderableTravelSpeed::initialize() {
    _targetNode = sceneGraphNode(_targetName);
    if (_targetNode == nullptr) {
        throw ghoul::RuntimeError("Could not find Target Node");
    }
}

void RenderableTravelSpeed::initializeGL() {
    _shaderProgram = BaseModule::ProgramObjectManager.request(
        "Travelspeed",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "Travelspeed",
                absPath("${MODULE_SPACE}/shaders/travelspeed_vs.glsl"),
                absPath("${MODULE_SPACE}/shaders/travelspeed_fs.glsl")
            );
        }
    );

    glGenVertexArrays(1, &_vaoId);
    glGenBuffers(1, &_vBufferId);

    ghoul::opengl::updateUniformLocations(*_shaderProgram, _uniformCache, UniformNames);
}

void RenderableTravelSpeed::deinitializeGL() {
    BaseModule::ProgramObjectManager.release(
        "Travelspeed",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    glDeleteVertexArrays(1, &_vaoId);
    glDeleteBuffers(1, &_vBufferId);
}

void RenderableTravelSpeed::calculateDirectionVector() {
    _directionVector = glm::normalize(_targetPosition - _sourcePosition);
}

void RenderableTravelSpeed::calculateVerticesPositions() {
    // 3: start of light, 2: start of fade, 1: end of fade
    _vertexPositions.headOfLight = _travelSpeed * _timeSinceStart * _directionVector;

    // This if statment is there to not start the line from behind the source node
    if (_timeSinceStart < _indicatorLength) {
        _vertexPositions.betweenLightAndFade = glm::vec3(0.0, 0.0, 0.0); // = source node
    }
    else {
        _vertexPositions.betweenLightAndFade =
            _travelSpeed * (_timeSinceStart - _indicatorLength) * _directionVector;
    }

    // This if statment is there to not start the line from behind the source node
    if (_timeSinceStart < (_indicatorLength + _fadeLength)) {
        _vertexPositions.endOfFade = glm::vec3(0.0, 0.0, 0.0); // = source node
    }
    else {
        _vertexPositions.endOfFade = _travelSpeed *
            (_timeSinceStart - _indicatorLength - _fadeLength) * _directionVector;
    }
}

void RenderableTravelSpeed::updateVertexData() {
    calculateVerticesPositions();

    glBindVertexArray(_vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferId);
    glBufferData(
        GL_ARRAY_BUFFER,
        sizeof(VertexPositions),
        &_vertexPositions,
        GL_DYNAMIC_DRAW
    );
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), nullptr);
    glBindVertexArray(0);
}

void RenderableTravelSpeed::reinitiateTravel() {
    _initiationTime = global::timeManager->time().j2000Seconds();
    _arrivalTime = _initiationTime + _travelTime;
}

bool RenderableTravelSpeed::isReady() const{
    return _shaderProgram != nullptr;
}

void RenderableTravelSpeed::update(const UpdateData& data) {
    if (_initiationTime == -1.0) {
        _initiationTime = data.time.j2000Seconds();
        _arrivalTime = _initiationTime + _travelTime;
    }

    _targetPosition = _targetNode->worldPosition();
    SceneGraphNode* mySGNPointer = parent();
    ghoul_assert(mySGNPointer, "Renderable have to be owned by scene graph node");
    _sourcePosition = mySGNPointer->worldPosition();

    _travelTime = glm::distance(_targetPosition, _sourcePosition) / _travelSpeed;

    const double currentTime = data.time.j2000Seconds();
    // Unless we've reached the target
    if (_initiationTime < currentTime && _arrivalTime > currentTime) {
        _timeSinceStart = currentTime - _initiationTime;
        calculateDirectionVector();
        updateVertexData();
    }
    else { // in case we've reached the target
        reinitiateTravel();
    }

    _shaderProgram->setUniform("lineColor", _lineColor);
    _shaderProgram->setUniform("opacity", opacity());
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

#ifndef __APPLE__
    glLineWidth(_lineWidth);
#else
    glLineWidth(1.f);
#endif
    glBindVertexArray(_vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferId);
    glDrawArrays(GL_LINE_STRIP, 0, 3);
    glBindVertexArray(0);

    _shaderProgram->deactivate();
}
} // namespace openspace
