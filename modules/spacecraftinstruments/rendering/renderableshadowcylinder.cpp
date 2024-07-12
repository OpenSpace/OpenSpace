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

#include <modules/spacecraftinstruments/rendering/renderableshadowcylinder.h>

#include <modules/spacecraftinstruments/spacecraftinstrumentsmodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo NumberPointsInfo = {
        "NumberOfPoints",
        "Points",
        "The number of control points used for constructing the shadow geometry. The "
        "higher this number, the more detailed the shadow is, but it will have a "
        "negative impact on the performance.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShadowLengthInfo = {
        "ShadowLength",
        "Shadow Length",
        "A factor that controls the length of the shadow that is cast by the target "
        "object. The total length of the shadow is equal to the distance from the "
        "target to the light source multiplied with this value.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShadowColorInfo = {
        "ShadowColor",
        "Shadow Color",
        "The color used for the shadow cylinder.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo TerminatorTypeInfo = {
        "TerminatorType",
        "Terminator Type",
        "Determines the type of terminator to use for calculating the shadow eclipse.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo LightSourceInfo = {
        "LightSource",
        "Light Source",
        "The SPICE name of the object that is used as the illuminator for computing the "
        "shadow cylinder.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ObserverInfo = {
        "Observer",
        "Observer",
        "The SPICE name of the object that is the observer.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BodyInfo = {
        "Body",
        "Target Body",
        "The SPICE name of target body that is used as the shadow caster.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BodyFrameInfo = {
        "BodyFrame",
        "Body Frame",
        "The SPICE name of the reference frame in which the shadow cylinder is "
        "expressed.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AberrationInfo = {
        "Aberration",
        "Aberration",
        "The aberration method that is used for computing the shadow cylinder. The "
        "options are \"NONE\", \"LT\" (Light Time), \"LT + S\" (Light Time Stellar), "
        "\"CN\" (Converged Newtonian), and \"CN + S\" (Converged Newtonian Stellar).",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableShadowCylinder)]] Parameters {
        // [[codegen::verbatim(NumberPointsInfo.description)]]
        std::optional<int> numberOfPoints;

        // [[codegen::verbatim(ShadowLengthInfo.description)]]
        std::optional<float> shadowLength;

        // [[codegen::verbatim(ShadowColorInfo.description)]]
        std::optional<glm::vec3> shadowColor [[codegen::color()]];

        enum class [[codegen::map(openspace::SpiceManager::TerminatorType)]] Terminator {
            Umbral [[codegen::key("UMBRAL")]],
            Penumbral [[codegen::key("PENUMBRAL")]]
        };
        // [[codegen::verbatim(TerminatorTypeInfo.description)]]
        Terminator terminatorType;

        // [[codegen::verbatim(LightSourceInfo.description)]]
        std::string lightSource;

        // [[codegen::verbatim(ObserverInfo.description)]]
        std::string observer;

        // [[codegen::verbatim(BodyInfo.description)]]
        std::string body;

        // [[codegen::verbatim(BodyFrameInfo.description)]]
        std::string bodyFrame;

        // [[codegen::verbatim(AberrationInfo.description)]]
        std::string aberration [[codegen::inlist("NONE", "LT", "LT+S", "CN", "CN+S")]];
    };
#include "renderableshadowcylinder_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableShadowCylinder::Documentation() {
    return codegen::doc<Parameters>("spacecraftinstruments_renderableshadowcylinder");
}

RenderableShadowCylinder::RenderableShadowCylinder(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _numberOfPoints(NumberPointsInfo, 190, 1, 300)
    , _shadowLength(ShadowLengthInfo, 0.1f, 0.f, 0.5f)
    , _shadowColor(ShadowColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _terminatorType(
        TerminatorTypeInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _lightSource(LightSourceInfo)
    , _observer(ObserverInfo)
    , _body(BodyInfo)
    , _bodyFrame(BodyFrameInfo)
    , _aberration(AberrationInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    _numberOfPoints = p.numberOfPoints.value_or(_numberOfPoints);
    addProperty(_numberOfPoints);

    _shadowLength = p.shadowLength.value_or(_shadowLength);
    addProperty(_shadowLength);

    _shadowColor = p.shadowColor.value_or(_shadowColor);
    _shadowColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_shadowColor);

    _terminatorType.addOptions({
        { static_cast<int>(SpiceManager::TerminatorType::Umbral), "Umbral" },
        { static_cast<int>(SpiceManager::TerminatorType::Penumbral), "Penumbral" }
    });
    _terminatorType =
        static_cast<int>(codegen::map<SpiceManager::TerminatorType>(p.terminatorType));
    addProperty(_terminatorType);

    _lightSource = p.lightSource;
    _observer = p.observer;
    _body = p.body;
    _bodyFrame = p.bodyFrame;

    _lightSource.setReadOnly(true);
    addProperty(_lightSource);
    _observer.setReadOnly(true);
    addProperty(_observer);
    _body.setReadOnly(true);
    addProperty(_body);
    _bodyFrame.setReadOnly(true);
    addProperty(_bodyFrame);

    using T = SpiceManager::AberrationCorrection::Type;
    _aberration.addOptions({
        { static_cast<int>(T::None), "None" },
        { static_cast<int>(T::ConvergedNewtonian), "Converged Newtonian" },
        { static_cast<int>(T::ConvergedNewtonianStellar), "Converged Newtonian Stellar" },
        { static_cast<int>(T::LightTime), "Light Time" },
        { static_cast<int>(T::LightTimeStellar), "Light Time Stellar" },
    });
    const SpiceManager::AberrationCorrection abbcorr = SpiceManager::AberrationCorrection(
        p.aberration
    );
    _aberration = static_cast<int>(abbcorr.type);
    _aberration.setReadOnly(true);
    addProperty(_aberration);
}

void RenderableShadowCylinder::initializeGL() {
    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_vbo);

    _shader = SpacecraftInstrumentsModule::ProgramObjectManager.request(
        "ShadowCylinderProgram",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "ShadowCylinderProgram",
                absPath(
                    "${MODULE_SPACECRAFTINSTRUMENTS}/shaders/terminatorshadow_vs.glsl"
                ),
                absPath(
                    "${MODULE_SPACECRAFTINSTRUMENTS}/shaders/terminatorshadow_fs.glsl"
                )
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);
}

void RenderableShadowCylinder::deinitializeGL() {
    SpacecraftInstrumentsModule::ProgramObjectManager.release(
        "ShadowCylinderProgram",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _shader = nullptr;

    glDeleteVertexArrays(1, &_vao);
    _vao = 0;
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;
}

bool RenderableShadowCylinder::isReady() const {
    return _shader;
}

void RenderableShadowCylinder::render(const RenderData& data, RendererTasks&) {
    glDepthMask(false);
    glDisable(GL_CULL_FACE);

    _shader->activate();

    // Model transform and view transform needs to be in double precision
    _shader->setUniform(
        _uniformCache.modelViewProjectionTransform,
        glm::mat4(calcModelViewProjectionTransform(data))
    );

    _shader->setUniform(_uniformCache.shadowColor, _shadowColor);
    _shader->setUniform(_uniformCache.opacity, opacity());

    glBindVertexArray(_vao);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, static_cast<GLsizei>(_vertices.size()));
    glBindVertexArray(0);

    _shader->deactivate();

    glDisable(GL_CULL_FACE);
    glDepthMask(true);
}

void RenderableShadowCylinder::update(const UpdateData& data) {
    _stateMatrix = SpiceManager::ref().positionTransformMatrix(
        _bodyFrame,
        "GALACTIC",
        data.time.j2000Seconds()
    );

    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);
    }
    createCylinder(data.time.j2000Seconds());
}

void RenderableShadowCylinder::createCylinder(double time) {
    SpiceManager::TerminatorEllipseResult res = SpiceManager::ref().terminatorEllipse(
        _body,
        _observer,
        _bodyFrame,
        _lightSource,
        static_cast<SpiceManager::TerminatorType>(_terminatorType.value()),
        {
            SpiceManager::AberrationCorrection::Type(_aberration.value()),
            SpiceManager::AberrationCorrection::Direction::Reception
        },
        time,
        _numberOfPoints
    );

    std::vector<glm::vec3> terminatorPoints;
    std::transform(
        res.terminatorPoints.begin(),
        res.terminatorPoints.end(),
        std::back_inserter(terminatorPoints),
        [](const glm::dvec3& p) { return p * 1000.0; }
    );

    double lt = 0.0;
    glm::dvec3 vecLightSource = SpiceManager::ref().targetPosition(
        _body,
        _lightSource,
        "GALACTIC",
        {
            SpiceManager::AberrationCorrection::Type(_aberration.value()),
            SpiceManager::AberrationCorrection::Direction::Reception
        },
        time,
        lt
    );

    vecLightSource = glm::inverse(_stateMatrix) * vecLightSource;

    vecLightSource *= _shadowLength;
    _vertices.clear();

    for (const glm::vec3& v : terminatorPoints) {
        _vertices.push_back({ v[0], v[1], v[2], 0.f });
        glm::vec3 f = v + glm::vec3(vecLightSource);
        _vertices.push_back({ f[0], f[1], f[2], 0.f });
    }
    _vertices.push_back(_vertices[0]);
    _vertices.push_back(_vertices[1]);

    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertices.size() * sizeof(CylinderVBOLayout),
        nullptr,
        GL_DYNAMIC_DRAW
    );
    glBufferSubData(
        GL_ARRAY_BUFFER,
        0,
        _vertices.size() * sizeof(CylinderVBOLayout),
        _vertices.data()
    );

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, nullptr);
    glBindVertexArray(0);
}

} // namespace openspace
