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

#include <modules/spacecraftinstruments/rendering/renderableshadowcylinder.h>

#include <modules/spacecraftinstruments/spacecraftinstrumentsmodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/filesystem/filesystem.h>
#include <openspace/misc/dictionary.h>
#include <openspace/opengl/programobject.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>
#include <algorithm>
#include <iterator>
#include <memory>

namespace {
    using namespace openspace;

    constexpr IntPropertyInfo NumberPointsInfo = {
        .identifier = "NumberOfPoints",
        .name = "Points",
        .description = "The number of control points used for constructing the shadow "
            "geometry. The higher this number, the more detailed the shadow is, but it "
            "will have a negative impact on the performance.",
        .visibility = Visibility::AdvancedUser,
        .defaults = {
            .value = 190,
            .minMax = { 1, 300 }
        }
    };

    constexpr FloatPropertyInfo ShadowLengthInfo = {
        .identifier = "ShadowLength",
        .name = "Shadow length",
        .description = "A factor that controls the length of the shadow that is cast by "
            "the target object. The total length of the shadow is equal to the distance "
            "from the target to the light source multiplied with this value.",
        .visibility = Visibility::AdvancedUser,
        .defaults = {
            .value = 0.1f,
            .minMax = { 0.f, 0.5f }
        }
    };

    constexpr Vec3PropertyInfo ShadowColorInfo = {
        .identifier = "ShadowColor",
        .name = "Shadow color",
        .description = "The color used for the shadow cylinder.",
        .visibility = Visibility::User,
        .defaults = {
            .value = glm::vec3(1.f),
            .minMax = { glm::vec3(0.f), glm::vec3(1.f) }
        }
    };

    constexpr OptionPropertyInfo TerminatorTypeInfo = {
        .identifier = "TerminatorType",
        .name = "Terminator type",
        .description = "Determines the type of terminator to use for calculating the "
            "shadow eclipse.",
        .visibility = Visibility::AdvancedUser
    };

    constexpr StringPropertyInfo LightSourceInfo = {
        .identifier = "LightSource",
        .name = "Light source",
        .description = "The SPICE name of the object that is used as the illuminator for "
            "computing the shadow cylinder.",
        .visibility = Visibility::AdvancedUser
    };

    constexpr StringPropertyInfo ObserverInfo = {
        .identifier = "Observer",
        .name = "Observer",
        .description = "The SPICE name of the object that is the observer.",
        .visibility = Visibility::AdvancedUser
    };

    constexpr StringPropertyInfo BodyInfo = {
        .identifier = "Body",
        .name = "Target body",
        .description = "The SPICE name of target body that is used as the shadow caster.",
        .visibility = Visibility::AdvancedUser
    };

    constexpr StringPropertyInfo BodyFrameInfo = {
        .identifier = "BodyFrame",
        .name = "Body frame",
        .description = "The SPICE name of the reference frame in which the shadow "
            "cylinder is expressed.",
        .visibility = Visibility::AdvancedUser
    };

    constexpr OptionPropertyInfo AberrationInfo = {
        .identifier = "Aberration",
        .name = "Aberration",
        .description = "The aberration method that is used for computing the shadow "
            "cylinder. The options are \"NONE\", \"LT\" (Light Time), \"LT + S\" (Light "
            "Time Stellar), \"CN\" (Converged Newtonian), and \"CN + S\" (Converged "
            "Newtonian Stellar).",
        .visibility = Visibility::AdvancedUser
    };

    // Displays a shadow cylinder behind a planetary body. Given the SPICE name of a
    // planetary body and an observer, it will show a cylinder extending behind the body
    // away from the observer to highlight the areas of space from which the observer is
    // occluded by the body. A concrete example is using the Sun as the observer, in which
    // case the shadow cylinder indicates the areas in which there is darkness.
    struct [[codegen::Dictionary(RenderableShadowCylinder)]] Parameters {
        // [[codegen::verbatim(NumberPointsInfo.description)]]
        std::optional<int> numberOfPoints [[codegen::greaterequal(0)]];

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
        std::string lightSource [[codegen::identifier()]];

        // [[codegen::verbatim(ObserverInfo.description)]]
        std::string observer;

        // [[codegen::verbatim(BodyInfo.description)]]
        std::string body;

        // [[codegen::verbatim(BodyFrameInfo.description)]]
        std::string bodyFrame;

        // [[codegen::verbatim(AberrationInfo.description)]]
        std::string aberration [[codegen::inlist("NONE", "LT", "LT+S", "CN", "CN+S")]];
    };
} // namespace
#include "renderableshadowcylinder_codegen.cpp"

namespace openspace {

Documentation RenderableShadowCylinder::Documentation() {
    return codegen::doc<Parameters>(
        "spacecraftinstruments_renderable_shadowcylinder",
        Renderable::Documentation()
    );
}

RenderableShadowCylinder::RenderableShadowCylinder(const Dictionary& dictionary)
    : Renderable(dictionary)
    , _numberOfPoints(NumberPointsInfo)
    , _shadowLength(ShadowLengthInfo)
    , _shadowColor(ShadowColorInfo)
    , _terminatorType(TerminatorTypeInfo)
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
    _shadowColor.setViewOption(Property::ViewOptions::Color);
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
        { static_cast<int>(T::LightTimeStellar), "Light Time Stellar" }
    });
    const SpiceManager::AberrationCorrection abbcorr = SpiceManager::AberrationCorrection(
        p.aberration
    );
    _aberration = static_cast<int>(abbcorr.type);
    _aberration.setReadOnly(true);
    addProperty(_aberration);
}

void RenderableShadowCylinder::initializeGL() {
    glCreateBuffers(1, &_vbo);
    glCreateVertexArrays(1, &_vao);
    glVertexArrayVertexBuffer(_vao, 0, _vbo, 0, sizeof(CylinderVBOLayout));

    glEnableVertexArrayAttrib(_vao, 0);
    glVertexArrayAttribFormat(_vao, 0, 4, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_vao, 0, 0);

    _shader = SpacecraftInstrumentsModule::ProgramObjectManager.request(
        "ShadowCylinderProgram",
        []() -> std::unique_ptr<opengl::ProgramObject> {
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

    opengl::updateUniformLocations(*_shader, _uniformCache);
}

void RenderableShadowCylinder::deinitializeGL() {
    SpacecraftInstrumentsModule::ProgramObjectManager.release(
        "ShadowCylinderProgram",
        [](opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _shader = nullptr;

    glDeleteVertexArrays(1, &_vao);
    glDeleteBuffers(1, &_vbo);
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
    glDrawArrays(GL_TRIANGLE_STRIP, 0, _count);
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
        opengl::updateUniformLocations(*_shader, _uniformCache);
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

    vecLightSource *= _shadowLength.value();

    std::vector<CylinderVBOLayout> vertices;
    vertices.reserve(terminatorPoints.size() * 2 + 2);

    for (const glm::vec3& v : terminatorPoints) {
        vertices.push_back({ v[0], v[1], v[2], 0.f });
        glm::vec3 f = v + glm::vec3(vecLightSource);
        vertices.push_back({ f[0], f[1], f[2], 0.f });
    }
    vertices.push_back(vertices[0]);
    vertices.push_back(vertices[1]);

    glNamedBufferData(
        _vbo,
        vertices.size() * sizeof(CylinderVBOLayout),
        vertices.data(),
        GL_DYNAMIC_DRAW
    );

    _count = static_cast<GLsizei>(vertices.size());
}

} // namespace openspace
