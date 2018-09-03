/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>
#include <openspace/util/spicemanager.h>

namespace {
    constexpr const char* ProgramName = "ShadowCylinderProgram";
    constexpr const char* MainFrame = "GALACTIC";

    constexpr const std::array<const char*, 2> UniformNames = {
        "modelViewProjectionTransform", "shadowColor"
    };

    constexpr openspace::properties::Property::PropertyInfo NumberPointsInfo = {
        "AmountOfPoints",
        "Points",
        "This value determines the number of control points that is used to construct "
        "the shadow geometry. The higher this number, the more detailed the shadow is, "
        "but it will have a negative impact on the performance."
    };

    constexpr openspace::properties::Property::PropertyInfo ShadowLengthInfo = {
        "ShadowLength",
        "Shadow Length",
        "This value determines the length of the shadow that is cast by the target "
        "object. The total distance of the shadow is equal to the distance from the "
        "target to the Sun multiplied with this value."
    };

    constexpr openspace::properties::Property::PropertyInfo ShadowColorInfo = {
        "ShadowColor",
        "Shadow Color",
        "This value determines the color that is used for the shadow cylinder."
    };

    constexpr openspace::properties::Property::PropertyInfo TerminatorTypeInfo = {
        "TerminatorType",
        "Terminator Type",
        "This value determines the type of the terminator that is used to calculate the "
        "shadow eclipse."
    };

    constexpr openspace::properties::Property::PropertyInfo LightSourceInfo = {
        "LightSource",
        "Light Source",
        "This value determines the SPICE name of the object that is used as the "
        "illuminator for computing the shadow cylinder."
    };

    constexpr openspace::properties::Property::PropertyInfo ObserverInfo = {
        "Observer",
        "Observer",
        "This value specifies the SPICE name of the object that is the observer of the "
        "shadow cylinder."
    };

    constexpr openspace::properties::Property::PropertyInfo BodyInfo = {
        "Body",
        "Target Body",
        "This value is the SPICE name of target body that is used as the shadow caster "
        "for the shadow cylinder."
    };

    constexpr openspace::properties::Property::PropertyInfo BodyFrameInfo = {
        "BodyFrame",
        "Body Frame",
        "This value is the SPICE name of the reference frame in which the shadow "
        "cylinder is expressed."
    };

    constexpr openspace::properties::Property::PropertyInfo AberrationInfo = {
        "Aberration",
        "Aberration",
        "This value determines the aberration method that is used to compute the shadow "
        "cylinder."
    };

    glm::vec4 psc_addition(glm::vec4 v1, glm::vec4 v2) {
        const float k = 10.f;
        const float ds = v2.w - v1.w;
        if (ds >= 0) {
            float p = pow(k, -ds);
            return glm::vec4(v1.x*p + v2.x, v1.y*p + v2.y, v1.z*p + v2.z, v2.w);
        }
        else {
            float p = pow(k, ds);
            return glm::vec4(v1.x + v2.x*p, v1.y + v2.y*p, v1.z + v2.z*p, v1.w);
        }
    }
} // namespace

namespace openspace {

documentation::Documentation RenderableShadowCylinder::Documentation() {
    using namespace documentation;
    return {
        "RenderableShadowCylinder",
        "newhorizons_renderable_shadowcylinder",
        {
            {
                "Type",
                new StringEqualVerifier("RenderableShadowCylinder"),
                Optional::No,
                ""
            },
            {
                NumberPointsInfo.identifier,
                new IntVerifier,
                Optional::Yes,
                NumberPointsInfo.description
            },
            {
                ShadowLengthInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                ShadowLengthInfo.description
            },
            {
                ShadowColorInfo.identifier,
                new DoubleVector4Verifier,
                Optional::Yes,
                ShadowColorInfo.description
            },
            {
                TerminatorTypeInfo.identifier,
                new StringInListVerifier({
                    // Synchronized with SpiceManager::terminatorTypeFromString
                    "UMBRAL", "PENUMBRAL"
                }),
                Optional::No,
                TerminatorTypeInfo.description
            },
            {
                LightSourceInfo.identifier,
                new StringVerifier,
                Optional::No,
                LightSourceInfo.description
            },
            {
                ObserverInfo.identifier,
                new StringVerifier,
                Optional::No,
                ObserverInfo.description
            },
            {
                BodyInfo.identifier,
                new StringVerifier,
                Optional::No,
                BodyInfo.description
            },
            {
                BodyFrameInfo.identifier,
                new StringVerifier,
                Optional::No,
                BodyFrameInfo.description
            },
            {
                AberrationInfo.identifier,
                new StringInListVerifier({
                    // SpiceManager::AberrationCorrection::AberrationCorrection
                    "NONE", "LT", "LT+S", "CN", "CN+S"
                }),
                Optional::No,
                AberrationInfo.description
            },
        }
    };
}

RenderableShadowCylinder::RenderableShadowCylinder(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _numberOfPoints(NumberPointsInfo, 190, 1, 300)
    , _shadowLength(ShadowLengthInfo, 0.1f, 0.f, 0.5f)
    , _shadowColor(
        ShadowColorInfo,
        glm::vec4(1.f, 1.f, 1.f, 0.25f),
        glm::vec4(0.f), glm::vec4(1.f)
    )
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
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableShadowCylinder"
    );

    if (dictionary.hasKey(NumberPointsInfo.identifier)) {
        _numberOfPoints = static_cast<int>(
            dictionary.value<double>(NumberPointsInfo.identifier)
        );
    }
    addProperty(_numberOfPoints);


    if (dictionary.hasKey(ShadowLengthInfo.identifier)) {
        _shadowLength = static_cast<float>(
            dictionary.value<double>(ShadowLengthInfo.identifier)
        );
    }
    addProperty(_shadowLength);


    if (dictionary.hasKey(ShadowColorInfo.identifier)) {
        _shadowColor = dictionary.value<glm::vec4>(ShadowLengthInfo.identifier);
    }
    _shadowColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_shadowColor);


    _terminatorType.addOptions({
        { static_cast<int>(SpiceManager::TerminatorType::Umbral), "Umbral" },
        { static_cast<int>(SpiceManager::TerminatorType::Penumbral), "Penumbral" }
    });
    _terminatorType = static_cast<int>(SpiceManager::terminatorTypeFromString(
        dictionary.value<std::string>(TerminatorTypeInfo.identifier)
    ));
    addProperty(_terminatorType);


    _lightSource = dictionary.value<std::string>(LightSourceInfo.identifier);
    _observer = dictionary.value<std::string>(ObserverInfo.identifier);
    _body = dictionary.value<std::string>(BodyInfo.identifier);
    _bodyFrame = dictionary.value<std::string>(BodyFrameInfo.identifier);

    using T = SpiceManager::AberrationCorrection::Type;
    _aberration.addOptions({
        { static_cast<int>(T::None), "None" },
        { static_cast<int>(T::ConvergedNewtonian), "Converged Newtonian" },
        { static_cast<int>(T::ConvergedNewtonianStellar), "Converged Newtonian Stellar" },
        { static_cast<int>(T::LightTime), "Light Time" },
        { static_cast<int>(T::LightTimeStellar), "Light Time Stellar" },

    });
    SpiceManager::AberrationCorrection aberration = SpiceManager::AberrationCorrection(
        dictionary.value<std::string>(AberrationInfo.identifier)
    );
    _aberration = static_cast<int>(aberration.type);
}

void RenderableShadowCylinder::initializeGL() {
    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_vbo);

    _shader = SpacecraftInstrumentsModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine.buildRenderProgram(
                ProgramName,
                absPath(
                    "${MODULE_SPACECRAFTINSTRUMENTS}/shaders/terminatorshadow_vs.glsl"
                ),
                absPath(
                    "${MODULE_SPACECRAFTINSTRUMENTS}/shaders/terminatorshadow_fs.glsl"
                )
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
}

void RenderableShadowCylinder::deinitializeGL() {
    SpacecraftInstrumentsModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );
    _shader = nullptr;

    glDeleteVertexArrays(1, &_vao);
    _vao = 0;
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;
}

bool RenderableShadowCylinder::isReady() const {
    return true;
}

void RenderableShadowCylinder::render(const RenderData& data, RendererTasks&) {
    glDepthMask(false);
    _shader->activate();

    // Model transform and view transform needs to be in double precision
    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _shader->setUniform(
        _uniformCache.modelViewProjectionTransform,
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform)
    );

    _shader->setUniform(_uniformCache.shadowColor, _shadowColor);

    glBindVertexArray(_vao);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, static_cast<GLsizei>(_vertices.size()));
    glBindVertexArray(0);

    _shader->deactivate();

    glDepthMask(true);
}

void RenderableShadowCylinder::update(const UpdateData& data) {
    _stateMatrix = SpiceManager::ref().positionTransformMatrix(
        _bodyFrame,
        MainFrame,
        data.time.j2000Seconds()
    );

    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
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

    std::vector<psc> terminatorPoints;
    std::transform(
        res.terminatorPoints.begin(),
        res.terminatorPoints.end(),
        std::back_inserter(terminatorPoints),
        [](const glm::dvec3& p) {
            psc coord = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
            coord[3] += 3;
            return coord;
        }
    );

    double lt;
    glm::dvec3 vecLightSource = SpiceManager::ref().targetPosition(
        _body,
        _lightSource,
        MainFrame,
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

    const psc endpoint = psc::CreatePowerScaledCoordinate(
        vecLightSource.x,
        vecLightSource.y,
        vecLightSource.z
    );
    for (const psc& v : terminatorPoints) {
        _vertices.push_back({ v[0], v[1], v[2], v[3] });
        glm::vec4 f = psc_addition(v.vec4(), endpoint.vec4());
        _vertices.push_back({ f[0], f[1], f[2], f[3] });
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
        &_vertices[0]
    );

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, nullptr);
    glBindVertexArray(0);
}

} // namespace openspace
