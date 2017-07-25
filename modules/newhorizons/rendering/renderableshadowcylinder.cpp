/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/newhorizons/rendering/renderableshadowcylinder.h>

#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/opengl/programobject.h>

namespace {
    const char* KeyMainFrame   = "MainFrame";

    static const openspace::properties::Property::PropertyInfo NumberPointsInfo = {
        "AmountOfPoints",
        "Points",
        "This value determines the number of control points that is used to construct "
        "the shadow geometry. The higher this number, the more detailed the shadow is, "
        "but it will have a negative impact on the performance."
    };

    static const openspace::properties::Property::PropertyInfo ShadowLengthInfo = {
        "ShadowLength",
        "Shadow Length",
        "This value determines the length of the shadow that is cast by the target "
        "object. The total distance of the shadow is equal to the distance from the "
        "target to the Sun multiplied with this value."
    };

    static const openspace::properties::Property::PropertyInfo ShadowColorInfo = {
        "ShadowColor",
        "Shadow Color",
        "This value determines the color that is used for the shadow cylinder."
    };

    static const openspace::properties::Property::PropertyInfo TerminatorTypeInfo = {
        "TerminatorType",
        "Terminator Type",
        "This value determines the type of the terminator that is used to calculate the "
        "shadow eclipse."
    };

    static const openspace::properties::Property::PropertyInfo LightSourceInfo = {
        "LightSource",
        "Light Source",
        "This value determines the SPICE name of the object that is used as the "
        "illuminator for computing the shadow cylinder."
    };

    static const openspace::properties::Property::PropertyInfo ObserverInfo = {
        "Observer",
        "Observer",
        "This value specifies the SPICE name of the object that is the observer of the "
        "shadow cylinder."
    };

    static const openspace::properties::Property::PropertyInfo BodyInfo = {
        "Body",
        "Target Body",
        "This value is the SPICE name of target body that is used as the shadow caster "
        "for the shadow cylinder."
    };

    static const openspace::properties::Property::PropertyInfo BodyFrameInfo = {
        "BodyFrame",
        "Body Frame",
        "This value is the SPICE name of the reference frame in which the shadow "
        "cylinder is expressed."
    };

    static const openspace::properties::Property::PropertyInfo AberrationInfo = {
        "Aberration",
        "Aberration",
        "This value determines the aberration method that is used to compute the shadow "
        "cylinder."
    };
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
                "",
                Optional::No
            },
            {
                NumberPointsInfo.identifier,
                new IntVerifier,
                NumberPointsInfo.description,
                Optional::Yes
            },
            {
                ShadowLengthInfo.identifier,
                new DoubleVerifier,
                ShadowLengthInfo.description,
                Optional::Yes
            },
            {
                ShadowColorInfo.identifier,
                new DoubleVector4Verifier,
                ShadowColorInfo.description,
                Optional::Yes
            },
            {
                TerminatorTypeInfo.identifier,
                new StringInListVerifier({
                    // Synchronized with SpiceManager::terminatorTypeFromString
                    "UMBRAL", "PENUMBRAL"
                }),
                TerminatorTypeInfo.description,
                Optional::No
            },
            {
                LightSourceInfo.identifier,
                new StringVerifier,
                LightSourceInfo.description,
                Optional::No
            },
            {
                ObserverInfo.identifier,
                new StringVerifier,
                ObserverInfo.description,
                Optional::No
            },
            {
                BodyInfo.identifier,
                new StringVerifier,
                BodyInfo.description,
                Optional::No
            },
            {
                BodyFrameInfo.identifier,
                new StringVerifier,
                BodyFrameInfo.description,
                Optional::No
            },
            {
                AberrationInfo.identifier,
                new StringInListVerifier({
                    // SpiceManager::AberrationCorrection::AberrationCorrection
                    "NONE", "LT", "LT+S", "CN", "CN+S"
                }),
        AberrationInfo.description,
                Optional::No
            },
        },
        Exhaustive::Yes
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
    , _mainFrame({"mainFrame", "Main Frame", ""}) // @TODO Remove this
    , _aberration(AberrationInfo)
    , _shader(nullptr)
    , _vao(0)
    , _vbo(0)
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
    _mainFrame = dictionary.value<std::string>(KeyMainFrame);
    

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

bool RenderableShadowCylinder::initialize() {
    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_vbo);

    RenderEngine& renderEngine = OsEng.renderEngine();
    _shader = renderEngine.buildRenderProgram(
        "ShadowCylinderProgram",
        "${MODULE_NEWHORIZONS}/shaders/terminatorshadow_vs.glsl",
        "${MODULE_NEWHORIZONS}/shaders/terminatorshadow_fs.glsl"
    );

    return true;
}

bool RenderableShadowCylinder::deinitialize() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }

    glDeleteVertexArrays(1, &_vao);
    _vao = 0;
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;

    return true;
}

bool RenderableShadowCylinder::isReady() const {
    return true;
}
    
void RenderableShadowCylinder::render(const RenderData& data, RendererTasks&) {
    glDepthMask(false);
    _shader->activate();

    // Model transform and view transform needs to be in double precision
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _shader->setUniform("modelViewProjectionTransform",
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform));

    //_shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    //_shader->setUniform("ModelTransform", glm::mat4(_stateMatrix));


    _shader->setUniform("shadowColor", _shadowColor);
    //setPscUniforms(*_shader.get(), data.camera, data.position);
    
    glBindVertexArray(_vao);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, static_cast<GLsizei>(_vertices.size()));
    glBindVertexArray(0);

    _shader->deactivate();

    glDepthMask(true);
}

void RenderableShadowCylinder::update(const UpdateData& data) {
    _stateMatrix = SpiceManager::ref().positionTransformMatrix(
        _bodyFrame,
        _mainFrame,
        data.time.j2000Seconds()
    );
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
    }
    createCylinder(data.time.j2000Seconds());
}

glm::vec4 psc_addition(glm::vec4 v1, glm::vec4 v2) {
    float k = 10.f;
    float ds = v2.w - v1.w;
    if (ds >= 0) {
        float p = pow(k, -ds);
        return glm::vec4(v1.x*p + v2.x, v1.y*p + v2.y, v1.z*p + v2.z, v2.w);
    }
    else {
        float p = pow(k, ds);
        return glm::vec4(v1.x + v2.x*p, v1.y + v2.y*p, v1.z + v2.z*p, v1.w);
    }
}

void RenderableShadowCylinder::createCylinder(double time) {
    auto res = SpiceManager::ref().terminatorEllipse(
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
            PowerScaledCoordinate psc = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
            psc[3] += 3;
            return psc;
        }
    );
    
    double lt;
    glm::dvec3 vecLightSource = SpiceManager::ref().targetPosition(
        _body,
        _lightSource,
        _mainFrame,
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

    psc endpoint = psc::CreatePowerScaledCoordinate(vecLightSource.x, vecLightSource.y, vecLightSource.z);
    for (const auto& v : terminatorPoints) {
        _vertices.push_back({ v[0], v[1], v[2], v[3] });
        glm::vec4 f = psc_addition(v.vec4(), endpoint.vec4());
        _vertices.push_back({ f[0], f[1], f[2], f[3] });
    }
    _vertices.push_back(_vertices[0]);
    _vertices.push_back(_vertices[1]);

    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(GL_ARRAY_BUFFER, _vertices.size() * sizeof(CylinderVBOLayout), NULL, GL_DYNAMIC_DRAW);
    glBufferSubData(GL_ARRAY_BUFFER, 0, _vertices.size() * sizeof(CylinderVBOLayout), &_vertices[0]);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, 0);
    glBindVertexArray(0);
}

} // namespace openspace
