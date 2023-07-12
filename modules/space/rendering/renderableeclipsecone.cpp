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

#include <modules/space/rendering/renderableeclipsecone.h>

#include <modules/spacecraftinstruments/spacecraftinstrumentsmodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>

namespace {
    constexpr std::array<const char*, 3> UniformNames = {
        "modelViewProjectionTransform", "shadowColor", "opacity"
    };

    constexpr openspace::properties::Property::PropertyInfo NumberPointsInfo = {
        "AmountOfPoints",
        "Points",
        "This value determines the number of control points that is used to construct "
        "the shadow geometry. The higher this number, the more detailed the shadow is, "
        "but it will have a negative impact on the performance",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShadowLengthInfo = {
        "ShadowLength",
        "Shadow Length",
        "This value determines the length of the shadow that is cast by the target "
        "object. The total distance of the shadow is equal to the distance from the "
        "target to the Sun multiplied with this value",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShadowColorInfo = {
        "ShadowColor",
        "Shadow Color",
        "This value determines the color that is used for the shadow cylinder",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LightSourceInfo = {
        "LightSource",
        "Light Source",
        "This value determines the SPICE name of the object that is used as the "
        "illuminator for computing the shadow cylinder",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo LightSourceFrameInfo = {
        "LightSourceFrame",
        "Light Source Frame",
        "This value is the SPICE name of the body-fixed reference frame for the light "
        "source",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShadowerInfo = {
        "Shadower",
        "Shadower",
        "This value specifies the SPICE name of the object that is casting the shadow on "
        "the shadowee",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShadowerFrameInfo = {
        "ShadowerFrame",
        "Shadower Frame",
        "This value is the SPICE name of the body-fixed reference frame for the shadower",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShadoweeInfo = {
        "Shadowee",
        "Shadowee",
        "This value is the SPICE name of object that is receiving the shadow from the "
        "shadower",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableEclipseCone)]] Parameters {
        // [[codegen::verbatim(NumberPointsInfo.description)]]
        std::optional<int> numberOfPoints [[codegen::key("AmountOfPoints")]];

        // [[codegen::verbatim(ShadowLengthInfo.description)]]
        std::optional<float> shadowLength;

        // [[codegen::verbatim(ShadowColorInfo.description)]]
        std::optional<glm::vec3> shadowColor [[codegen::color()]];

        // [[codegen::verbatim(LightSourceInfo.description)]]
        std::string lightSource;

        // [[codegen::verbatim(LightSourceFrameInfo.description)]]
        std::string lightSourceFrame;

        // [[codegen::verbatim(ShadowerInfo.description)]]
        std::string shadower;

        // [[codegen::verbatim(ShadowerFrameInfo.description)]]
        std::string shadowerFrame;

        // [[codegen::verbatim(ShadoweeInfo.description)]]
        std::string shadowee;
    };
#include "renderableeclipsecone_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableEclipseCone::Documentation() {
    return codegen::doc<Parameters>("space_renderableeclipsecone");
}

RenderableEclipseCone::RenderableEclipseCone(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _numberOfPoints(NumberPointsInfo, 190, 1, 300)
    , _shadowLength(ShadowLengthInfo, 0.1f, 0.f, 2.f)
    , _shadowColor(ShadowColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _lightSource(LightSourceInfo)
    , _lightSourceFrame(LightSourceFrameInfo)
    , _shadower(ShadowerInfo)
    , _shadowerFrame(ShadowerFrameInfo)
    , _shadowee(ShadoweeInfo)
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


    _lightSource = p.lightSource;
    _lightSourceFrame = p.lightSourceFrame;
    _shadower = p.shadower;
    _shadowee = p.shadowee;
    _shadowerFrame = p.shadowerFrame;
}

void RenderableEclipseCone::initializeGL() {
    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_vbo);

    _shader = SpacecraftInstrumentsModule::ProgramObjectManager.request(
        "ShadowCylinderProgram",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "ShadowCylinderProgram",
                absPath("${MODULE_SPACE}/shaders/eclipsecone_vs.glsl"),
                absPath("${MODULE_SPACE}/shaders/eclipsecone_fs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
}

void RenderableEclipseCone::deinitializeGL() {
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

bool RenderableEclipseCone::isReady() const {
    return _shader;
}

void RenderableEclipseCone::render(const RenderData& data, RendererTasks&) {
    glDepthMask(false);
    glDisable(GL_CULL_FACE);

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
    _shader->setUniform(_uniformCache.opacity, opacity());

    glBindVertexArray(_vao);
    //glDrawArrays(GL_TRIANGLE_STRIP, 0, static_cast<GLsizei>(_vertices.size()));
    glDrawArrays(GL_LINES, 0, _numberOfPoints * 2);
    glBindVertexArray(0);

    _shader->deactivate();

    glDisable(GL_CULL_FACE);
    glDepthMask(true);
}

void RenderableEclipseCone::update(const UpdateData& data) {
    //_stateMatrix = SpiceManager::ref().positionTransformMatrix(
    //    _shadowerFrame,
    //    "GALACTIC",
    //    data.time.j2000Seconds()
    //);

    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
    }
    createCone(data.time.j2000Seconds());
}

void RenderableEclipseCone::createCone(double et) {
    // Big picture for the calculation for this example (lightSource = Sun,
    // shadower = Moon, shadowee = Earth). We get the limb (= penumbral terminator) of the
    // Sun as viewed from the Moon, then the limb of the Moon as viewed from the Sun.
    // The umbral shadow cone is constructed by connecting the points of the limbs in
    // order. The penumbral shadow cone is constructed by connecting them 180 deg out of
    // phase (meaning top to bottom). We want the cone to eminate from the shadower, so
    // we take the distance from the shadower to the shadowee and use that as a scale for
    // the resulting vectors we get (also including the _shadowLength) as an additional
    // scale factor

    // 1. Get the penumbral terminator of the lightsource from the view of the shadower
    SpiceManager::TerminatorEllipseResult resSrc = SpiceManager::ref().terminatorEllipse(
        _lightSource,
        _shadowee, // The actual value of this doesn't matter
        _lightSourceFrame,
        _shadower,
        SpiceManager::TerminatorType::Penumbral,
        {
            SpiceManager::AberrationCorrection::Type::None,
            SpiceManager::AberrationCorrection::Direction::Reception
        },
        et,
        _numberOfPoints
    );


    // 2. Get the penumbral terminator of the shadower from the lightsource
    SpiceManager::TerminatorEllipseResult resDst = SpiceManager::ref().terminatorEllipse(
        _shadower,
        _shadowee, // The actual value of this doesn't matter
        _shadowerFrame,
        _lightSource,
        SpiceManager::TerminatorType::Penumbral,
        {
            SpiceManager::AberrationCorrection::Type::None,
            SpiceManager::AberrationCorrection::Direction::Reception
        },
        et,
        _numberOfPoints
    );

    ghoul_assert(
        resSrc.terminatorPoints.size() == resDst.terminatorPoints.size(),
        "Inconsistent number of terminator points retrieved"
    );


    // 3. Get the distance from the shadower to the shadowee
    glm::dvec3 diff = SpiceManager::ref().targetPosition(
        _shadowee,
        _shadower,
        "GALACTIC",
        {
            SpiceManager::AberrationCorrection::Type::None,
            SpiceManager::AberrationCorrection::Direction::Reception
        },
        et
    );
    const double distance = glm::length(diff) * 1000.0; // to KM


    // 4. Construct the umbral shadow
    const glm::dvec3 shadowerToLightSource = SpiceManager::ref().targetPosition(
        _lightSource,
        _shadower,
        _shadowerFrame,
        {
            SpiceManager::AberrationCorrection::Type::None,
            SpiceManager::AberrationCorrection::Direction::Reception
        },
        et
    );
    glm::dmat3 lightSourceToShadower = SpiceManager::ref().frameTransformationMatrix(
        _lightSourceFrame, _shadowerFrame, et
    );

    std::vector<VBOLayout> vertices;
    for (size_t i = 0; i < resDst.terminatorPoints.size(); i++) {
        const glm::dvec3 src = lightSourceToShadower * resSrc.terminatorPoints[i] + shadowerToLightSource;
        const glm::dvec3 dst = resDst.terminatorPoints[i];
        const glm::dvec3 dir = glm::normalize(dst - src);

        //glm::vec3 p1 = src * 1000.0;
        glm::vec3 p1 = dst * 1000.0;
        vertices.push_back({ p1.x, p1.y, p1.z });

        glm::vec3 p2 = dst * 1000.0 + dir * distance * static_cast<double>(_shadowLength);
        //glm::vec3 p2 = glm::vec3(0.f, 0.f, 0.f);
        vertices.push_back({ p2.x, p2.y, p2.z });
    }

    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        vertices.size() * sizeof(VBOLayout),
        nullptr,
        GL_DYNAMIC_DRAW
    );
    glBufferSubData(
        GL_ARRAY_BUFFER,
        0,
        vertices.size() * sizeof(VBOLayout),
        vertices.data()
    );

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);
    glBindVertexArray(0);
}

} // namespace openspace
