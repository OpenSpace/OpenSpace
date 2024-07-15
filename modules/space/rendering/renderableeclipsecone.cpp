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

    struct VBOLayout {
        float x = 0.f;
        float y = 0.f;
        float z = 0.f;
    };

    constexpr openspace::properties::Property::PropertyInfo NumberPointsInfo = {
        "NumberOfPoints",
        "Points",
        "The number of control points used for constructing the shadow geometry. The "
        "higher this number, the more detailed the shadow is. However, it will have a "
        "negative impact on the performance. Also note that rendering errors will occur "
        "if this value is an even number.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShadowLengthInfo = {
        "ShadowLength",
        "Shadow Length",
        "A factor that controls the length of the rendered shadow cone. The total length "
        "will be the distance from the shadower to the shadowee multiplied by this "
        "value.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShowUmbralShadowInfo = {
        "ShowUmbralShadow",
        "Show Umbral Shadow",
        "Decides whether the umbral portion of the shadow should be shown.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo UmbralShadowColorInfo = {
        "UmbralShadowColor",
        "Umbral Shadow Color",
        "The color for the shadow cylinder that represents the umbral shadow.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ShowPenumbralShadowInfo = {
        "ShowPenumbralShadow",
        "Show Penumbral Shadow",
        "Decides whether the penumbral portion of the shadow should be shown.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo PenumbralShadowColorInfo = {
        "PenumbralShadowColor",
        "Penumbral Shadow Color",
        "The color for the shadow cylinder that represents the penumbral shadow.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LightSourceInfo = {
        "LightSource",
        "Light Source",
        "The SPICE name of the object that is used as the illuminator when computing the "
        "shadow cylinder.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo LightSourceFrameInfo = {
        "LightSourceFrame",
        "Light Source Frame",
        "The SPICE name of the body-fixed reference frame for the light source.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShadowerInfo = {
        "Shadower",
        "Shadower",
        "The SPICE name of the object that is casting the shadow on the shadowee.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShadowerFrameInfo = {
        "ShadowerFrame",
        "Shadower Frame",
        "The SPICE name of the body-fixed reference frame for the shadower.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShadoweeInfo = {
        "Shadowee",
        "Shadowee",
        "The SPICE name of object that is receiving the shadow from the shadower.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableEclipseCone)]] Parameters {
        // [[codegen::verbatim(NumberPointsInfo.description)]]
        std::optional<int> numberOfPoints;

        // [[codegen::verbatim(ShadowLengthInfo.description)]]
        std::optional<float> shadowLength;

        // [[codegen::verbatim(ShowUmbralShadowInfo.description)]]
        std::optional<bool> showUmbralShadow;

        // [[codegen::verbatim(UmbralShadowColorInfo.description)]]
        std::optional<glm::vec4> umbralShadowColor [[codegen::color()]];

        // [[codegen::verbatim(ShowPenumbralShadowInfo.description)]]
        std::optional<bool> showPenumbralShadow;

        // [[codegen::verbatim(PenumbralShadowColorInfo.description)]]
        std::optional<glm::vec4> penumbralShadowColor [[codegen::color()]];

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
    , _numberOfPoints(NumberPointsInfo, 191, 1, 300)
    , _shadowLength(ShadowLengthInfo, 0.1f, 0.f, 2.f)
    , _showUmbralShadow(ShowUmbralShadowInfo, true)
    , _umbralShadowColor(
        UmbralShadowColorInfo,
        glm::vec4(1.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _showPenumbralShadow(ShowPenumbralShadowInfo, true)
    , _penumbralShadowColor(
        PenumbralShadowColorInfo,
        glm::vec4(1.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _lightSource(LightSourceInfo)
    , _lightSourceFrame(LightSourceFrameInfo)
    , _shadower(ShadowerInfo)
    , _shadowerFrame(ShadowerFrameInfo)
    , _shadowee(ShadoweeInfo)
    //, _test({"ABC", "ABC", ""}, 1, 0, 380)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    //addProperty(_test);
    addProperty(Fadeable::_opacity);

    _numberOfPoints = p.numberOfPoints.value_or(_numberOfPoints);
    addProperty(_numberOfPoints);

    _shadowLength = p.shadowLength.value_or(_shadowLength);
    addProperty(_shadowLength);

    _showUmbralShadow = p.showUmbralShadow.value_or(_showUmbralShadow);
    addProperty(_showUmbralShadow);
    _umbralShadowColor = p.umbralShadowColor.value_or(_umbralShadowColor);
    _umbralShadowColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_umbralShadowColor);

    _showPenumbralShadow = p.showPenumbralShadow.value_or(_showPenumbralShadow);
    addProperty(_showPenumbralShadow);
    _penumbralShadowColor = p.penumbralShadowColor.value_or(_penumbralShadowColor);
    _penumbralShadowColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_penumbralShadowColor);

    _lightSource = p.lightSource;
    _lightSourceFrame = p.lightSourceFrame;
    _shadower = p.shadower;
    _shadowee = p.shadowee;
    _shadowerFrame = p.shadowerFrame;

    setRenderBin(RenderBin::PostDeferredTransparent);
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
    const glm::dmat4 modelViewProjectionTransform =
        calcModelViewProjectionTransform(data);

    _shader->setUniform(
        _uniformCache.modelViewProjectionTransform,
        glm::mat4(modelViewProjectionTransform)
    );

    _shader->setUniform(_uniformCache.opacity, opacity());

    glBindVertexArray(_vao);
    if (_showUmbralShadow) {
        _shader->setUniform(_uniformCache.shadowColor, _umbralShadowColor);
        glDrawArrays(GL_TRIANGLE_STRIP, 0, _nVertices);
    }
    if (_showPenumbralShadow) {
        // The shadow vertices live in the same VBO so the start index might be offset
        const int startIndex = _showUmbralShadow ? _nVertices : 0;
        _shader->setUniform(_uniformCache.shadowColor, _penumbralShadowColor);
        glDrawArrays(GL_TRIANGLE_STRIP, startIndex, _nVertices);
    }
    glBindVertexArray(0);

    _shader->deactivate();

    glDisable(GL_CULL_FACE);
    glDepthMask(true);
}

void RenderableEclipseCone::update(const UpdateData& data) {
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
    }
    createCone(data.time.j2000Seconds());
}

std::vector<VBOLayout> calculateShadowPoints(const std::vector<glm::dvec3>& srcTerminator,
                                             const std::vector<glm::dvec3>& dstTerminator,
                                             const glm::dvec3& shadowerToLightSource,
                                             const glm::dmat3& lightSourceToShadower,
                                             double lengthScale)
{
    ghoul_assert(srcTerminator.size() == dstTerminator.size(), "Unmatched termiator pts");

    std::vector<VBOLayout> vertices;
    vertices.reserve(dstTerminator.size() * 2);
    for (size_t i = 0; i < dstTerminator.size(); i++) {
        // Convert the terminator points from the reference frame of the Sun to the
        // reference frame of the Moon
        const glm::dvec3 src =
            lightSourceToShadower * srcTerminator[i] + shadowerToLightSource;
        const glm::dvec3& dst = dstTerminator[i];
        const glm::dvec3 dir = glm::normalize(dst - src);

        // The start point is the terminator point on the Moon
        const glm::vec3 p1 = dst;
        vertices.push_back({ p1.x, p1.y, p1.z });

        // The end point is calculated by forward propagating the incoming direction
        const glm::vec3 p2 = dst + dir * lengthScale;
        vertices.push_back({ p2.x, p2.y, p2.z });
    }
    return vertices;
}

void RenderableEclipseCone::createCone(double et) {
    ZoneScoped;

    // Big picture for the calculation for this example (lightSource = Sun,
    // shadower = Moon, shadowee = Earth). We get the limb (= penumbral terminator) of the
    // Sun as viewed from the Moon, then the limb of the Moon as viewed from the Sun.
    // The penumbral shadow cone is constructed by connecting the points of the limbs in
    // order. The umbral shadow cone is constructed by connecting them 180 deg out of
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
    // convert to meter
    for (glm::dvec3& p : resSrc.terminatorPoints) {
        p *= 1000.0;
    }

    // 1a. For some reason in some situations the angular position of the first vertex is
    // rotating, which causes a mismatch in the direction calculations. In order to
    // prevent that, we rotate the positions so that the point with the highest z
    // component is always the first point
    auto it = std::max_element(
        resSrc.terminatorPoints.begin(),
        resSrc.terminatorPoints.end(),
        [](const glm::dvec3& p1, const glm::dvec3& p2) { return p1.z > p2.z; }
    );
    std::rotate(resSrc.terminatorPoints.begin(), it, resSrc.terminatorPoints.end());


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
    // convert to meter
    for (glm::dvec3& p : resDst.terminatorPoints) {
        p *= 1000.0;
    }

    // 2a. Doing the same as in 1a
    auto jt = std::max_element(
        resDst.terminatorPoints.begin(),
        resDst.terminatorPoints.end(),
        [](const glm::dvec3& p1, const glm::dvec3& p2) { return p1.z > p2.z; }
    );
    std::rotate(resDst.terminatorPoints.begin(), jt, resDst.terminatorPoints.end());

    // 2b. Spice calculates the terminator points in a fixed counterclockwise direction
    // from the point of the view of the observer. Since we are switching target and
    // observer, this means that one of the sets of points is clockwise, while the other
    // is counterclockwise. In order for the right points to match up, we need to reverse
    // the order of one of them. It doesn't matter which one, so we pick this one
    std::reverse(resDst.terminatorPoints.begin(), resDst.terminatorPoints.end());

    ghoul_assert(
        resSrc.terminatorPoints.size() == resDst.terminatorPoints.size(),
        "Inconsistent number of terminator points retrieved"
    );


    // 3. Get the necessary conversion distances and matrices
    const glm::dvec3 diff = SpiceManager::ref().targetPosition(
        _shadowee,
        _shadower,
        "GALACTIC",
        {
            SpiceManager::AberrationCorrection::Type::None,
            SpiceManager::AberrationCorrection::Direction::Reception
        },
        et
    );
    const double distance = glm::length(diff) * 1000.0; // to meter


    const glm::dvec3 shadowerToLightSource = SpiceManager::ref().targetPosition(
        _lightSource,
        _shadower,
        _shadowerFrame,
        {
            SpiceManager::AberrationCorrection::Type::None,
            SpiceManager::AberrationCorrection::Direction::Reception
        },
        et
    ) * 1000.0; // to meter
    const glm::dmat3 lightToShadower = SpiceManager::ref().frameTransformationMatrix(
        _lightSourceFrame, _shadowerFrame, et
    );


    // 4. Construct the umbral shadow
    std::vector<VBOLayout> umbralVertices;
    if (_showUmbralShadow) {
        umbralVertices = calculateShadowPoints(
            resSrc.terminatorPoints,
            resDst.terminatorPoints,
            shadowerToLightSource,
            lightToShadower,
            distance * static_cast<double>(_shadowLength)
        );

        // We need to duplicate the first two vertices to close the cylinder at the seam
        umbralVertices.push_back(umbralVertices[0]);
        umbralVertices.push_back(umbralVertices[1]);
    }


    // 5. Construct the penumbral shadow
    std::vector<VBOLayout> penumbralVertices;
    if (_showPenumbralShadow) {
        // For the penumbral shadow, we need to mix the terminator points with a 180
        // degree phase shift, so that the top terminator point of the sun gets matched
        // with the bottom terminator point of the Moon, etc
        std::rotate(
            resSrc.terminatorPoints.begin(),
            resSrc.terminatorPoints.begin() + resSrc.terminatorPoints.size() / 2,
            resSrc.terminatorPoints.end()
        );
        penumbralVertices = calculateShadowPoints(
            resSrc.terminatorPoints,
            resDst.terminatorPoints,
            shadowerToLightSource,
            lightToShadower,
            distance * static_cast<double>(_shadowLength)
        );

        // We need to duplicate the first two vertices to close the cylinder at the seam
        penumbralVertices.push_back(penumbralVertices[0]);
        penumbralVertices.push_back(penumbralVertices[1]);
    }


    // 6. Combine vertices
    std::vector<VBOLayout> vertices;
    vertices.reserve(umbralVertices.size() + penumbralVertices.size());
    vertices.insert(vertices.end(), umbralVertices.begin(), umbralVertices.end());
    vertices.insert(vertices.end(), penumbralVertices.begin(), penumbralVertices.end());

    _nVertices = 0;
    if (_showPenumbralShadow) {
        _nVertices = static_cast<int>(penumbralVertices.size());
    }
    if (_showUmbralShadow) {
        _nVertices = static_cast<int>(umbralVertices.size());
    }

    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        vertices.size() * sizeof(VBOLayout),
        vertices.data(),
        GL_DYNAMIC_DRAW
    );

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);
    glBindVertexArray(0);
}

} // namespace openspace
