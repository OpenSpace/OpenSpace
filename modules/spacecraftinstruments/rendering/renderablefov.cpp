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

#include <modules/spacecraftinstruments/rendering/renderablefov.h>

#include <modules/spacecraftinstruments/spacecraftinstrumentsmodule.h>
#include <modules/spacecraftinstruments/util/imagesequencer.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/projection.hpp>
#include <optional>

namespace {
    constexpr int InterpolationSteps = 5;
    constexpr double Epsilon = 1e-4;

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "The width of the lines that connect the instrument to the corners of the field "
        "of view.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo StandoffDistanceInfo = {
        "StandOffDistance",
        "Standoff Distance Factor",
        "A standoff distance factor which influences the distance of the plane to the "
        "focus object. If the value is 1, the field of view will be rendered exactly on "
        "the surface of, for example, a planet. With a value of smaller than 1, the "
        "field of view will hover of the surface, thus making it more visible.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AlwaysDrawFovInfo = {
        "AlwaysDrawFov",
        "Always Draw FOV",
        "If enabled, the field of view will always be drawn, regardless of whether image "
        "information is currently available or not.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DefaultStartColorInfo = {
        "DefaultStart",
        "Default Start",
        "The color that is used for the field of view frustum close to the instrument. "
        "The final colors are interpolated between this value and the end color.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorDefaultEndInfo = {
        "DefaultEnd",
        "Default End",
        "The color that is used for the field of view frustum close to the target. The "
        "final colors are interpolated between this value and the start color.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorActiveInfo = {
        "Active",
        "Active",
        "The color that is used when the instrument's field of view is active.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorTargetInFovInfo = {
        "TargetInFieldOfView",
        "Target in Field of View",
        "The color that is used if the target is inside the field of view of the "
        "instrument but the instrument is not yet active.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorIntersectionStartInfo = {
        "IntersectionStart",
        "Intersection Start",
        "The color that is used close to the instrument if one of the field of view "
        "corners are intersecting the target object. The final color is an "
        "interpolation of this color and the intersection end color.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorIntersectionEndInfo = {
        "IntersectionEnd",
        "Intersection End",
        "The color that is used close to the target if one of the field of view corners "
        "is intersecting the target object. The final color is an interpolation of this "
        "color and the intersection begin color.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SquareColorInfo = {
        "Square",
        "Orthogonal Square",
        "The color that is used for the field of view square when there is no "
        "intersection and that the instrument is not currently active.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    template <typename Func>
    double bisect(const glm::dvec3& p1, const glm::dvec3& p2, Func testFunction,
          const glm::dvec3& previousHalf = glm::dvec3(std::numeric_limits<double>::max()))
    {
        const double Tolerance = 0.00000001;
        const glm::dvec3 half = glm::mix(p1, p2, 0.5);
        if (glm::distance(previousHalf, half) < Tolerance) {
            // The two points are so close to each other that we can stop
            return 0.5;
        }
        if (testFunction(half)) {
            return 0.5 + 0.5 * bisect(half, p2, testFunction, half);
        }
        else {
            return 0.5 * bisect(p1, half, testFunction, half);
        }
    }

    // Needs support for std::map first for the frameConversions
    struct [[codegen::Dictionary(RenderableFov)]] Parameters {
        // The SPICE name of the source body for which the field of view should be
        // rendered.
        std::string body;

        // The SPICE name of the source body's frame in which the field of view should be
        // rendered.
        std::string frame;

        struct Instrument {
            // The SPICE name of the instrument that is rendered.
            std::string name;

            // The aberration correction that is used for this field of view. The default
            // is 'NONE'.
            std::optional<std::string> aberration [[codegen::inlist("NONE",
                "LT", "LT+S", "CN", "CN+S", "XLT", "XLT+S", "XCN", "XCN+S")]];
        };
        // A table describing the instrument whose field of view should be rendered.
        Instrument instrument;

        // [[codegen::verbatim(AlwaysDrawFovInfo.description)]]
        std::optional<bool> alwaysDrawFov;

        // A list of potential targets (specified as SPICE names) that the field of view
        // should be tested against.
        std::vector<std::string> potentialTargets;

        // A list of frame conversions that should be registered with the SpiceManager.
        std::optional<std::map<std::string, std::string>> frameConversions;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(StandoffDistanceInfo.description)]]
        std::optional<float> standOffDistance;

        // If true, the field of view's bounds values will be simplified on load. Bound
        // vectors will be removed if they are the strict linear interpolation between the
        // two neighboring vectors. This value is disabled by default.
        std::optional<bool> simplifyBounds;
    };
#include "renderablefov_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableFov::Documentation() {
    return codegen::doc<Parameters>("spacecraftinstruments_renderablefieldofview");
}

RenderableFov::RenderableFov(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _lineWidth(LineWidthInfo, 1.f, 1.f, 20.f)
    , _standOffDistance(StandoffDistanceInfo, 0.9999, 0.99, 1.0, 0.000001)
    , _alwaysDrawFov(AlwaysDrawFovInfo, false)
    , _colors({
        properties::PropertyOwner({"Colors", "Colors"}),
        { DefaultStartColorInfo, glm::vec3(0.4f), glm::vec3(0.f), glm::vec3(1.f) },
        { ColorDefaultEndInfo, glm::vec3(0.85f), glm::vec3(0.f), glm::vec3(1.f) },
        { ColorActiveInfo, glm::vec3(0.f, 1.f, 0.f), glm::vec3(0.f), glm::vec3(1.f) },
        {
            ColorTargetInFovInfo,
            glm::vec3(0.f, 0.5f, 0.7f),
            glm::vec3(0.f),
            glm::vec3(1.f)
        },
        {
            ColorIntersectionStartInfo,
            glm::vec3(1.f, 0.89f, 0.f),
            glm::vec3(0.f),
            glm::vec3(1.f)
        },
        {
            ColorIntersectionEndInfo,
            glm::vec3(1.f, 0.29f, 0.f),
            glm::vec3(0.f),
            glm::vec3(1.f)
        },
        { SquareColorInfo, glm::vec3(0.85f), glm::vec3(0.f), glm::vec3(1.f) }
    })
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _instrument.spacecraft = p.body;
    _instrument.referenceFrame = p.frame;
    _instrument.name = p.instrument.name;
    if (p.instrument.aberration.has_value()) {
        _instrument.aberrationCorrection = SpiceManager::AberrationCorrection(
            *p.instrument.aberration
        );
    }

    _instrument.potentialTargets = p.potentialTargets;

    if (p.frameConversions.has_value()) {
        for (const std::pair<const std::string, std::string>& fc : *p.frameConversions) {
            global::moduleEngine->module<SpacecraftInstrumentsModule>()->addFrame(
                fc.first,
                fc.second
            );
        }
    }

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    _standOffDistance = p.standOffDistance.value_or(_standOffDistance);
    addProperty(_standOffDistance);

    _alwaysDrawFov = p.alwaysDrawFov.value_or(_alwaysDrawFov);
    addProperty(_alwaysDrawFov);

    _simplifyBounds = p.simplifyBounds.value_or(_simplifyBounds);

    _colors.defaultStart.setViewOption(properties::Property::ViewOptions::Color, true);
    _colors.defaultEnd.setViewOption(properties::Property::ViewOptions::Color, true);
    _colors.active.setViewOption(properties::Property::ViewOptions::Color, true);
    _colors.targetInFieldOfView.setViewOption(
        properties::Property::ViewOptions::Color,
        true
    );
    _colors.intersectionStart.setViewOption(
        properties::Property::ViewOptions::Color,
        true
    );
    _colors.intersectionEnd.setViewOption(properties::Property::ViewOptions::Color, true);
    _colors.square.setViewOption(
        properties::Property::ViewOptions::Color,
        true
    );
    _colors.container.addProperty(_colors.defaultStart);
    _colors.container.addProperty(_colors.defaultEnd);
    _colors.container.addProperty(_colors.active);
    _colors.container.addProperty(_colors.targetInFieldOfView);
    _colors.container.addProperty(_colors.intersectionStart);
    _colors.container.addProperty(_colors.intersectionEnd);
    _colors.container.addProperty(_colors.square);

    addPropertySubOwner(_colors.container);
}

void RenderableFov::initializeGL() {
    _program = SpacecraftInstrumentsModule::ProgramObjectManager.request(
        "FovProgram",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "FovProgram",
                absPath("${MODULE_SPACECRAFTINSTRUMENTS}/shaders/fov_vs.glsl"),
                absPath("${MODULE_SPACECRAFTINSTRUMENTS}/shaders/fov_fs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache);

    // Fetch information about the specific instrument
    SpiceManager::FieldOfViewResult res = SpiceManager::ref().fieldOfView(
        _instrument.name
    );

    // Right now, we can only deal with rectangles or polygons. Circles and ellipses only
    // return one or two bound vectors that have to used to construct an approximation
    const bool supportedShape =
        res.shape == SpiceManager::FieldOfViewResult::Shape::Polygon ||
        res.shape == SpiceManager::FieldOfViewResult::Shape::Rectangle;
    if (!supportedShape) {
        throw ghoul::RuntimeError(
            std::format("'{}' has unsupported shape", _instrument.name),
            "RenderableFov"
        );
    }

    if (_simplifyBounds) {
        const size_t sizeBefore = res.bounds.size();
        for (size_t i = 1; i < res.bounds.size() - 1; i++) {
            const glm::dvec3& prev = res.bounds[i - 1];
            const glm::dvec3& curr = res.bounds[i];
            const glm::dvec3& next = res.bounds[i + 1];

            const double area = glm::length(glm::cross((curr - prev), (next - prev)));

            const bool isCollinear = area < Epsilon;

            if (isCollinear) {
                res.bounds.erase(res.bounds.begin() + i);

                // Need to subtract one as we have shortened the array and the next
                // item is now on the current position
                --i;
            }
        }
        const size_t sizeAfter = res.bounds.size();

        LINFOC(
            _instrument.name,
            std::format("Simplified from {} to {}", sizeBefore, sizeAfter)
        );
    }


    _instrument.bounds = std::move(res.bounds);
    _instrument.boresight = std::move(res.boresightVector);

    // These vectors hold the data that we will want to render. We need to subdivide the
    // range as an intersection test between the corners and the object might fail for
    // sufficiently small objects:
    //
    //    x---------------------x  Field of view
    //    |                     |
    //    |                     |
    //    |       *****         |
    //    |      *     *        |
    //    x-----*-------*-------x
    //           *     *
    //            *****  Target object
    //

    // The orthogonal plane shows the footprint of the instrument on the surface of the
    // object. Since it should follow the potential curvature, we might need to
    // interpolate, hence the extra storage
    _orthogonalPlane.data.resize(_instrument.bounds.size() * InterpolationSteps);

    // The field of views are originating from the space craft, so the location of the
    // space craft has to be repeated for each vertex, hence the * 2. On the other hand,
    // the field of view does **not** need to be interpolated
    _fieldOfViewBounds.data.resize(2 * _instrument.bounds.size());

    // Field of view boundaries
    glGenVertexArrays(1, &_fieldOfViewBounds.vao);
    glBindVertexArray(_fieldOfViewBounds.vao);
    glGenBuffers(1, &_fieldOfViewBounds.vbo);
    glBindBuffer(GL_ARRAY_BUFFER, _fieldOfViewBounds.vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        _fieldOfViewBounds.data.size() * sizeof(RenderInformation::VBOData),
        nullptr,
        GL_STREAM_DRAW
    );
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(RenderInformation::VBOData),
        nullptr
    );
    glEnableVertexAttribArray(1);
    glVertexAttribIPointer(
        1,
        1,
        GL_INT,
        sizeof(RenderInformation::VBOData),
        reinterpret_cast<void*>(offsetof(RenderInformation::VBOData, color))
    );

    // Orthogonal Plane
    glGenVertexArrays(1, &_orthogonalPlane.vao);
    glGenBuffers(1, &_orthogonalPlane.vbo);
    glBindVertexArray(_orthogonalPlane.vao);
    glBindBuffer(GL_ARRAY_BUFFER, _orthogonalPlane.vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        _orthogonalPlane.data.size() * sizeof(RenderInformation::VBOData),
        nullptr,
        GL_STREAM_DRAW
    );

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(RenderInformation::VBOData),
        nullptr
    );
    glEnableVertexAttribArray(1);
    glVertexAttribIPointer(
        1,
        1,
        GL_INT,
        sizeof(RenderInformation::VBOData),
        reinterpret_cast<void*>(offsetof(RenderInformation::VBOData, color))
    );

    glBindVertexArray(0);
}

void RenderableFov::deinitializeGL() {
    SpacecraftInstrumentsModule::ProgramObjectManager.release(
        "FovProgram",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _program = nullptr;

    glDeleteBuffers(1, &_orthogonalPlane.vbo);
    glDeleteVertexArrays(1, &_orthogonalPlane.vao);

    glDeleteBuffers(1, &_fieldOfViewBounds.vbo);
    glDeleteVertexArrays(1, &_fieldOfViewBounds.vao);
}

bool RenderableFov::isReady() const {
    return _program != nullptr && !_instrument.bounds.empty();
}

// Orthogonal projection next to planets surface
glm::dvec3 RenderableFov::orthogonalProjection(const glm::dvec3& vecFov, double time,
                                               const std::string& target) const
{
    if (target.empty()) {
        constexpr glm::dvec3 Up = glm::dvec3(1.0, 0.0, 0.0);
        return glm::normalize(glm::cross(Up, vecFov));
    }
    else {
        const glm::dvec3 vecToTarget = SpiceManager::ref().targetPosition(
            target,
            _instrument.spacecraft,
            _instrument.referenceFrame,
            _instrument.aberrationCorrection,
            time
        );
        const glm::dvec3 fov = SpiceManager::ref().frameTransformationMatrix(
            _instrument.name,
            _instrument.referenceFrame,
            time
        ) * vecFov;
        const glm::dvec3 p = glm::proj(vecToTarget, fov);
        return p * 1000.0; // km -> m
    }
}

void RenderableFov::computeIntercepts(double time, const std::string& target,
                                      bool isInFov)
{
    auto makeBodyFixedReferenceFrame =
        [&target](const std::string& ref) -> std::pair<std::string, bool>
    {
        const bool convert = (ref.find("IAU_") == std::string::npos);
        if (convert) {
            SpacecraftInstrumentsModule* m =
                global::moduleEngine->module<SpacecraftInstrumentsModule>();
            return { m->frameFromBody(target), true };
        }
        else {
            return { ref, false };
        }
    };

    // First we fill the field-of-view bounds array by testing each bounds vector against
    // the object. We need to test it against the object (rather than using a fixed
    // distance) as the field of view rendering should stop at the surface
    for (size_t i = 0; i < _instrument.bounds.size(); i++) {
        const glm::dvec3& bound = _instrument.bounds[i];

        RenderInformation::VBOData& first = _fieldOfViewBounds.data[2 * i];
        RenderInformation::VBOData& second = _fieldOfViewBounds.data[2 * i + 1];

        // Regardless of what happens next, the position of every second element is going
        // to be the same. Only the color attribute might change
        first = {
            .position = { 0.f, 0.f, 0.f },
            .color = RenderInformation::VertexColorTypeDefaultStart
        };

        if (!isInFov) {
            // If the target is not in the field of view, we don't need to perform any
            // surface intercepts
            const glm::vec3 o = orthogonalProjection(bound, time, target);

            second = {
                .position = { o.x, o.y, o.z },
                .color = RenderInformation::VertexColorTypeDefaultEnd
            };
        }
        else {
            // The target is in the field of view, but not the entire field of view has to
            // be filled by the target
            const std::pair<std::string, bool> ref = makeBodyFixedReferenceFrame(
                _instrument.referenceFrame
            );

            SpiceManager::SurfaceInterceptResult r = SpiceManager::ref().surfaceIntercept(
                target,
                _instrument.spacecraft,
                _instrument.name,
                ref.first,
                _instrument.aberrationCorrection,
                time,
                bound
            );

            if (r.interceptFound) {
                // This point intersected the target
                first.color = RenderInformation::VertexColorTypeIntersectionStart;

                // If we had to convert the reference frame into a body-fixed frame, we
                // need to apply this change here:
                if (ref.second) {
                    r.surfaceVector = SpiceManager::ref().frameTransformationMatrix(
                        ref.first,
                        _instrument.referenceFrame,
                        time
                    ) * r.surfaceVector;
                }

                // Convert the KM scale that SPICE uses to meter
                glm::vec3 srfVec = r.surfaceVector * 1000.0;

                // Standoff distance, we would otherwise end up *exactly* on the surface
                srfVec *= _standOffDistance;

                second = {
                    .position = { srfVec.x, srfVec.y, srfVec.z },
                    .color = RenderInformation::VertexColorTypeIntersectionEnd
                };
            }
            else {
                // This point did not intersect the target though others did
                const glm::vec3 o = orthogonalProjection(bound, time, target);
                second = {
                    .position = { o.x, o.y, o.z },
                    .color = RenderInformation::VertexColorTypeInFieldOfView
                };
            }
        }
    }

    // After finding the positions for the field of view boundaries, we can create the
    // vertices for the orthogonal plane as well, reusing the computations we performed
    // earlier

    // Each boundary in _instrument.bounds has 'InterpolationSteps' steps between
    auto indexForBounds = [](size_t idx) -> size_t { return idx * InterpolationSteps; };

    auto copyFieldOfViewValues = [this](size_t iBound, size_t begin, size_t end) -> void {
        std::fill(
            _orthogonalPlane.data.begin() + begin,
            _orthogonalPlane.data.begin() + end,
            _fieldOfViewBounds.data[2 * iBound + 1]
        );
    };

    // An early out for when the target is not in field of view
    if (!isInFov) {
        for (size_t i = 0; i < _instrument.bounds.size(); i++) {
            // If none of the points are able to intersect with the target, we can just
            // copy the values from the field-of-view boundary. So we take each second
            // item (the first one is (0,0,0)) and replicate it 'InterpolationSteps' times
            copyFieldOfViewValues(i, indexForBounds(i), indexForBounds(i + 1));
        }
    }
    else {
        // At least one point will intersect
        for (size_t i = 0; i < _instrument.bounds.size(); i++) {
            // Wrap around the array index to 0
            const size_t j = (i == _instrument.bounds.size() - 1) ? 0 : i + 1;

            const glm::dvec3& iBound = _instrument.bounds[i];
            const glm::dvec3& jBound = _instrument.bounds[j];

            auto intercepts = [&](const glm::dvec3& probe) -> bool {
                return SpiceManager::ref().surfaceIntercept(
                    target,
                    _instrument.spacecraft,
                    _instrument.name,
                    makeBodyFixedReferenceFrame(_instrument.referenceFrame).first,
                    _instrument.aberrationCorrection,
                    time,
                    probe
                ).interceptFound;
            };

            // Computes the intercept vector between the 'probe' and the target
            // the intercept vector is in meter and contains a standoff distance offset
            auto interceptVector = [&](const glm::dvec3& probe) -> glm::dvec3 {
                auto ref = makeBodyFixedReferenceFrame(_instrument.referenceFrame);
                SpiceManager::SurfaceInterceptResult r =
                    SpiceManager::ref().surfaceIntercept(
                        target,
                        _instrument.spacecraft,
                        _instrument.name,
                        ref.first,
                        _instrument.aberrationCorrection,
                        time,
                        probe
                    );

                if (ref.second) {
                    r.surfaceVector = SpiceManager::ref().frameTransformationMatrix(
                        ref.first,
                        _instrument.referenceFrame,
                        time
                    ) * r.surfaceVector;
                }

                // Convert the KM scale that SPICE uses to meter
                // Standoff distance, we would otherwise end up *exactly* on the surface
                return r.surfaceVector * 1000.0 * _standOffDistance.value();
            };

            for (size_t m = 0; m < InterpolationSteps; ++m) {
                const double t = static_cast<double>(m) / (InterpolationSteps);
                const glm::dvec3 tBound = glm::mix(iBound, jBound, t);

                if (intercepts(tBound)) {
                    const glm::vec3 icpt = interceptVector(tBound);
                    _orthogonalPlane.data[indexForBounds(i) + m] = {
                        .position = { icpt.x, icpt.y, icpt.z },
                        .color = RenderInformation::VertexColorTypeSquare
                    };
                }
                else {
                    const glm::vec3 o = orthogonalProjection(tBound, time, target);
                    _orthogonalPlane.data[indexForBounds(i) + m] = {
                        .position = { o.x, o.y, o.z },
                        .color = RenderInformation::VertexColorTypeSquare
                    };
                }
            }
        }
    }


#if 0 // DEBUG_THIS
    // At least one point will intersect
    for (size_t i = 0; i < _instrument.bounds.size(); i++) {
        // Wrap around the array index to 0
        const size_t j = (i == _instrument.bounds.size() - 1) ? 0 : i + 1;

        const glm::dvec3& iBound = _instrument.bounds[i];
        const glm::dvec3& jBound = _instrument.bounds[j];

        auto intercepts = [&](const glm::dvec3& probe) -> bool {
            return SpiceManager::ref().surfaceIntercept(
                target,
                _instrument.spacecraft,
                _instrument.name,
                makeBodyFixedReferenceFrame(_instrument.referenceFrame).first,
                _instrument.aberrationCorrection,
                time,
                probe
            ).interceptFound;
        };

        constexpr uint8_t NoIntersect   = 0b00;
        constexpr uint8_t ThisIntersect = 0b01;
        constexpr uint8_t NextIntersect = 0b10;
        constexpr uint8_t BothIntersect = 0b11;

        const uint8_t type = (intersects[i] ? 1 : 0) + (intersects[j] ? 2 : 0);
        switch (type) {
            case NoIntersect:
            {
                // If both points don't intercept, the target might still pass between
                // them, so we need to check the intermediate point

                const glm::dvec3 half = glm::mix(iBound, jBound, 0.5);
                if (intercepts(half)) {
                    // The two outer points do not intersect, but the middle point
                    // does; so we need to find the intersection points
                    const double t1 = bisect(half, iBound, intercepts);
                    const double t2 = 0.5 + bisect(half, jBound, intercepts);

                    //
                    // The target is sticking out somewhere between i and j, so we
                    // have three regions here:
                    // The first (0,t1) and second (t2,1) are not intersecting
                    // The third between (t1,t2) is intersecting
                    //
                    //   i       p1    p2       j
                    //            *****
                    //   x-------*     *-------x
                    //   0       t1    t2      1

                    // OBS: i and j are in bounds-space,  p1, p2 are in
                    // _orthogonalPlane-space
                    const size_t p1 = static_cast<size_t>(
                        indexForBounds(i) + t1 * InterpolationSteps
                    );
                    const size_t p2 = static_cast<size_t>(
                        indexForBounds(i) + t2 * InterpolationSteps
                    );

                    // We can copy the non-intersecting parts
                    copyFieldOfViewValues(i, indexForBounds(i), p1);
                    copyFieldOfViewValues(i, p2, indexForBounds(j));

                    // Are recompute the intersecting ones
                    for (size_t k = 0; k <= (p2 - p1); k++) {
                        const double t = t1 + k * (t2 - t1);
                        const glm::dvec3 interpolated = glm::mix(iBound, jBound, t);
                        const glm::vec3 icpt = interceptVector(interpolated);
                        _orthogonalPlane.data[p1 + k] = {
                            icpt.x, icpt.y, icpt.z,
                            RenderInformation::VertexColorTypeSquare
                        };
                    }
                }
                else {
                    copyFieldOfViewValues(
                        i,
                        indexForBounds(i),
                        indexForBounds(i + 1)
                    );
                }
                break;
            }
            case ThisIntersect:
            case NextIntersect:
            case BothIntersect:
                break;
            default:
                throw ghoul::MissingCaseException();
        }
    }
#endif
}

void RenderableFov::render(const RenderData& data, RendererTasks&) {
    if (!_drawFOV) {
        return;
    }

    _program->activate();

    // Model transform and view transform needs to be in double precision
    const glm::mat4 modelViewProjectionTransform =
        calcModelViewProjectionTransform(data);

    _program->setUniform(
        _uniformCache.modelViewProjectionTransform,
        modelViewProjectionTransform
    );

    _program->setUniform(_uniformCache.colorStart, _colors.defaultStart);
    _program->setUniform(_uniformCache.colorEnd, _colors.defaultEnd);
    _program->setUniform(_uniformCache.activeColor, _colors.active);
    _program->setUniform(
        _uniformCache.targetInFieldOfViewColor,
        _colors.targetInFieldOfView
    );
    _program->setUniform(_uniformCache.intersectionStartColor, _colors.intersectionStart);
    _program->setUniform(_uniformCache.intersectionEndColor, _colors.intersectionEnd);
    _program->setUniform(_uniformCache.squareColor, _colors.square);
    _program->setUniform(_uniformCache.interpolation, _interpolationTime);

    glLineWidth(_lineWidth);
    glBindVertexArray(_fieldOfViewBounds.vao);
    glDrawArrays(GL_LINES, 0, static_cast<int>(_fieldOfViewBounds.data.size()));

    glLineWidth(2.f);
    glBindVertexArray(_orthogonalPlane.vao);
    glDrawArrays(GL_LINE_LOOP, 0, static_cast<int>(_orthogonalPlane.data.size()));
    glBindVertexArray(0);
    glLineWidth(1.f);

    _program->deactivate();
}

void RenderableFov::update(const UpdateData& data) {
    _drawFOV = _alwaysDrawFov;
    if (ImageSequencer::ref().isReady()) {
        _drawFOV = ImageSequencer::ref().isInstrumentActive(
            data.time.j2000Seconds(),
            _instrument.name
        );
    }

    // TODO: figure out if time has changed
    if (_drawFOV /* && time changed */) {
        const std::pair<std::string, bool>& t = determineTarget(data.time.j2000Seconds());

        computeIntercepts(data.time.j2000Seconds(), t.first, t.second);
        updateGPU();

        const double t2 = ImageSequencer::ref().nextCaptureTime(data.time.j2000Seconds());
        const double diff = (t2 - data.time.j2000Seconds());
        _interpolationTime = 0.f;
        const float interpolationStart = 7.f; // seconds before
        if (diff <= interpolationStart) {
            _interpolationTime = static_cast<float>(1.f - (diff / interpolationStart));
        }

        if (diff < 0.0) {
            _interpolationTime = 0.f;
        }
    }

    if (_program->isDirty()) {
        _program->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_program, _uniformCache);
    }
}

std::pair<std::string, bool> RenderableFov::determineTarget(double time) {
    SpiceManager::UseException oldValue = SpiceManager::ref().exceptionHandling();
    defer { SpiceManager::ref().setExceptionHandling(oldValue); };

    // First, for all potential targets, check whether they are in the field of view
    for (const std::string& pt : _instrument.potentialTargets) {
        const bool inFOV = SpiceManager::ref().isTargetInFieldOfView(
            pt,
            _instrument.spacecraft,
            global::moduleEngine->module<SpacecraftInstrumentsModule>()->frameFromBody(
                pt
            ),
            _instrument.name,
            SpiceManager::FieldOfViewMethod::Ellipsoid,
            _instrument.aberrationCorrection,
            time
        );

        if (inFOV) {
            _previousTarget = pt;
            return { pt, true };
        }
    }

    // If none of the targets is in field of view, either use the last target or if there
    // hasn't been one, find the closest target
    if (_previousTarget.empty() && !_instrument.potentialTargets.empty()) {
        // If we reached this, we haven't found a target in field of view and we don't
        // have a previously selected target, so the next best heuristic for a target is
        // the closest one
        std::vector<double> distances(_instrument.potentialTargets.size());
        std::transform(
            _instrument.potentialTargets.begin(),
            _instrument.potentialTargets.end(),
            distances.begin(),
            [&i = _instrument, &t = time] (const std::string& pt) {
                double lt = 0.0;
                const glm::dvec3 p = SpiceManager::ref().targetPosition(
                    pt,
                    i.spacecraft,
                    i.referenceFrame,
                    {},
                    t,
                    lt
                );
                return glm::length(p);
            }
        );

        // The iterator points to the item with the minimal distance
        const auto iterator = std::min_element(distances.begin(), distances.end());

        // Since the two vectors are ordered the same, we can use the distance as offset
        _previousTarget = _instrument.potentialTargets[
            std::distance(distances.begin(), iterator)
        ];
    }

    return { _previousTarget, false };
}

void RenderableFov::updateGPU() {
    // @SPEEDUP:  Only upload the part of the data that has changed ---abock
    glBindBuffer(GL_ARRAY_BUFFER, _fieldOfViewBounds.vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        _fieldOfViewBounds.data.size() * sizeof(RenderInformation::VBOData),
        _fieldOfViewBounds.data.data(),
        GL_STREAM_DRAW
    );

    glBindBuffer(GL_ARRAY_BUFFER, _orthogonalPlane.vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        _orthogonalPlane.data.size() * sizeof(RenderInformation::VBOData),
        _orthogonalPlane.data.data(),
        GL_STREAM_DRAW
    );
}

} // namespace openspace
