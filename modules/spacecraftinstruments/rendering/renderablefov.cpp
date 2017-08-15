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

#include <modules/spacecraftinstruments/rendering/renderablefov.h>

#include <modules/spacecraftinstruments/util/imagesequencer.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/opengl/programobject.h>

#include <glm/gtx/projection.hpp>

#include <openspace/performance/performancemeasurement.h>

namespace {
    const char* KeyBody                 = "Body";
    const char* KeyFrame                = "Frame";
//    const char* KeyColor                = "RGB";

    const char* KeyInstrument           = "Instrument";
    const char* KeyInstrumentName       = "Name";
    const char* KeyInstrumentAberration = "Aberration";

    const char* KeyPotentialTargets     = "PotentialTargets";
    const char* KeyFrameConversions     = "FrameConversions";

    const int InterpolationSteps = 5;

    static const openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value determines width of the lines connecting the instrument to the "
        "corners of the field of view."
    };

    static const openspace::properties::Property::PropertyInfo DrawSolidInfo = {
        "SolidDraw",
        "Solid Draw",
        "This value determines whether the field of view should be rendered as a solid "
        "or as lines only."
    };

    static const openspace::properties::Property::PropertyInfo StandoffDistanceInfo = {
        "StandOffDistance",
        "Standoff Distance Factor",
        "This value determines the standoff distance factor which influences the "
        "distance of the plane to the focus object. If this value is '1', the field of "
        "view will be rendered exactly on the surface of, for example, a planet. With a "
        "value of smaller than 1, the field of view will hover of ther surface, thus "
        "making it more visible."
    };

    static const openspace::properties::Property::PropertyInfo DefaultStartColorInfo = {
        "Colors.DefaultStart",
        "Start of default color",
        "This value determines the color of the field of view frustum close to the "
        "instrument. The final colors are interpolated between this value and the end "
        "color."
    };

    static const openspace::properties::Property::PropertyInfo DefaultEndColorInfo = {
        "Colors.DefaultEnd",
        "End of default color",
        "This value determines the color of the field of view frustum close to the "
        "target. The final colors are interpolated between this value and the start "
        "color."
    };

    static const openspace::properties::Property::PropertyInfo ActiveColorInfo = {
        "Colors.Active",
        "Active Color",
        "This value determines the color that is used when the instrument's field of "
        "view is active."
    };

    static const openspace::properties::Property::PropertyInfo TargetInFovInfo = {
        "Colors.TargetInFieldOfView",
        "Target in field-of-view Color",
        "This value determines the color that is used if the target is inside the field "
        "of view of the instrument but the instrument is not yet active."
    };

    static const openspace::properties::Property::PropertyInfo IntersectionStartInfo = {
        "Colors.IntersectionStart",
        "Start of the intersection",
        "This value determines the color that is used close to the instrument if one of "
        "the field of view corners is intersecting the target object. The final color is "
        "retrieved by interpolating between this color and the intersection end color."
    };

    static const openspace::properties::Property::PropertyInfo IntersectionEndInfo = {
        "Colors.IntersectionEnd",
        "End of the intersection",
        "This value determines the color that is used close to the target if one of the "
        "field of view corners is intersecting the target object. The final color is "
        "retrieved by interpolating between this color and the intersection begin color."
    };

    static const openspace::properties::Property::PropertyInfo SquareColorInfo = {
        "Colors.Square",
        "Orthogonal Square",
        "This value determines the color that is used for the field of view square in "
        "the case that there is no intersection and that the instrument is not currently "
        "active."
    };

} // namespace

namespace openspace {

documentation::Documentation RenderableFov::Documentation() {
    using namespace documentation;
    return {
        "RenderableFieldOfView",
        "newhorizons_renderable_fieldofview",
        {
            {
                KeyBody,
                new StringVerifier,
                Optional::No,
                "The SPICE name of the source body for which the field of view should be "
                "rendered."
            },
            {
                KeyFrame,
                new StringVerifier,
                Optional::No,
                "The SPICE name of the source body's frame in which the field of view "
                "should be rendered."
            },
            {
                KeyInstrument,
                new TableVerifier({
                    {
                        KeyInstrumentName,
                        new StringVerifier,
                        Optional::No,
                        "The SPICE name of the instrument that is rendered"
                    },
                    {
                        KeyInstrumentAberration,
                        new StringInListVerifier({
                            // Taken from SpiceManager::AberrationCorrection
                                "NONE",
                                "LT", "LT+S",
                                "CN", "CN+S",
                                "XLT", "XLT+S",
                                "XCN", "XCN+S"
                        }),
                        Optional::Yes,
                        "The aberration correction that is used for this field of view. "
                        "The default is 'NONE'."
                    }
                }),
                Optional::No,
                "A table describing the instrument whose field of view should be "
                "rendered."
            },
            {
                KeyPotentialTargets,
                new StringListVerifier,
                Optional::No,
                "A list of potential targets (specified as SPICE names) that the field "
                "of view should be tested against."
            },
            {
                KeyFrameConversions,
                new TableVerifier({
                    {
                        DocumentationEntry::Wildcard,
                        new StringVerifier,
                        Optional::No
                    }
                }),
                Optional::Yes,
                "A list of frame conversions that should be registered with the "
                "SpiceManager."
            },
            {
                LineWidthInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LineWidthInfo.description
            },
            {
                StandoffDistanceInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                StandoffDistanceInfo.description
            }
        }
    };
}


RenderableFov::RenderableFov(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _lineWidth(LineWidthInfo, 1.f, 1.f, 20.f)
    , _drawSolid(DrawSolidInfo, false)
    , _standOffDistance(StandoffDistanceInfo, 0.9999, 0.99, 1.0, 0.000001)
    , _programObject(nullptr)
    , _drawFOV(false)
    , _colors({
        { DefaultStartColorInfo, glm::vec4(0.4f) },
        { DefaultEndColorInfo, glm::vec4(0.85f, 0.85f, 0.85f, 1.f) },
        { ActiveColorInfo, glm::vec4(0.f, 1.f, 0.f, 1.f) },
        { TargetInFovInfo, glm::vec4(0.f, 0.5f, 0.7f, 1.f) },
        { IntersectionStartInfo, glm::vec4(1.f, 0.89f, 0.f, 1.f) },
        { IntersectionEndInfo, glm::vec4(1.f, 0.29f, 0.f, 1.f) },
        { SquareColorInfo, glm::vec4(0.85f, 0.85f, 0.85f, 1.f) }
    })
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableFov"
    );
    
    _instrument.spacecraft = dictionary.value<std::string>(KeyBody);
    _instrument.referenceFrame = dictionary.value<std::string>(KeyFrame);
    
    _instrument.name = dictionary.value<std::string>(
        std::string(KeyInstrument) + "." + KeyInstrumentName
    );

    std::string ia = std::string(KeyInstrument) + "." + KeyInstrumentAberration;
    if (dictionary.hasKey(ia)) {
        std::string ac = dictionary.value<std::string>(ia);
        _instrument.aberrationCorrection = SpiceManager::AberrationCorrection(ac);
    }

    ghoul::Dictionary pt = dictionary.value<ghoul::Dictionary>(KeyPotentialTargets);
    _instrument.potentialTargets.reserve(pt.size());
    for (size_t i = 1; i <= pt.size(); ++i) {
        std::string target = pt.value<std::string>(std::to_string(i));
        _instrument.potentialTargets.push_back(target);
    }

    if (dictionary.hasKey(KeyFrameConversions)) {
        ghoul::Dictionary fc = dictionary.value<ghoul::Dictionary>(KeyFrameConversions);
        for (const std::string& key : fc.keys()) {
            openspace::SpiceManager::ref().addFrame(
                key,
                fc.value<std::string>(key)
            );
        }
    }

    if (dictionary.hasKey(LineWidthInfo.identifier)) {
        _lineWidth = static_cast<float>(dictionary.value<double>(
            LineWidthInfo.identifier
        ));
    }

    if (dictionary.hasKey(StandoffDistanceInfo.identifier)) {
        _standOffDistance = static_cast<float>(dictionary.value<double>(
            StandoffDistanceInfo.identifier
        ));
    }

    addProperty(_lineWidth);
    addProperty(_drawSolid);
    addProperty(_standOffDistance);

    addProperty(_colors.defaultStart);
    addProperty(_colors.defaultEnd);
    addProperty(_colors.active);
    addProperty(_colors.targetInFieldOfView);
    addProperty(_colors.intersectionStart);
    addProperty(_colors.intersectionEnd);
    addProperty(_colors.square);
}

void RenderableFov::initialize() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    _programObject = renderEngine.buildRenderProgram(
        "FovProgram",
        "${MODULE_NEWHORIZONS}/shaders/fov_vs.glsl",
        "${MODULE_NEWHORIZONS}/shaders/fov_fs.glsl"
    );

    // Fetch information about the specific instrument
    SpiceManager::FieldOfViewResult res = SpiceManager::ref().fieldOfView(_instrument.name);

    // Right now, we can only deal with rectangles or polygons. Circles and ellipses only
    // return one or two bound vectors that have to used to construct an approximation
    const bool supportedShape =
        res.shape == SpiceManager::FieldOfViewResult::Shape::Polygon ||
        res.shape == SpiceManager::FieldOfViewResult::Shape::Rectangle;
    if (!supportedShape) {
        throw ghoul::RuntimeError(
            "'" + _instrument.name + "' has unsupported shape",
            "RenderableFov"
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
        nullptr // = reinterpret_cast<void*>(offsetof(RenderInformation::VBOData, position))
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
        nullptr // = reinterpret_cast<void*>(offsetof(RenderInformation::VBOData, position))
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

void RenderableFov::deinitialize() {
    OsEng.renderEngine().removeRenderProgram(_programObject);
    _programObject = nullptr;

    glDeleteBuffers(1, &_orthogonalPlane.vbo);
    glDeleteVertexArrays(1, &_orthogonalPlane.vao);

    glDeleteBuffers(1, &_fieldOfViewBounds.vbo);
    glDeleteVertexArrays(1, &_fieldOfViewBounds.vao);
}

bool RenderableFov::isReady() const {
    return _programObject != nullptr && !_instrument.bounds.empty();
}

// Orthogonal projection next to planets surface
glm::dvec3 RenderableFov::orthogonalProjection(const glm::dvec3& vecFov, double time, const std::string& target) const {
    glm::dvec3 vecToTarget = SpiceManager::ref().targetPosition(target, _instrument.spacecraft, _instrument.referenceFrame, _instrument.aberrationCorrection, time);
    glm::dvec3 fov = SpiceManager::ref().frameTransformationMatrix(_instrument.name, _instrument.referenceFrame, time) * vecFov;
    glm::dvec3 p = glm::proj(vecToTarget, fov);
    return p  * 1000.0; // km -> m
}

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

void RenderableFov::computeIntercepts(const UpdateData& data, const std::string& target, bool isInFov) {
    auto makeBodyFixedReferenceFrame = [&target](std::string ref) -> std::pair<std::string, bool> {
        bool convert = (ref.find("IAU_") == std::string::npos);
        if (convert) {
            return { SpiceManager::ref().frameFromBody(target), true };
        }
        else {
            return { ref, false };
        }
    };

    //std::vector<bool> intersects(_instrument.bounds.size());

    // First we fill the field-of-view bounds array by testing each bounds vector against
    // the object. We need to test it against the object (rather than using a fixed
    // distance) as the field of view rendering should stop at the surface and not
    // continue
    for (size_t i = 0; i < _instrument.bounds.size(); ++i) {
        const glm::dvec3& bound = _instrument.bounds[i];

        RenderInformation::VBOData& first = _fieldOfViewBounds.data[2 * i];
        RenderInformation::VBOData& second = _fieldOfViewBounds.data[2 * i + 1];

        // Regardless of what happens next, the position of every second element is going
        // to be the same. Only the color attribute might change
        first = {
            { 0.f, 0.f, 0.f },
            RenderInformation::VertexColorTypeDefaultStart
        };

        if (!isInFov) {
            // If the target is not in the field of view, we don't need to perform any
            // surface intercepts
            glm::vec3 o = orthogonalProjection(bound, data.time.j2000Seconds(), target);

            second = {
                { o.x, o.y, o.z },
                RenderInformation::VertexColorTypeDefaultEnd // This had a different color (0.4) before ---abock
            };
        }
        else {
            // The target is in the field of view, but not the entire field of view has to
            // be filled by the target
            auto ref = makeBodyFixedReferenceFrame(_instrument.referenceFrame);
            SpiceManager::SurfaceInterceptResult r = SpiceManager::ref().surfaceIntercept(
                target,
                _instrument.spacecraft,
                _instrument.name,
                ref.first,
                _instrument.aberrationCorrection,
                data.time.j2000Seconds(),
                bound
            );

            //intersects[i] = r.interceptFound;


            if (r.interceptFound) {
                // This point intersected the target
                first.color = RenderInformation::VertexColorTypeIntersectionStart;

                // If we had to convert the reference frame into a body-fixed frame, we
                // need to apply this change here:
                if (ref.second) {
                    r.surfaceVector = SpiceManager::ref().frameTransformationMatrix(
                        ref.first,
                        _instrument.referenceFrame,
                        data.time.j2000Seconds()
                    ) * r.surfaceVector;
                }

                // Convert the KM scale that SPICE uses to meter
                glm::vec3 srfVec = r.surfaceVector * 1000.0;

                // Standoff distance, we would otherwise end up *exactly* on the surface
                srfVec *= _standOffDistance;

                second = {
                    { srfVec.x, srfVec.y, srfVec.z },
                    RenderInformation::VertexColorTypeIntersectionEnd
                };
            }
            else {
                // This point did not intersect the target though others did
                glm::vec3 o = orthogonalProjection(bound, data.time.j2000Seconds(), target);
                second = {
                    { o.x, o.y, o.z },
                    RenderInformation::VertexColorTypeInFieldOfView
                };
            }
        }
    }

    // After finding the positions for the field of view boundaries, we can create the
    // vertices for the orthogonal plane as well, reusing the computations we performed
    // earlier
    
    // Each boundary in _instrument.bounds has 'InterpolationSteps' steps between
    auto indexForBounds = [](size_t idx) -> size_t {
        return idx * InterpolationSteps;
    };

    //auto boundsForIndex = [](size_t bnds) -> size_t {
    //    return bnds % InterpolationSteps;
    //};

    auto copyFieldOfViewValues = [&](size_t iBound, size_t begin, size_t end) -> void {
        std::fill(
            _orthogonalPlane.data.begin() + begin,
            _orthogonalPlane.data.begin() + end,
            _fieldOfViewBounds.data[2 * iBound + 1]
        );
    };

    // An early out for when the target is not in field of view
    if (!isInFov) {
        for (size_t i = 0; i < _instrument.bounds.size(); ++i) {
            // If none of the points are able to intersect with the target, we can just
            // copy the values from the field-of-view boundary. So we take each second
            // item (the first one is (0,0,0)) and replicate it 'InterpolationSteps' times
            copyFieldOfViewValues(i, indexForBounds(i), indexForBounds(i + 1));
        }

    }
    else {
        // At least one point will intersect
        for (size_t i = 0; i < _instrument.bounds.size(); ++i) {
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
                    data.time.j2000Seconds(),
                    probe
                ).interceptFound;
            };

            // Computes the intercept vector between the 'probe' and the target
            // the intercept vector is in meter and contains a standoff distance offset
            auto interceptVector = [&](const glm::dvec3& probe) -> glm::dvec3 {
                auto ref = makeBodyFixedReferenceFrame(_instrument.referenceFrame);
                SpiceManager::SurfaceInterceptResult r = SpiceManager::ref().surfaceIntercept(
                    target,
                    _instrument.spacecraft,
                    _instrument.name,
                    ref.first,
                    _instrument.aberrationCorrection,
                    data.time.j2000Seconds(),
                    probe
                );

                if (ref.second) {
                    r.surfaceVector = SpiceManager::ref().frameTransformationMatrix(
                        ref.first,
                        _instrument.referenceFrame,
                        data.time.j2000Seconds()
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
                        { icpt.x, icpt.y, icpt.z },
                        RenderInformation::VertexColorTypeSquare
                    };
                }
                else {
                    const glm::vec3 o = orthogonalProjection(tBound, data.time.j2000Seconds(), target);

                    _orthogonalPlane.data[indexForBounds(i) + m] = {
                        { o.x, o.y, o.z },
                        RenderInformation::VertexColorTypeSquare
                    };
                }

            }


        }
    }


#ifdef DEBUG_THIS
        // At least one point will intersect
        for (size_t i = 0; i < _instrument.bounds.size(); ++i) {
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
                    data.time,
                    probe
                ).interceptFound;
            };

            static const uint8_t NoIntersect   = 0b00;
            static const uint8_t ThisIntersect = 0b01;
            static const uint8_t NextIntersect = 0b10;
            static const uint8_t BothIntersect = 0b11;

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
                        const size_t p1 = static_cast<size_t>(indexForBounds(i) + t1 * InterpolationSteps);
                        const size_t p2 = static_cast<size_t>(indexForBounds(i) + t2 * InterpolationSteps);
                        
                        // We can copy the non-intersecting parts
                        copyFieldOfViewValues(i, indexForBounds(i), p1);
                        copyFieldOfViewValues(i, p2, indexForBounds(j));

                        // Are recompute the intersecting ones
                        for (size_t k = 0; k <= (p2 - p1); ++k) {
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
                        copyFieldOfViewValues(i, indexForBounds(i), indexForBounds(i + 1));
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
    }
            //size_t k = (i + 1 > _instrument.bounds.size() - 1) ? 0 : i + 1;

            //glm::dvec3 mid;
            //glm::dvec3 interpolated;

            //const glm::dvec3& current = _instrument.bounds[i];
            //const glm::dvec3& next = _instrument.bounds[k];

            //if (intercepts[i] == false) { // If point is non-interceptive, project it.


            //    insertPoint(_fovPlane, glm::vec4(orthogonalProjection(current, data.time, target), 0.0), tmp);
            //    _rebuild = true;
            //    if (intercepts[i + 1] == false) {
            //        // IFF incident point is also non-interceptive BUT something is within FOV
            //        // we need then to check if this segment makes contact with surface
            //        glm::dvec3 half = interpolate(current, next, 0.5f);

            //        std::string bodyfixed = "IAU_";
            //        bool convert = (_instrument.referenceFrame.find(bodyfixed) == std::string::npos);
            //        if (convert) {
            //            bodyfixed = SpiceManager::ref().frameFromBody(target);
            //        }
            //        else {
            //            bodyfixed = _instrument.referenceFrame;
            //        }

            //        SpiceManager::SurfaceInterceptResult res =
            //            SpiceManager::ref().surfaceIntercept(target, _instrument.spacecraft,
            //                _instrument.name, bodyfixed, _instrument.aberrationCorrection, data.time, half);

            //        if (convert) {
            //            res.surfaceVector = SpiceManager::ref().frameTransformationMatrix(bodyfixed, _instrument.referenceFrame, data.time) * res.surfaceVector;
            //        }

            //        bool intercepted = res.interceptFound;

            //        if (intercepted) {
            //            // find the two outer most points of intersection 
            //            glm::dvec3 root1 = bisection(half, current, data.time, target);
            //            glm::dvec3 root2 = bisection(half, next, data.time, target);

            //            insertPoint(_fovPlane, glm::vec4(orthogonalProjection(root1, data.time, target), 0.0), squareColor(diffTime));
            //            for (int j = 1; j < InterpolationSteps; ++j) {
            //                float t = (static_cast<float>(j) / InterpolationSteps);
            //                interpolated = interpolate(root1, root2, t);
            //                glm::dvec3 ivec = checkForIntercept(interpolated, data.time, target);
            //                insertPoint(_fovPlane, glm::vec4(ivec, 0.0), squareColor(diffTime));
            //            }
            //            insertPoint(_fovPlane, glm::vec4(orthogonalProjection(root2, data.time, target), 0.0), squareColor(diffTime));
            //        }
            //    }
            //}
            //if (interceptTag[i] == true && interceptTag[i + 1] == false) { // current point is interceptive, next is not
            //                                                               // find outer most point for interpolation
            //    mid = bisection(current, next, data.time, target);
            //    for (int j = 1; j <= InterpolationSteps; ++j) {
            //        float t = (static_cast<float>(j) / InterpolationSteps);
            //        interpolated = interpolate(current, mid, t);
            //        glm::dvec3 ivec = (j < InterpolationSteps) ? checkForIntercept(interpolated, data.time, target) : orthogonalProjection(interpolated, data.time, target);
            //        insertPoint(_fovPlane, glm::vec4(ivec, 0.0), squareColor(diffTime));
            //        _rebuild = true;
            //    }
            //}
            //if (interceptTag[i] == false && interceptTag[i + 1] == true) { // current point is non-interceptive, next is
            //    mid = bisection(next, current, data.time, target);
            //    for (int j = 1; j <= InterpolationSteps; ++j) {
            //        float t = (static_cast<float>(j) / InterpolationSteps);
            //        interpolated = interpolate(mid, next, t);
            //        glm::dvec3 ivec = (j > 1) ? checkForIntercept(interpolated, data.time, target) : orthogonalProjection(interpolated, data.time, target);
            //        insertPoint(_fovPlane, glm::vec4(ivec, 0.0), squareColor(diffTime));
            //        _rebuild = true;
            //    }
            //}
            //if (interceptTag[i] == true && interceptTag[i + 1] == true) { // both points intercept
            //    for (int j = 0; j <= InterpolationSteps; ++j) {
            //        float t = (static_cast<float>(j) / InterpolationSteps);
            //        interpolated = interpolate(current, next, t);
            //        glm::dvec3 ivec = checkForIntercept(interpolated, data.time, target);
            //        insertPoint(_fovPlane, glm::vec4(ivec, 0.0), squareColor(diffTime));
            //        _rebuild = true;
            //    }
            //}
            //// @CLEANUP-END
        //}
    //}
#endif

}

#if 0
void RenderableFov::computeIntercepts(const UpdateData& data, const std::string& target, bool inFOV) {
    double t2 = (openspace::ImageSequencer::ref().getNextCaptureTime());
    double diff = (t2 - data.time);
    float diffTime = 0.0;
    float interpolationStart = 7.0; //seconds before
    if (diff <= interpolationStart)
        diffTime = static_cast<float>(1.0 - (diff / interpolationStart));

    if (diff < 0.0)
        diffTime = 0.f;

    //PerfMeasure("computeIntercepts");
    // for each FOV vector
    bool interceptTag[35];

    _fovBounds.clear();
    for (int i = 0; i <= _instrument.bounds.size(); ++i) {
        int r = (i == _instrument.bounds.size()) ? 0 : i;
        std::string bodyfixed = "IAU_";
        bool convert = (_instrument.referenceFrame.find(bodyfixed) == std::string::npos);
        if (convert) {
            bodyfixed = SpiceManager::ref().frameFromBody(target);
        }
        else {
            bodyfixed = _instrument.referenceFrame;
        }
        
        SpiceManager::SurfaceInterceptResult res =
            SpiceManager::ref().surfaceIntercept(target, _instrument.spacecraft,
                _instrument.name, bodyfixed, _instrument.aberrationCorrection, data.time, _instrument.bounds[r]);
        
        if (convert) {
            res.surfaceVector = SpiceManager::ref().frameTransformationMatrix(bodyfixed, _instrument.referenceFrame, data.time) * res.surfaceVector;
        }
        
        interceptTag[r] = res.interceptFound;
        
        // if not found, use the orthogonal projected point
        glm::dvec3 b;
        if (!interceptTag[r]) {
            b = orthogonalProjection(_instrument.bounds[r], data.time, target);
        }

        glm::vec4 fovOrigin = glm::vec4(0); //This will have to be fixed once spacecraft is 1:1!

        if (interceptTag[r]) {
            // INTERCEPTIONS
            insertPoint(_fovBounds, fovOrigin, _colors.intersectionStart);
            insertPoint(_fovBounds, glm::vec4(res.surfaceVector, 0.0), endColor(diffTime));
        }
        else if (inFOV) {
            // OBJECT IN FOV, NO INTERCEPT FOR THIS FOV-RAY
            insertPoint(_fovBounds, fovOrigin, glm::vec4(0, 0, 1, 1));
            insertPoint(_fovBounds, glm::vec4(b, 0.0), _colors.targetInFieldOfView);
        }
        else {
            //glm::vec4 corner(_bounds[r][0], _bounds[r][1], _bounds[r][2], 8);
            ////glm::vec4 corner = _projectionBounds[r].vec4();
            //corner = _spacecraftRotation*corner;
            //// NONE OF THE FOV-RAYS INTERCEPT AND NO OBJECT IN FOV
            //insertPoint(_fovBounds, fovOrigin, col_gray);
            //insertPoint(_fovBounds, corner, glm::vec4(0));
            insertPoint(_fovBounds, fovOrigin, _colors.default);
            insertPoint(_fovBounds, glm::vec4(b, 0.0), glm::vec4(0.4));
        }
    }
    interceptTag[_instrument.bounds.size()] = interceptTag[0];
    //fovSurfaceIntercept(_interceptTag, _bounds, data.time);

    // FOV SURFACE INTERCEPT
    auto bounds = _instrument.bounds;
    _rebuild = false;
    _fovPlane.clear(); // empty the array

    glm::dvec3 mid;
    glm::dvec3 interpolated;
    glm::dvec3 current;
    glm::dvec3 next;
    glm::vec4 tmp(1);
    if (bounds.size() > 1) {
        for (int i = 0; i < bounds.size(); ++i) {
            int k = (i + 1 > bounds.size() - 1) ? 0 : i + 1;

            current = bounds[i];
            next = bounds[k];

            if (interceptTag[i] == false) { // If point is non-interceptive, project it.
                insertPoint(_fovPlane, glm::vec4(orthogonalProjection(current, data.time, target), 0.0), tmp);
                _rebuild = true;
                if (interceptTag[i + 1] == false && inFOV) {
                    // IFF incident point is also non-interceptive BUT something is within FOV
                    // we need then to check if this segment makes contact with surface
                    glm::dvec3 half = interpolate(current, next, 0.5f);

                    std::string bodyfixed = "IAU_";
                    bool convert = (_instrument.referenceFrame.find(bodyfixed) == std::string::npos);
                    if (convert) {
                        bodyfixed = SpiceManager::ref().frameFromBody(target);
                    }
                    else {
                        bodyfixed = _instrument.referenceFrame;
                    }

                    SpiceManager::SurfaceInterceptResult res =
                        SpiceManager::ref().surfaceIntercept(target, _instrument.spacecraft,
                            _instrument.name, bodyfixed, _instrument.aberrationCorrection, data.time, half);

                    if (convert) {
                        res.surfaceVector = SpiceManager::ref().frameTransformationMatrix(bodyfixed, _instrument.referenceFrame, data.time) * res.surfaceVector;
                    }

                    bool intercepted = res.interceptFound;

                    if (intercepted) {
                        // find the two outer most points of intersection 
                        glm::dvec3 root1 = bisection(half, current, data.time, target);
                        glm::dvec3 root2 = bisection(half, next, data.time, target);

                        insertPoint(_fovPlane, glm::vec4(orthogonalProjection(root1, data.time, target), 0.0), squareColor(diffTime));
                        for (int j = 1; j < InterpolationSteps; ++j) {
                            float t = (static_cast<float>(j) / InterpolationSteps);
                            interpolated = interpolate(root1, root2, t);
                            glm::dvec3 ivec = checkForIntercept(interpolated, data.time, target);
                            insertPoint(_fovPlane, glm::vec4(ivec,0.0) , squareColor(diffTime));
                        }
                        insertPoint(_fovPlane, glm::vec4(orthogonalProjection(root2, data.time, target), 0.0), squareColor(diffTime));
                    }
                }
            }
            if (interceptTag[i] == true && interceptTag[i + 1] == false) { // current point is interceptive, next is not
                                                     // find outer most point for interpolation
                mid = bisection(current, next, data.time, target);
                for (int j = 1; j <= InterpolationSteps; ++j) {
                    float t = (static_cast<float>(j) / InterpolationSteps);
                    interpolated = interpolate(current, mid, t);
                    glm::dvec3 ivec = (j < InterpolationSteps) ? checkForIntercept(interpolated, data.time, target) : orthogonalProjection(interpolated, data.time, target);
                    insertPoint(_fovPlane, glm::vec4(ivec, 0.0), squareColor(diffTime));
                    _rebuild = true;
                }
            }
            if (interceptTag[i] == false && interceptTag[i + 1] == true) { // current point is non-interceptive, next is
                mid = bisection(next, current, data.time, target);
                for (int j = 1; j <= InterpolationSteps; ++j) {
                    float t = (static_cast<float>(j) / InterpolationSteps);
                    interpolated = interpolate(mid, next, t);
                    glm::dvec3 ivec = (j > 1) ? checkForIntercept(interpolated, data.time, target) : orthogonalProjection(interpolated, data.time, target);
                    insertPoint(_fovPlane, glm::vec4(ivec, 0.0), squareColor(diffTime));
                    _rebuild = true;
                }
            }
            if (interceptTag[i] == true && interceptTag[i + 1] == true) { // both points intercept
                for (int j = 0; j <= InterpolationSteps; ++j) {
                    float t = (static_cast<float>(j) / InterpolationSteps);
                    interpolated = interpolate(current, next, t);
                    glm::dvec3 ivec = checkForIntercept(interpolated, data.time, target);
                    insertPoint(_fovPlane, glm::vec4(ivec, 0.0), squareColor(diffTime));
                    _rebuild = true;
                }
            }
        }
    }
    if (_rebuild) {
        //update size etc;
        _orthogonalPlane.size = static_cast<int>(_fovPlane.size());
    }
    //

    glm::mat4 spacecraftRotation = glm::mat4(
        SpiceManager::ref().positionTransformMatrix(_instrument.name, _instrument.referenceFrame, data.time)
    );

    glm::vec3 aim = (spacecraftRotation * glm::vec4(_instrument.boresight, 1));
    double lt;
    glm::dvec3 position =
    SpiceManager::ref().targetPosition(
        target,
        _instrument.spacecraft,
        _instrument.referenceFrame,
        _instrument.aberrationCorrection,
        data.time,
        lt
    );
    psc p = PowerScaledCoordinate::CreatePowerScaledCoordinate(position.x, position.y, position.z);
    pss length = p.length();
    if (length[0] < DBL_EPSILON) {
        _drawFOV = false;
        return;
    }
    //if aimed 80 deg away from target, dont draw white square
    if (glm::dot(glm::normalize(aim), glm::normalize(p.vec3())) < 0.2) {
        _drawFOV = false;
    }
}
#endif

void RenderableFov::render(const RenderData& data, RendererTasks&) {
    if (_drawFOV) {
        _programObject->activate();

        // Model transform and view transform needs to be in double precision
        glm::dmat4 modelTransform =
            glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
            glm::dmat4(data.modelTransform.rotation) *
            glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));
        
        glm::mat4 modelViewProjectionTransform =
            data.camera.projectionMatrix() *
            glm::mat4(data.camera.combinedViewMatrix() *
            modelTransform);

        _programObject->setUniform("modelViewProjectionTransform", modelViewProjectionTransform);

        _programObject->setUniform("defaultColorStart", _colors.defaultStart);
        _programObject->setUniform("defaultColorEnd", _colors.defaultEnd);
        _programObject->setUniform("activeColor", _colors.active);
        _programObject->setUniform("targetInFieldOfViewColor", _colors.targetInFieldOfView);
        _programObject->setUniform("intersectionStartColor", _colors.intersectionStart);
        _programObject->setUniform("intersectionEndColor", _colors.intersectionEnd);
        _programObject->setUniform("squareColor", _colors.square);
        _programObject->setUniform("interpolation", _interpolationTime);

        GLenum mode = _drawSolid ? GL_TRIANGLE_STRIP : GL_LINES;

        glLineWidth(_lineWidth);
        glBindVertexArray(_fieldOfViewBounds.vao);
        glDrawArrays(mode, 0, static_cast<int>(_fieldOfViewBounds.data.size()));

        glLineWidth(2.f);
        glBindVertexArray(_orthogonalPlane.vao);
        glDrawArrays(GL_LINE_LOOP, 0, static_cast<int>(_orthogonalPlane.data.size()));
        glBindVertexArray(0);
        glLineWidth(1.f);

        _programObject->deactivate();
    }
}

void RenderableFov::update(const UpdateData& data) {
    _drawFOV = false;
    if (openspace::ImageSequencer::ref().isReady()) {
        _drawFOV = ImageSequencer::ref().instrumentActive(_instrument.name);
    }

    if (_drawFOV && !data.time.paused()) {
        auto t = determineTarget(data.time.j2000Seconds());
        std::string target = t.first;
        bool inFOV = t.second;

        computeIntercepts(data, target, inFOV);
        updateGPU();

        double t2 = (ImageSequencer::ref().getNextCaptureTime());
        double diff = (t2 - data.time.j2000Seconds());
        _interpolationTime = 0.0;
        float interpolationStart = 7.0; //seconds before
        if (diff <= interpolationStart) {
            _interpolationTime = static_cast<float>(1.0 - (diff / interpolationStart));
        }

        if (diff < 0.0) {
            _interpolationTime = 0.f;
        }
    }
}

std::pair<std::string, bool> RenderableFov::determineTarget(double time) {
    // First, for all potential targets, check whether they are in the field of view
    for (const std::string& pt : _instrument.potentialTargets) {
        try {
            bool inFOV = SpiceManager::ref().isTargetInFieldOfView(
                pt,
                _instrument.spacecraft,
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
        catch (const openspace::SpiceManager::SpiceException&) {}
    }

    // If none of the targets is in field of view, either use the last target or if there
    // hasn't been one, find the closest target
    if (_previousTarget.empty()) {
        // If we reached this, we haven't found a target in field of view and we don't
        // have a previously selected target, so the next best heuristic for a target is
        // the closest one
        std::vector<double> distances(_instrument.potentialTargets.size());
        std::transform(
            _instrument.potentialTargets.begin(),
            _instrument.potentialTargets.end(),
            distances.begin(),
            [&o = _instrument.spacecraft, &f = _instrument.referenceFrame, &t = time](const std::string& pt) {
                double lt;
                glm::dvec3 p = SpiceManager::ref().targetPosition(pt, o, f, {}, t, lt);
                return glm::length(p);
            }
        );

        // The iterator points to the item with the minimal distance
        auto iterator = std::min_element(distances.begin(), distances.end());

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


    //glBindBuffer(GL_ARRAY_BUFFER, _bounds.vbo);
    //glBufferSubData(GL_ARRAY_BUFFER, 0, _bounds.size * sizeof(GLfloat), _fovBounds.data());

    ////LINFOC(_instrument, _boundsV.size);

    //if (!_rebuild) {
    //    // no new points
    //    glBindBuffer(GL_ARRAY_BUFFER, _orthogonalPlane.vbo);
    //    glBufferSubData(GL_ARRAY_BUFFER, 0, _orthogonalPlane.size * sizeof(GLfloat), _fovPlane.data());
    //}
    //else {
    //    // new points - memory change 
    //    glBindVertexArray(_orthogonalPlane.vao);
    //    glBindBuffer(GL_ARRAY_BUFFER, _orthogonalPlane.vbo);
    //    glBufferData(GL_ARRAY_BUFFER, _orthogonalPlane.size * sizeof(GLfloat), NULL, GL_STATIC_DRAW); // orphaning the buffer, sending NULL data.
    //    glBufferSubData(GL_ARRAY_BUFFER, 0, _orthogonalPlane.size * sizeof(GLfloat), _fovPlane.data());

    //    GLsizei st = sizeof(GLfloat) * Stride;
    //    glEnableVertexAttribArray(0);
    //    glEnableVertexAttribArray(1);
    //    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, st, (void*)0);
    //    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, st, (void*)(4 * sizeof(GLfloat)));
    //}

    //glBindVertexArray(0);
}

} // namespace openspace
