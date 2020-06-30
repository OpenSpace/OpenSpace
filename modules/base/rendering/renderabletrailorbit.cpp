/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <modules/base/rendering/renderabletrailorbit.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/scene/translation.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/opengl/programobject.h>
#include <numeric>

// This class is using a VBO ring buffer + a constantly updated point as follows:
// Structure of the array with a _resolution of 16. FF denotes the floating position that
// is updated every frame:
//  ---------------------------------------------------------------------------------
//  | FF |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |
//  ---------------------------------------------------------------------------------
//    0     1    2    3    4    5    6    7    8    9   10   11   12   13   14   15
//                    <------ newer in time                                    oldest
//
// In the begining the floating value starts at 0; this means that array element 0 is
// updated and uploaded to the GPU at every frame. The FF+1 element is the newest fixed
// location and FF-1 element is the oldest fixed location (including wrapping around the
// array) with the times of _lastPointTime and _firstPointTime.
//
// If the time progresses forwards and abs(time - _lastPointTime) becomes big enough, the
// oldest point is removed and a new fixed location is added. In the ring buffer this
// would be represented as:
//  ---------------------------------------------------------------------------------
//  |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    | FF |
//  ---------------------------------------------------------------------------------
//    0     1    2    3    4    5    6    7    8    9   10   11   12   13   14   15
//                    <------ newer in time                              oldest
//
// Thus making the floating point traverse backwards through the array and element 0 being
// the newest fixed point. If the time processes backwards, the floating point moves
// towards the upper areas of the array instead.
// In both cases, only the values that have been changed will be uploaded to the GPU.
//
// For the rendering, this is achieved by using an index buffer that is twice the size of
// the vertex buffer containing identical two sequences indexing the vertex array.
// In an example of size 8:
// ---------------------------------------------------------------------------------------
// |0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15| 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|15|
// ---------------------------------------------------------------------------------------
//  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
//
// The rendering step needs to know only the offset into the array (denoted by FF as the
// floating position above) and use the index array from the position. Since the indices
// in this array wrap around, so will the rendering of the vertices. Example:
// FF := 10
// Rendering 16 elements will 'generate' the index buffer:
// 10 11 12 13 14 15 00 01 02 03 04 05 06 07 08 09
//
//
// NB: This method was implemented without a ring buffer before by manually shifting the
// items in memory as was shown to be much slower than the current system.   ---abock

namespace {
    constexpr openspace::properties::Property::PropertyInfo PeriodInfo = {
        "Period",
        "Period (in days)",
        "The objects period, i.e. the length of its orbit around the parent object given "
        "in (Earth) days. In the case of Earth, this would be a sidereal year "
        "(=365.242 days). If this values is specified as multiples of the period, it is "
        "possible to show the effects of precession."
    };

    constexpr openspace::properties::Property::PropertyInfo ResolutionInfo = {
        "Resolution",
        "Number of samples along the orbit",
        "The number of samples along the orbit. This determines the resolution of the "
        "trail; the tradeoff being that a higher resolution is able to resolve more "
        "detail, but will take more resources while rendering, too. The higher, the "
        "smoother the trail, but also more memory will be used."
    };

    constexpr openspace::properties::Property::PropertyInfo RenderableTypeInfo = {
       "RenderableType",
       "RenderableType",
       "This value specifies if the orbit should be rendered in the Background,"
       "Opaque, Transparent, or Overlay rendering step. Default is Transparent."
    };

} // namespace

namespace openspace {

documentation::Documentation RenderableTrailOrbit::Documentation() {
    using namespace documentation;
    documentation::Documentation doc {
        "RenderableTrailOrbit",
        "base_renderable_renderabletrailorbit",
        {
            {
                PeriodInfo.identifier,
                new DoubleVerifier,
                Optional::No,
                PeriodInfo.description
            },
            {
                ResolutionInfo.identifier,
                new IntVerifier,
                Optional::No,
                ResolutionInfo.description
            },
            {
                RenderableTypeInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                RenderableTypeInfo.description
            }
        }
    };

    // Insert the parents documentation entries until we have a verifier that can deal
    // with class hierarchy
    documentation::Documentation parentDoc = RenderableTrail::Documentation();
    doc.entries.insert(
        doc.entries.end(),
        parentDoc.entries.begin(),
        parentDoc.entries.end()
    );

    return doc;
}

RenderableTrailOrbit::RenderableTrailOrbit(const ghoul::Dictionary& dictionary)
    : RenderableTrail(dictionary)
    , _period(PeriodInfo, 0.0, 0.0, 1e9)
    , _resolution(ResolutionInfo, 10000, 1, 1000000)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableTrailOrbit"
    );

    _translation->onParameterChange([this]() { _needsFullSweep = true; });

    // Period is in days
    using namespace std::chrono;
    const long long sph = duration_cast<seconds>(hours(24)).count();
    _period = dictionary.value<double>(PeriodInfo.identifier) * sph;
    _period.onChange([&] { _needsFullSweep = true; _indexBufferDirty = true; });
    addProperty(_period);

    _resolution = static_cast<int>(dictionary.value<double>(ResolutionInfo.identifier));
    _resolution.onChange([&] { _needsFullSweep = true; _indexBufferDirty = true; });
    addProperty(_resolution);

    // We store the vertices with (excluding the wrapping) decending temporal order
    _primaryRenderInformation.sorting = RenderInformation::VertexSorting::NewestFirst;

    if (dictionary.hasKey(RenderableTypeInfo.identifier)) {
        std::string renderType = dictionary.value<std::string>(
            RenderableTypeInfo.identifier
            );
        if (renderType == "Background") {
            setRenderBin(Renderable::RenderBin::Background);
        }
        else if (renderType == "Opaque") {
            setRenderBin(Renderable::RenderBin::Opaque);
        }
        else if (renderType == "Transparent") {
            setRenderBin(Renderable::RenderBin::Transparent);
        }
        else if (renderType == "Overlay") {
            setRenderBin(Renderable::RenderBin::Overlay);
        }
    }
    else {
        setRenderBin(Renderable::RenderBin::Overlay);
    }
}

void RenderableTrailOrbit::initializeGL() {
    RenderableTrail::initializeGL();

    glGenVertexArrays(1, &_primaryRenderInformation._vaoID);
    glGenBuffers(1, &_primaryRenderInformation._vBufferID);
    glGenBuffers(1, &_primaryRenderInformation._iBufferID);
}

void RenderableTrailOrbit::deinitializeGL() {
    glDeleteVertexArrays(1, &_primaryRenderInformation._vaoID);
    glDeleteBuffers(1, &_primaryRenderInformation._vBufferID);
    glDeleteBuffers(1, &_primaryRenderInformation._iBufferID);

    RenderableTrail::deinitializeGL();
}

void RenderableTrailOrbit::update(const UpdateData& data) {
    // Overview:
    // 1. Update trails
    // 2. Update floating position
    // 3. Determine which parts of the array to upload and upload the data

    // 1
    // Update the trails; the report contains whether any of the other values has been
    // touched and if so, how many
    const UpdateReport report = updateTrails(data);
    _previousTime = data.time.j2000Seconds();

    // Do not do anything if no point needs to be updated
    if (!report.permanentPointsNeedUpdate && !report.floatingPointNeedsUpdate) {
        return;
    }

    // 2
    // Write the current location into the floating position
    const glm::vec3 p = _translation->position({
        {},
        data.time,
        Time(0.0),
        false
    });
    _vertexArray[_primaryRenderInformation.first] = { p.x, p.y, p.z };

    glBindVertexArray(_primaryRenderInformation._vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _primaryRenderInformation._vBufferID);

    // 3
    if (!report.permanentPointsNeedUpdate) {
        if (report.floatingPointNeedsUpdate) {
            // If no other values have been touched, we only need to upload the
            // floating value
            glBufferSubData(
                GL_ARRAY_BUFFER,
                _primaryRenderInformation.first * sizeof(TrailVBOLayout),
                sizeof(TrailVBOLayout),
                _vertexArray.data() + _primaryRenderInformation.first
            );
        }
    }
    else {
        // Otherwise we need to check how many values have been changed
        if (report.nUpdated == UpdateReport::All) {
            // If all of the values have been invalidated, we need to upload the entire
            // array
            glBufferData(
                GL_ARRAY_BUFFER,
                _vertexArray.size() * sizeof(TrailVBOLayout),
                _vertexArray.data(),
                GL_STREAM_DRAW
            );

            if (_indexBufferDirty) {
                // We only need to upload the index buffer if it has been invalidated
                // by changing the number of values we want to represent
                glBindBuffer(
                    GL_ELEMENT_ARRAY_BUFFER,
                    _primaryRenderInformation._iBufferID
                );
                glBufferData(
                    GL_ELEMENT_ARRAY_BUFFER,
                    _indexArray.size() * sizeof(unsigned int),
                    _indexArray.data(),
                    GL_STATIC_DRAW
                );
                _indexBufferDirty = false;
            }
        }
        else {
            // The lambda expression that will upload parts of the array starting at
            // begin and containing length number of elements
            auto upload = [this](int begin, int length) {
                glBufferSubData(
                    GL_ARRAY_BUFFER,
                    begin * sizeof(TrailVBOLayout),
                    sizeof(TrailVBOLayout) * length,
                    _vertexArray.data() + begin
                );
            };

            // Only update the changed ones
            // Since we are using a ring buffer, the number of updated needed might be
            // bigger than our current points, which means we have to split the upload
            // into two calls.
            if (report.nUpdated > 0) {
                // deltaT is positive, so the pointer is moving backwards and update has
                // to happen towards the front

                // Starting index
                const int i = _primaryRenderInformation.first;
                // Number of values
                const int n = report.nUpdated + 1; // +1 for the floating position
                // Total size of the array
                const int s = _primaryRenderInformation.count;

                if (i + n <= s) {
                    // The current index is small enough to just use one upload call
                    upload(i, n);
                }
                else {
                    // The current index is too close to the wrap around part, so we need
                    // to split the upload into two parts:
                    // 1. from the current index to the end of the array
                    // 2. the rest starting from the beginning of the array
                    const int first = s - i;
                    const int second = n - first;
                    upload(i, first);  // 1
                    upload(0, second); // 2
                }
            }
            else {
                // deltaT is negative, so the pointer is moving forwards

                // The current index
                const int i = _primaryRenderInformation.first;
                // Number of values
                const int n = std::abs(report.nUpdated) + 1; // +1 for the floating pos
                // Total size of the array
                const int s = _primaryRenderInformation.count;

                if (i + 1 >= n) {
                    // The current index is big enough to fit everything into one call
                    upload(i+1 - n, n);
                }
                else {
                    // The current index is too close to the beginning of the array, so we
                    // need to split the upload into two parts:
                    // 1. from the beginning of the array to the current index
                    // 2. filling the back of the array with the rest
                    const int b = n - (i + 1);
                    upload(0, i + 1); // 1
                    upload(s-b, b);   // 2
                }
            }
        }
    }

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

    glBindVertexArray(0);

    // Updating bounding sphere
    glm::vec3 maxVertex(-std::numeric_limits<float>::max());
    glm::vec3 minVertex(std::numeric_limits<float>::max());

    auto setMax = [&maxVertex, &minVertex](const TrailVBOLayout& vertexData) {
        maxVertex.x = std::max(maxVertex.x, vertexData.x);
        maxVertex.y = std::max(maxVertex.y, vertexData.y);
        maxVertex.z = std::max(maxVertex.z, vertexData.z);

        minVertex.x = std::min(minVertex.x, vertexData.x);
        minVertex.y = std::min(minVertex.y, vertexData.y);
        minVertex.z = std::min(minVertex.z, vertexData.z);
    };

    std::for_each(_vertexArray.begin(), _vertexArray.end(), setMax);

    setBoundingSphere(glm::distance(maxVertex, minVertex) / 2.f);
}

RenderableTrailOrbit::UpdateReport RenderableTrailOrbit::updateTrails(
                                                                   const UpdateData& data)
{
    if (_needsFullSweep) {
        fullSweep(data.time.j2000Seconds());
        return { false, true, UpdateReport::All } ;
    }


    constexpr const double Epsilon = 1e-7;
    // When time stands still (at the iron hill), we don't need to perform any work
    if (std::abs(data.time.j2000Seconds() - _previousTime) < Epsilon) {
        return { false, false, 0 };
    }

    const double secondsPerPoint = _period / (_resolution - 1);
    // How much time has passed since the last permanent point
    const double delta = data.time.j2000Seconds() - _lastPointTime;

    // We'd like to test for equality with 0 here, but due to rounding issues, we won't
    // get there. If this check is not here, we will trigger the positive or negative
    // branch below even though we don't have to
    //
    // This might become a bigger issue if we are starting to look at very short time
    // intervals

    if (std::abs(delta) < Epsilon) {
        return { false, false, 0 };
    }

    if (delta > 0.0) {
        // Check whether we need to drop a new permanent point. This is only the case if
        // enough (> secondsPerPoint) time has passed since the last permanent point
        if (std::abs(delta) < secondsPerPoint) {
            return { true, false, 0 };
        }

        // See how many points we need to drop
        const int nNewPoints = static_cast<int>(floor(delta / secondsPerPoint));

        // If we would need to generate more new points than there are total points in the
        // array, it is faster to regenerate the entire array
        if (nNewPoints >= _resolution) {
            fullSweep(data.time.j2000Seconds());
            return { false, true, UpdateReport::All };
        }

        for (int i = 0; i < nNewPoints; ++i) {
            _lastPointTime += secondsPerPoint;

            // Get the new permanent point and write it into the (previously) floating
            // location
            const glm::vec3 p = _translation->position({
                {},
                Time(_lastPointTime),
                Time(0.0),
                false
            });
            _vertexArray[_primaryRenderInformation.first] = { p.x, p.y, p.z };

            // Move the current pointer back one step to be used as the new floating
            // location
            --_primaryRenderInformation.first;
            // And loop around if necessary
            if (_primaryRenderInformation.first < 0) {
                _primaryRenderInformation.first += _primaryRenderInformation.count;
            }
        }

        // The previously oldest permanent point has been moved nNewPoints steps into the
        // future
        _firstPointTime += nNewPoints * secondsPerPoint;

        return { false, true, nNewPoints };
    }
    else {
        // See how many new points needs to be generated. Delta is negative, so we need
        // to invert the ratio
        const int nNewPoints = -(static_cast<int>(floor(delta / secondsPerPoint)));

        // If we would need to generate more new points than there are total points in the
        // array, it is faster to regenerate the entire array
        if (nNewPoints >= _resolution) {
            fullSweep(data.time.j2000Seconds());
            return { false, true, UpdateReport::All };
        }

        for (int i = 0; i < nNewPoints; ++i) {
            _firstPointTime -= secondsPerPoint;

            // Get the new permanent point and write it into the (previously) floating
            // location
            const glm::vec3 p = _translation->position({
                {},
                Time(_firstPointTime),
                Time(0.0),
                false
            });
            _vertexArray[_primaryRenderInformation.first] = { p.x, p.y, p.z };

            // if we are on the upper bounds of the array, we start at 0
            if (_primaryRenderInformation.first == _primaryRenderInformation.count - 1) {
                // If it is at the beginning, set it to the end first
                _primaryRenderInformation.first = 0;
            }
            else {
                // Move the current pointer fowards one step  to be used as the new
                // floating
                ++_primaryRenderInformation.first;
            }
        }

        // The previously youngest point has become nNewPoints steps older
        _lastPointTime -= nNewPoints * secondsPerPoint;

        return { false, true, -nNewPoints };
    }
}

void RenderableTrailOrbit::fullSweep(double time) {
    // Reserve the space for the vertices
    _vertexArray.clear();
    _vertexArray.resize(_resolution);

    // The index buffer stays constant until we change the size of the array
    if (_indexBufferDirty) {
        // Create the index buffer and fill it with two ranges for [0, _resolution)
        _indexArray.clear();
        _indexArray.resize(_resolution * 2);
        std::iota(_indexArray.begin(), _indexArray.begin() + _resolution, 0);
        std::iota(_indexArray.begin() + _resolution, _indexArray.end(), 0);
    }

    _lastPointTime = time;

    const double secondsPerPoint = _period / (_resolution - 1);
    // starting at 1 because the first position is a floating current one
    for (int i = 1; i < _resolution; ++i) {
        const glm::vec3 p = _translation->position({ {}, Time(time), Time(0.0), false });
        _vertexArray[i] = { p.x, p.y, p.z };

        time -= secondsPerPoint;
    }

    _primaryRenderInformation.first = 0;
    _primaryRenderInformation.count = _resolution;

    _firstPointTime = time + secondsPerPoint;
    _needsFullSweep = false;
}

} // namespace openspace
