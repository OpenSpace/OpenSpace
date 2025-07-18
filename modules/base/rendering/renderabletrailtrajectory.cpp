/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/base/rendering/renderabletrailtrajectory.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/scene/translation.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timeconstants.h>
#include <openspace/util/updatestructures.h>
#include <optional>

// This class creates the entire trajectory at once and keeps it in memory the entire
// time. This means that there is no need for updating the trail at runtime, but also that
// the whole trail has to fit in memory.
// Opposed to the RenderableTrailOrbit, no index buffer is needed as the vertex can be
// written into the vertex buffer object continuously and then selected by using the
// count variable from the RenderInformation struct to toggle rendering of the entire path
// or subpath.
// In addition, this RenderableTrail implementation uses an additional RenderInformation
// bucket that contains the line from the last shown point to the current location of the
// object iff not the entire path is shown and the object is between _startTime and
// _endTime. This buffer is updated every frame.

namespace {
    constexpr openspace::properties::Property::PropertyInfo StartTimeInfo = {
        "StartTime",
        "Start Time",
        "The start time for the range of this trajectory. The date must be in ISO 8601 "
        "format: YYYY MM DD HH:mm:ss.xxx.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo EndTimeInfo = {
        "EndTime",
        "End Time",
        "The end time for the range of this trajectory. The date must be in ISO 8601 "
        "format: YYYY MM DD HH:mm:ss.xxx.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SampleIntervalInfo = {
        "SampleInterval",
        "Sample Interval",
        "The interval between samples of the trajectory. This value (together with "
        "'TimeStampSubsampleFactor') determines how far apart (in seconds) the samples "
        "are spaced along the trajectory.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TimeSubSampleInfo = {
        "TimeStampSubsampleFactor",
        "Time Stamp Subsampling Factor",
        "The factor that is used to create subsamples along the trajectory. This value "
        "(together with 'SampleInterval') determines how far apart (in seconds) the "
        "samples are spaced along the trajectory. Subsamples are rendered as smaller "
		"points compared to normal samples (from 'SampleInterval') when rendering the "
		"trail as points.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RenderFullPathInfo = {
        "ShowFullTrail",
        "Render Full Trail",
        "If true, the entire trail will be rendered. If false, only the trail until "
        "the current time in the application will be shown.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo AccurateTrailInfo = {
        "AccurateTrail",
        "Use Accurate Trail",
        "If true, the trail around the spacecraft will be recalculated to present a "
        "smoother trail. If false, the original trail will be used.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AccurateTrailPositionsInfo = {
        "AccurateTrailPositions",
        "Number of Accurate Trail Points",
        "The number of vertices, each side of the object, that will be recalculated "
        "for greater accuracy. This also ensures that the object connects with the "
        "trail.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableTrailTrajectory)]] Parameters {
        // [[codegen::verbatim(StartTimeInfo.description)]]
        std::string startTime [[codegen::annotation("A valid date in ISO 8601 format")]];

        // [[codegen::verbatim(EndTimeInfo.description)]]
        std::string endTime [[codegen::annotation("A valid date in ISO 8601 format")]];

        // [[codegen::verbatim(SampleIntervalInfo.description)]]
        // The final interval is calculated as SampleInterval/TimeStampSubsampleFactor.
        // If SampleInterval is not specified, it will be automatically calculated to produce
        // two samples per day between the 'StartTime' and 'EndTime'.
        std::optional<double> sampleInterval;

        // [[codegen::verbatim(TimeSubSampleInfo.description)]]
        // The final interval is calculated as SampleInterval/TimeStampSubsampleFactor.
        std::optional<int> timeStampSubsampleFactor;

        // [[codegen::verbatim(RenderFullPathInfo.description)]]
        std::optional<bool> showFullTrail;

        // [[codegen::verbatim(AccurateTrailInfo.description)]]
        std::optional<bool> useAccurateTrail;

        // [[codegen::verbatim(AccurateTrailPositionsInfo.description)]]
        std::optional<int> accurateTrailPositions;
    };
#include "renderabletrailtrajectory_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableTrailTrajectory::Documentation() {
    return codegen::doc<Parameters>(
        "base_renderable_renderabletrailtrajectory",
        RenderableTrail::Documentation()
    );
}

RenderableTrailTrajectory::RenderableTrailTrajectory(const ghoul::Dictionary& dictionary)
    : RenderableTrail(dictionary)
    , _startTime(StartTimeInfo)
    , _endTime(EndTimeInfo)
    , _sampleInterval(
        SampleIntervalInfo,
        openspace::timeconstants::SecondsPerDay / 2.0,
        1.0,
        1e6
    )
    , _timeStampSubsamplingFactor(TimeSubSampleInfo, 1, 1, 100)
    , _renderFullTrail(RenderFullPathInfo, false)
    , _useAccurateTrail(AccurateTrailInfo, true)
    , _nReplacementPoints(AccurateTrailPositionsInfo, 100, 2, 1000)
    , _maxVertex(glm::vec3(-std::numeric_limits<float>::max()))
    , _minVertex(glm::vec3(std::numeric_limits<float>::max()))
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _translation->onParameterChange([this]() { reset(); });

    _renderFullTrail = p.showFullTrail.value_or(_renderFullTrail);
    addProperty(_renderFullTrail);

    _useAccurateTrail = p.useAccurateTrail.value_or(_useAccurateTrail);
    addProperty(_useAccurateTrail);

    _nReplacementPoints = p.accurateTrailPositions.value_or(_nReplacementPoints);
    _nReplacementPoints.onChange([this]() {
        _nReplacementPoints = std::max<int>(2, _nReplacementPoints);
    });
    addProperty(_nReplacementPoints);

    _startTime = p.startTime;
    _startTime.onChange([this] { reset(); });
    addProperty(_startTime);

    _endTime = p.endTime;
    _endTime.onChange([this] { reset(); });
    addProperty(_endTime);

    _sampleInterval = p.sampleInterval.value_or(_sampleInterval);

    _sampleInterval.onChange([this]() { reset(); });
    addProperty(_sampleInterval);

    _timeStampSubsamplingFactor =
        p.timeStampSubsampleFactor.value_or(_timeStampSubsamplingFactor);
	_timeStampSubsamplingFactor.onChange([this]() { reset(); });
    addProperty(_timeStampSubsamplingFactor);

    // We store the vertices with ascending temporal order
    _primaryRenderInformation.sorting = RenderInformation::VertexSorting::OldestFirst;
    _secondaryRenderInformation.sorting = RenderInformation::VertexSorting::OldestFirst;

    // Activate special render mode for renderableTrailTrajectory
    _useSplitRenderMode = true;
}

void RenderableTrailTrajectory::initializeGL() {
    RenderableTrail::initializeGL();

    // We don't need an index buffer, so we keep it at the default value of 0
    glGenVertexArrays(1, &_primaryRenderInformation._vaoID);
    glGenBuffers(1, &_primaryRenderInformation._vBufferID);

    // We do need an additional render information bucket for the additional line from the
    // last shown permanent line to the current position of the object
    glGenVertexArrays(1, &_floatingRenderInformation._vaoID);
    glGenBuffers(1, &_floatingRenderInformation._vBufferID);
    _floatingRenderInformation.sorting = RenderInformation::VertexSorting::OldestFirst;

    _secondaryRenderInformation._vaoID = _primaryRenderInformation._vaoID;
    _secondaryRenderInformation._vBufferID = _primaryRenderInformation._vBufferID;
}

void RenderableTrailTrajectory::deinitializeGL() {
    glDeleteVertexArrays(1, &_primaryRenderInformation._vaoID);
    glDeleteBuffers(1, &_primaryRenderInformation._vBufferID);

    glDeleteVertexArrays(1, &_floatingRenderInformation._vaoID);
    glDeleteBuffers(1, &_floatingRenderInformation._vBufferID);

    RenderableTrail::deinitializeGL();
}

void RenderableTrailTrajectory::reset() {
    _needsFullSweep = true;
    _maxVertex = glm::vec3(-std::numeric_limits<float>::max());
    _minVertex = glm::vec3(std::numeric_limits<float>::max());
}

void RenderableTrailTrajectory::updateBuffer() {
    // Convert the start and end time from string representations to J2000 seconds
    _start = SpiceManager::ref().ephemerisTimeFromDate(_startTime);
    _end = SpiceManager::ref().ephemerisTimeFromDate(_endTime);
    const double timespan = _end - _start;

    _totalSampleInterval = _sampleInterval / _timeStampSubsamplingFactor;
    _nVertices = static_cast<unsigned int>(std::ceil(timespan / _totalSampleInterval));

    // Make space for the vertices
    _vertexArray.clear();
    _dVertexArray.clear();
    _timeVector.clear();
    _vertexArray.resize(_nVertices + 1);
    _dVertexArray.resize(_nVertices + 1);
    _timeVector.resize(_nVertices + 1);

    // Calculate all vertex positions
    for (unsigned int i = 0; i < _nVertices; i++) {
        const glm::dvec3 dp = translationPosition(
            Time(_start + i * _totalSampleInterval)
        );
        const glm::vec3 p = dp;
        _vertexArray[i] = { p.x, p.y, p.z };
        _timeVector[i] = Time(_start + i * _totalSampleInterval).j2000Seconds();
        _dVertexArray[i] = { dp.x, dp.y, dp.z };

        // Set max and min vertex for bounding sphere calculations
        _maxVertex = glm::max(_maxVertex, dp);
        _minVertex = glm::min(_minVertex, dp);
    }

    // Full sweep is complete here.
    // Adds the last point in time to the _vertexArray so that we
    // ensure that points for _start and _end always exists
    const glm::dvec3 dp = translationPosition(Time(_end));
    const glm::vec3 p = dp;
    _vertexArray[_nVertices] = { p.x, p.y, p.z };
    _timeVector[_nVertices] = Time(_end).j2000Seconds();
    _dVertexArray[_nVertices] = { dp.x, dp.y, dp.z };

    setBoundingSphere(glm::distance(_maxVertex, _minVertex) / 2.0);

    // Upload vertices to the GPU
    glBindVertexArray(_primaryRenderInformation._vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _primaryRenderInformation._vBufferID);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexArray.size() * sizeof(TrailVBOLayout<float>),
        _vertexArray.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

    // We clear the indexArray just in case. The base class will take care not to use
    // it if it is empty
    _indexArray.clear();

    _primaryRenderInformation.stride = _timeStampSubsamplingFactor;
    _secondaryRenderInformation.stride = _timeStampSubsamplingFactor;
    _floatingRenderInformation.stride = _timeStampSubsamplingFactor;
    _needsFullSweep = false;
}

void RenderableTrailTrajectory::update(const UpdateData& data) {
    if (_needsFullSweep) {
        updateBuffer();
    }

    // This has to be done every update step;
    const double j2k = data.time.j2000Seconds();

    if (j2k >= _start && j2k < _end) {
        _replacementPoints.clear();

        // Calculates number of vertices for the first segment (start point to object)
        _primaryRenderInformation.count = static_cast<GLsizei>(
            std::distance(
                _timeVector.begin(),
                std::lower_bound(_timeVector.begin(), _timeVector.end(), j2k)
            )
        );

        if (_renderFullTrail) {
            // Calculates number of vertices for the second segment (object to end point)
            _secondaryRenderInformation.first = _primaryRenderInformation.count;
            _secondaryRenderInformation.count = static_cast<GLsizei>(
                _vertexArray.size() - _primaryRenderInformation.count
            );

            // Calculate number of vertices in the trail
            _nUniqueVertices = static_cast<GLsizei>(
                std::distance(_timeVector.begin(), _timeVector.end())
            );
        }
        else {
            // If we don't render full trail there's no trail after the object
            _secondaryRenderInformation.first = 0;
            _secondaryRenderInformation.count = 0;

            // Set number of vertices in the trail
            _nUniqueVertices = _primaryRenderInformation.count;
        }

        // Get current position of the object
        const glm::dvec3 p = translationPosition(data.time);

        // Determine the number of points before the object to be recalculated
        int prePaddingDelta = 0;
        if (_useAccurateTrail) {
            prePaddingDelta = std::min<int>(
                _primaryRenderInformation.count,
                _nReplacementPoints
            );
        }
        else if (_renderFullTrail) {
            _primaryRenderInformation.count += 1;
        }
        else {
            prePaddingDelta = std::min(_primaryRenderInformation.count, 2);
        }

        glm::dvec3 v = p;
        for (int i = 0; i < prePaddingDelta; i++) {
            const int floatPointIndex =
                (_primaryRenderInformation.count - prePaddingDelta) + i;

            glm::dvec3 fp = glm::dvec3(
                _vertexArray[floatPointIndex].x,
                _vertexArray[floatPointIndex].y,
                _vertexArray[floatPointIndex].z
            );

            glm::dvec3 dp = glm::dvec3(
                _dVertexArray[floatPointIndex].x,
                _dVertexArray[floatPointIndex].y,
                _dVertexArray[floatPointIndex].z
            );

            glm::dvec3 dv = fp - dp;
            glm::dvec3 newPoint = dp - v;

            // Scales offset for smooth transition from original to accurate points
            const double mult = (i == prePaddingDelta - 1 && i > 0) ?
                0.0 : (prePaddingDelta - i) / static_cast<double>(prePaddingDelta);

            newPoint += dv * pow(mult, 2.0);
            _replacementPoints.push_back({
                static_cast<float>(newPoint.x),
                static_cast<float>(newPoint.y),
                static_cast<float>(newPoint.z)
            });
        }

        // Mid-point (model-space position for the object)
        if (_useAccurateTrail || !_renderFullTrail) {
            _replacementPoints.push_back({ 0.f, 0.f, 0.f });
        }

        // Calculates all replacement points after the object
        int postPaddingDelta = _secondaryRenderInformation.count;
        if (_useAccurateTrail) {
            postPaddingDelta = std::min<int>(
                _secondaryRenderInformation.count,
                _nReplacementPoints
            );
        }

        v = p;
        for (int i = 0; i < postPaddingDelta; i++) {
            const int floatPointIndex = _secondaryRenderInformation.first + i;

            glm::dvec3 fp = glm::dvec3(
                _vertexArray[floatPointIndex].x,
                _vertexArray[floatPointIndex].y,
                _vertexArray[floatPointIndex].z
            );

            glm::dvec3 dp = glm::dvec3(
                _dVertexArray[floatPointIndex].x,
                _dVertexArray[floatPointIndex].y,
                _dVertexArray[floatPointIndex].z
            );

            glm::dvec3 dv = fp - dp;
            glm::dvec3 newPoint = dp - v;

            // Scales offset for smooth transition from original to accurate points
            double mult = 1.0;
            if (_useAccurateTrail && i != postPaddingDelta - 1) {
                mult -= (postPaddingDelta - i) / static_cast<double>(postPaddingDelta);
            }

            newPoint += dv * pow(mult, 2.0);
            _replacementPoints.push_back({
                static_cast<float>(newPoint.x),
                static_cast<float>(newPoint.y),
                static_cast<float>(newPoint.z)
            });
        }

        // Set variables for floating segments
        _floatingRenderInformation.first = 0;
        _floatingRenderInformation.count =
            static_cast<GLsizei>(_replacementPoints.size());

        _floatingRenderInformation._localTransform = glm::translate(glm::dmat4(1.0), p);

        // Adjusts number of unique vertices if we have inserted a new mid point
        if (_floatingRenderInformation.count > 0 && _renderFullTrail) {
            _nUniqueVertices += 1;
        }

        // Recalculate .count and .first based on the recalculated (floating) vertices
        _primaryRenderInformation.count -= std::max(0, prePaddingDelta - 1);
        _secondaryRenderInformation.first += std::max(0, postPaddingDelta - 1);
        _secondaryRenderInformation.count -= std::max(0, postPaddingDelta - 1);

        // Adjusts count such that it takes into account if we don't have any line
        // connecting with the object
        if (_renderFullTrail && _nReplacementPoints == 0) {
            _primaryRenderInformation.count += 1;
        }

        glBindVertexArray(_floatingRenderInformation._vaoID);
        glBindBuffer(GL_ARRAY_BUFFER, _floatingRenderInformation._vBufferID);
        glBufferData(
            GL_ARRAY_BUFFER,
            _replacementPoints.size() * sizeof(TrailVBOLayout<float>),
            _replacementPoints.data(),
            GL_DYNAMIC_DRAW
        );
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);
    }
    else {
        _primaryRenderInformation.first = 0;
        _primaryRenderInformation.count = 0;
        _secondaryRenderInformation.first = 0;
        _secondaryRenderInformation.count = 0;
        _floatingRenderInformation.first = 0;
        _floatingRenderInformation.count = 0;
        if (_renderFullTrail || j2k >= _end) {
            // Renders the whole trail if time has passed the end time
            _primaryRenderInformation.count = static_cast<GLsizei>(_vertexArray.size());
            _nUniqueVertices = _primaryRenderInformation.count;
        }
    }

    glBindVertexArray(0);
}

} // namespace openspace
