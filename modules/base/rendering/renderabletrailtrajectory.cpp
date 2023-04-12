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

#include <modules/base/rendering/renderabletrailtrajectory.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/scene/translation.h>
#include <openspace/util/spicemanager.h>
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
        "format: YYYY MM DD HH:mm:ss.xxx"
    };

    constexpr openspace::properties::Property::PropertyInfo EndTimeInfo = {
        "EndTime",
        "End Time",
        "The end time for the range of this trajectory. The date must be in ISO 8601 "
        "format: YYYY MM DD HH:mm:ss.xxx"
    };

    constexpr openspace::properties::Property::PropertyInfo SampleIntervalInfo = {
        "SampleInterval",
        "Sample Interval",
        "The interval between samples of the trajectory. This value (together with "
        "'TimeStampSubsampleFactor') determines how far apart (in time) the samples are "
        "spaced along the trajectory. The time interval between 'StartTime' and "
        "'EndTime' is split into 'SampleInterval' * 'TimeStampSubsampleFactor' segments"
    };

    constexpr openspace::properties::Property::PropertyInfo TimeSubSampleInfo = {
        "TimeStampSubsampleFactor",
        "Time Stamp Subsampling Factor",
        "The factor that is used to create subsamples along the trajectory. This value "
        "(together with 'SampleInterval') determines how far apart (in time) the samples "
        "are spaced along the trajectory. The time interval between 'StartTime' and "
        "'EndTime' is split into 'SampleInterval' * 'TimeStampSubsampleFactor' segments"
    };

    constexpr openspace::properties::Property::PropertyInfo RenderFullPathInfo = {
        "ShowFullTrail",
        "Render Full Trail",
        "If this value is set to 'true', the entire trail will be rendered; if it is "
        "'false', only the trail until the current time in the application will be shown"
    };

    constexpr openspace::properties::Property::PropertyInfo SweepChunkSizeInfo = {
        "SweepChunkSize",
        "Sweep Chunk Size",
        "The number of vertices that will be calculated each frame whenever the trail "
        "needs to be recalculated. "
        "A greater value will result in more calculations per frame."
    };

    struct [[codegen::Dictionary(RenderableTrailTrajectory)]] Parameters {
        // [[codegen::verbatim(StartTimeInfo.description)]]
        std::string startTime [[codegen::annotation("A valid date in ISO 8601 format")]];

        // [[codegen::verbatim(EndTimeInfo.description)]]
        std::string endTime [[codegen::annotation("A valid date in ISO 8601 format")]];

        // [[codegen::verbatim(SampleIntervalInfo.description)]]
        double sampleInterval;

        // [[codegen::verbatim(TimeSubSampleInfo.description)]]
        std::optional<int> timeStampSubsampleFactor;

        // [[codegen::verbatim(RenderFullPathInfo.description)]]
        std::optional<bool> showFullTrail;

        // [[codegen::verbatim(SweepChunkSizeInfo.description)]]
        std::optional<int> sweepChunkSize;
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
    , _sampleInterval(SampleIntervalInfo, 2.0, 2.0, 1e6)
    , _timeStampSubsamplingFactor(TimeSubSampleInfo, 1, 1, 1000000000)
    , _renderFullTrail(RenderFullPathInfo, false)
    , _maxVertex(glm::vec3(-std::numeric_limits<float>::max()))
    , _minVertex(glm::vec3(std::numeric_limits<float>::max()))
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _translation->onParameterChange([this]() { reset(); });

    _startTime = p.startTime;
    _startTime.onChange([this] { reset(); });
    addProperty(_startTime);

    _endTime = p.endTime;
    _endTime.onChange([this] { reset(); });
    addProperty(_endTime);

    _sampleInterval = p.sampleInterval;
    _sampleInterval.onChange([this] { reset(); });
    addProperty(_sampleInterval);

    _timeStampSubsamplingFactor =
        p.timeStampSubsampleFactor.value_or(_timeStampSubsamplingFactor);
    _timeStampSubsamplingFactor.onChange([this] { _subsamplingIsDirty = true; });
    addProperty(_timeStampSubsamplingFactor);

    _renderFullTrail = p.showFullTrail.value_or(_renderFullTrail);
    addProperty(_renderFullTrail);

    _sweepChunkSize = p.sweepChunkSize.value_or(_sweepChunkSize);

    // We store the vertices with ascending temporal order
    _primaryRenderInformation.sorting = RenderInformation::VertexSorting::OldestFirst;
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
    _sweepIteration = 0;
    _maxVertex = glm::vec3(-std::numeric_limits<float>::max());
    _minVertex = glm::vec3(std::numeric_limits<float>::max());
}

void RenderableTrailTrajectory::update(const UpdateData& data) {
    if (_needsFullSweep) {

        if (_sweepIteration == 0) {
            // Max number of vertices
            constexpr unsigned int maxNumberOfVertices = 1000000;

            // Convert the start and end time from string representations to J2000 seconds
            _start = SpiceManager::ref().ephemerisTimeFromDate(_startTime);
            _end = SpiceManager::ref().ephemerisTimeFromDate(_endTime);
            double timespan = _end - _start;

            _totalSampleInterval = _sampleInterval / _timeStampSubsamplingFactor;

            // Cap _numberOfVertices in order to prevent overflow and extreme performance
            // degredation/RAM usage
            _numberOfVertices = std::min(
                static_cast<unsigned int>(timespan / _totalSampleInterval),
                maxNumberOfVertices
            );

            // We need to recalcuate the _totalSampleInterval if _numberOfVertices eqals
            // maxNumberOfVertices. If we don't do this the position for each vertex
            // will not be correct for the number of vertices we are doing along the trail.
            _totalSampleInterval = (_numberOfVertices == maxNumberOfVertices) ?
                (timespan / _numberOfVertices) : _totalSampleInterval;

            // Make space for the vertices
            _vertexArray.clear();
            _vertexArray.resize(_numberOfVertices);
        }

        // Calculate sweeping range for this iteration
        unsigned int startIndex = _sweepIteration * _sweepChunkSize;
        unsigned int nextIndex = (_sweepIteration + 1) * _sweepChunkSize;
        unsigned int stopIndex = std::min(nextIndex, _numberOfVertices);

        // Calculate all vertex positions
        for (int i = startIndex; i < stopIndex; ++i) {
            const glm::vec3 p = _translation->position({
                {},
                Time(_start + i * _totalSampleInterval),
                Time(0.0)
            });
            _vertexArray[i] = { p.x, p.y, p.z };

            // Set max and min vertex for bounding sphere calculations
            _maxVertex = glm::max(_maxVertex, p);
            _minVertex = glm::min(_minVertex, p);
        }
        ++_sweepIteration;

        if (stopIndex == _numberOfVertices) {
            _sweepIteration = 0;
            setBoundingSphere(glm::distance(_maxVertex, _minVertex) / 2.f);
        }
        else {
            // Early return as we don't need to render if we are still
            // doing full sweep calculations
            return;
        }

        // Upload vertices to the GPU
        glBindVertexArray(_primaryRenderInformation._vaoID);
        glBindBuffer(GL_ARRAY_BUFFER, _primaryRenderInformation._vBufferID);
        glBufferData(
            GL_ARRAY_BUFFER,
            _vertexArray.size() * sizeof(TrailVBOLayout),
            _vertexArray.data(),
            GL_STATIC_DRAW
        );

        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

        // We clear the indexArray just in case. The base class will take care not to use
        // it if it is empty
        _indexArray.clear();

        _subsamplingIsDirty = true;
        _needsFullSweep = false;
    }

    // This has to be done every update step;
    if (_renderFullTrail) {
        // If the full trail should be rendered at all times, we can directly render the
        // entire set
        _primaryRenderInformation.first = 0;
        _primaryRenderInformation.count = static_cast<GLsizei>(_vertexArray.size());
    }
    else {
        // If only trail so far should be rendered, we need to find the corresponding time
        // in the array and only render it until then
        _primaryRenderInformation.first = 0;
        const double t = std::max(
            0.0,
            (data.time.j2000Seconds() - _start) / (_end - _start)
        );
        _primaryRenderInformation.count = std::min(
            static_cast<GLsizei>(ceil(_vertexArray.size() * t)),
            static_cast<GLsizei>(_vertexArray.size() - 1)
        );
    }

    // If we are inside the valid time, we additionally want to draw a line from the last
    // correct point to the current location of the object
    if (data.time.j2000Seconds() > _start &&
        data.time.j2000Seconds() <= _end && !_renderFullTrail)
    {
        ghoul_assert(_primaryRenderInformation.count > 0, "No vertices available");

        // Copy the last valid location
        glm::dvec3 v0(
            _vertexArray[_primaryRenderInformation.count - 1].x,
            _vertexArray[_primaryRenderInformation.count - 1].y,
            _vertexArray[_primaryRenderInformation.count - 1].z
        );

        // And get the current location of the object
        const glm::dvec3 p = _translation->position(data);
        const glm::dvec3 v1 = { p.x, p.y, p.z };

        // Comptue the difference between the points in double precision
        const glm::dvec3 p0 = v0 - v1;
        _auxiliaryVboData[0] = {
            static_cast<float>(p0.x),
            static_cast<float>(p0.y),
            static_cast<float>(p0.z)
        };
        _auxiliaryVboData[1] = { 0.f, 0.f, 0.f };

        // Fill the render info with the data
        _floatingRenderInformation.first = 0;
        _floatingRenderInformation.count = 2;

        _floatingRenderInformation._localTransform = glm::translate(glm::dmat4(1.0), v1);

        glBindVertexArray(_floatingRenderInformation._vaoID);
        glBindBuffer(GL_ARRAY_BUFFER, _floatingRenderInformation._vBufferID);
        glBufferData(
            GL_ARRAY_BUFFER,
            _auxiliaryVboData.size() * sizeof(TrailVBOLayout),
            _auxiliaryVboData.data(),
            GL_DYNAMIC_DRAW
        );

        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);
    }
    else {
        // if we are outside of the valid range, we don't render anything
        _floatingRenderInformation.first = 0;
        _floatingRenderInformation.count = 0;
    }

    if (_subsamplingIsDirty) {
        // If the subsampling information has changed (either by a property change or by
        // a request of a full sweep) we update it here
        _primaryRenderInformation.stride = _timeStampSubsamplingFactor;
        _floatingRenderInformation.stride = _timeStampSubsamplingFactor;
        _subsamplingIsDirty = false;
    }

    glBindVertexArray(0);

}

} // namespace openspace
