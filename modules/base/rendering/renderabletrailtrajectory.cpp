/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <openspace/documentation/verifier.h>
#include <openspace/scene/translation.h>
#include <openspace/util/spicemanager.h>

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
    const char* KeyTranslation = "Translation";
    const char* KeyStartTime = "StartTime";
    const char* KeyEndTime = "EndTime";
    const char* KeySampleInterval = "SampleInterval";
    const char* KeyTimeStampSubsample = "TimeStampSubsampleFactor";
    const char* KeyShowFullTrail = "ShowFullTrail";
}

namespace openspace {

openspace::Documentation RenderableTrailTrajectory::Documentation() {
    using namespace documentation;
    openspace::Documentation doc {
        "RenderableTrailTrajectory",
        "base_renderable_renderabletrailtrajectory",
        {
            {
                "Type",
                new StringEqualVerifier("RenderableTrailTrajectory"),
                "",
                Optional::No
            },
            {
                KeyStartTime,
                new StringAnnotationVerifier("A valid date"),
                "The start time for the range of this trajectory. The date must be in "
                "ISO 8601 format: YYYY MM DD HH:mm:ss.xxx",
                Optional::No
            },
            {
                KeyEndTime,
                new StringAnnotationVerifier("A valid date"),
                "The end time for the range of this trajectory. The date must be in "
                "ISO 8601 format: YYYY MM DD HH:mm:ss.xxx",
                Optional::No
            },
            {
                KeySampleInterval,
                new DoubleVerifier,
                "The interval between samples of the trajectory. This value (together "
                "with 'TimeStampSubsampleFactor') determines how far apart (in time) the "
                "samples are spaced along the trajectory. The time interval between "
                "'StartTime' and 'EndTime' is split into 'SampleInterval' * "
                "'TimeStampSubsampleFactor' segments.",
                Optional::No
            },
            {
                KeyTimeStampSubsample,
                new IntVerifier,
                "The factor that is used to create subsamples along the trajectory. This "
                "value (together with 'SampleInterval') determines how far apart (in "
                "time) the samples are spaced along the trajectory. The time interval "
                "between 'StartTime' and 'EndTime' is split into 'SampleInterval' * "
                "'TimeStampSubsampleFactor' segments. The default value for this is 1",
                Optional::Yes
            },
            {
                KeyShowFullTrail,
                new BoolVerifier,
                "If this value is set to 'true', the entire trail will be rendered; if "
                "it is 'false', only the trail until the current time in the application "
                "will be shown. The default value for this setting is 'false'.",
                Optional::Yes
            }
        }
    };

    // Insert the parents documentation entries until we have a verifier that can deal
    // with class hierarchy
    openspace::Documentation parentDoc = RenderableTrail::Documentation();
    doc.entries.insert(
        doc.entries.end(),
        parentDoc.entries.begin(),
        parentDoc.entries.end()
    );

    return doc;
}

RenderableTrailTrajectory::RenderableTrailTrajectory(const ghoul::Dictionary& dictionary)
    : RenderableTrail(dictionary)
    , _startTime("startTime", "Start Time")
    , _endTime("endTime", "End Time")
    , _sampleInterval("sampleInterval", "Sample Interval", 2.0, 2.0, 1e6)
    , _timeStampSubsamplingFactor(
        "subSample",
        "Time Stamp Subsampling Factor",
        1, 1, 1e9
    )
    , _renderFullTrail("renderFullTrail", "Render Full Trail", false)
    , _needsFullSweep(true)
    , _subsamplingIsDirty(true)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableTrailTrajectory"
    );

    _translation->onParameterChange([this]() {
        _needsFullSweep = true;
    });

    _startTime = dictionary.value<std::string>(KeyStartTime);
    _startTime.onChange([this] { _needsFullSweep = true; });
    addProperty(_startTime);

    _endTime = dictionary.value<std::string>(KeyEndTime);
    _endTime.onChange([this] { _needsFullSweep = true; });
    addProperty(_endTime);

    _sampleInterval = dictionary.value<double>(KeySampleInterval);
    _sampleInterval.onChange([this] { _needsFullSweep = true; });
    addProperty(_sampleInterval);

    if (dictionary.hasKeyAndValue<double>(KeyTimeStampSubsample)) {
        _timeStampSubsamplingFactor = dictionary.value<double>(KeyTimeStampSubsample);
    }
    _timeStampSubsamplingFactor.onChange([this] { _subsamplingIsDirty = true; });
    addProperty(_timeStampSubsamplingFactor);

    if (dictionary.hasKeyAndValue<bool>(KeyShowFullTrail)) {
        _renderFullTrail = dictionary.value<bool>(KeyShowFullTrail);
    }
    addProperty(_renderFullTrail);

    // We store the vertices with ascending temporal order
    _primaryRenderInformation.sorting = RenderInformation::VertexSorting::OldestFirst;
}

bool RenderableTrailTrajectory::initialize() {
    bool res = RenderableTrail::initialize();

    // We don't need an index buffer, so we keep it at the default value of 0
    glGenVertexArrays(1, &_primaryRenderInformation._vaoID);
    glGenBuffers(1, &_primaryRenderInformation._vBufferID);

    // We do need an additional render information bucket for the additional line from the
    // last shown permanent line to the current position of the object
    glGenVertexArrays(1, &_floatingRenderInformation._vaoID);
    glGenBuffers(1, &_floatingRenderInformation._vBufferID);
    _floatingRenderInformation.sorting = RenderInformation::VertexSorting::NoSorting;

    return res;
}

bool RenderableTrailTrajectory::deinitialize() {
    glDeleteVertexArrays(1, &_primaryRenderInformation._vaoID);
    glDeleteBuffers(1, &_primaryRenderInformation._vBufferID);

    glDeleteVertexArrays(1, &_floatingRenderInformation._vaoID);
    glDeleteBuffers(1, &_floatingRenderInformation._vBufferID);

    return RenderableTrail::deinitialize();
}

void RenderableTrailTrajectory::update(const UpdateData& data) {
    if (_needsFullSweep) {
        // Convert the start and end time from string representations to J2000 seconds
        _start = SpiceManager::ref().ephemerisTimeFromDate(_startTime);
        _end = SpiceManager::ref().ephemerisTimeFromDate(_endTime);

        double totalSampleInterval = _sampleInterval / _timeStampSubsamplingFactor;
        // How many values do we need to compute given the distance between the start and
        // end date and the desired sample interval
        int nValues = (_end - _start) / totalSampleInterval;

        // Make space for the vertices
        _vertexArray.clear();
        _vertexArray.resize(nValues);

        // ... fill all of the values
        for (int i = 0; i < nValues; ++i) {
            glm::vec3 p = _translation->position(_start + i * totalSampleInterval);
            _vertexArray[i] = { p.x, p.y, p.z };
        }

        // ... and upload them to the GPU
        glBindVertexArray(_primaryRenderInformation._vaoID);
        glBindBuffer(GL_ARRAY_BUFFER, _primaryRenderInformation._vBufferID);
        glBufferData(
            GL_ARRAY_BUFFER,
            _vertexArray.size() * sizeof(TrailVBOLayout),
            _vertexArray.data(),
            GL_STATIC_DRAW
        );

        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);
        
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
        _primaryRenderInformation.count = _vertexArray.size();
    }
    else {
        // If only trail so far should be rendered, we need to find the corresponding time
        // in the array and only render it until then
        _primaryRenderInformation.first = 0;
        double t = (data.time - _start) / (_end - _start);
        _primaryRenderInformation.count = std::min<GLsizei>(
            ceil(_vertexArray.size() * t),
            _vertexArray.size() - 1
        );
    }

    // If we are inside the valid time, we additionally want to draw a line from the last
    // correct point to the current location of the object
    if (data.time >= _start && data.time <= _end && !_renderFullTrail) {
        // Copy the last valid location
        glm::dvec3 v0(
            _vertexArray[_primaryRenderInformation.count - 1].x,
            _vertexArray[_primaryRenderInformation.count - 1].y,
            _vertexArray[_primaryRenderInformation.count - 1].z
        );
        
        // And get the current location of the object
        glm::dvec3 p = _translation->position(data.time);
        glm::dvec3 v1 = { p.x, p.y, p.z };

        // Comptue the difference between the points in double precision
        glm::dvec3 p0 = v0 - v1;
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
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);
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
