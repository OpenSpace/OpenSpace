/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLETRAILTRAJECTORY___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLETRAILTRAJECTORY___H__

#include <modules/base/rendering/renderabletrail.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <array>

namespace openspace {

namespace documentation { struct Documentation; }

/**
 * This concrete implementation of a RenderableTrail renders a fixed trail, regardless of
 * its shape. The trail is sampled equitemporal (with an interval of _sampleInterval in
 * seconds) between the _startTime and the _endTime. No further update is needed until any
 * of these three values is changed. If _renderFullTrail is true, the entirety of the
 * trail is rendered, regardless of the simulation time. If it is false, the trail is only
 * rendered from the past to the current simulation time, not showing any part of the
 * trail in the future. If _renderFullTrail is false, the current position of the object
 * has to be updated constantly to make the trail connect to the object that has the
 * trail.
 */
class RenderableTrailTrajectory : public RenderableTrail {
public:
    explicit RenderableTrailTrajectory(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    /// The start time of the trail
    properties::StringProperty _startTime;
    /// The end time of the trail
    properties::StringProperty _endTime;
    /// The interval (in seconds) between sample points
    properties::DoubleProperty _sampleInterval;
    /// The factor that determines the time stamp subsampling, using different sized
    /// points along the trajectory
    properties::IntProperty _timeStampSubsamplingFactor;
    /// Determines whether the full trail should be rendered or the future trail removed
    properties::BoolProperty _renderFullTrail;

    /// Dirty flag that determines whether the full vertex buffer needs to be resampled
    bool _needsFullSweep = true;

    /// Dirty flag to determine whether the stride information needs to be changed
    bool _subsamplingIsDirty = true;

    std::array<TrailVBOLayout, 2> _auxiliaryVboData = {};

    /// The conversion of the _startTime into the internal time format
    double _start = 0.0;
    /// The conversion of the _endTime into the internal time format
    double _end = 0.0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLETRAILTRAJECTORY___H__
