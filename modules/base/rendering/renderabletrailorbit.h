/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLETRAILORBIT___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLETRAILORBIT___H__

#include <modules/base/rendering/renderabletrail.h>

#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/intproperty.h>

namespace openspace {

namespace documentation { struct Documentation; }

/**
 * This concrete implementation of a RenderableTrail renders an updated trail behind an
 * object that is likely to have an orbit-like path. However, this is not required and
 * this class can render any kind of trail as long as it's individual positions can be
 * dynamically queried at runtime. This class always renders a number of points equal to
 * the _resolution number. If the time progresses, old points are discarded and new points
 * are rendered. Each of these fixed points are fixed time steps apart, where as the most
 * current point is floating and updated every frame. The _period determines the length of
 * the trail (the distance between the newest and oldest point being _period days).
 */
class RenderableTrailOrbit : public RenderableTrail {
public:
    explicit RenderableTrailOrbit(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    /**
     * Performs a full sweep of the orbit and fills the entire vertex buffer object.
     * \param time The current time up to which the full sweep should be performed
     */
    void fullSweep(double time);

    /// This structure is returned from the #updateTrails method and gives information
    /// about which parts of the vertex array to update
    struct UpdateReport {
        static const int All = 0; ///< The entire array was touched in the update
        /// If \c true the floating point needs to be updated
        bool floatingPointNeedsUpdate;
        /// If \c true at least one of their permanent point were touched
        bool permanentPointsNeedUpdate;
        /// Returns the number of fixed points that were touched in the update method
        /// If this value is negative, the newest values were replaced, if positive the
        /// oldest
        int nUpdated;
    };
    /**
     * Updates the trail based on the new incoming UpdateData information. This function
     * might update an arbitrary number of values in the vertex buffer and returns an
     * UpdateReport that will tell the #update method how many values were modified.
     * \param data The UpdateData struct that comes from the #update method
     * \return The UpdateReport containing information which array parts were touched
     */
    UpdateReport updateTrails(const UpdateData& data);

    /// The orbital period of the RenderableTrail in days
    properties::DoubleProperty _period;
    /// The number of points that should be sampled between _period and now
    properties::IntProperty _resolution;

    /// A dirty flag that determines whether a full sweep (recomputing of all values)
    /// is necessary
    bool _needsFullSweep = true;
    /// A dirty flag to determine whether the index buffer needs to be regenerated and
    /// then reuploaded
    bool _indexBufferDirty = true;

    /// The time stamp of the oldest point in the array
    double _firstPointTime = 0.0;
    /// The time stamp of the newest fixed point in the array
    double _lastPointTime = 0.0;
    /// The time stamp of when the last valid trail was generated.
    double _previousTime = 0.0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLETRAILORBIT___H__
