/**********************************************************************
 *
 * GEOS - Geometry Engine Open Source
 * http://geos.osgeo.org
 *
 * Copyright (C) 2005-2006 Refractions Research Inc.
 * Copyright (C) 2001-2002 Vivid Solutions Inc.
 *
 * This is free software; you can redistribute and/or modify it under
 * the terms of the GNU Lesser General Public Licence as published
 * by the Free Software Foundation.
 * See the COPYING file for more information.
 *
 **********************************************************************/

#pragma once

#include <cstddef>
#include <geos/export.h>
#include <geos/geomgraph/index/SweepLineEventObj.h> // for inheritance

// Forward declarations
namespace geos {
namespace geom {
class CoordinateSequence;
}
namespace geomgraph {
class Edge;
namespace index {
class SegmentIntersector;
}
}
}

namespace geos {
namespace geomgraph { // geos::geomgraph
namespace index { // geos::geomgraph::index

class GEOS_DLL SweepLineSegment: public SweepLineEventOBJ {
public:
    SweepLineSegment(Edge* newEdge, std::size_t newPtIndex);
    ~SweepLineSegment() override = default;
    double getMinX();
    double getMaxX();
    void computeIntersections(SweepLineSegment* ss, SegmentIntersector* si);
protected:
    Edge* edge;
    const geom::CoordinateSequence* pts;
    std::size_t ptIndex;
};



} // namespace geos.geomgraph.index
} // namespace geos.geomgraph
} // namespace geos

