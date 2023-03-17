/**********************************************************************
 *
 * GEOS - Geometry Engine Open Source
 * http://geos.osgeo.org
 *
 * Copyright (C) 2011 Sandro Santilli <strk@kbt.io>
 * Copyright (C) 2005 Refractions Research Inc.
 * Copyright (C) 2001-2002 Vivid Solutions Inc.
 *
 * This is free software; you can redistribute and/or modify it under
 * the terms of the GNU Lesser General Public Licence as published
 * by the Free Software Foundation.
 * See the COPYING file for more information.
 *
 **********************************************************************
 *
 * Last port: operation/buffer/BufferCurveSetBuilder.java r378 (JTS-1.12)
 *
 **********************************************************************/

#include <geos/constants.h>
#include <geos/algorithm/Distance.h>
#include <geos/algorithm/Orientation.h>
#include <geos/util/IllegalArgumentException.h>
#include <geos/util/UnsupportedOperationException.h>
#include <geos/operation/buffer/BufferCurveSetBuilder.h>
#include <geos/operation/valid/RepeatedPointRemover.h>
#include <geos/geom/CoordinateSequence.h>
#include <geos/geom/Geometry.h>
#include <geos/geom/GeometryCollection.h>
#include <geos/geom/Point.h>
#include <geos/geom/LinearRing.h>
#include <geos/geom/LineString.h>
#include <geos/geom/Polygon.h>
#include <geos/geom/Location.h>
#include <geos/geom/Triangle.h>
#include <geos/geom/Position.h>
#include <geos/geomgraph/Label.h>
#include <geos/noding/NodedSegmentString.h>
#include <geos/util.h>

#include <algorithm> // for min
#include <cmath>
#include <cassert>
#include <memory>
#include <vector>
#include <typeinfo>

#ifndef GEOS_DEBUG
#define GEOS_DEBUG 0
#endif

using namespace geos::geom;
using geos::noding::NodedSegmentString;
using geos::noding::SegmentString;
using geos::geomgraph::Label;
using geos::algorithm::Distance;
using geos::algorithm::Orientation;

namespace geos {
namespace operation { // geos.operation
namespace buffer { // geos.operation.buffer


BufferCurveSetBuilder::~BufferCurveSetBuilder()
{
    for(std::size_t i = 0, n = curveList.size(); i < n; ++i) {
        SegmentString* ss = curveList[i];
        delete ss;
    }
    for(std::size_t i = 0, n = newLabels.size(); i < n; ++i) {
        delete newLabels[i];
    }
}

/* public */
std::vector<SegmentString*>&
BufferCurveSetBuilder::getCurves()
{
    add(inputGeom);
    return curveList;
}

/*public*/
void
BufferCurveSetBuilder::addCurves(const std::vector<CoordinateSequence*>& lineList,
                                 geom::Location leftLoc, geom::Location rightLoc)
{
    for(std::size_t i = 0, n = lineList.size(); i < n; ++i) {
        CoordinateSequence* coords = lineList[i];
        addCurve(coords, leftLoc, rightLoc);
    }
}

/*private*/
void
BufferCurveSetBuilder::addCurve(CoordinateSequence* coord,
                                geom::Location leftLoc, geom::Location rightLoc)
{
#if GEOS_DEBUG
    std::cerr << __FUNCTION__ << ": coords=" << coord->toString() << std::endl;
#endif
    // don't add null curves!
    if(coord->getSize() < 2) {
#if GEOS_DEBUG
        std::cerr << " skipped (size<2)" << std::endl;
#endif
        delete coord;
        return;
    }

    // add the edge for a coordinate list which is a raw offset curve
    Label* newlabel = new Label(0, Location::BOUNDARY, leftLoc, rightLoc);

    // coord ownership transferred to SegmentString
    SegmentString* e = new NodedSegmentString(coord, newlabel);

    // SegmentString doesnt own the sequence, so we need to delete in
    // the destructor
    newLabels.push_back(newlabel);
    curveList.push_back(e);
}


/*private*/
void
BufferCurveSetBuilder::add(const Geometry& g)
{
    if(g.isEmpty()) {
#if GEOS_DEBUG
        std::cerr << __FUNCTION__ << ": skip empty geometry" << std::endl;
#endif
        return;
    }

    const Polygon* poly = dynamic_cast<const Polygon*>(&g);
    if(poly) {
        addPolygon(poly);
        return;
    }

    const LineString* line = dynamic_cast<const LineString*>(&g);
    if(line) {
        addLineString(line);
        return;
    }

    const Point* point = dynamic_cast<const Point*>(&g);
    if(point) {
        addPoint(point);
        return;
    }

    const GeometryCollection* collection = dynamic_cast<const GeometryCollection*>(&g);
    if(collection) {
        addCollection(collection);
        return;
    }

    std::string out = typeid(g).name();
    throw util::UnsupportedOperationException("GeometryGraph::add(Geometry &): unknown geometry type: " + out);
}

/*private*/
void
BufferCurveSetBuilder::addCollection(const GeometryCollection* gc)
{
    for(std::size_t i = 0, n = gc->getNumGeometries(); i < n; i++) {
        const Geometry* g = gc->getGeometryN(i);
        add(*g);
    }
}

/*private*/
void
BufferCurveSetBuilder::addPoint(const Point* p)
{
    // a zero or negative width buffer of a point is empty
    if(distance <= 0.0) {
        return;
    }
    const CoordinateSequence* coord = p->getCoordinatesRO();
    if (coord->size() >= 1 && ! coord->getAt(0).isValid()) {
        return;
    }
    std::vector<CoordinateSequence*> lineList;
    curveBuilder.getLineCurve(coord, distance, lineList);

    addCurves(lineList, Location::EXTERIOR, Location::INTERIOR);
}

/*private*/
void
BufferCurveSetBuilder::addLineString(const LineString* line)
{
    if (curveBuilder.isLineOffsetEmpty(distance)) {
        return;
    }

    auto coord = operation::valid::RepeatedPointRemover::removeRepeatedAndInvalidPoints(line->getCoordinatesRO());

    /**
     * Rings (closed lines) are generated with a continuous curve,
     * with no end arcs. This produces better quality linework,
     * and avoids noding issues with arcs around almost-parallel end segments.
     * See JTS #523 and #518.
     *
     * Singled-sided buffers currently treat rings as if they are lines.
     */
    if (coord->isRing() && ! curveBuilder.getBufferParameters().isSingleSided()) {
        addRingBothSides(coord.get(), distance);
    }
    else {
        std::vector<CoordinateSequence*> lineList;
        curveBuilder.getLineCurve(coord.get(), distance, lineList);
        addCurves(lineList, Location::EXTERIOR, Location::INTERIOR);
    }

}


/*private*/
void
BufferCurveSetBuilder::addPolygon(const Polygon* p)
{
    double offsetDistance = distance;

    int offsetSide = Position::LEFT;
    if(distance < 0.0) {
        offsetDistance = -distance;
        offsetSide = Position::RIGHT;
    }

    const LinearRing* shell = p->getExteriorRing();

    // optimization - don't bother computing buffer
    // if the polygon would be completely eroded
    if(distance < 0.0 && isErodedCompletely(shell, distance)) {
#if GEOS_DEBUG
        std::cerr << __FUNCTION__ << ": polygon is eroded completely " << std::endl;
#endif
        return;
    }

    auto shellCoord =
            operation::valid::RepeatedPointRemover::removeRepeatedAndInvalidPoints(shell->getCoordinatesRO());

    // don't attempt to buffer a polygon
    // with too few distinct vertices
    if(distance <= 0.0 && shellCoord->size() < 3) {
        return;
    }

    addRingSide(
        shellCoord.get(),
        offsetDistance,
        offsetSide,
        Location::EXTERIOR,
        Location::INTERIOR);

    for(std::size_t i = 0, n = p->getNumInteriorRing(); i < n; ++i) {
        const LineString* hls = p->getInteriorRingN(i);
        const LinearRing* hole = detail::down_cast<const LinearRing*>(hls);

        // optimization - don't bother computing buffer for this hole
        // if the hole would be completely covered
        if(distance > 0.0 && isErodedCompletely(hole, -distance)) {
            continue;
        }

        auto holeCoord = valid::RepeatedPointRemover::removeRepeatedAndInvalidPoints(hole->getCoordinatesRO());

        // Holes are topologically labelled opposite to the shell,
        // since the interior of the polygon lies on their opposite
        // side (on the left, if the hole is oriented CCW)
        addRingSide(
            holeCoord.get(),
            offsetDistance,
            Position::opposite(offsetSide),
            Location::INTERIOR,
            Location::EXTERIOR);
    }
}

/* private */
void
BufferCurveSetBuilder::addRingBothSides(const CoordinateSequence* coord, double p_distance)
{
    addRingSide(coord, p_distance,
                Position::LEFT,
                Location::EXTERIOR, Location::INTERIOR);
    /* Add the opposite side of the ring
    */
    addRingSide(coord, p_distance,
                Position::RIGHT,
                Location::INTERIOR, Location::EXTERIOR);
}


/* private */
void
BufferCurveSetBuilder::addRingSide(const CoordinateSequence* coord,
                                      double offsetDistance, int side, geom::Location cwLeftLoc, geom::Location cwRightLoc)
{

    // don't bother adding ring if it is "flat" and
    // will disappear in the output
    if(offsetDistance == 0.0 && coord->size() < LinearRing::MINIMUM_VALID_SIZE) {
        return;
    }

    Location leftLoc = cwLeftLoc;
    Location rightLoc = cwRightLoc;
#if GEOS_DEBUG
    std::cerr << "BufferCurveSetBuilder::addPolygonRing: ";
    try {
        bool isCcw = Orientation::isCCW(coord);
        std::cerr << (isCcw ? "CCW" : "CW");
    } catch (const util::IllegalArgumentException& ex) {
        std::cerr << "failed to determine orientation: " << ex.what();
    }
    std::cerr << std::endl;
#endif
    bool isCCW = isRingCCW(coord);
    if (coord->size() >= LinearRing::MINIMUM_VALID_SIZE && isCCW)
    {
        leftLoc = cwRightLoc;
        rightLoc = cwLeftLoc;
#if GEOS_DEBUG
        std::cerr << " side " << side << " becomes " << Position::opposite(side) << std::endl;
#endif
        side = Position::opposite(side);
    }
    std::vector<CoordinateSequence*> lineList;
    curveBuilder.getRingCurve(coord, side, offsetDistance, lineList);
    // ASSERT: lineList contains exactly 1 curve (this is teh JTS semantics)
    if (lineList.size() > 0) {
        const CoordinateSequence* curve = lineList[0];
        /**
         * If the offset curve has inverted completely it will produce
         * an unwanted artifact in the result, so skip it.
         */
        if (isRingCurveInverted(coord, offsetDistance, curve)) {
            for( auto line: lineList ) {
                delete line;
            }
            return;
        }
    }
    addCurves(lineList, leftLoc, rightLoc);
}

/* private static*/
bool
BufferCurveSetBuilder::isRingCurveInverted(
    const CoordinateSequence* inputPts, double dist,
    const CoordinateSequence* curvePts)
{
    if (dist == 0.0) return false;
    /**
     * Only proper rings can invert.
     */
    if (inputPts->size() <= 3) return false;
    /**
     * Heuristic based on low chance that a ring with many vertices will invert.
     * This low limit ensures this test is fairly efficient.
     */
    if (inputPts->size() >= MAX_INVERTED_RING_SIZE) return false;

    /**
     * An inverted curve has no more points than the input ring.
     * This also eliminates concave inputs (which will produce fillet arcs)
     */
    if (curvePts->size() > INVERTED_CURVE_VERTEX_FACTOR * inputPts->size()) return false;

    /**
     * Check if the curve vertices are all closer to the input ring
     * than the buffer distance.
     * If so, the curve is NOT a valid buffer curve.
     */
    double distTol = NEARNESS_FACTOR * fabs(dist);
    double maxDist = maxDistance(curvePts, inputPts);
    bool isCurveTooClose = maxDist < distTol;
    return isCurveTooClose;
}

/**
 * Computes the maximum distance out of a set of points to a linestring.
 *
 * @param pts the points
 * @param line the linestring vertices
 * @return the maximum distance
 */
/* private static */
double
BufferCurveSetBuilder::maxDistance(const CoordinateSequence*  pts, const CoordinateSequence*  line) {
    double maxDistance = 0;
    for (std::size_t i = 0; i < pts->size(); i++) {
        const Coordinate& p = pts->getAt(i);
        double dist = Distance::pointToSegmentString(p, line);
        if (dist > maxDistance) {
            maxDistance = dist;
        }
    }
    return maxDistance;
}

/*private*/
bool
BufferCurveSetBuilder::isErodedCompletely(const LinearRing* ring,
        double bufferDistance)
{
    const CoordinateSequence* ringCoord = ring->getCoordinatesRO();

    // degenerate ring has no area
    if(ringCoord->getSize() < 4) {
        return bufferDistance < 0;
    }

    // important test to eliminate inverted triangle bug
    // also optimizes erosion test for triangles
    if(ringCoord->getSize() == 4) {
        return isTriangleErodedCompletely(ringCoord, bufferDistance);
    }

    const Envelope* env = ring->getEnvelopeInternal();
    double envMinDimension = std::min(env->getHeight(), env->getWidth());
    if(bufferDistance < 0.0 && 2 * std::abs(bufferDistance) > envMinDimension) {
        return true;
    }
    return false;
}

/*private*/
bool
BufferCurveSetBuilder::isTriangleErodedCompletely(
    const CoordinateSequence* triangleCoord, double bufferDistance)
{
    Triangle tri(triangleCoord->getAt(0), triangleCoord->getAt(1), triangleCoord->getAt(2));

    Coordinate inCentre;
    tri.inCentre(inCentre);
    double distToCentre = Distance::pointToSegment(inCentre, tri.p0, tri.p1);
    bool ret = distToCentre < std::fabs(bufferDistance);
    return ret;
}


/*private*/
bool
BufferCurveSetBuilder::isRingCCW(const CoordinateSequence* coords) const
{
    bool isCCW = algorithm::Orientation::isCCWArea(coords);
    //--- invert orientation if required
    if (isInvertOrientation) return ! isCCW;
    return isCCW;
}

} // namespace geos.operation.buffer
} // namespace geos.operation
} // namespace geos
