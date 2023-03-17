/**********************************************************************
 *
 * GEOS - Geometry Engine Open Source
 * http://geos.osgeo.org
 *
 * Copyright (C) 2006      Refractions Research Inc.
 * Copyright (C) 2001-2002 Vivid Solutions Inc.
 *
 * This is free software; you can redistribute and/or modify it under
 * the terms of the GNU Lesser General Licence as published
 * by the Free Software Foundation.
 * See the COPYING file for more information.
 *
 **********************************************************************
 *
 * Last port: noding/SegmentNodeList.java rev. 1.8 (JTS-1.10)
 *
 **********************************************************************/

#include <cassert>
#include <set>
#include <algorithm>

#include <geos/profiler.h>
#include <geos/util.h>
#include <geos/util/GEOSException.h>
#include <geos/noding/SegmentNodeList.h>
#include <geos/noding/NodedSegmentString.h>
#include <geos/noding/SegmentString.h> // for use
#include <geos/geom/Coordinate.h>
#include <geos/geom/CoordinateSequence.h>
#include <geos/geom/CoordinateArraySequence.h> // FIXME: should we really be using this ?
#include <geos/geom/FixedSizeCoordinateSequence.h>

#ifndef GEOS_DEBUG
#define GEOS_DEBUG 0
#endif

#if GEOS_DEBUG
#include <iostream>
#endif

//
using namespace geos::geom;

namespace geos {
namespace noding { // geos.noding

#if PROFILE
static Profiler* profiler = Profiler::instance();
#endif


void
SegmentNodeList::add(const Coordinate& intPt, std::size_t segmentIndex)
{
    // SegmentNode sn(edge, intPt, segmentIndex, edge.getSegmentOctant(segmentIndex));
    nodeMap.emplace_back(edge, intPt, segmentIndex, edge.getSegmentOctant(segmentIndex));
    ready = false;
}

void SegmentNodeList::prepare() const {
    if (!ready) {
        std::sort(nodeMap.begin(), nodeMap.end(), [](const SegmentNode& s1, const SegmentNode& s2) {
            return s1.compareTo(s2) < 0;
        });

        nodeMap.erase(std::unique(nodeMap.begin(), nodeMap.end(), [](const SegmentNode& s1, const SegmentNode& s2) {
            return s1.compareTo(s2) == 0;
        }), nodeMap.end());

        ready = true;
    }
}

void
SegmentNodeList::addEndpoints()
{
    std::size_t maxSegIndex = edge.size() - 1;
    add(&(edge.getCoordinate(0)), 0);
    add(&(edge.getCoordinate(maxSegIndex)), maxSegIndex);
}

/* private */
void
SegmentNodeList::addCollapsedNodes()
{
    std::vector<size_t> collapsedVertexIndexes;

    findCollapsesFromInsertedNodes(collapsedVertexIndexes);
    findCollapsesFromExistingVertices(collapsedVertexIndexes);

    // node the collapses
    for(std::size_t vertexIndex : collapsedVertexIndexes) {
        add(edge.getCoordinate(vertexIndex), vertexIndex);
    }
}


/* private */
void
SegmentNodeList::findCollapsesFromExistingVertices(
    std::vector<size_t>& collapsedVertexIndexes) const
{
    if(edge.size() < 2) {
        return;    // or we'll never exit the loop below
    }

    for(std::size_t i = 0, n = edge.size() - 2; i < n; ++i) {
        const Coordinate& p0 = edge.getCoordinate(i);
        const Coordinate& p2 = edge.getCoordinate(i + 2);
        if(p0.equals2D(p2)) {
            // add base of collapse as node
            collapsedVertexIndexes.push_back(i + 1);
        }
    }
}

/* private */
void
SegmentNodeList::findCollapsesFromInsertedNodes(
    std::vector<size_t>& collapsedVertexIndexes) const
{
    std::size_t collapsedVertexIndex;

    // there should always be at least two entries in the list,
    // since the endpoints are nodes
    auto it = begin();
    const SegmentNode* eiPrev = &(*it);
    ++it;
    for(auto itEnd = end(); it != itEnd; ++it) {
        const SegmentNode* ei = &(*it);
        bool isCollapsed = findCollapseIndex(*eiPrev, *ei,
                                             collapsedVertexIndex);
        if(isCollapsed) {
            collapsedVertexIndexes.push_back(collapsedVertexIndex);
        }

        eiPrev = ei;
    }
}

/* private */
bool
SegmentNodeList::findCollapseIndex(const SegmentNode& ei0, const SegmentNode& ei1,
                                   size_t& collapsedVertexIndex)
{
    assert(ei1.segmentIndex >= ei0.segmentIndex);
    // only looking for equal nodes
    if(! ei0.coord.equals2D(ei1.coord)) {
        return false;
    }

    auto numVerticesBetween = ei1.segmentIndex - ei0.segmentIndex;
    if(! ei1.isInterior()) {
        numVerticesBetween--;
    }

    // if there is a single vertex between the two equal nodes,
    // this is a collapse
    if(numVerticesBetween == 1) {
        collapsedVertexIndex = ei0.segmentIndex + 1;
        return true;
    }
    return false;
}


/* public */
void
SegmentNodeList::addSplitEdges(std::vector<SegmentString*>& edgeList)
{

    // testingOnly
#if GEOS_DEBUG
    std::cerr << __FUNCTION__ << " entered" << std::endl;
    std::vector<SegmentString*> testingSplitEdges;
#endif

    // ensure that the list has entries for the first and last
    // point of the edge
    addEndpoints();
    addCollapsedNodes();

    // there should always be at least two entries in the list
    // since the endpoints are nodes
    auto it = begin();
    const SegmentNode* eiPrev = &(*it);
    assert(eiPrev);
    ++it;
    for(auto itEnd = end(); it != itEnd; ++it) {
        const SegmentNode* ei = &(*it);
        assert(ei);

        if(! ei->compareTo(*eiPrev)) {
            continue;
        }

        std::unique_ptr<SegmentString> newEdge = createSplitEdge(eiPrev, ei);
        edgeList.push_back(newEdge.release());
#if GEOS_DEBUG
        testingSplitEdges.push_back(edgeList.back());
#endif
        eiPrev = ei;
    }
#if GEOS_DEBUG
    std::cerr << __FUNCTION__ << " finished, now checking correctness" << std::endl;
    checkSplitEdgesCorrectness(testingSplitEdges);
#endif
}

/*private*/
void
SegmentNodeList::checkSplitEdgesCorrectness(const std::vector<SegmentString*>& splitEdges) const
{
    if (splitEdges.empty())
        return;

    const CoordinateSequence* edgePts = edge.getCoordinates();
    assert(edgePts);

    // check that first and last points of split edges
    // are same as endpoints of edge
    SegmentString* split0 = splitEdges[0];
    assert(split0);

    const Coordinate& pt0 = split0->getCoordinate(0);
    if(!(pt0 == edgePts->getAt(0))) {
        throw util::GEOSException("bad split edge start point at " + pt0.toString());
    }

    SegmentString* splitn = splitEdges[splitEdges.size() - 1];
    assert(splitn);

    const CoordinateSequence* splitnPts = splitn->getCoordinates();
    assert(splitnPts);

    const Coordinate& ptn = splitnPts->getAt(splitnPts->getSize() - 1);
    if(!(ptn == edgePts->getAt(edgePts->getSize() - 1))) {
        throw util::GEOSException("bad split edge end point at " + ptn.toString());
    }
}

/*private*/
std::unique_ptr<SegmentString>
SegmentNodeList::createSplitEdge(const SegmentNode* ei0, const SegmentNode* ei1) const
{
    auto pts = createSplitEdgePts(ei0, ei1);
    return detail::make_unique<NodedSegmentString>(pts.release(), edge.getData());
}


/*private*/
std::unique_ptr<CoordinateSequence>
SegmentNodeList::createSplitEdgePts(const SegmentNode* ei0, const SegmentNode* ei1) const
{
    bool twoPoints = (ei1->segmentIndex == ei0->segmentIndex);

    // if only two points in split edge they must be the node points
    if (twoPoints) {
        auto pts = detail::make_unique<FixedSizeCoordinateSequence<2>>();
        pts->setAt(ei0->coord, 0);
        pts->setAt(ei1->coord, 1);
        return RETURN_UNIQUE_PTR(pts);
    }

    const Coordinate& lastSegStartPt = edge.getCoordinate(ei1->segmentIndex);
    /**
    * If the last intersection point is not equal to the its segment start pt,
    * add it to the points list as well.
    * This check is needed because the distance metric is not totally reliable!
    * Also ensure that the created edge always has at least 2 points.
    * The check for point equality is 2D only - Z values are ignored
    */
    bool useIntPt1 = ei1->isInterior() || ! ei1->coord.equals2D(lastSegStartPt);

    std::vector<Coordinate> pts;
    pts.reserve(1 + (ei1->segmentIndex - ei0->segmentIndex) + useIntPt1);

    pts.emplace_back(ei0->coord);
    for (std::size_t i = ei0->segmentIndex + 1; i <= ei1->segmentIndex; i++) {
        pts.emplace_back(edge.getCoordinate(i));
    }
    if (useIntPt1) {
        pts.emplace_back(ei1->coord);
    }

    return std::unique_ptr<CoordinateSequence>(new CoordinateArraySequence(std::move(pts)));
}


/*public*/
std::vector<Coordinate>
SegmentNodeList::getSplitCoordinates()
{
    // ensure that the list has entries for the first and last point of the edge
    addEndpoints();
    std::vector<Coordinate> coordList;
    // there should always be at least two entries in the list, since the endpoints are nodes
    auto it = begin();
    const SegmentNode* eiPrev = &(*it);
    for(auto itEnd = end(); it != itEnd; ++it) {
        const SegmentNode* ei = &(*it);
        addEdgeCoordinates(eiPrev, ei, coordList);
        eiPrev = ei;
    }
    // Remove duplicate Coordinates from coordList
    coordList.erase(std::unique(coordList.begin(), coordList.end()), coordList.end());
    return coordList;
}


/*private*/
void
SegmentNodeList::addEdgeCoordinates(const SegmentNode* ei0, const SegmentNode* ei1, std::vector<Coordinate>& coordList) const {
    auto pts = createSplitEdgePts(ei0, ei1);
    // Append pts to coordList
    pts->toVector(coordList);
}



std::ostream&
operator<< (std::ostream& os, const SegmentNodeList& nlist)
{
    os << "Intersections: (" << nlist.nodeMap.size() << "):" << std::endl;

    for(const SegmentNode& ei: nlist.nodeMap) {
        os << " " << ei;
    }
    return os;
}

} // namespace geos.noding
} // namespace geos

