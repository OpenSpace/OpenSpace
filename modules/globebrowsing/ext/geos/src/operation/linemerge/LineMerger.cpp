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
 **********************************************************************
 *
 * Last port: operation/linemerge/LineMerger.java r378 (JTS-1.12)
 *
 **********************************************************************/

#include <geos/operation/linemerge/LineMerger.h>
#include <geos/operation/linemerge/LineMergeDirectedEdge.h>
#include <geos/operation/linemerge/EdgeString.h>
#include <geos/planargraph/DirectedEdge.h>
#include <geos/planargraph/Edge.h>
#include <geos/planargraph/Node.h>
//#include <geos/planargraph/GraphComponent.h>
#include <geos/geom/GeometryComponentFilter.h>
#include <geos/geom/LineString.h>
#include <geos/util.h>

#include <cassert>
#include <functional>
#include <vector>


using namespace geos::planargraph;
using namespace geos::geom;

#ifndef GEOS_DEBUG
#define GEOS_DEBUG 0
#endif

#if GEOS_DEBUG
#include <iostream>
#endif

namespace geos {
namespace operation { // geos.operation
namespace linemerge { // geos.operation.linemerge

void
LineMerger::add(std::vector<const Geometry*>* geometries)
{
    for(const Geometry* g : *geometries) {
        add(g);
    }
}

LineMerger::LineMerger(bool directed):
    isDirected(directed),
    factory(nullptr)
{
}

LineMerger::~LineMerger()
{
    for(std::size_t i = 0, n = edgeStrings.size(); i < n; ++i) {
        delete edgeStrings[i];
    }
}


struct LMGeometryComponentFilter: public GeometryComponentFilter {
    LineMerger* lm;

    LMGeometryComponentFilter(LineMerger* newLm): lm(newLm) {}

    void
    filter(const Geometry* geom)
    {
        const LineString* ls = dynamic_cast<const LineString*>(geom);
        if(ls) {
            lm->add(ls);
        }
    }
};


/**
 * Adds a Geometry to be processed. May be called multiple times.
 * Any dimension of Geometry may be added; the constituent linework will be
 * extracted.
 */
void
LineMerger::add(const Geometry* geometry)
{
    LMGeometryComponentFilter lmgcf(this);
    geometry->applyComponentFilter(lmgcf);
}

void
LineMerger::add(const LineString* lineString)
{
    if(factory == nullptr) {
        factory = lineString->getFactory();
    }
    graph.addEdge(lineString);
}

void
LineMerger::merge()
{
    if(!mergedLineStrings.empty()) {
        return;
    }

    // reset marks (this allows incremental processing)
    GraphComponent::setMarkedMap(graph.nodeIterator(), graph.nodeEnd(),
                                 false);
    GraphComponent::setMarked(graph.edgeIterator(), graph.edgeEnd(),
                              false);

    for(std::size_t i = 0, n = edgeStrings.size(); i < n; ++i) {
        delete edgeStrings[i];
    }
    edgeStrings.clear();

    buildEdgeStringsForObviousStartNodes();
    buildEdgeStringsForIsolatedLoops();

    auto numEdgeStrings = edgeStrings.size();
    mergedLineStrings.reserve(numEdgeStrings);
    for(std::size_t i = 0; i < numEdgeStrings; ++i) {
        EdgeString* edgeString = edgeStrings[i];
        mergedLineStrings.emplace_back(edgeString->toLineString());
    }
}

void
LineMerger::buildEdgeStringsForObviousStartNodes()
{
    buildEdgeStringsForNonDegree2Nodes();
}

void
LineMerger::buildEdgeStringsForIsolatedLoops()
{
    buildEdgeStringsForUnprocessedNodes();
}

void
LineMerger::buildEdgeStringsForUnprocessedNodes()
{
#if GEOS_DEBUG
    std::cerr << __FUNCTION__ << std::endl;
#endif
    typedef std::vector<Node*> Nodes;

    Nodes nodes;
    graph.getNodes(nodes);
    for(Nodes::size_type i = 0, in = nodes.size(); i < in; ++i) {
        Node* node = nodes[i];
#if GEOS_DEBUG
        std::cerr << "Node " << i << ": " << *node << std::endl;
#endif
        if(!node->isMarked()) {
            assert(node->getDegree() == 2);
            buildEdgeStringsStartingAt(node);
            node->setMarked(true);
#if GEOS_DEBUG
            std::cerr << " setMarked(true) : " << *node << std::endl;
#endif
        }
    }
}

void
LineMerger::buildEdgeStringsForNonDegree2Nodes()
{
#if GEOS_DEBUG
    std::cerr << __FUNCTION__ << std::endl;
#endif
    typedef std::vector<Node*> Nodes;

    Nodes nodes;
    graph.getNodes(nodes);
    for(Nodes::size_type i = 0, in = nodes.size(); i < in; ++i) {
        Node* node = nodes[i];
#if GEOS_DEBUG
        std::cerr << "Node " << i << ": " << *node << std::endl;
#endif
        bool isStartNode = (node->getDegree() != 2);

        // For directed merge a node also has to be processed
        // if both edges are incoming or outgoing
        if (!isStartNode && isDirected) {
            const auto& edges = node->getOutEdges()->getEdges();
            assert(edges.size() == 2);
            isStartNode = (edges[0]->getEdgeDirection() == edges[1]->getEdgeDirection());
        }

        if (isStartNode) {
            buildEdgeStringsStartingAt(node);
            node->setMarked(true);
#if GEOS_DEBUG
            std::cerr << " setMarked(true) : " << *node << std::endl;
#endif
        }
    }
}

void
LineMerger::buildEdgeStringsStartingAt(Node* node)
{
    std::vector<planargraph::DirectedEdge*>& edges = node->getOutEdges()->getEdges();
    std::size_t size = edges.size();
    for(std::size_t i = 0; i < size; i++) {
        LineMergeDirectedEdge* directedEdge =
                            detail::down_cast<LineMergeDirectedEdge*>(edges[i]);
        if(isDirected && !directedEdge->getEdgeDirection()) {
            continue;
        }
        if(directedEdge->getEdge()->isMarked()) {
            continue;
        }
        edgeStrings.push_back(buildEdgeStringStartingWith(directedEdge));
    }
}

EdgeString*
LineMerger::buildEdgeStringStartingWith(LineMergeDirectedEdge* start)
{
    EdgeString* edgeString = new EdgeString(factory);
    LineMergeDirectedEdge* current = start;
    do {
        edgeString->add(current);
        current->getEdge()->setMarked(true);
        current = current->getNext(isDirected);
    }
    while(current != nullptr && current != start);
    return edgeString;
}

/**
 * Returns the LineStrings built by the merging process.
 */
std::vector<std::unique_ptr<LineString>>
LineMerger::getMergedLineStrings()
{
    merge();

    // Explicitly give ownership to the caller.
    auto ret = std::move(mergedLineStrings);
    mergedLineStrings.clear();
    return ret;
}

} // namespace geos.operation.linemerge
} // namespace geos.operation
} // namespace geos
