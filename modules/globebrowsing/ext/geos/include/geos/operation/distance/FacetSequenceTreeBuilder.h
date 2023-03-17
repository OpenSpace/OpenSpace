/**********************************************************************
 *
 * GEOS - Geometry Engine Open Source
 * http://geos.osgeo.org
 *
 * Copyright (C) 2016 Daniel Baston
 *
 * This is free software; you can redistribute and/or modify it under
 * the terms of the GNU Lesser General Public Licence as published
 * by the Free Software Foundation.
 * See the COPYING file for more information.
 *
 **********************************************************************
 *
 * Last port: operation/distance/FacetSequenceTreeBuilder.java (f6187ee2 JTS-1.14)
 *
 **********************************************************************/

#pragma once

#include <geos/index/ItemVisitor.h>
#include <geos/index/strtree/TemplateSTRtree.h>
#include <geos/geom/Geometry.h>
#include <geos/geom/CoordinateSequence.h>
#include <geos/operation/distance/FacetSequence.h>

namespace geos {
namespace operation {
namespace distance {
class GEOS_DLL FacetSequenceTreeBuilder {
private:
    // 6 seems to be a good facet sequence size
    static const int FACET_SEQUENCE_SIZE = 6;

    // Seems to be better to use a minimum node capacity
    static const int STR_TREE_NODE_CAPACITY = 4;

    static void addFacetSequences(const geom::Geometry* geom,
                                  const geom::CoordinateSequence* pts,
                                  std::vector<FacetSequence> & sections);
    static std::vector<FacetSequence> computeFacetSequences(const geom::Geometry* g);

    class FacetSequenceTree : public geos::index::strtree::TemplateSTRtree<const FacetSequence*> {
    public:
        // TODO support TemplateSTRtree<std::unique_ptr<FacetSequence>> and dispense with holding vector.
        FacetSequenceTree(std::vector<FacetSequence> &&seq) :
            TemplateSTRtree(STR_TREE_NODE_CAPACITY, seq.size()), sequences(seq) {
            for (auto& fs : sequences) {
                TemplateSTRtree::insert(fs.getEnvelope(), &fs);
            }
        }

    private:
        std::vector<FacetSequence> sequences;
    };

public:
    /** \brief
     * Return a tree of FacetSequences constructed from the supplied Geometry.
     *
     * The FacetSequences are owned by the tree and are automatically deleted by
     * the tree on destruction.
     */
    static std::unique_ptr<geos::index::strtree::TemplateSTRtree<const FacetSequence*>> build(const geom::Geometry* g);
};
}
}
}

