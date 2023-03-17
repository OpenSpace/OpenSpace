/**********************************************************************
 *
 * GEOS - Geometry Engine Open Source
 * http://geos.osgeo.org
 *
 * Copyright (C) 2020 Paul Ramsey <pramsey@cleverelephant.ca>
 *
 * This is free software; you can redistribute and/or modify it under
 * the terms of the GNU Lesser General Public Licence as published
 * by the Free Software Foundation.
 * See the COPYING file for more information.
 *
 **********************************************************************/

#pragma once

#include <geos/geom/Location.h>
#include <geos/geom/Position.h>
#include <geos/export.h>

using geos::geom::Location;
using geos::geom::Position;

namespace geos {      // geos.
namespace operation { // geos.operation
namespace overlayng { // geos.operation.overlayng

/**
* A label for a pair of {@link OverlayEdge}s which records
* the topological information for the edge
* in the {@link OverlayGraph} containing it.
* The label is shared between both OverlayEdges
* of a symmetric pair.
* Accessors for orientation-sensitive information
* require the orientation of the containing OverlayEdge.
*
* A label contains the topological {@link geom::Location}s for
* the two overlay input geometries.
* A labelled input geometry may be either a Line or an Area.
* In both cases, the label locations are populated
* with the locations for the edge {@link geom::Position}s
* once they are computed by topological evaluation.
* The label also records the dimension of each geometry,
* and in the case of area boundary edges, the role
* of the originating ring (which allows
* determination of the edge role in collapse cases).
*
* For each input geometry, the label indicates that an edge
* is in one of the following states (identified by the "dim" field).
* Each state has some additional information about the edge.
*
* * A Boundary edge of an input Area (polygon)
*
*   * dim = DIM_BOUNDARY
*   * locLeft, locRight : the locations of the edge sides for the input Area
*   * isHole : whether the edge was in a shell or a hole
*
* * A Collapsed edge of an input Area
*   (which had two or more parent edges)
*
*   * dim = DIM_COLLAPSE
*   * locLine : the location of the
*     edge relative to the input Area
*   * isHole : whether some
*     contributing edge was in a shell (false),
*     or otherwise that all were in holes (true)
*
* * An edge from an input Line
*
*   * dim = DIM_LINE
*   * locLine : initialized to LOC_UNKNOWN, to simplify logic.
*
* * An edge which is Not Part of an input geometry
*   (and thus must be part of the other geometry).
*
*   * dim = NOT_PART
*
* Note that:
*
* * an edge cannot be both a Collapse edge and a Line edge in the same input geometry,
*   because each input geometry must be homogeneous.
* * an edge may be an Boundary edge in one input geometry
*   and a Line or Collapse edge in the other input.
*
* @author Martin Davis
*/
class GEOS_DLL OverlayLabel {

private:

    // Members
    int aDim = DIM_NOT_PART;
    bool aIsHole = false;
    Location aLocLeft = LOC_UNKNOWN;
    Location aLocRight = LOC_UNKNOWN;
    Location aLocLine = LOC_UNKNOWN;
    int bDim = DIM_NOT_PART;
    bool bIsHole = false;
    Location bLocLeft = LOC_UNKNOWN;
    Location bLocRight = LOC_UNKNOWN;
    Location bLocLine = LOC_UNKNOWN;

    std::string dimensionSymbol(int dim) const;
    void locationString(uint8_t index, bool isForward, std::ostream& os) const;


public:

    static constexpr Location LOC_UNKNOWN = Location::NONE;

    enum {
        DIM_UNKNOWN = -1,
        DIM_NOT_PART = -1,
        DIM_LINE = 1,
        DIM_BOUNDARY = 2,
        DIM_COLLAPSE = 3
    };

    OverlayLabel()
        : aDim(DIM_NOT_PART)
        , aIsHole(false)
        , aLocLeft(LOC_UNKNOWN)
        , aLocRight(LOC_UNKNOWN)
        , aLocLine(LOC_UNKNOWN)
        , bDim(DIM_NOT_PART)
        , bIsHole(false)
        , bLocLeft(LOC_UNKNOWN)
        , bLocRight(LOC_UNKNOWN)
        , bLocLine(LOC_UNKNOWN) {};

    explicit OverlayLabel(uint8_t p_index)
        : OverlayLabel()
    {
        initLine(p_index);
    };

    OverlayLabel(uint8_t p_index, Location p_locLeft, Location p_locRight, bool p_isHole)
        : OverlayLabel()
    {
        initBoundary(p_index, p_locLeft, p_locRight, p_isHole);
    };

    int dimension(uint8_t index) const { return index == 0 ? aDim : bDim; };
    void initBoundary(uint8_t index, Location locLeft, Location locRight, bool p_isHole);
    void initCollapse(uint8_t index, bool p_isHole);
    void initLine(uint8_t index);
    void initNotPart(uint8_t index);

    /**
    * Sets the line location.
    *
    * This is used to set the locations for linear edges
    * encountered during area label propagation.
    *
    * @param index source to update
    * @param loc location to set
    */
    void setLocationLine(uint8_t index, Location loc);
    void setLocationAll(uint8_t index, Location loc);
    void setLocationCollapse(uint8_t index);

    /*
    * Tests whether at least one of the sources is a Line.
    *
    * @return true if at least one source is a line
    */
    bool isLine() const
    {
        return aDim == DIM_LINE || bDim == DIM_LINE;
    };

    bool isLine(uint8_t index) const
    {
        return index == 0 ? aDim == DIM_LINE : bDim == DIM_LINE;
    };

    bool isLinear(uint8_t index) const
    {
        if (index == 0) {
            return aDim == DIM_LINE || aDim == DIM_COLLAPSE;
        }
        return bDim == DIM_LINE || bDim == DIM_COLLAPSE;
    };

    bool isKnown(uint8_t index) const
    {
        if (index == 0) {
            return aDim != DIM_UNKNOWN;
        }
        return bDim != DIM_UNKNOWN;
    };

    bool isNotPart(uint8_t index) const
    {
        if (index == 0) {
            return aDim == DIM_NOT_PART;
        }
        return bDim == DIM_NOT_PART;
    };

    bool isBoundaryEither() const
    {
        return aDim == DIM_BOUNDARY || bDim == DIM_BOUNDARY;
    };

    bool isBoundaryBoth() const
    {
        return aDim == DIM_BOUNDARY && bDim == DIM_BOUNDARY;
    };

    /**
    * Tests if the label is for a collapsed
    * edge of an area
    * which is coincident with the boundary of the other area.
    *
    * @return true if the label is for a collapse coincident with a boundary
    */
    bool isBoundaryCollapse() const
    {
        if (isLine()) return false;
        return ! isBoundaryBoth();
    };

    /**
    * Tests if a label is for an edge where two
    * area touch along their boundary.
    */
    bool isBoundaryTouch() const
    {
        return isBoundaryBoth() &&
               getLocation(0, Position::RIGHT, true) != getLocation(1, Position::RIGHT, true);
    };

    bool isBoundary(uint8_t index) const
    {
        if (index == 0) {
            return aDim == DIM_BOUNDARY;
        }
        return bDim == DIM_BOUNDARY;
    };

    bool isLineLocationUnknown(int index) const
    {
        if (index == 0) {
            return aLocLine == LOC_UNKNOWN;
        }
        else {
            return bLocLine == LOC_UNKNOWN;
        }
    };

    /**
    * Tests whether a label is for an edge which is a boundary of one geometry
    * and not part of the other.
    */
    bool isBoundarySingleton() const
    {
        if (aDim == DIM_BOUNDARY && bDim == DIM_NOT_PART) {
            return true;
        }

        if (bDim == DIM_BOUNDARY && aDim == DIM_NOT_PART) {
            return true;
        }

        return false;
    };

    /**
    * Tests if a line edge is inside
    * @param index
    * @return
    */
    bool isLineInArea(int8_t index) const
    {
        if (index == 0) {
            return aLocLine == Location::INTERIOR;
        }
        return bLocLine == Location::INTERIOR;
    };

    bool isHole(uint8_t index) const
    {
        if (index == 0) {
            return aIsHole;
        }
        else {
            return bIsHole;
        }
    };

    bool isCollapse(uint8_t index) const
    {
        return dimension(index) == DIM_COLLAPSE;
    };

    Location getLineLocation(uint8_t index) const
    {
        if (index == 0) {
            return aLocLine;
        }
        else {
            return bLocLine;
        }
    };

    /**
    * Tests if a label is a Collapse has location INTERIOR,
    * to at least one source geometry.
    */
    bool isInteriorCollapse() const
    {
        if (aDim == DIM_COLLAPSE && aLocLine == Location::INTERIOR)
            return true;
        if (bDim == DIM_COLLAPSE && bLocLine == Location::INTERIOR)
            return true;

        return false;
    };

    /**
    * Tests if a label is a Collapse
    * and NotPart with location INTERIOR for the other geometry.
    */
    bool isCollapseAndNotPartInterior() const;

    /**
    * Tests if a line is in the interior of a source geometry.
    *
    * @param index source geometry
    * @return true if the label is a line and is interior
    */
    bool isLineInterior(uint8_t index) const
    {
        if (index == 0) {
            return aLocLine == Location::INTERIOR;
        }
        return bLocLine == Location::INTERIOR;
    };

    /**
    * Gets the location for this label for either
    * a Boundary or a Line edge.
    * This supports a simple determination of
    * whether the edge should be included as a result edge.
    *
    * @param index the source index
    * @param position the position for a boundary label
    * @param isForward the direction for a boundary label
    * @return the location for the specified position
    */
    Location getLocationBoundaryOrLine(
        uint8_t index,
        int position,
        bool isForward) const
    {
        if (isBoundary(index)) {
            return getLocation(index, position, isForward);
        }
        return getLineLocation(index);
    };

    /**
    * Gets the linear location for the given source.
    *
    * @param index the source index
    * @return the linear location for the source
    */
    Location getLocation(uint8_t index) const {
        if (index == 0) {
            return aLocLine;
        }
        return bLocLine;
    };

    Location getLocation(uint8_t index, int position, bool isForward) const;

    bool hasSides(uint8_t index) const {
        if (index == 0) {
            return aLocLeft != LOC_UNKNOWN
                   || aLocRight != LOC_UNKNOWN;
        }
        return bLocLeft != LOC_UNKNOWN
               || bLocRight != LOC_UNKNOWN;
    };

    OverlayLabel copy() const
    {
        OverlayLabel lbl = *this;
        return lbl;
    };

    friend std::ostream& operator<<(std::ostream& os, const OverlayLabel& ol);
    void toString(bool isForward, std::ostream& os) const;


};


} // namespace geos.operation.overlayng
} // namespace geos.operation
} // namespace geos
