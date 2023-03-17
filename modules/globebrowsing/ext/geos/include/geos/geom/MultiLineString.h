/**********************************************************************
 *
 * GEOS - Geometry Engine Open Source
 * http://geos.osgeo.org
 *
 * Copyright (C) 2011 Sandro Santilli <strk@kbt.io>
 * Copyright (C) 2001-2002 Vivid Solutions Inc.
 * Copyright (C) 2005 2006 Refractions Research Inc.
 *
 * This is free software; you can redistribute and/or modify it under
 * the terms of the GNU Lesser General Public Licence as published
 * by the Free Software Foundation.
 * See the COPYING file for more information.
 *
 **********************************************************************
 *
 * Last port: geom/MultiLineString.java r320 (JTS-1.12)
 *
 **********************************************************************/

#pragma once

#include <geos/export.h>
#include <geos/geom/GeometryCollection.h> // for inheritance
#include <geos/geom/Dimension.h>
#include <geos/geom/LineString.h>
#include <geos/geom/MultiPoint.h>

#include <string>
#include <vector>


// Forward declarations
namespace geos {
namespace geom { // geos::geom
class Coordinate;
class CoordinateArraySequence;
}
}

namespace geos {
namespace geom { // geos::geom

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable:4250) // T1 inherits T2 via dominance
#endif

/// Models a collection of [LineStrings](@ref geom::LineString).
class GEOS_DLL MultiLineString: public GeometryCollection {

public:

    friend class GeometryFactory;

    ~MultiLineString() override = default;

    /// Returns line dimension (1)
    Dimension::DimensionType getDimension() const override;

    bool isDimensionStrict(Dimension::DimensionType d) const override {
        return d == Dimension::L;
    }

    /**
     * \brief
     * Returns Dimension::False if all [LineStrings](@ref geom::LineString) in the collection
     * are closed, 0 otherwise.
     */
    int getBoundaryDimension() const override;

    /// Returns a (possibly empty) [MultiPoint](@ref geom::MultiPoint)
    std::unique_ptr<Geometry> getBoundary() const override;

    const LineString* getGeometryN(std::size_t n) const override;

    std::string getGeometryType() const override;

    GeometryTypeId getGeometryTypeId() const override;

    bool isClosed() const;

    std::unique_ptr<MultiLineString> clone() const
    {
        return std::unique_ptr<MultiLineString>(cloneImpl());
    };

    /**
     * Creates a MultiLineString in the reverse
     * order to this object.
     * Both the order of the component LineStrings
     * and the order of their coordinate sequences
     * are reversed.
     *
     * @return a MultiLineString in the reverse order
     */
    std::unique_ptr<MultiLineString> reverse() const { return std::unique_ptr<MultiLineString>(reverseImpl()); }

protected:

    /**
     * \brief Constructs a MultiLineString.
     *
     * @param  newLines The [LineStrings](@ref geom::LineString) for this
     *                  MultiLineString, or `null`
     *                  or an empty array to create the empty geometry.
     *                  Elements may be empty LineString,
     *                  but not `null`s.
     *
     * @param newFactory The GeometryFactory used to create this geometry.
     *                   Caller must keep the factory alive for the life-time
     *                   of the constructed MultiLineString.
     *
     * @note Constructed object will take ownership of
     *       the vector and its elements.
     *
     */
    MultiLineString(std::vector<Geometry*>* newLines,
                    const GeometryFactory* newFactory);

    MultiLineString(std::vector<std::unique_ptr<LineString>> && newLines,
            const GeometryFactory& newFactory);

    MultiLineString(std::vector<std::unique_ptr<Geometry>> && newLines,
                    const GeometryFactory& newFactory);

    MultiLineString(const MultiLineString& mp)
        : GeometryCollection(mp)
        {};

    MultiLineString* cloneImpl() const override { return new MultiLineString(*this); }

    MultiLineString* reverseImpl() const override;

    int
    getSortIndex() const override
    {
        return SORTINDEX_MULTILINESTRING;
    };

};


} // namespace geos::geom
} // namespace geos


#ifdef _MSC_VER
#pragma warning(pop)
#endif





