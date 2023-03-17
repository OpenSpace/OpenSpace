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
 * Last port: operation/valid/TopologyValidationError.java rev. 1.16 (JTS-1.10)
 *
 **********************************************************************/

#pragma once

#include <geos/export.h>
#include <string>

#include <geos/geom/Coordinate.h> // for composition

// Forward declarations
// none required

namespace geos {
namespace operation { // geos::operation
namespace valid { // geos::operation::valid

/** \brief
 * Contains information about the nature and location of a geom::Geometry
 * validation error
 *
 */
class GEOS_DLL TopologyValidationError {
public:

    enum errorEnum {
        eError,
        eRepeatedPoint,
        eHoleOutsideShell,
        eNestedHoles,
        eDisconnectedInterior,
        eSelfIntersection,
        eRingSelfIntersection,
        eNestedShells,
        eDuplicatedRings,
        eTooFewPoints,
        eInvalidCoordinate,
        eRingNotClosed,
        oNoInvalidIntersection = -1
    };

    TopologyValidationError(int newErrorType, const geom::Coordinate& newPt);
    TopologyValidationError(int newErrorType);
    const geom::Coordinate& getCoordinate() const;
    std::string getMessage() const;
    int getErrorType() const;
    std::string toString() const;

private:
    // Used const char* to reduce dynamic allocations
    static const char* errMsg[];
    int errorType;
    const geom::Coordinate pt;
};


} // namespace geos.operation.valid
} // namespace geos.operation
} // namespace geos

