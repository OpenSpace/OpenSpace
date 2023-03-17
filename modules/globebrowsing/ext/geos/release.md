2022-07-01

- New things:
  - OffsetCurve (GH-530, Paul Ramsey/Martin Davis)
  - ConcaveHull (GH-549, Paul Ramsey/Martin Davis)
  - PolygonHull (GH-603, Paul Ramsey/Martin Davis)
  - LineMerger directed option (GH-597, Sergei Sh)
  - CAPI: GEOSHilbertCode (GH-556, Brendan Ward)
  - CAPI: GEOSGeom_createRectangle (GH-558, Brendan Ward)
  - CAPI: GEOSGeom_transformXY (GH-563, Dan Baston/Brendan Ward)
  - CAPI: GEOSRemoveRepeatedPoints (GH-599, Paul Ramsey)
  - CAPI: GEOSLineMergeDirected (GH-597, Sergei Sh)
  - CAPI: setFixStructure for WKB/WKT readers to automatically repair
    structural errors in the input (GH-639, Paul Ramsey)

- Fixes/Improvements:
  - Fix unaryUnion to avoid segfault with empty polygon (GH-501, Mike Taves)
  - Fix SnapRoundingNoder to use tolerance in noding; also fixes GeometryPrecisionReducer (#504, Sergei)
  - Allow direct setting of grid size (GH-513, Martin Davis)
  - Allow GEOS to be used as a CMake subproject (GH-518, Robert Coup)
  - Remove .inl inline files in favour of header declaration (GH-543, Paul Ramsey)
  - Add SnappingNoder seeding (Martin Davis)
  - Add OverlayNG area check heuristic (JTS-812, Paul Ramsey)
  - Fix RelateOp (and intersects predicate) for lines with intersections very near boundary (GH-570, Martin Davis)
  - Fix IsValidOp to handle repeated node points (JTS-845, Martin Davis)
  - Fix IsSimpleOp to handle closed LineStrings with repeated endpoints (JTS-851, Martin Davis)
  - Fix LengthIndexedLine (via LengthLocationMap fix) (JTS-859, Martin Davis)
  - Fix PolygonHoleJoiner (JTS-862, Martin Davis)
  - Improve `test_geos_unit` application error checking and reporting
  - Fix MinimumDiameter getMinimumRectangle for flat input (JTS-875, Martin Davis)
  - Fix BufferOp inverted ring check (JTS-878, Martin Davis)
  - Fix OverlayNG geomunion to avoid lines in result (Martin Davis)


