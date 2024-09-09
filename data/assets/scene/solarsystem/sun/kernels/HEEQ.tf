Heliocentric Earth Equatorial frame (HEEQ)
------------------------------------------

   Definition:
   -----------
   The Heliocentric Earth Equatorial frame is defined as follows (from [3]
   and [4]):

      -  X-Y plane is the solar equator of date, therefore, the +Z axis 
         is the primary vector and it is aligned to the Sun's north pole
         of date;

      -  +X axis is defined by the intersection between the Sun equatorial
         plane and the solar central meridian of date as seen from the Earth.
         The solar central meridian of date is defined as the meridian of the
         Sun that is turned toward the Earth. Therefore, +X axis is the
         component of the Sun-Earth vector that is orthogonal to the +Z axis;

      -  +Y axis completes the right-handed system;

      -  the origin of this frame is the Sun's center of mass.

   All vectors are geometric: no aberration corrections are used.


   Required Data:
   --------------
   This frame is defined as a two-vector frame using two different types
   of specifications for the primary and secondary vectors.

   The primary vector is defined as a constant vector in the IAU_SUN
   frame, which is a PCK-based frame, therefore a PCK file containing
   the orientation constants for the Sun has to be loaded before any
   evaluation of this frame.

   The secondary vector is defined as an 'observer-target position' vector,
   therefore, the ephemeris data required to compute the Sun-Earth vector
   in J2000 frame have to be loaded before using this frame.


   Remarks:
   --------
   This frame is defined based on the IAU_SUN frame, whose evaluation is
   based on the data included in the loaded PCK file: different
   orientation constants for the Sun's spin axis will lead to different
   frames. It is strongly recommended to indicate what data have been
   used in the evaluation of this frame when referring to it, i.e. HEEQ
   using IAU 2000 constants.

   Since the secondary vector of this frame is defined as an
   'observer-target position' vector, the usage of different planetary
   ephemerides conduces to different implementations of this frame,
   but only when these data lead to different solar central meridians,
   i.e. the projection of the Sun-Earth vector on the Sun equatorial
   plane obtained from the different ephemerides has a non-zero angular
   separation.

   Note that the effect of using different SPK files is smaller, in general,
   that using different Sun's spin axis constants. As an example, the
   average difference in the position of the +X axis of the frame, when
   using DE405 or DE403 ephemerides is about 14.3 micro-radians, with a
   maximum of 15.3 micro-radians.


  \begindata

      FRAME_HEEQ                    =  1501010
      FRAME_1501010_NAME            = 'HEEQ'
      FRAME_1501010_CLASS           =  5
      FRAME_1501010_CLASS_ID        =  1501010
      FRAME_1501010_CENTER          =  10
      FRAME_1501010_RELATIVE        = 'J2000'
      FRAME_1501010_DEF_STYLE       = 'PARAMETERIZED'
      FRAME_1501010_FAMILY          = 'TWO-VECTOR'
      FRAME_1501010_PRI_AXIS        = 'Z'
      FRAME_1501010_PRI_VECTOR_DEF  = 'CONSTANT'
      FRAME_1501010_PRI_FRAME       = 'IAU_SUN'
      FRAME_1501010_PRI_SPEC        = 'RECTANGULAR'
      FRAME_1501010_PRI_VECTOR      = ( 0, 0, 1 )
      FRAME_1501010_SEC_AXIS        = 'X'
      FRAME_1501010_SEC_VECTOR_DEF  = 'OBSERVER_TARGET_POSITION'
      FRAME_1501010_SEC_OBSERVER    = 'SUN'
      FRAME_1501010_SEC_TARGET      = 'EARTH'
      FRAME_1501010_SEC_ABCORR      = 'NONE'
