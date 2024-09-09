Heliocentric Earth Ecliptic frame (HEE)
---------------------------------------

   Definition:
   -----------
   The Heliocentric Earth Ecliptic frame is defined as follows (from [3]):

      -  X-Y plane is defined by the Earth Mean Ecliptic plane of date,
         therefore, the +Z axis is the primary vector,and it defined as
         the normal vector to the Ecliptic plane that points toward the
         north pole of date;

      -  +X axis is the component of the Sun-Earth vector that is
         orthogonal to the +Z axis;

      -  +Y axis completes the right-handed system;

      -  the origin of this frame is the Sun's center of mass.

   All vectors are geometric: no aberration corrections are used.


   Required Data:
   --------------
   This frame is defined as a two-vector frame using two different types
   of specifications for the primary and secondary vectors.

   The primary vector is defined as a constant vector in the ECLIPDATE
   frame and therefore no additional data is required to compute this
   vector.

   The secondary vector is defined as an 'observer-target position' vector,
   therefore, the ephemeris data required to compute the Sun-Earth vector
   in J2000 frame have to be loaded prior to using this frame.


   Remarks:
   --------
   SPICE imposes a constraint in the definition of dynamic frames:

   When the definition of a parameterized dynamic frame F1 refers to a
   second frame F2 the referenced frame F2 may be dynamic, but F2 must not
   make reference to any dynamic frame. For further information on this
   topic, please refer to [1].

   Therefore, no other dynamic frame should make reference to this frame.

   Since the secondary vector of this frame is defined as an
   'observer-target position' vector, the usage of different planetary
   ephemerides conduces to different implementations of this frame,
   but only when these data lead to different projections of the
   Sun-Earth vector on the Earth Ecliptic plane of date.

   As an example, note that the average difference in position of the +X
   axis of this frame, when using DE405 vs. DE403 ephemerides, is about
   14.3 micro-radians, with a maximum of 15.0 micro-radians.


  \begindata

      FRAME_HEE                     =  1500010
      FRAME_1500010_NAME            = 'HEE'
      FRAME_1500010_CLASS           =  5
      FRAME_1500010_CLASS_ID        =  1500010
      FRAME_1500010_CENTER          =  10
      FRAME_1500010_RELATIVE        = 'J2000'
      FRAME_1500010_DEF_STYLE       = 'PARAMETERIZED'
      FRAME_1500010_FAMILY          = 'TWO-VECTOR'
      FRAME_1500010_PRI_AXIS        = 'Z'
      FRAME_1500010_PRI_VECTOR_DEF  = 'CONSTANT'
      FRAME_1500010_PRI_FRAME       = 'ECLIPDATE'
      FRAME_1500010_PRI_SPEC        = 'RECTANGULAR'
      FRAME_1500010_PRI_VECTOR      = ( 0, 0, 1 )
      FRAME_1500010_SEC_AXIS        = 'X'
      FRAME_1500010_SEC_VECTOR_DEF  = 'OBSERVER_TARGET_POSITION'
      FRAME_1500010_SEC_OBSERVER    = 'SUN'
      FRAME_1500010_SEC_TARGET      = 'EARTH'
      FRAME_1500010_SEC_ABCORR      = 'NONE'
