Geocentric Solar Ecliptic (GSE) frame:
    -  X-Y plane is defined by the Earth Mean Ecliptic plane of date:
        the +Z axis, primary vector, is the normal vector to this plane,
        always pointing toward the North side of the invariant plane;

    -  +X axis is the component of the Earth-Sun vector that is orthogonal
        to the +Z axis;

    -  +Y axis completes the right-handed system;

    -  the origin of this frame is the Sun's center of mass.

   All the vectors are geometric: no aberration corrections are used.


   Required Data:
   --------------
   This frame is defined as a two-vector frame using two different types
   of specifications for the primary and secondary vectors.

   The primary vector is defined as a constant vector in the ECLIPDATE
   frame and therefore, no additional data is required to compute this
   vector.

   The secondary vector is defined as an 'observer-target position' vector,
   therefore, the ephemeris data required to compute the Earth-Sun vector
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
   Earth-Sun vector on the Earth Ecliptic plane of date.

   As an example, note that the average difference in position of the +X
   axis of this frame, when using DE405 vs. DE403 ephemerides, is about
   14.3 micro-radians, with a maximum of 15.0 micro-radians.

\begindata
FRAME_GSE                     =  15001399
FRAME_15001399_NAME            = 'GSE'
FRAME_15001399_CLASS           =  5
FRAME_15001399_CLASS_ID        =  15001399
FRAME_15001399_CENTER          =  399
FRAME_15001399_RELATIVE        = 'J2000'
FRAME_15001399_DEF_STYLE       = 'PARAMETERIZED'
FRAME_15001399_FAMILY          = 'TWO-VECTOR'

FRAME_15001399_PRI_AXIS        = 'Z'
FRAME_15001399_PRI_VECTOR_DEF  = 'CONSTANT'
FRAME_15001399_PRI_FRAME       = 'ECLIPDATE'
FRAME_15001399_PRI_SPEC        = 'RECTANGULAR'
FRAME_15001399_PRI_VECTOR      = ( 0, 0, 1 )

FRAME_15001399_SEC_AXIS        = 'X'
FRAME_15001399_SEC_VECTOR_DEF  = 'OBSERVER_TARGET_POSITION'
FRAME_15001399_SEC_OBSERVER    = 'EARTH'
FRAME_15001399_SEC_TARGET      = 'SUN'
FRAME_15001399_SEC_ABCORR      = 'NONE'
