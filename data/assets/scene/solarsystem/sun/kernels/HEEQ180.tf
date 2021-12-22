\begintext

Heliocentric Earth Equatorial (HEEQ+180) Frame
Used by the BATSRUS model
This Kernel's ID is not a valid ID!
     Definition of the Heliocentric Earth Equatorial frame:

              All vectors are geometric: no aberration corrections are
              used.

              The solar rotation axis is the primary vector: the Z axis points
	      in the solar north direction.

              The position of the sun relative to the earth is the secondary
              vector: the X axis is the component of this position vector
              orthogonal to the Z axis.

              The Y axis is Z cross X, completing the right-handed
              reference frame.

\begindata

        FRAME_HEEQ180                =  6666666
        FRAME_6666666_NAME           = 'HEEQ180'
        FRAME_6666666_CLASS          =  5
        FRAME_6666666_CLASS_ID       =  6666666
        FRAME_6666666_CENTER         =  10
        FRAME_6666666_RELATIVE       = 'J2000'
        FRAME_6666666_DEF_STYLE      = 'PARAMETERIZED'
        FRAME_6666666_FAMILY         = 'TWO-VECTOR'
        FRAME_6666666_PRI_AXIS       = 'Z'
        FRAME_6666666_PRI_VECTOR_DEF = 'CONSTANT'
        FRAME_6666666_PRI_FRAME      = 'IAU_SUN'
        FRAME_6666666_PRI_SPEC       = 'RECTANGULAR'
        FRAME_6666666_PRI_VECTOR      = ( 0, 0, 1 )
        FRAME_6666666_SEC_AXIS       = 'X'
        FRAME_6666666_SEC_VECTOR_DEF = 'OBSERVER_TARGET_POSITION'
        FRAME_6666666_SEC_OBSERVER   = 'EARTH'
        FRAME_6666666_SEC_TARGET     = 'SUN'
        FRAME_6666666_SEC_ABCORR     = 'NONE'
        FRAME_6666666_SEC_FRAME      = 'IAU_SUN'
