Solar Magnetospheric (SM) frame:
Used by the LFM or GAMERA model of the magnetosphere
 +Z is parallel to the north centered geomagnetic dipole.
 +X axis is normalized component of the geometric earth-sun position vector
    orthogonal to the Z axis.
 +Y completes the right-handed frame.
  -  the origin of this frame is the center of mass of the Earth.

\begindata
FRAME_EARTH_SM_IGRF2000      =  1592399
FRAME_1592399_NAME           = 'EARTH_SM_IGRF2000'
FRAME_1592399_CLASS          =  5
FRAME_1592399_CLASS_ID       =  1592399
FRAME_1592399_CENTER         =  399
FRAME_1592399_RELATIVE       = 'J2000'
FRAME_1592399_DEF_STYLE      = 'PARAMETERIZED'
FRAME_1592399_FAMILY         = 'TWO-VECTOR'

FRAME_1592399_PRI_AXIS       = 'Z'
FRAME_1592399_PRI_VECTOR_DEF = 'CONSTANT'
FRAME_1592399_PRI_FRAME      = 'IAU_EARTH'
FRAME_1592399_PRI_SPEC       = 'LATITUDINAL'
FRAME_1592399_PRI_UNITS      = 'DEGREES'
FRAME_1592399_PRI_LONGITUDE  = 288.43
FRAME_1592399_PRI_LATITUDE   = 79.54

FRAME_1592399_SEC_AXIS       = 'X'
FRAME_1592399_SEC_VECTOR_DEF = 'OBSERVER_TARGET_POSITION'
FRAME_1592399_SEC_OBSERVER   = 'EARTH'
FRAME_1592399_SEC_TARGET     = 'SUN'
FRAME_1592399_SEC_ABCORR     = 'NONE'
