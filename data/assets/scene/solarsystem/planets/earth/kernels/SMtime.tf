Solar Magnetospheric (SM) frame:
Used by the LFM or GAMERA model of the magnetosphere
 +Z is parallel to the north centered geomagnetic dipole.
 +X axis is normalized component of the geometric earth-sun position vector
      orthogonal to the Z axis.
 +Y completes the right-handed frame.
  -  the origin of this frame is the center of mass of the Earth.

For the _IGRF frames, the Earth's magnetic dipole axis direction is time-
variable and is defined by the EARTH_NORTH_POLE/399901 object location 
as seen from the Earth center provided in the SPK file 
earthnpole_19500101_20251231_v01.bsp [1]. To support the use of this SPK
this FK also defined the EARTH_NORTH_POLE/399901 name/ID mapping and maps
the EARTH_FIXED frame used in the SPK to ITRF93.

Source :

1.  http://spiftp.esac.esa.int/data/SPICE/esa_generic/kernels/spk/science/
      earthnpole_19500101_20251231_v01.bsp

\begindata
FRAME_EARTH_SM_IGRF          = 1594399
FRAME_1594399_NAME           = 'EARTH_SM_IGRF'
FRAME_1594399_CLASS          =  5
FRAME_1594399_CLASS_ID       = 1594399
FRAME_1594399_CENTER         = 399
FRAME_1594399_RELATIVE       = 'J2000'
FRAME_1594399_DEF_STYLE      = 'PARAMETERIZED'
FRAME_1594399_FAMILY         = 'TWO-VECTOR'

FRAME_1594399_PRI_AXIS       = 'Z'
FRAME_1594399_PRI_VECTOR_DEF = 'OBSERVER_TARGET_POSITION'
FRAME_1594399_PRI_OBSERVER   = 'EARTH'
FRAME_1594399_PRI_TARGET     = 'EARTH_NORTH_POLE'
FRAME_1594399_PRI_ABCORR     = 'NONE'

FRAME_1594399_SEC_AXIS       = 'X'
FRAME_1594399_SEC_VECTOR_DEF = 'OBSERVER_TARGET_POSITION'
FRAME_1594399_SEC_OBSERVER   = 'EARTH'
FRAME_1594399_SEC_TARGET     = 'SUN'
FRAME_1594399_SEC_ABCORR     = 'NONE'

NAIF_BODY_NAME              += ( 'EARTH_NORTH_POLE' )
NAIF_BODY_CODE              += ( 399901             )

TKFRAME_EARTH_FIXED_RELATIVE = 'ITRF93'
TKFRAME_EARTH_FIXED_SPEC     = 'MATRIX'
TKFRAME_EARTH_FIXED_MATRIX   = ( 1   0   0
                                 0   1   0
                                 0   0   1 )
