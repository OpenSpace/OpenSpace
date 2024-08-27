Geocentric Solar Magnetospheric (GSM) frame:
 +X is parallel to the geometric earth-sun position vector.
 +Z axis is normalized component of north centered geomagnetic dipole vector
      orthogonal to GSM +X axis.
 +Y completes the right-handed frame.

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
FRAME_EARTH_GSM_IGRF         = 1593399
FRAME_1593399_NAME           = 'EARTH_GSM_IGRF'
FRAME_1593399_CLASS          =  5
FRAME_1593399_CLASS_ID       = 1593399
FRAME_1593399_CENTER         = 399
FRAME_1593399_RELATIVE       = 'J2000'
FRAME_1593399_DEF_STYLE      = 'PARAMETERIZED'
FRAME_1593399_FAMILY         = 'TWO-VECTOR'

FRAME_1593399_PRI_AXIS       = 'X'
FRAME_1593399_PRI_VECTOR_DEF = 'OBSERVER_TARGET_POSITION'
FRAME_1593399_PRI_OBSERVER   = 'EARTH'
FRAME_1593399_PRI_TARGET     = 'SUN'
FRAME_1593399_PRI_ABCORR     = 'NONE'

FRAME_1593399_SEC_AXIS       = 'Z'
FRAME_1593399_SEC_VECTOR_DEF = 'OBSERVER_TARGET_POSITION'
FRAME_1593399_SEC_OBSERVER   = 'EARTH'
FRAME_1593399_SEC_TARGET     = 'EARTH_NORTH_POLE'
FRAME_1593399_SEC_ABCORR     = 'NONE'

NAIF_BODY_NAME              += ( 'EARTH_NORTH_POLE' )
NAIF_BODY_CODE              += ( 399901             )

TKFRAME_EARTH_FIXED_RELATIVE = 'ITRF93'
TKFRAME_EARTH_FIXED_SPEC     = 'MATRIX'
TKFRAME_EARTH_FIXED_MATRIX   = ( 1   0   0
                                 0   1   0
                                 0   0   1 )
