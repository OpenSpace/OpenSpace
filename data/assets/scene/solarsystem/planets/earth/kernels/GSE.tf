Geocentric Solar Ecliptic (GSE) frame:
 +X is parallel to the geometric earth-sun position vector.
 +Y axis is the normalized component of the geometric earth-sun velocity vector orthogonal to the GSE +X axis.
 +Z axis is parallel to the cross product of the GSE +X axis and the GSE +Y axis.

A comment here is that target and observer is swapped to that y is in opposing Earth motion

\begindata
 FRAME_GSE = 13371333
 FRAME_13371333_NAME = 'GSE'
 FRAME_13371333_CLASS = 5
 FRAME_13371333_CLASS_ID = 13371333
 FRAME_13371333_CENTER = 399
 FRAME_13371333_RELATIVE = 'J2000'
 FRAME_13371333_DEF_STYLE = 'PARAMETERIZED'
 FRAME_13371333_FAMILY = 'TWO-VECTOR'

 FRAME_13371333_PRI_AXIS = 'X'
 FRAME_13371333_PRI_VECTOR_DEF = 'OBSERVER_TARGET_POSITION'
 FRAME_13371333_PRI_OBSERVER = 'EARTH'
 FRAME_13371333_PRI_TARGET = 'SUN'
 FRAME_13371333_PRI_ABCORR = 'NONE'

 FRAME_13371333_SEC_AXIS = 'Y'
 FRAME_13371333_SEC_VECTOR_DEF = 'OBSERVER_TARGET_VELOCITY'
 FRAME_13371333_SEC_OBSERVER = 'EARTH'
 FRAME_13371333_SEC_TARGET = 'SUN'
 FRAME_13371333_SEC_ABCORR = 'NONE'
 FRAME_13371333_SEC_FRAME = 'J2000'
