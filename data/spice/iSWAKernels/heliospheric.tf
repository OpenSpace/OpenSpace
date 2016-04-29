Dynamic Heliospheric Coordinate Frames developed for the NASA STEREO mission

The coordinate frames in this file all have ID values based on the pattern
18ccple, where

	18 = Prefix to put in the allowed 1400000 to 2000000 range
	cc = 03 for geocentric, 10 for heliocentric
	p  = Pole basis: 1=geographic, 2=geomagnetic, 3=ecliptic, 4=solar
	l  = Longitude basis: 1=Earth-Sun, 2=ecliptic
	e  = Ecliptic basis: 0=J2000, 1=mean, 2=true

     Author:  William Thompson
	      NASA Goddard Space Flight Center
	      Code 612.1
	      Greenbelt, MD 20771

	      William.T.Thompson.1@gsfc.nasa.gov

History

    Version 1, 18-Feb-2005, WTT, initial release
	GSE and ECLIPDATE definitions from examples in frames.req by C.H. Acton
	HEE definition is based on the GSE example

    Version 2, 22-Feb-2005, WTT
	Modified HCI definition to tie to IAU_SUN frame
	Use RECTANGULAR specification in HEEQ frame

    Version 3, 23-Feb-2005, WTT
	Correct GSE and HEE definitions to use ECLIPDATE axis

    Version 4, 08-Aug-2008, WTT
	Add GEORTN coordinate system (comment added 30-Aug-2010)

Mean Ecliptic of Date (ECLIPDATE) Frame

     Definition of the Mean Ecliptic of Date frame:
 
              All vectors are geometric: no aberration corrections are
              used.
 
              The X axis is the first point in Aries for the mean ecliptic of
	      date, and the Z axis points along the ecliptic north pole.
 
              The Y axis is Z cross X, completing the right-handed
              reference frame.

	      This reference frame can be used to realize the HAE coordinate
	      system by using the sun as the observing body.

\begindata

        FRAME_ECLIPDATE              =  1803321
        FRAME_1803321_NAME           = 'ECLIPDATE'
        FRAME_1803321_CLASS          =  5
        FRAME_1803321_CLASS_ID       =  1803321
        FRAME_1803321_CENTER         =  399
        FRAME_1803321_RELATIVE       = 'J2000'
        FRAME_1803321_DEF_STYLE      = 'PARAMETERIZED'
        FRAME_1803321_FAMILY         = 'MEAN_ECLIPTIC_AND_EQUINOX_OF_DATE'
        FRAME_1803321_PREC_MODEL     = 'EARTH_IAU_1976'
        FRAME_1803321_OBLIQ_MODEL    = 'EARTH_IAU_1980'
        FRAME_1803321_ROTATION_STATE = 'ROTATING'

\begintext

Geocentric Solar Ecliptic (GSE) Frame
 
     Definition of the Geocentric Solar Ecliptic frame:
 
              All vectors are geometric: no aberration corrections are
              used.
 
              The position of the sun relative to the earth is the primary
              vector: the X axis points from the earth to the sun.
 
              The northern surface normal to the mean ecliptic of date is the
              secondary vector: the Z axis is the component of this vector
              orthogonal to the X axis.
 
              The Y axis is Z cross X, completing the right-handed
              reference frame.

\begindata

        FRAME_GSE                    =  1803311
        FRAME_1803311_NAME           = 'GSE'
        FRAME_1803311_CLASS          =  5
        FRAME_1803311_CLASS_ID       =  1803311
        FRAME_1803311_CENTER         =  399
        FRAME_1803311_RELATIVE       = 'J2000'
        FRAME_1803311_DEF_STYLE      = 'PARAMETERIZED'
        FRAME_1803311_FAMILY         = 'TWO-VECTOR'
        FRAME_1803311_PRI_AXIS       = 'X'
        FRAME_1803311_PRI_VECTOR_DEF = 'OBSERVER_TARGET_POSITION'
        FRAME_1803311_PRI_OBSERVER   = 'EARTH'
        FRAME_1803311_PRI_TARGET     = 'SUN'
        FRAME_1803311_PRI_ABCORR     = 'NONE'
        FRAME_1803311_SEC_AXIS       = 'Z'
        FRAME_1803311_SEC_VECTOR_DEF = 'CONSTANT'
        FRAME_1803311_SEC_FRAME      = 'ECLIPDATE'
        FRAME_1803311_SEC_SPEC       = 'RECTANGULAR'
        FRAME_1803311_SEC_VECTOR     = ( 0, 0, 1 )

\begintext

Heliocentric Inertial (HCI) Frame

     Definition of the Heliocentric Inertial frame:
 
              All vectors are geometric: no aberration corrections are
              used.
 
              The solar rotation axis is the primary vector: the Z axis points
	      in the solar north direction.
 
              The solar ascending node on the ecliptic of J2000 forms the X
              axis.
 
              The Y axis is Z cross X, completing the right-handed
              reference frame.

\begindata

        FRAME_HCI                    =  1810420
        FRAME_1810420_NAME           = 'HCI'
        FRAME_1810420_CLASS          =  5
        FRAME_1810420_CLASS_ID       =  1810420
        FRAME_1810420_CENTER         =  10
        FRAME_1810420_RELATIVE       = 'J2000'
        FRAME_1810420_DEF_STYLE      = 'PARAMETERIZED'
        FRAME_1810420_FAMILY         = 'TWO-VECTOR'
        FRAME_1810420_PRI_AXIS       = 'Z'
        FRAME_1810420_PRI_VECTOR_DEF = 'CONSTANT'
        FRAME_1810420_PRI_FRAME      = 'IAU_SUN'
        FRAME_1810420_PRI_SPEC       = 'RECTANGULAR'
        FRAME_1810420_PRI_VECTOR     = ( 0, 0, 1 )
        FRAME_1810420_SEC_AXIS       = 'Y'
        FRAME_1810420_SEC_VECTOR_DEF = 'CONSTANT'
        FRAME_1810420_SEC_FRAME      = 'ECLIPJ2000'
        FRAME_1810420_SEC_SPEC       = 'RECTANGULAR'
        FRAME_1810420_SEC_VECTOR     = ( 0, 0, 1 )

\begintext

Heliocentric Earth Ecliptic (HEE) Frame

     Definition of the Heliocentric Earth Ecliptic frame:
 
              All vectors are geometric: no aberration corrections are
              used.
 
              The position of the earth relative to the sun is the primary
              vector: the X axis points from the sun to the earth.
 
              The northern surface normal to the mean ecliptic of date is the
              secondary vector: the Z axis is the component of this vector
              orthogonal to the X axis.
 
              The Y axis is Z cross X, completing the right-handed
              reference frame.

\begindata

        FRAME_HEE                    =  1810311
        FRAME_1810311_NAME           = 'HEE'
        FRAME_1810311_CLASS          =  5
        FRAME_1810311_CLASS_ID       =  1810311
        FRAME_1810311_CENTER         =  10
        FRAME_1810311_RELATIVE       = 'J2000'
        FRAME_1810311_DEF_STYLE      = 'PARAMETERIZED'
        FRAME_1810311_FAMILY         = 'TWO-VECTOR'
        FRAME_1810311_PRI_AXIS       = 'X'
        FRAME_1810311_PRI_VECTOR_DEF = 'OBSERVER_TARGET_POSITION'
        FRAME_1810311_PRI_OBSERVER   = 'SUN'
        FRAME_1810311_PRI_TARGET     = 'EARTH'
        FRAME_1810311_PRI_ABCORR     = 'NONE'
        FRAME_1810311_SEC_AXIS       = 'Z'
        FRAME_1810311_SEC_VECTOR_DEF = 'CONSTANT'
        FRAME_1810311_SEC_FRAME      = 'ECLIPDATE'
        FRAME_1810311_SEC_SPEC       = 'RECTANGULAR'
        FRAME_1810311_SEC_VECTOR     = ( 0, 0, 1 )

\begintext

Heliocentric Earth Equatorial (HEEQ) Frame

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

        FRAME_HEEQ                   =  1810411
        FRAME_1810411_NAME           = 'HEEQ'
        FRAME_1810411_CLASS          =  5
        FRAME_1810411_CLASS_ID       =  1810411
        FRAME_1810411_CENTER         =  10
        FRAME_1810411_RELATIVE       = 'J2000'
        FRAME_1810411_DEF_STYLE      = 'PARAMETERIZED'
        FRAME_1810411_FAMILY         = 'TWO-VECTOR'
        FRAME_1810411_PRI_AXIS       = 'Z'
        FRAME_1810411_PRI_VECTOR_DEF = 'CONSTANT'
        FRAME_1810411_PRI_FRAME      = 'IAU_SUN'
        FRAME_1810411_PRI_SPEC       = 'RECTANGULAR'
        FRAME_1810411_PRI_VECTOR      = ( 0, 0, 1 )
        FRAME_1810411_SEC_AXIS       = 'X'
        FRAME_1810411_SEC_VECTOR_DEF = 'OBSERVER_TARGET_POSITION'
        FRAME_1810411_SEC_OBSERVER   = 'SUN'
        FRAME_1810411_SEC_TARGET     = 'EARTH'
        FRAME_1810411_SEC_ABCORR     = 'NONE'
        FRAME_1810411_SEC_FRAME      = 'IAU_SUN'

\begintext

Geocentric Radial Tangential Normal (GEORTN) Frame

     Definition of the Geocentric RTN Frame
 
              All vectors are geometric: no aberration corrections are used.
 
              The position of Earth relative to the Sun is the primary
              vector: the X axis points from the Sun center to Earth
 
              The solar rotation axis is the secondary vector: the Z axis is
	      the component of the solar north direction perpendicular to X.
 
              The Y axis is Z cross X, completing the right-handed reference
              frame.

\begindata

        FRAME_GEORTN                 =  1803410
        FRAME_1803410_NAME           = 'GEORTN'
        FRAME_1803410_CLASS          =  5
        FRAME_1803410_CLASS_ID       =  1803410
        FRAME_1803410_CENTER         =  10
        FRAME_1803410_RELATIVE       = 'J2000'
        FRAME_1803410_DEF_STYLE      = 'PARAMETERIZED'
        FRAME_1803410_FAMILY         = 'TWO-VECTOR'
        FRAME_1803410_PRI_AXIS       = 'X'
        FRAME_1803410_PRI_VECTOR_DEF = 'OBSERVER_TARGET_POSITION'
        FRAME_1803410_PRI_OBSERVER   = 'SUN'
        FRAME_1803410_PRI_TARGET     = 'EARTH'
        FRAME_1803410_PRI_ABCORR     = 'NONE'
        FRAME_1803410_PRI_FRAME      = 'IAU_SUN'
        FRAME_1803410_SEC_AXIS       = 'Z'
        FRAME_1803410_SEC_VECTOR_DEF = 'CONSTANT'
        FRAME_1803410_SEC_FRAME      = 'IAU_SUN'
        FRAME_1803410_SEC_SPEC       = 'RECTANGULAR'
        FRAME_1803410_SEC_VECTOR      = ( 0, 0, 1 )

\begintext