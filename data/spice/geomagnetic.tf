KPL/FK

    Inspired by William Thompson of NASA GSFC.

    The coordinate frames in this file all have ID values based on the pattern
    18ccple, where

    18 = Prefix to put in the allowed 1400000 to 2000000 range
    cc = 03 for geocentric, 10 for heliocentric
    p  = Pole basis: 1=geographic, 2=geomagnetic, 3=ecliptic, 4=solar
    l  = Longitude basis: 1=Earth-Sun, 2=ecliptic
    e  = Ecliptic basis: 0=J2000, 1=mean, 2=true

    Earth geomagnetic North pole vector from IGRF-11 at
    http://www.ngdc.noaa.gov/IAGA/vmod/igrf.html

\begindata
        FRAME_MAG                     = 1803222
        FRAME_1803222_NAME            = 'MAG'
        FRAME_1803222_CLASS           = 3
        FRAME_1803222_CLASS_ID        = 1803222
        FRAME_1803222_CENTER          = 399

        CK_1803222_SCLK               = -39900001
        CK_1803222_SPK                = 399

\begintext

    Geocentric Solar Magnetospheric frame (GSM):
      * Origin at the Earth centre
      * X along the origin-to-Sun vector
      * Z points in the direction of Earth geomagnetic North pole vector
      * Y is Z x X, completing the right-handed reference frame

\begindata

        FRAME_GSM                      = 1803212
        FRAME_1803212_NAME             = 'GSM'
        FRAME_1803212_CLASS            = 5
        FRAME_1803212_CLASS_ID         = 1803212
        FRAME_1803212_CENTER           = 399

        FRAME_1803212_RELATIVE         = 'J2000'
        FRAME_1803212_DEF_STYLE        = 'PARAMETERIZED'
        FRAME_1803212_FAMILY           = 'TWO-VECTOR'

        FRAME_1803212_PRI_AXIS         = 'X'
        FRAME_1803212_PRI_VECTOR_DEF   = 'OBSERVER_TARGET_POSITION'
        FRAME_1803212_PRI_OBSERVER     = 399
        FRAME_1803212_PRI_TARGET       = 10
        FRAME_1803212_PRI_ABCORR       = 'NONE'

        FRAME_1803212_SEC_AXIS         = 'Z'
        FRAME_1803212_SEC_VECTOR_DEF   = 'CONSTANT'
        FRAME_1803212_SEC_FRAME        = 'MAG'
        FRAME_1803212_SEC_SPEC         = 'RECTANGULAR'
        FRAME_1803212_SEC_VECTOR       = ( 0, 0, 1 )

        FRAME_1803212_ROTATION_STATE   = 'ROTATING'

\begintext

$Id: geomagnetic.tf 4746 2013-04-05 19:49:10Z bogdan $