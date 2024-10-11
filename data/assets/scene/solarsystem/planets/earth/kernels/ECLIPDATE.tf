Earth Mean Ecliptic and Equinox of Date frame (ECLIPDATE)
---------------------------------------------------------

   Definition:
   -----------
   The Earth Mean Ecliptic and Equinox of Date frame is defined as follows:

      -  +Z axis is aligned with the north-pointing vector normal to the
         mean orbital plane of the Earth;

      -  +X axis points along the ``mean equinox'', which is defined as the
         intersection of the Earth's mean orbital plane with the Earth's mean
         equatorial plane. It is aligned with the cross product of the
         north-pointing vectors normal to the Earth's mean equator and mean
         orbit plane of date;

      -  +Y axis is the cross product of the Z and X axes and completes the
         right-handed frame;

      -  the origin of this frame is the Earth's center of mass.

   The mathematical model used to obtain the orientation of the Earth's mean
   equator and equinox of date frame is the 1976 IAU precession model, built
   into SPICE.

   The mathematical model used to obtain the mean orbital plane of the Earth
   is the 1980 IAU obliquity model, also built into SPICE.

   The base frame for the 1976 IAU precession model is J2000.

   Required Data:
   --------------
   The usage of this frame does not require additional data since both the
   precession and the obliquity models used to define this frame are already
   built into SPICE.


   Remarks:
   --------
   None.


  \begindata

    FRAME_ECLIPDATE                =  1503399
    FRAME_1503399_NAME             = 'ECLIPDATE'
    FRAME_1503399_CLASS            =  5
    FRAME_1503399_CLASS_ID         =  1503399
    FRAME_1503399_CENTER           =  399
    FRAME_1503399_RELATIVE         = 'J2000'
    FRAME_1503399_DEF_STYLE        = 'PARAMETERIZED'
    FRAME_1503399_FAMILY           = 'MEAN_ECLIPTIC_AND_EQUINOX_OF_DATE
    FRAME_1503399_PREC_MODEL       = 'EARTH_IAU_1976'
    FRAME_1503399_OBLIQ_MODEL      = 'EARTH_IAU_1980'
    FRAME_1503399_ROTATION_STATE   = 'ROTATING'

