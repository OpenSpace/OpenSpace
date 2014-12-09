/*

-Procedure cylsph_c ( Cylindrical to spherical )

-Abstract

   Convert from cylindrical to spherical coordinates.

-Disclaimer

   THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
   CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
   GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
   ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
   PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
   TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
   WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
   PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
   SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
   SOFTWARE AND RELATED MATERIALS, HOWEVER USED.

   IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
   BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
   LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
   INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
   REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
   REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.

   RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
   THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
   CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
   ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.

-Required_Reading

   None.

-Keywords

   CONVERSION
   COORDINATES

*/

   #include <math.h>
   #include "SpiceUsr.h"
   #include "SpiceZmc.h"

   void cylsph_c ( SpiceDouble    r,
                   SpiceDouble    lonc,
                   SpiceDouble    z,
                   SpiceDouble *  radius,
                   SpiceDouble *  colat,
                   SpiceDouble *  lon )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  -------------------------------------------------
   r          I   Distance of point from z axis.
   lonc       I   Angle (radians) of point from XZ plane.
   z          I   Height of point above XY plane.
   radius     O   Distance of point from origin.
   colat      O   Polar angle (co-latitude in radians) of point.
   lon        O   Azimuthal angle (longitude) of point (radians).

-Detailed_Input

   r          Distance of the point of interest from z axis.

   lonc       Cylindrical angle (radians) of the point from the
              XZ plane.

   z          Height of the point above XY plane.

-Detailed_Output

   radius     Distance of the point from origin.

   colat      Polar angle (co-latitude in radians) of the point.

   lon        Azimuthal angle (longitude) of the point (radians).

-Parameters

   None.

-Particulars

   This returns the spherical coordinates of a point whose position
   is input through cylindrical coordinates.

-Examples


   Below are two tables:  The first is a set of input values
   the second is the result of the following sequence of
   calls to Spicelib routines.  Note all input and output angular
   quantities are in degrees.

       convrt_c ( lonc, "DEGREES", "RADIANS", lonc  );

       cylsph_c ( r, lonc, z, &radius, &colat, &lon );

       convrt_c ( lon,  "RADIANS", "DEGREES", lon   );
       convrt_c ( lat,  "RADIANS", "DEGREES", lat   );



   Inputs:                         Results:

   r        lonc     z             radius   lon      colat
   ------   ------   ------        ------   ------   ------
   1.0000     0       0            1.0000     0       90.00
   1.0000    90.00    0            1.0000    90.00    90.00
   1.0000   180.00    1.000        1.4142   180.00    45.00
   1.0000   180.00   -1.000        1.4142   180.00   135.00
   0.0000   180.00    1.000        1.0000   180.00     0.00
   0.0000    33.00    0            0.0000    33.00     0.00

-Restrictions

   None.

-Exceptions

   Error free.

-Files

   None.

-Author_and_Institution

   E.D. Wright     (JPL)
   W.L. Taber      (JPL)

-Literature_References

   None.

-Version

   -CSPICE Version 1.0.1, 08-FEB-1998 (EDW)

       Corrected and clarified header entries.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   cylindrical to spherical

-&
*/

{ /* Begin cylsph_c */

   /*
   Local variables
   */

   SpiceDouble             big;
   SpiceDouble             th;
   SpiceDouble             rh;
   SpiceDouble             x;
   SpiceDouble             y;


   /* Computing biggest absolute value */

   big = MaxAbs( r, z );

   if (big == 0.)
      {
      th = 0.;
      rh = 0.;
      }
   else
      {
      x  = r / big;
      y  = z / big;
      rh = big * sqrt( x * x + y * y);
      th = atan2( r, z );
      }


   /* Move the results to output variables */

   *lon    = lonc;
   *radius = rh;
   *colat  = th;


} /* End cylsph_c */
