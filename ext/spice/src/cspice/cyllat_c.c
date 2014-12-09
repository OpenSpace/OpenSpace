/*

-Procedure cyllat_c ( Cylindrical to latitudinal )

-Abstract
 
   Convert from cylindrical to latitudinal coordinates. 
 
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
 
   CONVERSION,  COORDINATES 
 
*/

   #include <math.h>
   #include "SpiceUsr.h"
   #include "SpiceZmc.h"

   void cyllat_c ( SpiceDouble    r,
                   SpiceDouble    lonc,
                   SpiceDouble    z,
                   SpiceDouble *  radius,
                   SpiceDouble *  lon,
                   SpiceDouble *  lat ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   r          I   Distance of point from z axis. 
   lonc       I   Cylindrical angle of point from XZ plane(radians). 
   z          I   Height of point above XY plane. 
   radius     O   Distance of point from origin. 
   lon        O   Longitude of point (radians). 
   lat        O   Latitude of point (radians). 
 
-Detailed_Input
 
   r          Distance of the input point from z axis. 
 
   lonc       Cylindrical angle of the point from XZ plane(radians). 
 
   z          Height of the point above XY plane. 
 
-Detailed_Output
 
   radius     Distance of the input point from origin. 
 
   lon        Longitude (i.e. angle from the XZ plane) of the input 
              point. 
 
   lat        Latitude (i.e. angle above the XY plane) of the input 
              point (radians). 
 
-Parameters
 
   None. 
 
-Particulars
 
   This routine converts coordinates given in cylindrical 
   coordinates to coordinates in latitudinal coordinates. 
 
   Latitudinal coordinates are the same coordinates as use for 
   the earth.  Latitude refers to angle above the equator, longitude 
   to angle east from a meridian, and radius to the distance from 
   an origin. 
 
-Examples
 
   Below are two tables:  The first is a set of input values 
   the second is the result of the following sequence of 
   calls to Spicelib routines.  Note all input and output angular 
   quantities are in degrees. 
 
       convrt_c ( lonc , "DEGREES", "RADIANS", lonc  );

       cyllat_c ( r, lonc , z, &radius, &lon, &lat   );

       convrt_c ( lon,  "RADIANS", "DEGREES", lon    );
       convrt_c ( lat,  "RADIANS", "DEGREES", lat    );
 
 
   Inputs:                         Results: 
 
   r        lonc     z             radius   lon     lat 
   ------   ------   ------        ------   ------   ------ 
   1.0000     0       0            1.0000     0        0 
   1.0000    90.00    0            1.0000    90.00     0 
   1.0000   180.00    1.000        1.4142   180.00    45.00 
   1.0000   180.00   -1.000        1.4142   180.00   -45.00 
   0.0000   180.00    1.000        1.0000   180.00    90.00 
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

   -CSPICE Version 1.1.0, 23-JUL-2001 (NJB)
     
      Removed tab characters from source file.

   -CSPICE Version 1.0.1, 08-FEB-1998 (EDW)

       Corrected and clarified header entries.  Removed return call.

   -CSPICE Version 1.0.0, 25-OCT-1997   (EDW)

-Index_Entries
 
   cylindrical to latitudinal 
 
-&
*/

{ /* Begin cyllat_c */

   /*
   Local variables
   */

   SpiceDouble    lattud;
   SpiceDouble    rho;
   SpiceDouble    x;
   SpiceDouble    y;
   SpiceDouble    big;


   /* Computing biggest absolute value */

   big = MaxAbs( r, z);

   if (big > 0.)
      {
      x = r / big;
      y = z / big;
      rho = big * sqrt(x * x + y * y);
      }
   else
      {
      rho = 0.;
      }

   if (rho == 0.)
      {
      lattud = 0.;
      }
   else
      {
      lattud = atan2( z, r );
      }


   /*  Move results to output variables */

   *lon    = lonc;
   *radius = rho;
   *lat    = lattud;


} /* End cyllat_c */
