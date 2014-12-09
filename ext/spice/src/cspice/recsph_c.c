/*

-Procedure      recsph_c ( Rectangular to spherical coordinates )

-Abstract
 
   Convert from rectangular coordinates to spherical coordinates. 
 
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
   #undef    recsph_c


   void recsph_c ( ConstSpiceDouble     rectan[3], 
                   SpiceDouble        * r, 
                   SpiceDouble        * colat,
                   SpiceDouble        * lon      ) 

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   rectan     I   Rectangular coordinates of a point. 
   r          O   Distance of the point from the origin. 
   colat      O   Angle of the point from the positive Z-axis. 
   lon        O   Longitude of the point in radians. 
 
-Detailed_Input
 
   rectan     The rectangular coordinates of a point. 
 
-Detailed_Output
 
   r          Distance of the point from the origin. 
 
   colat      Angle between the point and the positive z-axis. 
 
   lon        Longitude of the point in radians.  This is the angle
              between the positive X-axis and the orthogonal
              projection of the point onto the XY plane.  LONG
              increases in the counterclockwise sense about the
              positive Z-axis.  The range of LONG is:

                 -pi < LONG <= pi
 
-Parameters
 
   None. 
 
-Particulars
 
   This routine returns the spherical coordinates of a point 
   whose position is input in rectangular coordinates. 
 
   spherical coordinates are defined by a distance from a central 
   reference point, an angle from a reference meridian, and an angle 
   from the z-axis. 
 
-Examples
 
   Below are two tables. 
 
   Listed in the first table (under X(1), X(2) and X(3) ) are a 
   number of points whose rectangular coordinates are 
   taken from the set {-1, 0, 1}. 
 
   The result of the code fragment 
 
        recsph_c ( X, r, colat, lon  ) 
 
        Use the CSPICE routine convrt_c to convert the angular 
        quantities to degrees 
 
        convrt_c ( colat, "RADIANS", "DEGREES", colat ) 
        convrt_c (  lon,  "RADIANS", "DEGREES", lon   ) 
 
   are listed to 4 decimal places in the second parallel table under 
   r (radius), colat (co-latitude), and  lon  (longitude). 
 
     X(1)       X(2)     X(3)        r         colat       lon  
     --------------------------      ---------------------------- 
     0.0000     0.0000   0.0000      0.0000     0.0000     0.0000 
     1.0000     0.0000   0.0000      1.0000    90.0000     0.0000 
     0.0000     1.0000   0.0000      1.0000    90.0000    90.0000 
     0.0000     0.0000   1.0000      1.0000     0.0000     0.0000 
    -1.0000     0.0000   0.0000      1.0000    90.0000   180.0000 
     0.0000    -1.0000   0.0000      1.0000    90.0000   -90.0000 
     0.0000     0.0000  -1.0000      1.0000   180.0000     0.0000 
     1.0000     1.0000   0.0000      1.4142    90.0000    45.0000 
     1.0000     0.0000   1.0000      1.4142    45.0000     0.0000 
     0.0000     1.0000   1.0000      1.4142    45.0000    90.0000 
     1.0000     1.0000   1.0000      1.7320    54.7356    45.0000 
 
-Restrictions
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Author_and_Institution
 
   W.L. Taber      (JPL) 
   E.D. Wright     (JPL)
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.1.1, 07-JAN-2002 (NJB) (EDW)

     Fixed description of lon in Brief_I/O and Detailed_I/O
     header sections.

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

      Made input coordinate array const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries
 
   rectangular to spherical coordinates 
 
-&
*/

{ /* Begin recsph_c */

   /*
   Local constants
   */

   SpiceDouble    x;
   SpiceDouble    y;
   SpiceDouble    z;
   SpiceDouble    big;


   /* Computing maximum magnitude of the elements of rectan */

   big = MaxAbs( rectan[0], MaxAbs( rectan[1], rectan[2] ) );

   if (big > 0.) 
      {

      x = rectan[0] / big;
      y = rectan[1] / big;
      z = rectan[2] / big;

      *r     = big * sqrt(x * x + y * y + z * z );
      *colat = atan2( sqrt(x * x + y * y), z );

      x = rectan[0];
      y = rectan[1];

      if (x == 0. && y == 0.)
         {
         *lon = 0.;
         } 
      else 
         {
         *lon = atan2(y, x);
         }
      }

   else
      {
      *r     = 0.;
      *colat = 0.;
      *lon   = 0.;
      }


} /* End recsph_c */
