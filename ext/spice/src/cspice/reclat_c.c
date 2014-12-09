/*

-Procedure   reclat_c ( Rectangular to latitudinal coordinates )

-Abstract

   Convert from rectangular coordinates to latitudinal coordinates.
 
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
   #include "SpiceZim.h"
   #undef    reclat_c
   

   void reclat_c ( ConstSpiceDouble    rectan[3],
                   SpiceDouble       * radius,
                   SpiceDouble       * longitude,
                   SpiceDouble       * latitude  )

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   rectan     I   Rectangular coordinates of a point.
   radius     O   Distance of the point from the origin.
   longitude  O   Longitude of the point in radians.
   latitude   O   Latitude of the point in radians.
 
-Detailed_Input 

   rectan     The rectangular coordinates of the input point.  `rectan'
              is a 3-vector.
 
-Detailed_Output 
 
   radius     Distance of the point from the origin.

              The units associated with `radius' are those
              associated with the input `rectan'.

   longitude  Longitude of the input point.  This is angle between the
              prime meridian and the meridian containing `rectan'.  The
              direction of increasing longitude is from the +X axis
              towards the +Y axis.
 
              Longitude is output in radians. The range of `longitude'
              is [-pi, pi].


   latitude   Latitude of the input point.  This is the angle from
              the XY plane of the ray from the origin through the
              point. 

              Latitude is output in radians.  The range of `latitude'
              is [-pi/2, pi/2].

-Files 

   None. 
 
-Exceptions 
 
   Error free. 

   1) If the X and Y components of `rectan' are both zero, the
      longitude is set to zero.

   2) If `rectan' is the zero vector, longitude and latitude are
      both set to zero.
 
-Particulars 

   None.

-Parameters 
 
   None. 

-Examples

   Below are two tables.
 
   Listed in the first table (under rectan[0], rectan[1], and
   rectan[2]) are a number of points whose rectangular coordinates are
   taken from the set {-1, 0, 1}.
 
   The results of the code fragment

      reclat_c ( rectan, &r, &longitude, &latitude );

      latitude  *=  dpr_c();
      longitude *=  dpr_c();

   are listed to four decimal places in the second parallel table under
   r (radius), longitude, and latitude.

   rectan[0]  rectan[1] rectan[2]    r       longitude  latitude
   -------------------------------   ----------------------------
     0.0000     0.0000   0.0000      0.0000    0.0000    0.0000
     1.0000     0.0000   0.0000      1.0000    0.0000    0.0000
     0.0000     1.0000   0.0000      1.0000   90.0000    0.0000
     0.0000     0.0000   1.0000      1.0000    0.0000   90.0000
    -1.0000     0.0000   0.0000      1.0000  180.0000    0.0000
     0.0000    -1.0000   0.0000      1.0000  -90.0000    0.0000
     0.0000     0.0000  -1.0000      1.0000    0.0000  -90.0000
     1.0000     1.0000   0.0000      1.4142   45.0000    0.0000
     1.0000     0.0000   1.0000      1.4142    0.0000   45.0000
     0.0000     1.0000   1.0000      1.4142   90.0000   45.0000
     1.0000     1.0000   1.0000      1.7320   45.0000   35.2643
 
-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
   E.D. Wright     (JPL)

-Version

   -CSPICE Version 1.2.1, 30-JUL-2003 (NJB)

       Various header changes were made to improve clarity.  Some
       minor header corrections were made.

   -CSPICE Version 1.2.0, 28-AUG-2001 (NJB)
     
       Removed tab characters from source file.  Now includes 
       interface macro header SpiceZim.h.

   -CSPICE Version 1.1.0, 21-OCT-1998 (NJB)

      Made input vector const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries

   rectangular to latitudinal coordinates

-&
*/

{ /* Begin reclat_c */

   /*
   Local variables and definitions.
   */

   SpiceDouble   vmax;
   SpiceDouble   x1;
   SpiceDouble   y1;
   SpiceDouble   z1;


   /* Function Body */

   vmax = MaxAbs(  rectan[0], MaxAbs( rectan[1], rectan[2] )   );

   if ( vmax > 0.)
      {
      x1        = rectan[0] / vmax;
      y1        = rectan[1] / vmax;
      z1        = rectan[2] / vmax;
      *radius   = vmax * sqrt( x1*x1 + y1*y1 + z1*z1 );
      *latitude = atan2(z1, sqrt( x1*x1 + y1*y1 ) );


      if ( x1 == 0. && y1 == 0.)
         {
         *longitude = 0.;
         }

      else
        {
        *longitude = atan2(y1, x1);
        }

      }

   else
      {

      /* 
      The vector is the zero vector. 
      */

      *radius    = 0.;
      *longitude = 0.;
      *latitude  = 0.;
      }


} /* End reclat_c */
