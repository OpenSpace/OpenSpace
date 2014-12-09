/*

-Procedure sphrec_c ( Spherical to rectangular coordinates )

-Abstract
 
   Convert from spherical coordinates to rectangular coordinates. 
 
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


   void sphrec_c ( SpiceDouble    r,
                   SpiceDouble    colat, 
                   SpiceDouble    lon,
                   SpiceDouble    rectan[3] ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   r          I   Distance of a point from the origin. 
   colat      I   Angle of the point from the positive Z-axis. 
   lon        I   Angle of the point from the XZ plane in radians. 
   rectan     O   Rectangular coordinates of the point. 
 
-Detailed_Input
 
   r          Distance of the point from the origin. 
 
   colat      Angle between the point and the positive z-axis. 
 
   lon        Angle of the projection of the point to the XY 
              plane from the positive X-axis.  The positive 
              Y-axis is at longitude PI/2 radians. 
 
-Detailed_Output
 
   rectan     The rectangular coordinates of a point. 
 
-Parameters
 
   None. 
 
-Particulars
 
   This routine returns the rectangular coordinates of a point 
   whose position is input in spherical coordinates. 
 
   Spherical coordinates are defined by a distance from a central 
   reference point, an angle from a reference meridian, and an angle 
   from the z-axis.  The co-latitude of the positive Z-axis is 
   zero.  The longitude of the posive Y-axis is PI/2 radians. 
 
-Examples
 
   Below are two tables. 
 
   Listed in the first table (under r, colat and lon  ) are 
   spherical coordinate triples that approximately represent points 
   whose rectangular coordinates are taken from the set {-1, 0, 1}. 
   (Angular quantities are given in degrees.) 
 
   The result of the code fragment 
 
        Use the CSPICE routine convrt_c to convert the angular 
        quantities to radians 
 
        convrt_c ( colat, "DEGREES", "RADIANS", lat  ) 
        convrt_c (  lon,  "DEGREES", "RADIANS", lon  ) 
 
        sphrec_c ( r, colat,  lon, X ) 
 
 
   are listed in the second parallel table under X(1), X(2) and X(3). 
 
     r          colat      lon            X(1)       X(2)     X(3) 
     ----------------------------         -------------------------- 
     0.0000     0.0000     0.0000         0.0000     0.0000   0.0000 
     1.0000    90.0000     0.0000         1.0000     0.0000   0.0000 
     1.0000    90.0000    90.0000         0.0000     1.0000   0.0000 
     1.0000     0.0000     0.0000         0.0000     0.0000   1.0000 
     1.0000    90.0000   180.0000        -1.0000     0.0000   0.0000 
     1.0000    90.0000   -90.0000         0.0000    -1.0000   0.0000 
     1.0000   180.0000     0.0000         0.0000     0.0000  -1.0000 
     1.4142    90.0000    45.0000         1.0000     1.0000   0.0000 
     1.4142    45.0000     0.0000         1.0000     0.0000   1.0000 
     1.4142    45.0000    90.0000         0.0000     1.0000   1.0000 
     1.7320    54.7356    45.0000         1.0000     1.0000   1.0000 
 
 
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
 
   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries
 
   spherical to rectangular coordinates 
 
-&
*/

{ /* Begin sphrec_c */

   /*
   Local variables
   */

   SpiceDouble    x;
   SpiceDouble    y;
   SpiceDouble    z;


   /* Function Body */

   x = r * cos( lon  ) * sin( colat );
   y = r * sin( lon  ) * sin( colat );
   z = r * cos( colat );


   /* Move the results to the output variables */

   rectan[0] = x;
   rectan[1] = y;
   rectan[2] = z;


} /* End sphrec_c */
