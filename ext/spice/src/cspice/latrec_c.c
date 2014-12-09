/*

-Procedure   latrec_c ( Latitudinal to rectangular coordinates )

-Abstract

   Convert from latitudinal coordinates to rectangular coordinates.

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

   void latrec_c ( SpiceDouble    radius,
                   SpiceDouble    longitude,
                   SpiceDouble    latitude,
                   SpiceDouble    rectan[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   radius     I   Distance of a point from the origin.
   longitude  I   Longitude of point in radians.
   latitude   I   Latitude of point in radians.
   rectan     O   Rectangular coordinates of the point.

-Detailed_Input

   radius     Distance of a point from the origin.
 
   longitude  Longitude of the input point.  This is the angle between
              the prime meridian and the meridian containing `rectan'.
              The direction of increasing longitude is from the +X axis
              towards the +Y axis.
 
              Longitude is measured in radians.  On input, the range 
              of longitude is unrestricted.

   latitude   Latitude of the input point. This is the angle from
              the XY plane of the ray from the origin through the
              point.

              Latitude is measured in radians. On input, the range of
              latitude is unrestricted.

-Detailed_Output

   rectan     The rectangular coordinates of the input point.
              `rectan' is a 3 vector.

              The units associated with `rectan' are those
              associated with the input radius.
-Files

   None.

-Exceptions

   Error free.

-Particulars

   This routine returns the rectangular coordinates of a point
   whose position is input in latitudinal coordinates.

   Latitudinal coordinates are defined by a distance from a central
   reference point, an angle from a reference meridian, and an angle
   above the equator of a sphere centered at the central reference
   point.

-Parameters

   None.

-Examples

   Below are two tables.

   Listed in the first table (under r, longitude and latitude ) are
   latitudinal coordinate triples that approximately represent
   points whose rectangular coordinates are taken from the set
   {-1, 0, 1}.  (Angular quantities are given in degrees.)

   The results of the code fragment

      /.
      Use the CSPICE routine rpd_c() to convert the angular
      quantities to radians
      ./
      latitude  *= rpd_c();
      longitude *= rpd_c();

      latrec_c ( r, longitude, latitude, rectan );


   are listed in the second parallel table under rectan[0], rectan[1],
   and rectan[2].


    r       longitude  latitude      rectan[0]  rectan[1] rectan[2].
   ----------------------------      -------------------------------
    0.0000    0.0000    0.0000         0.0000     0.0000   0.0000
    1.0000    0.0000    0.0000         1.0000     0.0000   0.0000
    1.0000   90.0000    0.0000         0.0000     1.0000   0.0000
    1.0000    0.0000   90.0000         0.0000     0.0000   1.0000
    1.0000  180.0000    0.0000        -1.0000     0.0000   0.0000
    1.0000  -90.0000    0.0000         0.0000    -1.0000   0.0000
    1.0000    0.0000  -90.0000         0.0000     0.0000  -1.0000
    1.4142   45.0000    0.0000         1.0000     1.0000   0.0000
    1.4142    0.0000   45.0000         1.0000     0.0000   1.0000
    1.4142   90.0000   45.0000         0.0000     1.0000   1.0000
    1.7320   45.0000   35.2643         1.0000     1.0000   1.0000

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   C.H. Acton      (JPL)
   N.J. Bachman    (JPL)
   E.D. Wright     (JPL)
   W.L. Taber      (JPL)

-Version

   -CSPICE Version 1.0.1, 29-JUL-2003 (NJB) (CHA)

       Various header corrections were made.

   -CSPICE Version 1.0.0, 16-APR-1999 (EDW)
     
-Index_Entries

   latitudinal to rectangular coordinates

-&
*/

{ /* Begin latrec_c */

   /* Function Body */

   rectan[0] = radius * cos( longitude ) * cos( latitude );
   rectan[1] = radius * sin( longitude ) * cos( latitude );
   rectan[2] = radius * sin( latitude  );

} /* End latrec_c */
