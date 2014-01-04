/*

-Procedure srfrec_c ( Surface to rectangular coordinates )

-Abstract
 
   Convert planetocentric latitude and longitude of a surface 
   point on a specified body to rectangular coordinates. 
 
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
 
   KERNEL 
   NAIF_IDS 
 
-Keywords
 
   CONVERSION 
   COORDINATES 
   TRANSFORMATION 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"


   void srfrec_c ( SpiceInt      body,
                   SpiceDouble   longitude,
                   SpiceDouble   latitude,
                   SpiceDouble   rectan[3] ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   body       I   NAIF integer code of an extended body. 
   longitude  I   Longitude of point in radians.
   latitude   I   Latitude of point in radians.
   rectan     O   Rectangular coordinates of the point. 
 
-Detailed_Input
 
   body       is the NAIF integer code of an extended body 
              on which a surface point of interest is located. 
              The body is modeled as a triaxial ellipsoid.
 
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

   rectan     The rectangular coordinates of the input point. `rectan'
              is a 3-vector.

              Units are the same as those used to define the radii of
              `body'.  Normally, these units are km.
 
-Parameters
 
    None. 
 
-Exceptions
 
   1)  If radii for `body' are not found in the kernel pool, the error 
       will be diagnosed by routines called by this routine. 
 
   2)  If radii for `body' are invalid, the error will be diagnosed by 
       routines called by this routine.  The radii should be 
       positive. 
 
-Files
 
   None. 
 
-Particulars
 
   This routine returns the rectangular coordinates of a surface 
   point on an extended body with known radii, where the location 
   of the surface point is specified in planetocentric latitudinal 
   coordinates. 
 
   Latitudinal coordinates are defined by a distance from a central 
   reference point, an angle from a reference meridian, and an angle 
   above the equator of a sphere centered at the central reference 
   point.  In this case, the distance from the central reference 
   point is not required as an input because the fact that the 
   point is on the body's surface allows one to deduce this quantity. 
 
   Below are two tables that demonstrate by example the relationship 
   between rectangular and latitudinal coordinates. 

   Listed in the first table (under r, longitude and latitude ) are
   latitudinal coordinate triples that approximately represent
   points whose rectangular coordinates are taken from the set
   {-1, 0, 1}.  (Angular quantities are given in degrees.)


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


   This routine is related to the CSPICE routine latrec_c, which 
   accepts a radius, longitude, and latitude as inputs and produces 
   equivalent rectangular coordinates as outputs. 
 
-Examples
 
   1)  Find the rectangular coordinates of the point 
 
          100 degrees planetocentric longitude 
          -35 degrees planetocentric latitude 
 
       on the Earth; then convert these coordinates back to 
       latitudinal coordinates.  We should be able to recover 
       our original longitude and latitude values. 


             #include <stdio.h>
             #include "SpiceUsr.h"


             int main()
          {

             #define  EARTH          399

             SpiceDouble             lat;
             SpiceDouble             lon;
             SpiceDouble             x      [3];
             SpiceDouble             radius;

             /.
             Load the kernel pool with a PCK file that contains 
             values for the radii of the Earth.
             ./
             furnsh_c ( "pck00008.tpc" );

             /.
             Find `x', the rectangular coordinates of the surface point
             defined by `lat' and `long'.  The NAIF integer code for
             the Earth is 399. (See the NAIF_IDS required reading file
             for the complete set of codes.)
             ./
             lon   =  100.0;
             lat   =  -35.0;

             printf ( "Original latitudinal coordinates:\n"
                      "\n"
                      "Longitude (deg):  %f\n"
                      "Latitude  (deg):  %f\n",
                      lon,
                      lat                                   );

             /.
             Convert angles to radians forr input to srfrec_c.
             ./
             srfrec_c ( EARTH, lon*rpd_c(), lat*rpd_c(), x );

             printf ( "\n"
                      "Rectangular coordinates:\n"
                      "\n"
                      "X (km):  %24.16f\n"
                      "Y (km):  %24.16f\n"
                      "Z (km):  %25.16f\n",
                      x[0],
                      x[1],
                      x[2]                         );

             /.
             Now try to recover the original latitudinal coordinates
             from the rectangular coordinates found by srfrec_c.
             ./
             reclat_c ( x, &radius, &lon, &lat );

             /.
             Convert angles back to degree for display.
             ./
             printf ( "\n"
                      "Latitudinal coordinates recovered from "
                      "rectangular coordinates:\n"
                      "\n"
                      "Longitude (deg):  %f\n"
                      "Latitude  (deg):  %f\n"
                      "Radius    (km):   %f\n",
                      lon * dpr_c(),
                      lat * dpr_c(),
                      radius                       );

             return ( 0 );
          }

 
-Restrictions
 
   1)  A SPICE text kernel containing the body radius definitions 
       required by this routine must be loaded into the kernel 
       pool prior to any calls to this routine. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
   W.L. Taber     (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 03-NOV-2005 (NJB) (WLT)

-Index_Entries
 
   convert bodyfixed latitudinal coordinates to rectangular 
   convert surface latitudinal coordinates to rectangular 
   surface point latitudinal coordinates to rectangular 
 
-&
*/

{ /* Begin srfrec_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "srfrec_c" );

   srfrec_ ( (SpiceInt    *) &body,
             (SpiceDouble *) &longitude,
             (SpiceDouble *) &latitude,
             (SpiceDouble *) rectan      );


   chkout_c ( "srfrec_c" );

} /* End srfrec_c */
