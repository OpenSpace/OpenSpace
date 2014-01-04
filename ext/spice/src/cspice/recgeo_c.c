/*

-Procedure      recgeo_c ( Rectangular to geodetic )

-Abstract
 
   Convert from rectangular coordinates to geodetic coordinates. 
 
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

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZim.h"
   #undef    recgeo_c


   void recgeo_c ( ConstSpiceDouble     rectan[3], 
                   SpiceDouble          re, 
                   SpiceDouble          f, 
                   SpiceDouble        * lon,
                   SpiceDouble        * lat,
                   SpiceDouble        * alt        )
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   rectan     I   Rectangular coordinates of a point. 
   re         I   Equatorial radius of the reference spheroid. 
   f          I   Flattening coefficient. 
   lon        O   Geodetic longitude of the point (radians). 
   lat        O   Geodetic latitude  of the point (radians). 
   alt        O   Altitude of the point above reference spheroid. 
 
-Detailed_Input
 
   rectan    Rectangular coordinates of the input point.  
 
   re        Equatorial radius of a reference spheroid.  This spheroid
             is a volume of revolution:  its horizontal cross sections
             are circular.  The shape of the spheroid is defined by
             an equatorial radius `re' and a polar radius `rp'.
 
   f         Flattening coefficient = (re-rp) / re, where rp is 
             the polar radius of the spheroid. 
 
-Detailed_Output
 
   lon       Geodetic longitude of the input point.  This is the
             angle between the prime meridian and the meridian
             containing `rectan'.  The direction of increasing
             longitude is from the +X axis towards the +Y axis.
 
             `lon' is output in radians.  The range of `lon' is
             [-pi, pi].


   lat       Geodetic latitude of the input point.  For a point P
             on the reference spheroid, this is the angle between the
             XY plane and the outward normal vector at P. For a point P
             not on the reference spheroid, the geodetic latitude is
             that of the closest point to P on the spheroid.

             `lat' is output in radians. The range of `lat' is 
             [-pi/2, pi/2].
 
 
   alt       Altitude of point above the reference spheroid. 

             The units associated with `alt' are those associated with
             the input `rectan'.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the equatorial radius is non-positive, the error 
      SPICE(VALUEOUTOFRANGE) is signaled. 
 
   2) If the flattening coefficient is greater than or equal to 
      one, the error SPICE(VALUEOUTOFRANGE) is signaled. 

   3) For points inside the reference ellipsoid, the nearest point on
      the ellipsoid to `rectan' may not be unique, so latitude may not
      be well-defined.
 
-Files
 
   None. 
 
-Particulars
 
   Given the body-fixed rectangular coordinates of a point, and the 
   constants describing the reference spheroid,  this routine 
   returns the geodetic coordinates of the point.  The body-fixed 
   rectangular frame is that having the x-axis pass through the 
   0 degree latitude 0 degree longitude point.  The y-axis passes 
   through the 0 degree latitude 90 degree longitude.  The z-axis 
   passes through the 90 degree latitude point.  For some bodies 
   this coordinate system may not be a right-handed coordinate 
   system. 
 
-Examples
 
   This routine can be used to convert body fixed rectangular 
   coordinates (such as the Satellite Tracking and Data Network 
   of 1973) to geodetic coordinates such as those used by the 
   United States Geological Survey topographic maps. 
 
   The code would look something like this 
 
      /. 
      Shift the STDN-73 coordinates to line up with the center 
      of the Clark66 reference system. 
      ./   

      vsub_c ( stdnx, offset, x );

      /.
      Using the equatorial radius of the Clark66 spheroid 
      (CLARKR = 6378.2064 km) and the Clark 66 flattening 
      factor (CLARKF = 1.0 / 294.9787 ) convert to 
      geodetic coordinates of the North American Datum of 1927. 
      ./

      recgeo_c ( x, CLARKR, CLARKF, &lon, &lat, &alt ) 
  
 
   Below are two tables. 
 
   Listed in the first table (under X[0], X[1] and X[2]) are a 
   number of points whose rectangular coordinates are 
   taken from the set {-1, 0, 1}. 
 

   The results of the code fragment 
 
      recgeo_c ( x, CLARKR, CLARKF, &lon, &lat, &alt );

      /.
      Use the CSPICE routine convrt_c to convert the angular 
      quantities to degrees 
      ./
      convrt_c ( lat, "RADIANS", "DEGREES", &lat );
      convrt_c ( lon, "RADIANS", "DEGREES", &lon );


   are listed to four decimal places in the second parallel table under 
   lon (longitude), lat (latitude), and  alt (altitude). 
 
     X[0]       X[1]     X[2]         lon       lat       alt 
     --------------------------       ---------------------------- 
     0.0000     0.0000   0.0000       0.0000    90.0000   -6356.5838 
     1.0000     0.0000   0.0000       0.0000     0.0000   -6377.2063 
     0.0000     1.0000   0.0000      90.0000     0.0000   -6377.2063 
     0.0000     0.0000   1.0000       0.0000    90.0000   -6355.5838 
    -1.0000     0.0000   0.0000     180.0000     0.0000   -6377.2063 
     0.0000    -1.0000   0.0000     -90.0000     0.0000   -6377.2063 
     0.0000     0.0000  -1.0000       0.0000   -90.0000   -6355.5838 
     1.0000     1.0000   0.0000      45.0000     0.0000   -6376.7921 
     1.0000     0.0000   1.0000       0.0000    88.7070   -6355.5725 
     0.0000     1.0000   1.0000      90.0000    88.7070   -6355.5725 
     1.0000     1.0000   1.0000      45.0000    88.1713   -6355.5612 
 
-Restrictions
 
   None.
 
-Literature_References
 
   See FUNDAMENTALS OF ASTRODYNAMICS, Bate, Mueller, White 
   published by Dover for a description of geodetic coordinates. 
 
-Author_and_Institution

   C.H. Acton      (JPL)
   N.J. Bachman    (JPL)
   H.A. Neilan     (JPL) 
   W.L. Taber      (JPL) 
 
-Version
 
   -CSPICE Version 1.2.2, 02-JUL-2007 (NJB)

      In Examples section of header, heading and description of
      right-hand table was updated to use correct names of columns.
      Term "bodyfixed" is now hyphenated.

   -CSPICE Version 1.2.1, 30-JUL-2003 (NJB) (CHA)

      Various header changes were made to improve clarity.  Some
      minor header corrections were made.

   -CSPICE Version 1.2.0, 28-AUG-2001 (NJB)
     
      Removed tab characters from source file.  Include interface
      macro definition file SpiceZim.h. 

   -CSPICE Version 1.1.0, 21-OCT-1998 (NJB)

      Made input vector const.

   -CSPICE Version 1.0.0, 08-FEB-1998   (EDW)

-Index_Entries
 
   rectangular to geodetic 
 
-&
*/

{ /* Begin recgeo_c */

   /*
   Participate in error handling
   */

   chkin_c ( "recgeo_c");


   /*
   Call the f2c'd routine.
   */

   recgeo_( ( doublereal * ) rectan,
            ( doublereal * ) &re,
            ( doublereal * ) &f,
            ( doublereal * ) lon,
            ( doublereal * ) lat,
            ( doublereal * ) alt);


   chkout_c ( "recgeo_c");


} /* End recgeo_c */
