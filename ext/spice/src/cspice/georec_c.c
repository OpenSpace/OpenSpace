/*

-Procedure georec_c ( Geodetic to rectangular coordinates )

-Abstract

   Convert geodetic coordinates to rectangular coordinates.

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
   #include "SpiceZst.h"

   void georec_c ( SpiceDouble lon,
                   SpiceDouble lat,
                   SpiceDouble alt,
                   SpiceDouble re,
                   SpiceDouble f,
                   SpiceDouble rectan[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   lon        I   Geodetic longitude of point (radians).
   lat        I   Geodetic latitude  of point (radians).
   alt        I   Altitude of point above the reference spheroid.
   re         I   Equatorial radius of the reference spheroid.
   f          I   Flattening coefficient.
   rectan     O   Rectangular coordinates of point.

-Detailed_Input

   lon        Geodetic longitude of the input point.  This is the 
              angle between the prime meridian and the meridian
              containing `rectan'.  The direction of increasing
              longitude is from the +X axis towards the +Y axis.
 
              Longitude is measured in radians.  On input, the 
              range of longitude is unrestricted. 


   lat        Geodetic latitude of the input point.  For a point P on
              the reference spheroid, this is the angle between the XY
              plane and the outward normal vector at P. For a point P
              not on the reference spheroid, the geodetic latitude is
              that of the closest point to P on the spheroid.

              Latitude is measured in radians.  On input, the 
              range of latitude is unrestricted.


   alt        Altitude of point above the reference spheroid.


   re         Equatorial radius of a reference spheroid.  This spheroid
              is a volume of revolution:  its horizontal cross sections
              are circular.  The shape of the spheroid is defined by
              an equatorial radius `re' and a polar radius `rp'.


   f          Flattening coefficient = (re-rp) / re,  where `rp' is
              the polar radius of the spheroid.

-Detailed_Output

   rectan     Rectangular coordinates of the input point. 

              The units associated with `rectan' are those associated
              with the input `alt'.

-Parameters

   None.

-Exceptions

   1) If the equatorial radius is less than or equal to zero,
      the error SPICE(VALUEOUTOFRANGE) is signaled.

   2) If the flattening coefficient is greater than or equal to
      one, the error SPICE(VALUEOUTOFRANGE) is signaled.

-Files

   None.

-Particulars

   Given the geodetic coordinates of a point, and the constants
   describing the reference spheroid,  this routine returns the
   bodyfixed rectangular coordinates of the point.  The bodyfixed
   rectangular frame is that having the x-axis pass through the 0
   degree latitude 0 degree longitude point.  The y-axis passes through
   the 0 degree latitude 90 degree longitude.  The z-axis passes
   through the 90 degree latitude point.  For some bodies this
   coordinate system may not be a right-handed coordinate system.

-Examples

   This routine can be used to convert body fixed geodetic
   coordinates (such as the used for United States Geological
   Survey topographic maps) to bodyfixed rectangular coordinates
   such as the Satellite Tracking and Data Network of 1973.

   The code would look something like this

      /.
      Using the equatorial radius of the Clark66 spheroid
      (CLARKR = 6378.2064 km) and the Clark 66 flattening
      factor (CLARKF = 1.0 / 294.9787 ) convert to
      body fixed rectangular coordinates.
      ./

      georec_c ( lon, lat, alt, CLARKR, CLARKF, x );

      /.
      Add the North American Datum of 1927 to STDN 73 center
      offset
      ./

      vadd_c  ( x, offset, stdnx );


   Below are two tables.

   Listed in the first table (under lon, lat, and alt ) are
   geodetic coordinate triples that approximately represent points
   whose rectangular coordinates are taken from the set {-1, 0, 1}.
   (Angular quantities are given in degrees.)

   The results of the code fragment

      /.
      Convert the angular quantities to degrees
      ./
      lat = lat * rpd_c();
      lon = lon * rpd_c();

      georec_c ( lon, lat, alt, CLARKR, CLARKF, x );


   are listed in the second parallel table under x[0], x[1] and x[2].


     lon       lat        alt            x[0]       x[1]     x[2]
     ------------------------------      --------------------------
     0.0000    90.0000   -6356.5838      0.0000     0.0000   0.0000
     0.0000     0.0000   -6377.2063      1.0000     0.0000   0.0000
    90.0000     0.0000   -6377.2063      0.0000     1.0000   0.0000
     0.0000    90.0000   -6355.5838      0.0000     0.0000   1.0000
   180.0000     0.0000   -6377.2063     -1.0000     0.0000   0.0000
   -90.0000     0.0000   -6377.2063      0.0000    -1.0000   0.0000
     0.0000   -90.0000   -6355.5838      0.0000     0.0000  -1.0000
    45.0000     0.0000   -6376.7921      1.0000     1.0000   0.0000
     0.0000    88.7070   -6355.5725      1.0000     0.0000   1.0000
    90.0000    88.7070   -6355.5725      0.0000     1.0000   1.0000
    45.0000    88.1713   -6355.5612      1.0000     1.0000   1.0000


-Restrictions

   None.

-Author_and_Institution

   C.H. Acton      (JPL)
   N.J. Bachman    (JPL)
   H.A. Neilan     (JPL)
   W.L. Taber      (JPL)
   E.D. Wright     (JPL)

-Literature_References

   See FUNDAMENTALS OF ASTRODYNAMICS, Bate, Mueller, White
   published by Dover for a description of geodetic coordinates.

-Version

   -CSPICE Version 1.0.2, 30-JUL-2003 (NJB)

      Various header corrections were made.

   -CSPICE Version 1.0.1, 11-JAN-2003   (EDW)

       Removed a spurious non-printing character.

   -CSPICE Version 1.0.0, 08-FEB-1998   (EDW)

-Index_Entries

   geodetic to rectangular coordinates

-&
*/

{ /* Begin georec_c */

   /*
   Participate in error handling
   */

   chkin_c ( "georec_c");


   /*
   Call the f2c'd routine.
   */

   georec_( ( doublereal * ) &lon,
            ( doublereal * ) &lat,
            ( doublereal * ) &alt,
            ( doublereal * ) &re,
            ( doublereal * ) &f,
            ( doublereal * ) rectan );


   chkout_c ( "georec_c");


} /* End georec_c */
