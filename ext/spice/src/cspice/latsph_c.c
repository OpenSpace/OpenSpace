/*

-Procedure latsph_c ( Latitudinal to spherical coordinates )

-Abstract

   Convert from latitudinal coordinates to spherical coordinates.

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


   void latsph_c ( SpiceDouble    radius,
                   SpiceDouble    lon,
                   SpiceDouble    lat,
                   SpiceDouble *  rho,
                   SpiceDouble *  colat,
                   SpiceDouble *  lons )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   radius     I   Distance of a point from the origin.
   lon        I   Angle of the point from the XZ plane in radians.
   lat        I   Angle of the point from the XY plane in radians.
   rho        O   Distance of the point from the origin.
   colat      O   Angle of the point from positive z axis (radians).
   lons       O   Angle of the point from the XZ plane (radians).

-Detailed_Input

   radius     Distance of a point from the origin.

   lon        Angle of the point from the XZ plane in radians.

   lat        Angle of the point from the XY plane in radians.

-Detailed_Output

   rho        Distance of the point from the origin.

   colat      Angle between the vector from the origin to the point
              and the positive z axis in radians.

   lons       Angle of the point from the XZ plane (radians).

-Parameters

   None.

-Particulars

   This routine returns the spherical coordinates of a point
   whose position is input in latitudinal coordinates.

   Latitudinal coordinates are defined by a distance from a central
   reference point, an angle from a reference meridian, and an angle
   above the equator of a sphere centered at the central reference
   point.

   Spherical coordinates are defined by a distance from a central
   reference point, an angle from a reference meridian, and an angle
   from the z-axis.

-Examples

   Co-latitude is obtained by subtracting latitude from HALFPI()
   Radius and longitude mean the same thing in both latitudinal
   and spherical coordinates.  The table below lists lat
   corresponding lat in terms of degrees.

           lat            lat
          ------         ------
            0             90
           20             70
           45             45
          -30            120
           90              0
          -45            135

-Restrictions

   None.

-Exceptions

   Error free.

-Files

   None.

-Author_and_Institution

   W.L. Taber      (JPL)

-Literature_References

   None.

-Version

   -CSPICE Version 1.0.1, 13-DEC-2005   (EDW)

      Corrected typo in Deatiled_Output, substituted
      "colat" for "lat."

   -CSPICE Version 1.0.0, 08-FEB-1998   (EDW)

-Index_Entries

   latitudinal to spherical coordinates

-&
*/

{ /* Begin latsph_c */

   /*
   Local variables
   */

   SpiceDouble    th;
   SpiceDouble    ph;


   /*
   Convert to spherical coordinates, storing the results in
   temporary variables
   */

   th = halfpi_c() - lat;
   ph = lon;


   /* Move results to output variables */

   *rho   = radius;
   *colat = th;
   *lons  = ph;


} /* End latsph_c */
