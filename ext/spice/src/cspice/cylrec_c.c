/*

-Procedure cylrec_c ( Cylindrical to rectangular )

-Abstract

   Convert from cylindrical to rectangular coordinates.

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

   #include  <math.h>
   #include "SpiceUsr.h"

   void cylrec_c ( SpiceDouble r,
                   SpiceDouble lon,
                   SpiceDouble z,
                   SpiceDouble rectan[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  -------------------------------------------------
   r          I   Distance of a point from z axis.
   lon        I   Angle (radians) of a point from xZ plane
   z          I   Height of a point above xY plane.
   rectan     O   Rectangular coordinates of the point.

-Detailed_Input

   r          Distance of the point of interest from z axis.

   lon        Cylindrical angle (in radians) of the point of
              interest from XZ plane.

   z          Height of the point above XY plane.

-Detailed_Output

   rectan     Rectangular coordinates of the point of interest.

-Parameters

   None.

-Particulars

   This routine transforms the coordinates of a point from
   cylindrical to rectangular coordinates.

-Examples

   Below are two tables.

   Listed in the first table (under r, lon and z ) are
   cylindrical coordinate triples that approximately represent
   points whose rectangular coordinates are taken from the set
   {-1, 0, 1}.  (Angular quantities are given in degrees.)

   The result of the code fragment

        Use the CSPICE routine convrt_c to convert the angular
        quantities to radians

        convrt_c (  lon, "DEGREES", "RADIANS", lon );

        cylrec_c ( r, lon, z, x );


   are listed in the second parallel table under x(1), x(2) and x(3).


     r         lon     z            x(1)       x(2)     x(3)
     -------------------------       --------------------------
     0.0000    0.0000   0.0000       0.0000     0.0000   0.0000
     1.0000    0.0000   0.0000       1.0000     0.0000   0.0000
     1.0000   90.0000   0.0000       0.0000     1.0000   0.0000
     0.0000    0.0000   1.0000       0.0000     0.0000   1.0000
     1.0000  180.0000   0.0000      -1.0000     0.0000   0.0000
     1.0000  270.0000   0.0000       0.0000    -1.0000   0.0000
     0.0000    0.0000  -1.0000       0.0000     0.0000  -1.0000
     1.4142   45.0000   0.0000       1.0000     1.0000   0.0000
     1.0000    0.0000   1.0000       1.0000     0.0000   1.0000
     1.0000   90.0000   1.0000       0.0000     1.0000   1.0000
     1.4142   45.0000   1.0000       1.0000     1.0000   1.0000


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

       Corrected and clarified header entries.  Removed return call.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   cylindrical to rectangular

-&
*/

{ /* Begin cylrec_c */

   /*
   Local variables
   */

   SpiceDouble    x;
   SpiceDouble    y;


   /* Function Body */

   x =  r * cos(  lon );
   y =  r * sin(  lon );


   /*  Move the results to the output variables. */

   rectan[0] =  x;
   rectan[1] =  y;
   rectan[2] =  z;


} /* End cylrec_c */
