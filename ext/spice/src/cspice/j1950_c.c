/*

-Procedure j1950_c ( Julian Date of 1950.0 JAN 1.0 )

-Abstract

   Return the Julian Date of 1950 JAN 01 00:00:00 (1950 JAN 1.0).

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

   CONSTANTS

*/

   #include "SpiceUsr.h"

   SpiceDouble j1950_c ( void )

/*

-Brief_I/O

   The function returns the Julian Date of 1950 JAN 01 00:00:00
   (1950 JAN 1.0).

-Detailed_Input

   None.

-Detailed_Output

   The function returns 2433282.5, the Julian Date corresponding
   to 1950 JAN 01 00:00:00 (1950 JAN 1.0).

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   The function always returns the constant value shown above.

-Examples

   The following code fragment illustrates the use of j1950_c.

            /.
            Convert Julian Date to UTC seconds past the reference
            epoch (j1950_c).
            ./

            spref = ( jd - j1950_c() ) * spd_c()

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   E.D. Wright     (JPL)
   W.L. Taber      (JPL)
   I.M. Underwood  (JPL)

-Version

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries

   julian date of 1950.0 jan 1.0

-&
*/

{ /* Begin j1950_c */

   return 2433282.5;

} /* End j1950_c */
