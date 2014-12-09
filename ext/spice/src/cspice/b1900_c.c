/*

-Procedure b1900_c ( Besselian Date 1900.0 )

-Abstract

   Return the Julian Date corresponding to Besselian Date 1900.0.

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

   SpiceDouble b1900_c ( void )

/*

-Brief_I/O

   The function returns the Julian Date corresponding to Besselian
   date 1900.0.

-Detailed_Input

   None.

-Detailed_Output

   The function returns 2415020.31352, the Julian Date corresponding
   to Besselian Date 1900.0 as reported by Lieske [1].

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   Lieske [1] defines a mapping from Julian Ephemeris Date 
   to Besselian:

      BE = 1900. + (JED - 2415020.31352)/365.242198781

   The inverse mapping being:

      JED = (BE - 1900.)*365.242198781 + 2415020.31352

-Examples

   The following code fragment illustrates the use of b1900_c.

      /.
      Convert Julian Date to UTC seconds past the reference
      epoch (B1900).
      ./

      spref = ( jd - b1900_c() ) * spd_c();

-Restrictions

   None.

-Literature_References

   [1] Jay Lieske, ``Precession Matrix Based on IAU (1976)
       System of Astronomical Constants,'' Astron. Astrophys.
       73, 282-284 (1979).

-Author_and_Institution

   W.L. Taber      (JPL)
   I.M. Underwood  (JPL)

-Version

   -CSPICE Version 1.1.0, 01-SEP-2005 (EDW)

       Added journal reference and associated citations.

   -CSPICE Version 1.0.1, 08-FEB-1998 (EDW)

       Corrected and clarified header entries.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   besselian date 1900.0

-&
*/

{ /* Begin b1900_c */

  return 2415020.31352;

} /* End b1900_c */
