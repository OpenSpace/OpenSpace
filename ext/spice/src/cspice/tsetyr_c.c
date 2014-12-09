/*

-Procedure tsetyr_c ( Time --- set year expansion boundaries )

-Abstract

  Set the lower bound on the 100 year range

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

   TIME

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void tsetyr_c ( SpiceInt year )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   year       I   Lower bound on the 100 year interval of expansion

-Detailed_Input

   year       is the year associated with the lower bound on all
              year expansions computed by texpyr_.  For example
              if year is 1980, then the range of years that
              can be abbreviated is from 1980 to 2079.

-Detailed_Output

   None.

-Parameters

   None.

-Files

   None.

-Exceptions

   Error free.

   1) If year is less than 1 no action is taken

-Particulars

   This routine allows all of the SPICE time subsystem to handle
   uniformly the expansion of "abbreviated" years.  (i.e. the
   remainder after dividing the actual year by 100).  The input
   supplied to this routine represents the lower bound of the
   expansion interval.  The upper bound of the expansion interval
   is year + 99.

   The default expansion interval is from 1969 to 2068.

   The default behavior is as follows

   year input      year Output
   ----------      -----------
   00              2000
   01              2001
    .                .
    .                .
    .                .
   67              2067
   68              2068
   69              1969
   70              1970
    .                .
    .                .
    .                .
   99              1999

-Examples

   Suppose that you need to manipulate time strings and that
   you want to treat years components in the range from 0 to 99
   as being abbreviations for years in the range from
   1980 to 2079 (provided that the years are not modified by
   an ERA substring).  The code fragment below shows how you
   could go about this.

      Early in your application set up the lower bound for the
      expansion of abbreviated years.

      tsetyr_c ( 1980 );

   year input      year Output
   ----------      -----------
   00              2000
   01              2001
    .                .
    .                .
    .                .
   48              2048
   49              2049
    .                .
    .                .
    .                .
   79              2079
   80              1980
    .                .
   99              1999



-Restrictions

   None.

-Author_and_Institution

   W.L. Taber      (JPL)
   E.D. Wright     (JPL)

-Literature_References

   None.

-Version

   -CSPICE Version 1.0.0, 11-FEB-1998   (EDW)

-Index_Entries

   Set the interval of expansion for abbreviated years

-&
*/

{ /* Begin tsetyr_c */


   /* Make the call to the f2c'd routine.  Not much else. */

   tsetyr_ ( &year );


} /* End tsetyr_c */
