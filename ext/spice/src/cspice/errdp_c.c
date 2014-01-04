/*

-Procedure errdp_c  ( Insert D.P. Number into Error Message Text )

-Abstract

   Substitute a double precision number for the first occurrence of
   a marker found in the current long error message.

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

   ERROR

-Keywords

   ERROR, CONVERSION

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void errdp_c ( ConstSpiceChar  * marker,
                  SpiceDouble       number  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   marker     I   A substring of the error message to be replaced.
   number     I   The d.p. number to substitute for marker.

-Detailed_Input


   marker     is a character string which marks a position in
              the long error message where a character string
              representing an double precision number is to be
              substituted.  Leading and trailing blanks in marker
              are not significant.

              Case IS significant;  "XX" is considered to be
              a different marker from "xx".

   number     is an double precision number whose character
              representation will be substituted for the first
              occurrence of marker in the long error message.
              This occurrence of the substring indicated by marker
              will be removed, and replaced by a character string,
              with no leading or trailing blanks, representing
              number.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1) The error SPICE(EMPTYSTRING) is signalled if the input
      string does not contain at least one character, since the
      input string cannot be converted to a Fortran-style string
      in this case.

   2) The error SPICE(NULLPOINTER) is signalled if the input string
      pointer is null.

-Files

   None.

-Particulars

   The effect of this routine is to update the current long
   error message.  If no marker is found, (e.g., in the
   case that the long error message is blank), the routine
   has no effect.  If multiple instances of the marker
   designated by marker are found, only the first one is
   replaced.

   If the character string resulting from the substitution
   exceeds the maximum length of the long error message, the
   characters on the right are lost.  No error is signalled.

   This routine has no effect if changes to the long message
   are not allowed.

-Examples


    1.   In this example, the marker is:   #


         The current long error message is:

            "Invalid operation value.  The value was #".


         After the call,

            errdp_c ( "#",  5.0  );

         The long error message becomes:

         "Invalid operation value.  The value was 5.0".




    2.   In this example, the marker is:   XX


         The current long error message is:

            "Left endpoint exceeded right endpoint.  The left"//
            "endpoint was:  XX.  The right endpoint was:  XX."


         After the call,

            errdp_c ( "XX",  5.0  );

         The long error message becomes:

            "Left endpoint exceeded right endpoint.  The left"//
            "endpoint was:  5.0.  The right endpoint was:  XX."


-Restrictions

   The caller must ensure that the message length, after sub-
   stitution is performed, doesn't exceed LMSGLN characters.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman    (JPL)

-Version

   -CSPICE Version 1.2.0, 08-FEB-1998 (NJB)

      Re-implemented routine without dynamically allocated, temporary
      strings.  Made various header fixes.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   insert d.p. number into error message text

-&
*/

{
   /*
   Check the input string marker to make sure the pointer is non-null
   and the string length is non-zero.  Since we don't check in
   prior to this, use the discovery check-in option.
   */
   CHKFSTR ( CHK_DISCOVER, "errdp_c", marker );


   /*
   Call the f2c'd Fortran routine.
   */
   errdp_ ( ( char       * ) marker,
            ( doublereal * ) &number,
            ( ftnlen       ) strlen(marker)  );


} /* end errdp_c */
