/*

-Procedure dp2hx_c ( D.p. number to hexadecimal string )

-Abstract

   Convert a double precision number to an equivalent character
   string using base 16 ``scientific notation.''

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

   ALPHANUMERIC
   CONVERSION

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void dp2hx_c ( SpiceDouble   number,
                  SpiceInt      lenout,
                  SpiceChar   * string,
                  SpiceInt    * length
                 )

/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   number     I   D.p. number to be converted.
   lenout     I   Available space for output string 'string'.
   string     O   Equivalent character string, left justified.
   length     O   Length of the character string produced.

-Detailed_Input

   number   The double precision number to be converted to a
            character string representation.

   lenout   is the maximum length of the output 'string'. The value
            defined by lenout should be one plus the value large 
            enough to hold any possible output.

-Detailed_Output

   string   The character string produced by this routine that
            represents 'number' in base 16 ``scientific notation,''
            e.g.:

               672.0 = '2A^3' = ( 2/16 + 10/( 16**2 ) ) * 16**3

            and

               -11.0 = '-B^1' = - ( 11/16 ) * 16**1.

            The following table describes the character set used to
            represent the hexadecimal digits and their corresponding
            values.

                 Character    Value         Character    Value
                 ---------    ------        ---------    ------
                   '0'         0.0D0          '8'         8.0D0
                   '1'         1.0D0          '9'         9.0D0
                   '2'         2.0D0          'A'        10.0D0
                   '3'         3.0D0          'B'        11.0D0
                   '4'         4.0D0          'C'        12.0D0
                   '5'         5.0D0          'D'        13.0D0
                   '6'         6.0D0          'E'        14.0D0
                   '7'         7.0D0          'F'        15.0D0

            The caret, or hat, character, '^', is used to distinguish 
            the exponent.

            The plus sign, '+', and the minus sign, '-' have the expected 
            meanings.

            In order to obtain the entire character string produced
            by this routine, the output character string should be
            at least N characters long, where


                      # of bits per double precision mantissa + 3
            N = 3 + ----------------------------------------------
                                          4

                      # of bits per double precision exponent + 3
                  + ---------------------------------------------- .
                                          4

            There should be one character position for the sign of
            the mantissa, one for the sign of the exponent, one for
            the exponentiation character, and one for each
            hexadecimal digit that could be produced from a mantissa
            and an exponent.

            The following table contains minimum output string
            lengths necessary to obtain the complete character
            string produced by this routine for some typical
            implementations of double precision numbers.

            Double precision number
            Size Mantissa Exponent    Minimum output string length
            bits   bits     bits
            ---- -------- --------    ----------------------------
            64   48       15          3 + 12 + 4 = 19
            64   55+1     8           3 + 14 + 2 = 19 (VAX)
            64   52       11          3 + 13 + 3 = 19 (IEEE)

            The base 16 ``scientific notation'' character string
            produced by this routine will be left justified and
            consist of a contiguous sequence of characters with one
            of the following formats:

                (1)   h h h h  ... h ^H H  ... H
                       1 2 3 4      n  1 2      m

                (2)   -h h h h  ... h ^H H  ... H
                        1 2 3 4      n  1 2      m

                (3)   h h h h  ... h ^-H H  ... H
                       1 2 3 4      n   1 2      m

                (4)   -h h h h  ... h ^-H H  ... H
                        1 2 3 4      n   1 2      m

            where

               h   and  H   denote hexadecimal digits
                i        j

               '^'          denotes exponentiation ( base 16 )

            and

               '+' and '-'  have their usual interpretations.

   length   the length of the base 16 ``scientific notation'' character
            'string' returned by this routine.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

     This routine converts a double precision number into an equivalent
     character string using a base 16 ``scientific notation.'' This
     representation allows the full precision of a number to be placed
     in a format that is suitable for porting or archival storage.

     This routine is one of a pair of routines which are used to
     perform conversions between double precision numbers and
     an equivalent base 16 ``scientific notation'' character string
     representation:

        dp2hx_c -- Convert a double precision number into a base 16
                   ``scientific notation'' character string.

        hx2dp_c -- Convert a base 16 ``scientific notation''
                   character string into a double precision number.

-Examples

   The following input and output argument values illustrate the
   action of dp2hx_c for various input values of 'number'.

   Note: The hat or caret, '^', signals an exponent.

      number             string                         length
      -----------------  -----------------------------  ------
           2.0D-9         89705F4136B4A6^-7             17
           1.0D0          1^1                           3
          -1.0D0         -1^1                           4
        1024.0D0          4^3                           3
       -1024.0D0         -4^3                           4
      521707.0D0          7F5EB^5                       7
          27.0D0          1B^2                          4
           0.0D0          0^0                           3

-Restrictions

   The maximum number of characters permitted in the output string
   is specified by the variable 'lenout'.

-Literature_References

   None.

-Author_and_Institution

   K.R. Gehringer   (JPL)

-Version

   CSPICE Version 1.0.0, 10-APR-2010 (EDW)

-Index_Entries

   convert d.p. to signed normalized hexadecimal string
   convert d.p. number to encoded d.p. number
   convert d.p. to base 16 scientific notation

-&
*/

{ /* Begin dp2hx_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dp2hx_c" );

   /*
   Make sure the output string has at least enough room for one
   output character and a null terminator.  Also check for a null
   pointer.
   */

   CHKOSTR ( CHK_STANDARD, "dp2hx_c", string, lenout );

   (void) dp2hx_( ( doublereal  * ) &number,
                  ( char        * ) string,
                  ( integer     * ) length,
                  ( ftnlen        ) lenout -1 );


   /*
   Convert the output string from Fortran to C style.
   */
   F2C_ConvertStr( lenout, string );

   chkout_c ( "dp2hx_c" );

} /* End dp2hx_c */
