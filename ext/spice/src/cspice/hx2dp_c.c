/*

-Procedure hx2dp_c ( Hexadecimal string to d.p. number )

-Abstract
 
   Convert a string representing a double precision number in a
   base 16 ``scientific notation'' into its equivalent double
   precision number.
 
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
   
   void hx2dp_c ( ConstSpiceChar  * string,
                  SpiceInt          lenout,
                  SpiceDouble     * number,
                  SpiceBoolean    * error,
                  SpiceChar       * errmsg  ) 

/*

-Brief_I/O 

   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   string     I   Hex form string to convert to double precision.
   lenout     I   Available space for output string 'errmsg'.
   number     O   Double precision value to be returned.
   error      O   A logical flag which is true on error.
   errmsg     O   A descriptive error message.

-Detailed_Input
 
   string   a character string containing a base 16 ``scientific
            notation'' representation of a double precision number
            which is to be converted to a double precision number,
            e.g.:

               '2A^3' = ( 2/16 + 10/( 16**2 ) ) * 16**3 = 672.0

            and

               '-B^1' = - ( 11/16 ) * 16**1             = -11.0

            The following table describes the character set used to
            represent the hexadecimal digits and their corresponding
            values.

            Character     Value         Character     Value
            ---------    -------        ---------    -------
              '0'         0.0D0           '8'         8.0D0
              '1'         1.0D0           '9'         9.0D0
              '2'         2.0D0         'A','a'      10.0D0
              '3'         3.0D0         'B','b'      11.0D0
              '4'         4.0D0         'C','c'      12.0D0
              '5'         5.0D0         'D','d'      13.0D0
              '6'         6.0D0         'E','e'      14.0D0
              '7'         7.0D0         'F','f'      15.0D0

            The caret, or hat, character, '^', is used to
            distinguish the exponent.

            The plus sign, '+', and the minus sign, '-', are used,
            and they have their usual meanings.

            A base 16 ``scientific notation'' character string which
            is to be parsed by this routine should consist of a sign,
            '+' or '-' (the plus sign is optional for nonnegative
            numbers), followed immediately by a contiguous sequence
            of hexadecimal digits, the exponent character, and a
            signed hexadecimal exponent. The exponent is required,
            but the sign is optional for a nonnegative exponent.

            A number in base 16 ``scientific notation'' consists of
            a contiguous sequence of characters with one of the
            following formats:

                (1)   h h h h  ... h ^H H  ... H
                       1 2 3 4      n  1 2      m

                (2)   +h h h h  ... h ^H H  ... H
                        1 2 3 4      n  1 2      m

                (3)   -h h h h  ... h ^H H  ... H
                        1 2 3 4      n  1 2      m

                (4)    h h h h  ... h ^+H H  ... H
                        1 2 3 4      n   1 2      m

                (5)   +h h h h  ... h ^+H H  ... H
                        1 2 3 4      n   1 2      m

                (6)   -h h h h  ... h ^+H H  ... H
                        1 2 3 4      n   1 2      m

                (7)   h h h h  ... h ^-H H  ... H
                       1 2 3 4      n   1 2      m

                (8)   +h h h h  ... h ^-H H  ... H
                        1 2 3 4      n   1 2      m

                (9)   -h h h h  ... h ^-H H  ... H
                        1 2 3 4      n   1 2      m

            where

               h  and H  denote hexadecimal digits;
                i      j

               '^'         denotes exponentiation;

            and

               + and - have their usual interpretations.

            'string' may have leading and trailing blanks, but blanks
            embedded within the significant portion of the input
            string are not allowed.

   lenout   the maximum length of the output 'errmsg'. The value
            defined by lenout should be one plus the value large 
            enough to hold any possible output.

-Detailed_Output

   number   the double precision value to be returned. The value of
            this argument is not changed if an error occurs while
            parsing the input string.

   error    a logical flag which indicates whether an error occurred
            while attempting to parse 'number' from the input
            character string 'string'. 'error' will have the value
            true if an error occurs. It will have the value
            false otherwise.

   errmsg   contains a descriptive error message if an error
            occurs while attempting to parse the number 'number'
            from the hexadecimal character string 'string', blank
            otherwise.

-Parameters
 
   None. 
   
-Exceptions

   None. 
 
-Files
 
   None. 
 
-Particulars

   This routine will convert a character string containing a number
   in base 16 ``scientific notation'' into its equivalent double
   precision number.

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
   action of hx2dp_c for various input values of 'string'. 

   Note: The hat or caret, '^', signals an exponent.

      string                  number         error   errmsg
      ----------------------  -------------  ------  ------
       89705F4136B4A6^-7            2.0D-9   false   " "
       1^1                          1.0D0    false   " "
      -1^1                         -1.0D0    false   " "
       4^3                       1024.0D0    false   " "
      -4^3                      -1024.0D0    false   " "
       7F5EB^5                 521707.0D0    false   " "
       7F5eb^5                 521707.0D0    false   " "
       7f5eb^5                 521707.0D0    false   " "
       1B^2                        27.0D0    false   " "
      +1B^2                        27.0D0    false   " "
      +1B^+2                       27.0D0    false   " "
       0^0                          0.0D0    false   " "

-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution

   K.R. Gehringer   (JPL)

-Version

   CSPICE Version 1.0.0, 10-APR-2010 (EDW)

-Index_Entries

   convert signed normalized hexadecimal string to d.p.
   convert encoded d.p. number to d.p. number
   convert base 16 scientific notation d.p. number
 
-&
*/

{ /* Begin hx2dp_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "hx2dp_c" );

   /*
   Check the input time string to make sure the pointer is non-null and
   the string length is non-zero.
   */
   CHKFSTR ( CHK_DISCOVER, "hx2dp_c", string );

   /*
   Check the output error message string to make sure the pointer is 
   non-null and the string length is at least 2.
   */
   CHKOSTR ( CHK_DISCOVER, "hx2dp_c", errmsg, lenout );

   /*
   Call the f2c'd routine.
   */ 

   (void) hx2dp_ ( ( char        * ) string,
                   ( doublereal  * ) number,
                   ( logical     * ) error,
                   ( char        * ) errmsg,
                   ( ftnlen        ) strlen(string),
                   ( ftnlen        ) lenout - 1);

   /*
   Convert the error message from Fortran to C style.
   */
   F2C_ConvertStr ( lenout, errmsg );

   chkout_c ( "hx2dp_c" );
   
} /* End hx2dp_c */

