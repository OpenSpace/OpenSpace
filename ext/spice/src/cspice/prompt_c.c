/*

-Procedure prompt_c ( Prompt a user for a string )

-Abstract

   This function prompts a user for keyboard input.

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

   UTILITY

*/

   #include <stdlib.h>
   #include <stdio.h>

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"

   SpiceChar * prompt_c ( ConstSpiceChar * prmptStr,
                          SpiceInt         lenout,
                          SpiceChar      * buffer )

/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   prmptStr   I   The prompt string to display when asking for input.
   lenout     I   Minimum number of characters for response plus one.
   buffer     O   The string containing the response typed by a user.

   The routine also returns a pointer to the output buffer.

-Detailed_Input

   prmptStr   A character string displayed from the current cursor
              position which describes the requested input.  The prompt
              string should be relatively short, i.e., 50 or fewer
              characters, so a response may be typed on the line where
              the prompt appears.

              All characters (including trailing blanks) in prmptStr
              are considered significant and will be displayed.

   lenout     The integer number of characters plus one for the
              response string.

-Detailed_Output

   buffer     The user supplied string which holds the response.  The
              string's memory is allocated in the calling routine.

   The routine returns a pointer to buffer as well as passing the
   pointer back via an argument.

-Parameters

   None.

-Exceptions

   1) If the output string has length less than two characters, it
      is too short to contain one character of output data plus a null
      terminator, so it cannot be passed to the underlying Fortran
      routine.  In this event, the error SPICE(STRINGTOOSHORT) is
      signaled and a null pointer is returned.

-Files

   None.

-Particulars

   This is a utility that allows you to "easily" request information
   from a program user.  The calling program declares an array or
   allocate memory to contain the user's response to the prompt.

-Examples

   Suppose you have an interactive program that computes state
   vectors by calling spkezr_c.  The program prompts the user for
   the inputs to spkezr_c.  After each prompt is written, the program
   leaves the cursor at the end of the string as shown here:

      Enter UTC epoch  > _

   (The underscore indicates the cursor position).

   The following program illustrates the aquisition of input
   values using prompt_c:

   #include <stdlib.h>
   #include <stdio.h>

   #include "SpiceUsr.h"

   #define   STRLEN    32

   void main()
      {
      SpiceChar    utc    [STRLEN];
      SpiceChar    obs    [STRLEN];
      SpiceChar    targ   [STRLEN];
      SpiceChar  * utc1;
      SpiceChar  * obs1;
      SpiceChar  * targ1;


      /. Call the routine as a subroutine. ./

      prompt_c ( "Enter UTC epoch             > ", STRLEN, utc  );
      prompt_c ( "Enter observer name         > ", STRLEN, obs  );
      prompt_c ( "Enter target name           > ", STRLEN, targ );


      /. Or call the routine as a function. ./

      utc1   = ( SpiceChar * ) malloc (STRLEN);
      obs1   = ( SpiceChar * ) malloc (STRLEN);
      targ1  = ( SpiceChar * ) malloc (STRLEN);


      utc1 = prompt_c ( "Enter UTC epoch        > ", STRLEN, utc1 );
      obs1 = prompt_c ( "Enter observer name    > ", STRLEN, obs1 );
      targ1= prompt_c ( "Enter target name      > ", STRLEN, targ1);


      /.
      Now do stuff with your strings.
      ./

        ...

      }

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman    (JPL)
   K.R. Gehringer  (JPL)
   W.L. Taber      (JPL)
   E.D. Wright     (JPL)

-Version

   -CSPICE Version 1.0.0, 25-JUN-1999 (EDW) (NJB)

-Index_Entries

   Prompt for keyboard input
   Prompt for input with a user supplied message

-&
*/

{ /* Begin prompt_c */

   /*
   Local variables
   */
   SpiceChar               c;
   SpiceInt                i;


   /*
   Participate in error tracing.
   */
   chkin_c ( "prompt_c" );


   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR_VAL ( CHK_STANDARD, "prompt_c", buffer, lenout, NULLCPTR );


   /*
   Initialize i to zero.
   */
   i = 0;


   /*
   Display the prompt string.
   */
   printf ( "%s", prmptStr );


   /*
   Get input from stdin, check for an end of line terminator.
   The loop continues until the terminator is found.
   */

   c = getchar();

   while ( ( c !=  (char)'\n') )
      {

      /*
      We have room for lenout characters, the last of which will
      be a null terminator.  Slurp only (lenout - 1) characters
      from the input into buffer.  Ignore anything afterwards.
      */
      if ( i < (lenout - 1 ) )
         {

         /*
         Read in no more than lenout - 1 chracters.
         */
         buffer[i] = c;
         i++;

         }

         /*
         Get the next character from the input line.
         */
         c  = getchar();

      }


   /*
   Null terminate the current buffer.  The counter i points to the
   first free location in the buffer.
   */
   buffer[i] = NULLCHAR;


   /*
   Done.  Checkout.
   */
   chkout_c ( "prompt_c");


   /*
   Return the buffer so the user may elect to use the function call
   capability.
   */

   return buffer;


} /* End prompt_c */

