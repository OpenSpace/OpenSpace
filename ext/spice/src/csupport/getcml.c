/*

-Procedure getcml_ ( Get the command line as a string )

-Abstract

   Get the command line arguments and return them in a single string.

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

   #include <string.h>
   #include <stdlib.h>

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   SpiceInt getcml_ ( char    * outline,
                      ftnlen    line_len )

/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   outline    O   The command line arguments string.
   line_len   I   Length for the output string.

-Detailed_Input

   line_len   is the length for the output string.

-Detailed_Output

   outline    is a one-dimensional character array containing the 
              command line arguments.  The command (pointed to by 
              argv[0] in a traditional C program) is not part of the 
              output.
              
              The output array contains a "Fortran style" string:  it's
              padded with trailing blanks and does not contain a null 
              terminator.  

-Parameters

   None.

-Exceptions

   1) The output string is checked to make sure the pointer is non-null.
      If the pointer is null, the error SPICE(NULLPOINTER) is signalled.
      
   2) The output string is checked to make sure the string length is 
      at least 2.  If not, the error SPICE(STRINGTOOSHORT) is signalled.

-Files

   None.

-Particulars

   This routine is for the f2c'd C code that requires a call to getcml_.
   The routine may be called from any program module.  It is necessary
   to call putcml_c from the main module to initialize the storage
   of argv and argc.

   The routine copies the entire command line to a single string, then
   returns line_len of the string to the caller.  If line_len is greater
   than the length of the command string, the complete string is
   returned.  If less than, the command string is truncted to line_len
   characters.

-Examples

   Given the following command line

     % inputs this is the command line input

   getcml_ will return the string:

     this is the command line input


   Example:


   #include "SpiceUsr.h"
   #include "SpiceZmc.h"

   #define  LINE_LEN   20

   void main( int argc, char *argv[] )
   {

      /. 
      Local variables 
      ./

      SpiceChar  outline[LINE_LEN];


      /. 
      Store argv and argc for later access. 
      ./

      putcml_c ( argc, argv );


      /. 
      Now get the blank-padded, Fortran-style string. 
      ./

      getcml_ ( outline, LINE_LEN );


      /. 
      Null-terminate the string so it can be passed to printf. 
      ./
      
      outline[LINE_LEN-1] = NULLCHAR;


      printf ( "Argument line is '%s'\n", outline );

      exit(0);
   }



-Restrictions

   1) This routine should not be called by users' applications.
      It should be called only from C routines produced by running
      f2c on Fortran routines.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman   (JPL)
   K.R. Gehringer (JPL)
   H.A. Neilan    (JPL)
   M.J. Spencer   (JPL)
   E.D. Wright    (JPL)

-Version

   -CSPICE Version 3.2.0, 09-JUN-2010   (EDW)

       A return on failure check added after the getcml_c call.
       A memory error can occur if program flow continues after a
       getcml_c error.

   -CSPICE Version 3.1.0, 14-MAY-2003   (NJB)

       Prototype declaration was changed to match that created
       by running f2c on getcml.f.  This change was made to 
       suppress potential compiler warnings; the effective
       data types of the arguments have not changed.

   -CSPICE Version 3.0.0, 06-NOV-1998   (NJB)

       Modified to output a blank-padded, Fortran style string 
       rather than a C string.

   -CSPICE Version 2.0.1, 08-FEB-1998   (EDW)

       Argument list modified so that it conforms to expected f2c
       output format.


   -CSPICE Version 2.0.0,  6-JAN-1997   (EDW)

       This version is a complete rewrite of the routine using getcml_c
       to access the stored values of argc and argv.

-Index_Entries

 get command line arguments as a string

-&
*/

{
   /*
   Local variables 
   */

   SpiceChar            ** argv;

   SpiceInt                argc;
   SpiceInt                avail;
   SpiceInt                chrpos;
   SpiceInt                endpos;
   SpiceInt                i;
   SpiceInt                nmove;
   SpiceInt                required;



   /*
   Participate in error tracing. 
   */
   chkin_c ( "getcml_" );


   /*
   Validate the output string.
   */
   CHKOSTR_VAL ( CHK_STANDARD, "getcml_", outline, line_len, 0 );


   /*
   Retrieve the argv and argc values.
   */
   getcml_c ( &argc, &argv );

   /*
   If a SPICE error signaled in getcml_c, return to the calling routine.
   This check prevents a memory error if an error in getcml_c signals
   when the SPICE error subsystem is in RETURN mode.
   */
   if ( failed_c() ) 
      { 
      return EXIT_FAILURE;
      }

   /*
   Initialize the string end pointer and available space counter. 
   */
   endpos = 0;
   avail  = line_len;
   
   
   /*
   Append all arguments after the first to the output string.
   Separate the arguments by blanks.  Stop when we run out of room.
   */
   for(  i = 1;    ( i < argc ) && ( avail > 0 );   i++   )
   {
      required = strlen( argv[i] );
      
      /*
      If this is not the first argument, append a leading blank to the
      output string.
      */
      
      if ( i > 1 )
      {
         outline[endpos] = BLANK;
         
         endpos ++;
         avail  --;
      }
      

      /*
      Move as much as possible of the current argument into the 
      output line.
      */
      nmove  = MinVal ( required, avail );
      
      for ( chrpos = 0;  chrpos < nmove;  chrpos++ )
      {
         outline[endpos+chrpos] =  *(  argv[i] + chrpos  );
      }
      
      
      /*
      Advance the end pointer by however many characters we moved.
      That number could be zero.  The available space decreased by
      the same amount.
      */
      endpos += nmove;
      avail  -= nmove;

   }
   
   /*
   Since the output is a Fortran style string, any remaining space
   must be filled with blanks.
   */
   
   if ( avail > 0 )
   {
      memset ( (outline+endpos), BLANK, avail );
   }
 
   
   chkout_c ( "getcml_" );

   return 0;

  }


