/*

-Procedure rdtext_c ( Read a line from a text file )

-Abstract

   Read the next line of text from a text file.

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

   FILES
   TEXT

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void rdtext_c ( ConstSpiceChar * file,
                   SpiceInt         lenout,
                   SpiceChar      * line,
                   SpiceBoolean   * eof    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  ---------------------------------------------------
   file       I   Name of text file.
   lenout     I   Available room in output line.
   line       O   Next line from the text file.
   eof        O   End-of-file indicator.

-Detailed_Input

   file        is the name of the text file from which the next
               line is to be read. If the file is not currently
               open, it is opened with a logical unit determined
               at run time, and the first line of the file is
               returned. Otherwise, the next line not yet read
               from the file is read and returned.
               
   lenout      is the available room in the output line, including
               the terminating null.  If the maximum expected length
               of an output line is N, lenout should be at least N+1.

-Detailed_Output

   line        is next line of text in the specified file.
               If the end of the file is reached, LINE is blank.

   eof         is true when the end of the file is reached, and is
               otherwise false.

-Parameters

   None.

-Exceptions

   1) If too many files are open already, the error
      SPICE(TOOMANYFILESOPEN) is signaled.

   2) If the attempt to open the file fails, the error
      SPICE(FILEOPENFAILED) is signaled.

   3) If the attempt to read from the file fails, the error
      SPICE(FILEREADFAILED) is signaled.

   4) If the attempt to "inquire" the status of the file fails,
      the error SPICE(INQUIREFAILED) is signaled.

-Files

   See input FILE.

-Particulars

   rdtext_c reads the next line from a text file. If the file is
   not currently open, it is opened with a logical unit determined
   at run time, and the first line of the file is returned.
   Otherwise, the next line not yet read from the file is returned.

   If the end of the file is reached, an empty line is returned,
   the end-of-file indicator is true, and the file is closed.

   Several files may be opened and read simultaneously. Thus,
   you may begin reading from one file before the end of another
   file has been reached. rdtext_c maintains a separate file pointer
   for each file.

-Examples

   Let FILE.1 contain the following lines.

      Mary had a little lamb
      Everywhere that Mary went

   Let FILE.2 contain the following lines.

      Its fleece was white as snow.
      The lamb was sure to go.

      Note:  You do not what and end-of-file on the same line as
      text.  That text will be ignored.


   Then the code fragment

   #include "SpiceUsr.h"
   #define LENOUT 32

   main(void)
      {

      SpiceBoolean eof;
      SpiceChar    line[LENOUT];

      eof = SPICEFALSE;

      do {
         rdtext_c ( "file.1", LENOUT, line, &eof );
         printf ( "%s \n", line );

         rdtext_c ( "file.2", LENOUT, line, &eof );
         printf ( "%s \n", line );
         }
      while ( !eof );

      }

   produces the following output

      Mary had a little lamb
      Its fleece was white as snow.
      Everywhere that Mary went
      The lamb was sure to go.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman    (JPL)
   H.A. Neilan     (JPL)
   M.J. Spencer    (JPL)
   I.M. Underwood  (JPL)

-Version

   -CSPICE Version 2.0.0, 07-OCT-1999   (NJB)

      Changed argument list to conform to SPICE convention:  LENOUT
      now precedes the output string.
      
      Added description of lenout to the header.
      
      Added local logical variable for EOF flag.
      
   -CSPICE Version 1.0.0, 25-MAY-1999   (EDW)

-Index_Entries

   read a line from a text file

-&
*/

{ /* Begin rdtext_c */

   /*
   Local variables
   */
   logical                 endfil;
   
   
   /*
   Participate in error tracing.
   */
   chkin_c ( "rdtext_c" );


   /*
   Check the strings: file, line to insure the pointer is
   non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "rdtext_c", file );
   CHKOSTR ( CHK_STANDARD, "rdtext_c", line, lenout );


   /* Call the f2c'd routine. */
   rdtext_ ( ( char    * ) file,
             ( char    * ) line,
             ( logical * ) &endfil,
             ( ftnlen    ) strlen(file),
             ( ftnlen    ) lenout - 1 );
   
   /*
   Assign the SpiceBoolean EOF flag the logical value obtained  
   from the f2c'd routine.
   */
   
   *eof  =  endfil;
   

   /* The string, line, is a Fortranish type string. Convert to C. */
   F2C_ConvertStr ( lenout, line );


   /* Checkout. */
   chkout_c ( "rdtext_c" );


} /* End rdtext_c */
