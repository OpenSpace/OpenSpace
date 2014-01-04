/*

-Procedure kinfo_c ( Kernel Information )

-Abstract
 
   Return information about a loaded kernel specified by name. 
 
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
 
   KERNEL 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   

   void kinfo_c ( ConstSpiceChar  * file,
                  SpiceInt          typlen,
                  SpiceInt          srclen,
                  SpiceChar       * filtyp,
                  SpiceChar       * source,
                  SpiceInt        * handle,
                  SpiceBoolean    * found  )
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   file       I   Name of a kernel to fetch information for 
   typlen     I   Available space in output kernel type string.
   srclen     I   Available space in output source string.
   filtyp     O   The type of the kernel. 
   source     O   Name of the source file used to load file. 
   handle     O   The handle attached to file. 
   found      O   SPICETRUE if the specified file could be located. 
 
-Detailed_Input
 
   file        is the name of a kernel file for which descriptive 
               information is desired. 
 
   typlen      is the amount of available space in the output kernel 
               type string.
               
   srclen      is the amount of available space in the output kernel 
               source string.
               
               
-Detailed_Output
 
   filtyp      is the type of the kernel specified by file.  filtyp 
               will be empty if file is not on the list of kernels
               loaded via furnsh_c. 
 
   source      is the name of the source file that was used to 
               specify file as one to load.  If file was loaded 
               directly via a call to furnsh_c, source will be empty. 
               If file is not on the list of kernels loaded via
               furnsh_c, source will be empty.
 
   handle      is the handle attached to file if it is a binary 
               kernel.  If file is a text kernel or meta-text kernel 
               handle will be zero. If file is not on the list of 
               kernels loaded via furnsh_c, handle will be set to zero.
 
   found       is returned SPICETRUE if the specified file exists.
               If there is no such file, found will be set to 
               SPICEFALSE. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the specified file is not on the list of files that 
      are currently loaded via the interface furnsh_c, found 
      will be SPICEFALSE, handle will be set to zero and filtyp 
      and source will be set to empty strings. 
 
   2) If any input or output character argument pointer is null, the 
      error SPICE(NULLPOINTER) will be signaled.
      
   3) If either output string length argument is less than 1, the
      error SPICE(STRINGTOOSHORT) will be signaled. 

   4) If either output string has length at least 1 but is too short to
      contain the output string, the corresponding is truncated on the 
      right.  The output string is still null-terminated.
 
-Files
 
   None. 
 
-Particulars
 
   This entry point allows you to request information directly 
   for a specific SPICE kernel. 
 
-Examples
 
   Suppose you wish to determine the type of a loaded kernel 
   so that you can call the correct summarizing routines 
   for the kernel.  The following bit of pseudo code shows 
   how you might use this entry point together with summarizing 
   code to produce a report on the file.  (Note that the 
   routines spk_summry, ck_summry, pck_summry and ek_summry 
   are simply names to indicate what you might do with the 
   information returned by kinfo_c.  They are not routines that 
   are part of the SPICE Toolkit.) 

      #include <stdio.h>
      #include "SpiceUsr.h"

      #define  FILLEN   128
      #define  TYPLEN   32
      #define  SRCLEN   128

      SpiceInt        which;
      SpiceInt        count;
      SpiceInt        handle;

      SpiceChar       file  [FILLEN];
      SpiceChar       filtyp[TYPLEN];
      SpiceChar       source[SRCLEN];

      SpiceBoolean    found;

      int main()
         { 
         furnsh_c( "/kernels/standard.tm" );
   
         ktotal_c ( "all", &count );
    
         if ( count == 0 )
            {
            printf ( "No files loaded at this time.\n" );
            }
         else
            {
            printf ( "The loaded files files are: \n\n" );
            }
    
         for ( which = 0;  which < count;  which++ )
            {

            kdata_c ( which,  "all",    FILLEN,   TYPLEN, SRCLEN, 
                     file,   filtyp,  source,  &handle,  &found ); 

            kinfo_c ( file, TYPLEN, SRCLEN, filtyp, source, &handle, &found );

               if (  eqstr_c ( filtyp, "SPK" )  )
                  {
                  printf ( "%s is an SPK file.\n", file );
                  }
               else if (  eqstr_c ( filtyp, "CK" )  )
                  {
                  printf ( "%s is a CK file.\n", file );
                  }
               else if (  eqstr_c ( filtyp, "PCK" )  )
                  {
                  printf ( "%s is a PCK file.\n", file );
                  }
               else if (  eqstr_c ( filtyp, "EK" )  )
                  {
                  printf ( "%s is an EK file.\n", file );
                  }
               else if (  eqstr_c ( filtyp, "META" )  )
                  {
                  printf ( "%s is a meta-text kernel.\n", file );
                  }
               else 
                {
                printf ( "%s is a text kernel.\n", file );
                }
                
             }

          }
      
 
-Restrictions
 
   None.
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
 
-Version

   -CSPICE Version 1.1.2, 02-MAY-2008 (EDW)

      standard.ker renamed standard.tm

   -CSPICE Version 1.1.1, 05-SEP-2007 (EDW)
 
      Expanded Examples section to a full, compilable program. 
 
   -CSPICE Version 1.1.0, 02-FEB-2003 (EDW)

      Corrected example code to match routine's argument list.

   -CSPICE Version 1.0.0, 01-SEP-1999 (NJB) (WLT)

-Index_Entries
 
   Fetch information about a loaded SPICE kernel 
 
-&
*/

{ /* Begin kinfo_c */

   /*
   Local variables
   */
   logical                 fnd;



   /*
   Participate in error tracing.
   */
   chkin_c ( "kinfo_c" );


   /*
   Check the input string file to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "kinfo_c", file );
   
   
   /*
   Make sure the output string filtyp has at least enough room for one
   output character and a null terminator.  Also check for a null 
   pointer.
   */
   CHKOSTR ( CHK_STANDARD, "kinfo_c", filtyp, typlen );


   /*
   Make sure the output string source has at least enough room for one
   output character and a null terminator.  Also check for a null 
   pointer.
   */
   CHKOSTR ( CHK_STANDARD, "kinfo_c", source, srclen );

   /*
   Call the f2c'd routine.
   */
   kinfo_ (  ( char      * ) file,
             ( char      * ) filtyp,
             ( char      * ) source,
             ( integer   * ) handle,
             ( logical   * ) &fnd,
             ( ftnlen      ) strlen(file),
             ( ftnlen      ) typlen-1,
             ( ftnlen      ) srclen-1     );
             

   /*
   Convert the output strings from Fortran style to C style.  Set 
   the SpiceBoolean output found flag.
   */
   F2C_ConvertStr( typlen, filtyp );
   F2C_ConvertStr( srclen, source );

   *found = fnd;


   chkout_c ( "kinfo_c" );

} /* End kinfo_c */
