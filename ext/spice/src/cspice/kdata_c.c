/*

-Procedure kdata_c ( Kernel Data )

-Abstract
 
   Return data for the nth kernel that is among a list of specified 
   kernel types. 
 
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
   

   void kdata_c ( SpiceInt          which,
                  ConstSpiceChar  * kind,
                  SpiceInt          fillen,
                  SpiceInt          typlen,
                  SpiceInt          srclen,
                  SpiceChar       * file,
                  SpiceChar       * filtyp,
                  SpiceChar       * source,
                  SpiceInt        * handle,
                  SpiceBoolean    * found  )
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   which      I   Index of kernel to fetch from the list of kernels. 
   kind       I   The kind of kernel to which fetches are limited. 
   fillen     I   Available space in output file string.
   typlen     I   Available space in output kernel type string.
   srclen     I   Available space in output source string.
   file       O   The name of the kernel file. 
   filtyp     O   The type of the kernel. 
   source     O   Name of the source file used to load file. 
   handle     O   The handle attached to file. 
   found      O   SPICETRUE if the specified file could be located. 
 
-Detailed_Input
 
   which      is the number of the kernel to fetch (matching the 
              type specified by kind) from the list of kernels that 
              have been loaded through the entry point furnsh_c but 
              that have not been unloaded through the entry point 
              unload_c. 
              
              The range of which is 0 to count-1, where count is 
              the number of kernels loaded via furnsh_c.  This
              count may be obtained by calling ktotal_c.  See the 
              Examples section for an illustrative code fragment.
              
 
   kind       is a list of types of kernels to be considered when 
              fetching kernels from the list of loaded kernels. KIND 
              should consist of a list of words of kernels to 
              examine.  Recognized types are 
 
                 SPK  --- All SPK files are counted in the total. 
                 CK   --- All CK files are counted in the total. 
                 PCK  --- All binary PCK files are counted in the 
                          total. 
                 EK   --- All EK files are counted in the total. 
                 TEXT --- All text kernels that are not meta-text 
                          kernels are included in the total. 
                 META --- All meta-text kernels are counted in the 
                          total. 
                 ALL  --- Every type of kernel is counted in the 
                          total. 
 
               kind is case insensitive.  If a word appears in kind 
               that is not one of those listed above it is ignored. 
 
               See the entry point ktotal_c for examples of the use 
               of kind. 
 
   fillen      is the amount of available space in the output file 
               string, including room for the terminating null. 
               Normally, this is the declared length of the output
               string.
               
   typlen      is the amount of available space in the output kernel 
               type string.
               
   srclen      is the amount of available space in the output kernel 
               source string.
               
               
-Detailed_Output

 
   file        is the name of the file having index which in the 
               sequence of files of type kind currently loaded via 
               furnsh_c.  file will be blank if there is no such kernel
               is loaded. 
 
   filtyp      is the type of the kernel specified by file.  filtyp 
               will be empty if there is no file matching the 
               specification of which and kind. 
 
   source      is the name of the source file that was used to 
               specify file as one to load.  If file was loaded 
               directly via a call to furnsh_c, source will be empty. 
               If there is no file matching the specification of 
               which and kind, source will be empty. 
 
   handle      is the handle attached to file if it is a binary 
               kernel.  If file is a text kernel or meta-text kernel 
               handle will be zero.  If there is no file matching 
               the specification of which and kind, handle will be 
               set to zero. 
 
   found       is returned SPICETRUE if a file matching the 
               specification of which and kind exists.  If there is no 
               such file, found will be set to SPICEFALSE. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If a file is not loaded matching the specification of which 
      and kind, found will be SPICEFALSE; file, filtyp, and source 
      will be empty and handle will be set to zero. 

   2) If any input or output character argument pointer is null, the 
      error SPICE(NULLPOINTER) will be signaled.
      
   3) If any of the output string length arguments are less than 1, the
      error SPICE(STRINGTOOSHORT) will be signaled. 

   4) If any output string has length at least 1 but is too short to
      contain the output string, the corresponding is truncated on the 
      right.  The output string is still null-terminated.
 
-Files
 
   None. 
 
-Particulars
 
   This entry point allows you to determine which kernels have 
   been loaded via furnsh_c and to obtain information sufficient 
   to directly query those files. 
 
-Examples
 
   The following example shows how you could print a summary 
   of SPK files that have been loaded through the interface 
   furnsh_c. 
    
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
   
         ktotal_c ( "spk", &count );
    
         if ( count == 0 )
            {
            printf ( "No SPK files loaded at this time.\n" );
            }
         else
            {
            printf ( "The loaded SPK files are: \n\n" );
            }
    
         for ( which = 0;  which < count;  which++ )
            {
            kdata_c ( which,  "spk",    FILLEN,   TYPLEN, SRCLEN, 
                      file,   filtyp,  source,  &handle,  &found ); 
            printf ( "%s\n",  file   );
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

   -CSPICE Version 1.1.3, 02-MAY-2008 (EDW)

      standard.ker renamed standard.tm

   -CSPICE Version 1.1.2, 05-SEP-2007 (EDW)
 
      Expanded Examples section to a full, compilable program. 
 
   -CSPICE Version 1.1.1, 29-DEC-2004 (LSE)

      Corrected example code to match routine's argument list.
      (2 arguments reversed)
 
   -CSPICE Version 1.1.0, 02-FEB-2003 (EDW)

      Corrected example code to match routine's argument list.

   -CSPICE Version 1.0.0, 12-SEP-1999 (NJB) (WLT)

-Index_Entries
 
   Retrieve information on loaded SPICE kernels 
 
-&
*/

{ /* Begin kdata_c */


   /*
   Local variables
   */
   logical                 fnd;


   /*
   Participate in error tracing.
   */
   chkin_c ( "kdata_c" );


   /*
   Check the input string kind to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "kdata_c", kind );


   /*
   Make sure the output string file has at least enough room for one
   output character and a null terminator.  Also check for a null 
   pointer.
   */
   CHKOSTR ( CHK_STANDARD, "kdata_c", file, fillen );


   /*
   Make sure the output string filtyp has at least enough room for one
   output character and a null terminator.  Also check for a null 
   pointer.
   */
   CHKOSTR ( CHK_STANDARD, "kdata_c", filtyp, typlen );


   /*
   Make sure the output string source has at least enough room for one
   output character and a null terminator.  Also check for a null 
   pointer.
   */
   CHKOSTR ( CHK_STANDARD, "kdata_c", source, srclen );


   /*
   Map the input index from C to Fortran style.
   */

   which++;
   
   
   /*
   Call the f2c'd routine.
   */
   kdata_ (  ( integer   * ) &which,
             ( char      * ) kind,
             ( char      * ) file,
             ( char      * ) filtyp,
             ( char      * ) source,
             ( integer   * ) handle,
             ( logical   * ) &fnd,
             ( ftnlen      ) strlen(kind),
             ( ftnlen      ) fillen-1,
             ( ftnlen      ) typlen-1,
             ( ftnlen      ) srclen-1     );
             

   /*
   Convert the output strings from Fortran style to C style.  Set 
   the SpiceBoolean output found flag.
   */
   F2C_ConvertStr( fillen, file   );
   F2C_ConvertStr( typlen, filtyp );
   F2C_ConvertStr( srclen, source );

   *found = fnd;
   
             
   chkout_c ( "kdata_c" );

} /* End kdata_c */
