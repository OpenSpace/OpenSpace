/*

-Procedure ldpool_c ( Load variables from a kernel file into the pool )

-Abstract
 
   Load the variables contained in a NAIF ASCII kernel file into the 
   kernel pool. 
 
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
 
   KERNEL 
 
-Keywords
 
   CONSTANTS 
   FILES 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void ldpool_c ( ConstSpiceChar * filename )

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   filename   I   Name of the kernel file. 
 
-Detailed_Input
 
   filename   is the name of the kernel file whose variables will be 
              loaded into the pool. 
 
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
 
   The NAIF ASCII kernel file kernel is opened by rdknew_. 
 
-Particulars

   Text kernels input to this routine need not have native line
   terminators for the platform. Lower level CSPICE routines can
   read and process non-native text files. This functionality does
   not exist in the Fortran SPICELIB.

   Only text kernel readers include the non-native read capability,
   (ldpool_c and furnsh_c), the generic text file line reader, rdtext_c
   requires native text files.

   Please refer to kernel.req for additiional information.

-Examples
 
   The following code fragment demonstrates how the data from 
   several kernel files can be loaded into a kernel pool. After the 
   pool is loaded, the values in the pool are written to a kernel 
   file. 
 
      /.
      Store in an array the names of the kernel files whose 
      values will be loaded into the kernel pool. 
      ./
      kernel [0] = "AXES.KER"; 
      kernel [1] = "GM.KER";
      kernel [2] = "LEAP_SECONDS.KER";

      /.
      Clear the kernel pool. (This is optional.) 
      ./
      clpool_c();

      /.
      Load the variables from the three kernel files into the 
      the kernel pool. 
      ./
      for ( i = 0; i < 3; i++ )
         {
         ldpool_c ( kernel [i] );
         } 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   kernel.req 
 
-Author_and_Institution
 
   R.E. Thurman    (JPL) 
   I.M. Underwood  (JPL) 
   B.V. Semenov    (JPL)
   W.L. Taber      (JPL) 
   E.D. Wright     (JPL)
 
-Version

   -CSPICE Version 2.0.2, 27-FEB-2008 (BVS)

       Corrected the contents of the Required_Reading section of 
       the header.

   -CSPICE Version 2.0.1, 17-OCT-2005 (EDW)

      Added text to Particulars section informing of the
      non-native kernel text file reading capability.

   -CSPICE Version 2.0.0, 08-FEB-1998 (NJB)

      Input argument kernel was changed to type ConstSpiceChar * and
      was given the new name "filename."
      
      Re-implemented routine without dynamically allocated, temporary 
      strings.  Made several corrections to the code example.  Renamed
      the argument "filename" to "kernel" for consistency with the
      header comments.
 
   -CSPICE Version 1.0.0, 25-OCT-1997  (EDW) 

-Index_Entries
 
   LOAD variables from a text kernel file into the pool 
 
-&
*/

{ /* Begin ldpool_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "ldpool_c" );


   /*
   Check the input string filename to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ldpool_c", filename );


   /*
   Call the f2c'd Fortran routine.
   */
   ldpool_ ( ( char   * ) filename, 
             ( ftnlen   ) strlen(filename) );


   chkout_c ( "ldpool_c" );
   

} /* End ldpool_c */
