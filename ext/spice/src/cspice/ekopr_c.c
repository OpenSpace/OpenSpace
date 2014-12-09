/*

-Procedure ekopr_c ( EK, open file for reading )

-Abstract
 
   Open an existing E-kernel file for reading. 
 
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
 
   EK 
 
-Keywords
 
   EK 
   FILES 
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   

   void ekopr_c ( ConstSpiceChar  * fname,
                  SpiceInt        * handle ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   fname      I   Name of EK file. 
   handle     O   Handle attached to EK file. 
 
-Detailed_Input
 
   fname          is the name of an existing E-kernel file to be 
                  opened for read access. 
 
-Detailed_Output
 
   handle         is the EK file handle of the file designated by 
                  fname.  This handle is used to identify the file 
                  to other EK routines. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the indicated file cannot be opened, the error will be 
       diagnosed by routines called by this routine.  The new file 
       will be deleted. 
 
   2)  If the indicated file has the wrong architecture version, the 
       error will be diagnosed by routines called by this routine. 
 
   3)  If an I/O error occurs while reading the indicated file, the 
       error will be diagnosed by routines called by this routine. 
 
-Files
 
   See the EK Required Reading for a discussion of the EK file 
   format. 
 
-Particulars
 
   This routine should be used to open an EK file for read access. 
   EKs opened for read access may not be modified. 
 
   Opening an EK file with this routine makes the EK accessible to 
   the CSPICE EK readers 
 
      ekrcec_c
      ekrced_c
      ekrcei_c
 
   all of which expect an EK file handle as an input argument.  These 
   readers allow a caller to read individual EK column entries. 
 
   To make an EK available to the EK query system, the file must be 
   loaded via eklef_c, rather than by this routine.  See the EK 
   Required Reading for further information. 
 
-Examples
 
   1)  Open the file my.ek for read access: 
 
          ekopr_c ( "my.ek", &handle );
 
-Restrictions
 
   1)  No more than CSPICE_EK_MAX_OPEN_FILES EK files may be opened
       or loaded within the EK system simultaneously.  See the header
       file SpicePar.h for the numeric value of the limit.
        
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.1.0, 23-JUL-2001 (NJB)
     
       Removed tab characters from source file.

   -CSPICE Version 1.0.0, 27-MAR-1998

       Based on SPICELIB Version 1.0.0, 26-AUG-1995 (NJB)
     
-Index_Entries
 
   open EK for reading 
 
-&
*/

{ /* Begin ekopr_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "ekopr_c" );

   /*
   Check the file name to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekopr_c", fname );

   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.
   */
   ekopr_ (  ( char     * )  fname,
             ( integer  * )  handle, 
             ( ftnlen     )  strlen(fname)  );


   chkout_c ( "ekopr_c" );

} /* End ekopr_c */
