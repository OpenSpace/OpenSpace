/*

-Procedure cklpf_c ( C-kernel, load pointing file )

-Abstract
 
   Load a CK pointing file for use by the CK readers.  Return that 
   file's handle, to be used by other CK routines to refer to the 
   file. 
 
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
 
   CK 
   DAF 
 
-Keywords
 
   POINTING 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void cklpf_c ( ConstSpiceChar * filename,
                  SpiceInt       * handle    )

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   filename   I   Name of the CK file to be loaded. 
   handle     O   Loaded file's handle. 
 
-Detailed_Input
 
   filename   is the name of a C-kernel file to be loaded. 
 
-Detailed_Output
 
   handle     is an integer handle assigned to the file upon loading. 
              Almost every other CK routine will subsequently use 
              this number to refer to the file. 
 
-Parameters
 
   ftsize     is the maximum number of pointing files that can 
              be loaded by CKLPF at any given time for use by the 
              readers. 
 
-Exceptions
 
   1) If an attempt is made to load more files than is specified 
      by the parameter ftsize, the error "SPICE(CKTOOMANYFILES)" 
      is signalled. 
 
   2) If an attempt is made to open more DAF files than is specified 
      by the parameter ftsize in DAFAH, an error is signalled by a 
      routine that this routine calls. 
 
   3) If the file specified by filename can not be opened, an error 
      is signalled by a routine that this routine calls. 
 
   4) If the file specified by filename has already been loaded, 
      it will become the "last-loaded" file.  (The readers 
      search the last-loaded file first.) 
 
-Files
 
   The C-kernel file specified by filename is loaded.  The file is 
   assigned an integer handle by CKLPF.  Other CK routines will refer 
   to this file by its handle. 
 
-Particulars
 
   See Particulars in ckbsr. 
 
   If there is room for a new file, CKLPF opens the file for 
   reading.  This routine must be called prior to a call to CKGP or 
   CKGPAV. 
 
   CK readers search files loaded with CKLPF in the reverse order 
   in which they were loaded.  That is, last-loaded files are 
   searched first. 
 
-Examples

   ck_kern  = "/kernels/mpf/ck/lander_nominal.bck";
   cklpf_c ( ck_kern, &hand );

   Also see the Example in ckbsr.for. 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution

   J.M. Lynch     (JPL) 
   J.E. McLean    (JPL) 
   M.J. Spencer   (JPL) 
   R.E. Thurman   (JPL) 
   I.M. Underwood (JPL) 
   E.D. Wright    (JPL) 
   B.V. Semenov   (JPL)
   
-Version
 
   -CSPICE Version 2.0.1, 31-JAN-2008 (BVS)

       Removed '-Revisions' from the header.

   -CSPICE Version 2.0.0, 08-FEB-1998 (NJB)  
   
       Input argument filename changed to type ConstSpiceChar *;
       name was changed to "filename" from "fname."
   
       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly.  String checks are now done using
       the macro CHKFSTR.
       
   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries
 
   load ck pointing file 
 
-& 
*/

{ /* Begin spklef_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "cklpf_c" );


   /*
   Check the input string filename to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "cklpf_c", filename );
   

   /*
   Call the f2c'd Fortran routine.
   */
   cklpf_ ( ( char     * )  filename, 
            ( integer  * )  handle, 
            ( ftnlen     )  strlen(filename) );


   chkout_c ( "cklpf_c" );   

} /* end cklpf_c */
