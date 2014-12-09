/*

-Procedure ckupf_c ( C-kernel, Unload pointing file )

-Abstract
 
   Unload a CK pointing file so that it will no longer be searched 
   by the readers. 
 
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

   void ckupf_c ( SpiceInt handle )

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     I   Handle of CK file to be unloaded 
 
-Detailed_Input
 
   handle     Integer handle assigned to the file upon loading. 
 
-Detailed_Output
 
   None. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
   1) If the file specified by handle does not appear in the file 
      table, nothing happens. 
 
-Files
 
   The file referred to by handle is unloaded. 
 
-Particulars
 
   See Particulars section above, in ckbsr.for. 
 
   Unloading a file with ckupf_c removes that file from consideration 
   by the CK readers.  In doing so, it frees up space for another 
   file to be loaded. 
 
-Examples
 
   See the Example in ckbsr.for. 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution

   J.M. Lynch     (JPL) 
   R.E. Thurman   (JPL) 
   I.M. Underwood (JPL)
   E.D. Wright    (JPL) 
   B.V. Semenov   (JPL)
   
-Version
 
   -CSPICE Version 1.0.2, 31-JAN-2008 (BVS)

      Removed '-Revisions' from the header.

   -CSPICE Version 1.0.1, 03-JUN-2003 (EDW)

      Correct typo in Procedure line.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries
 
   unload ck pointing file 
 
-&
*/

{ /* Begin ckupf_c */

   /*
   Participate in error handling
   */

   chkin_c ( "ckupf_c");


   /*
   Call the f2c'd Fortran routine.
   */
   ckupf_ ( &handle );


   chkout_c ( "ckupf_c");

} /* End ckupf_c */
