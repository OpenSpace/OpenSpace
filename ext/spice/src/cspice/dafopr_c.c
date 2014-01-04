/*

-Procedure dafopr_c ( DAF, open for read )

-Abstract
 
   Open a DAF for subsequent read requests. 
 
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
 
   DAF 
 
-Keywords
 
   DAF 
   FILES 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void dafopr_c ( ConstSpiceChar    * fname,
                   SpiceInt          * handle  ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   fname      I   Name of DAF to be opened. 
   handle     O   Handle assigned to DAF. 
 
-Detailed_Input
 
   fname       is the file name of a DAF to be opened for read 
               access. 
 
-Detailed_Output
 
   handle      is the file handle associated with the file. This 
               handle is used to identify the file in subsequent 
               calls to other DAF routines. 
 
-Parameters
 
  None. 
 
-Files
 
   See argument fname. 
 
-Exceptions
 
   1) If the specified file has already been opened for read 
      access, the handle already associated with the file is 
      returned. 
 
   2) If the specified file has already been opened for write 
      access, the error SPICE(DAFRWCONFLICT) is signaled. 
 
   3) If the specified file has already been opened by a non-DAF 
      routine, the error SPICE(DAFIMPROPOPEN) is signaled. 
 
   4) If the specified file cannot be opened without exceeding 
      the maximum number of files, the error SPICE(DAFFTFULL) 
      is signaled. 
 
   5) If (for some reason) the file cannot be opened properly, 
      the error SPICE(DAFOPENFAIL) is signaled. 
 
   6) If the attempt to read the file's ID word fails, the error 
      SPICE(FILEREADFAILED) will be signaled. 
 
   7) If the specified file is not a DAF file, as indicated by the 
      file's ID word, the error SPICE(NOTADAFFILE) is signaled. 
 
   8) If no logical units are available, the error will be 
      signaled by routines called by this routine. 
 
   9) If the file does not exist, the error SPICE(FILEDOESNOTEXIST) 
      is signaled. 
 
  10) If the INQUIRE fails, the error SPICE(INQUIREFAILED) 
      is signaled. 
 
  11) If the file record cannot (for some reason) be read, 
      the error SPICE(DAFFRNOTFOUND) is signaled. 
 
  12) If the file name is blank, the error SPICE(BLANKFILENAME) 
      is signaled. 
 
-Particulars
 
   Most DAFs require only read access. If you do not need to 
   change the contents of a file, you should open it with dafopr_c. 
 
-Examples
 
   In the following code fragment, dafopr_c is used to open a file, 
   which is then searched for DAFs containing data for a particular 
   object. 
 
      #include "SpiceUsr.h"
           .
           .
           .
      dafopr_c ( fname, &handle ); 
      dafbfs_c ( handle );
       
      daffna_c ( &found );
 
      while ( found ) 
      {
         dafgs_c ( sum );
         dafus_c ( sum, ND, NI, dc, ic );
 
         if ( ic[0] == target_object ) 
         { 
            . 
            . 
            . 
         }
 
         daffna_c ( &found );
      }
      
 
-Restrictions
 
   None. 
 
-Literature_References
 
   NAIF Document 167.0, "Double Precision Array Files (DAF) 
   Specification and User's Guide" 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
   K.R. Gehringer  (JPL) 
   J.M. Lynch      (JPL) 
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 01-AUG-1999 (NJB) (KRG) (JML) (WLT) (IMU)

-Index_Entries
 
   open daf for read 
 
-&
*/

{ /* Begin dafopr_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dafopr_c" );

   /*
   Check the file name to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "dafopr_c", fname );


   dafopr_ ( ( char    * ) fname,
             ( integer * ) handle,
             ( ftnlen    ) strlen(fname) );


   chkout_c ( "dafopr_c" );

} /* End dafopr_c */
