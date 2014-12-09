/*

-Procedure      ckopn_c ( CK, open new file. )

-Abstract
 
   Open a new CK file, returning the handle of the opened file. 
 
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
 
   CK 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void ckopn_c (  ConstSpiceChar   * fname, 
                   ConstSpiceChar   * ifname, 
                   SpiceInt           ncomch, 
                   SpiceInt         * handle  ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   fname      I   The name of the CK file to be opened. 
   ifname     I   The internal filename for the CK. 
   ncomch     I   The number of characters to reserve for comments. 
   handle     O   The handle of the opened CK file. 
 
-Detailed_Input
 
   fname    The name of the CK file to be opened. 
 
   ifname   The internal filename for the CK file that is being 
            created. The internal filename may be up to 60 characters 
            long. If you do not have any conventions for tagging your 
            files, an internal filename of "CK_file" is perfectly 
            acceptable. You may also leave it blank if you like. 
 
   ncomch   This is the space, measured in characters, to be 
            initially set aside for the comment area when a new CK 
            file is opened. The amount of space actually set aside 
            may be greater than the amount requested, due to the 
            manner in which comment records are allocated in an CK 
            file. However, the amount of space set aside for comments 
            will always be at least the amount that was requested. 
 
            The value of ncomch should be greater than or equal to 
            zero, i.e., 0 <= ncomch. A negative value, should one 
            occur, will be assumed to be zero. 
 
-Detailed_Output
 
   handle   The handle of the opened CK file. If an error occurs the 
            value of this variable will not represent a valid handle. 
 
-Parameters
 
    None. 
 
-Exceptions
 
   1) If the value of ncomch is negative, a value of zero will 
      be used for the number of comment characters to be set aside 
      for comments. 
 
   2) If an error occurs while attempting to open a CK file the 
      value of handle will not represent a valid file handle. 
 
-Files
 
    See fname and handle. 
 
-Particulars
 
   Open a new CK file, reserving room for comments if requested. 
 
-Examples
 
   Suppose that you want to create a new CK file called "new.ck" 
   that contains a single type 3 CK segment and has room for at 
   least 5000 comment characters. The following code fragment should 
   take care of this for you, assuming that all of the variables 
   passed to the CK type 3 segment writer have appropriate values. 
 
      fname  = "new.ck";
      ifname = "Test CK file";
 
      ckopn_c ( fname,   ifname,  5000,    &handle );
       
      ckw03_c ( handle,  begtim,  endtim,  inst,  
                ref,     avflag,  segid,   nrec,   
                sclkdp,  quats,   avvs,    nints,  starts );
                 
      ckcls_c ( handle );
       
 
-Restrictions
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman      (JPL)
   K.R. Gehringer    (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.1.1, 09-NOV-2006 (NJB)  

       Header comments indicating that `ncomch' is ignored have
       been deleted.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)  
   
       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly.  String checks are now done using
       the macro CHKFSTR.
       
   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)
   
      Based on SPICELIB Version 1.0.0, 26-JAN-1995 (KRG)

-Index_Entries
 
   open a new ck file 
 
-&
*/

{ /* Begin ckopn_c */

   /*
   Participate in error handling.
   */
   chkin_c ( "ckopn_c" );

   /*
   Check the input strings fname and ifname to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ckopn_c", fname  );
   CHKFSTR ( CHK_STANDARD, "ckopn_c", ifname );
   
   
   ckopn_ ( ( char     * )  fname,
            ( char     * )  ifname,
            ( integer  * )  &ncomch,
            ( integer  * )  handle,
            ( ftnlen     )  strlen(fname),
            ( ftnlen     )  strlen(ifname) );
            
   
   chkout_c ( "ckopn_c" );

} /* End ckopn_c */
