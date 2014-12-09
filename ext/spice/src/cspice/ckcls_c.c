/*

-Procedure      ckcls_c ( CK, Close file )

-Abstract
 
   Close an open CK file. 
 
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
   #include "SpiceZst.h"

   void ckcls_c ( SpiceInt handle ) 

/*

-Brief_I/O
 
    VARIABLE  I/O  DESCRIPTION 
    --------  ---  -------------------------------------------------- 
    handle     I   Handle of the CK file to be closed. 
 
-Detailed_Input
 
   handle   The handle of the CK file that is to be closed. 
 
-Detailed_Output
 
   None. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If there are no segments in the file the error 
      SPICE(NOSEGMENTSFOUND) will be signalled. 
 
-Files
 
   See Detailed_Input. 
 
-Particulars
 
   Close the CK file attached to handle. 
 
-Examples
 
   Suppose that you want to create a new CK file called "new.ck" 
   that contains a single type 3 CK segment and has room for at 
   least 5000 comment characters. The following code fragment should 
   take care of this for you, assuming that all of the variables 
   passed to the CK type 3 segment writer have appropriate values. 
 
      name   = "new.ck";
      ifname = "Test CK file";
 
      ckopn_c ( name,    ifname,  5000,    &handle );
       
      ckw03_c ( handle,  begtim,  endtim,  inst,  
                ref,     avflag,  segid,   nrec,   
                sclkdp,  quats,   avvs,    nints,  starts );
                 
      ckcls_c ( handle );

 
-Restrictions
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman      (NJB)  
   K.R. Gehringer    (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
    -CSPICE Version 1.0.1, 08-MAR-2002 (EDW)

      Corrected header typo. Examples" to Examples.
 
   -CSPICE Version 1.0.0, 08-FEB-1998 (NJB)
   
      Based on SPICELIB Version 1.0.0, 26-JAN-1995 (KRG)
      
-Index_Entries
 
   close a ck file 
 
-&
*/

{ /* Begin ckcls_c */


   /*
   Participate in error handling.
   */
   chkin_c ( "ckcls_c");


   ckcls_ ( ( integer * ) &handle );
   

   chkout_c ( "ckcls_c");

} /* End ckcls_c */

