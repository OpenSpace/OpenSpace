/*

-Procedure spkcls_c ( SPK, Close file )

-Abstract
 
   Close an open SPK file. 
 
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
 
   SPK 
 
-Keywords
 
   SPK 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void spkcls_c ( SpiceInt handle ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   handle     I   Handle of the SPK file to be closed. 
 
-Detailed_Input
 
   handle     The handle of the SPK file that is to be closed. 
 
-Detailed_Output
 
   None. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If there are no segments in the file, the error 
      SPICE(NOSEGMENTSFOUND) will be signaled. 
 
-Files
 
   See argument handle. 
 
-Particulars
 
   Close the SPK file attached to handle. 
 
-Examples
 
   Suppose that you want to create a new SPK file called "new.spk" 
   that contains a single type 5 SPK segment and has room for at 
   least 5000 comment characters. The following code fragment should 
   take care of this for you, assuming that all of the variables 
   passed to the SPK type 5 segment writer have appropriate values 
   and no errors occur. 
 
      #include "SpiceUsr.h"
         .
         .
         . 
      name   = "new.spk";
      ifname = "Test SPK file";
 
      spkopn_c ( name, ifname, 5000,  &handle ); 
      spkw05   ( handle, objid, cntrid, cframe, etbeg, 
                 etend, segmid, cntrgm, nstate, state, 
                 epoch                                 );
      spkcls_c ( handle );
 
-Restrictions
 
   None. 
 
-Author_and_Institution
 
   F.S. Turner        (JPL)
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.0, 16-MAR-1999 (FST)

-Index_Entries
 
   close an spk file 
 
-&
*/

{ /* Begin spkcls_c */

   /*
   Participate in error tracing.
   */

   chkin_c ( "spkcls_c" );

   spkcls_ ( ( integer * ) &handle );

   chkout_c ( "spkcls_c" );

} /* End spkcls_c */
