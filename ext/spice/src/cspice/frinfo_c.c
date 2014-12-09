/*

-Procedure frinfo_c ( Frame Information )

-Abstract
 
   Retrieve the minimal attributes associated with a frame 
   needed for converting transformations to and from it. 
 
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
 
   FRAMES 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void frinfo_c ( SpiceInt       frcode,
                   SpiceInt      *cent,
                   SpiceInt      *frclss,
                   SpiceInt      *clssid,
                   SpiceBoolean  *found   )

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   frcode     I   the idcode for some frame 
   cent       O   the center of the frame 
   frclss     O   the class (type) of the frame 
   clssid     O   the idcode for the frame within its class. 
   found      O   SPICETRUE if the requested information is available. 
 
-Detailed_Input
 
   frcode      is the ID code for some reference frame. 
 
-Detailed_Output
 
   cent        is the body ID code for the center of the reference 
               frame (if such an ID code is appropriate). 
 
   frclss      is the class or type of the frame.  This identifies 
               which subsystem will be used to perform frame 
               transformations. 
 
   clssid      is the ID-code used for the frame within its class. 
               This may be different from the frame ID-code. 
 
   found       is SPICETRUE if cent, frclss and frcode are available. 
               Otherwise, found is returned with the value SPICEFALSE. 
 
-Parameters
 
   None. 
 
-Files
 
   None. 
 
-Exceptions
 
   None. 
 
-Particulars
 
   This is a low level routine needed by state transformation 
   software to transform states and attitudes between different 
   reference frames. 
 
   The routine first examines local "hard-coded" information about 
   reference frames to see if the requested frame belongs to this 
   set.  If it does that information is returned. 
 
   If the requested information is not stored locally, the routine 
   then examines the kernel pool to see if the requested information 
   is stored there.  If it is and has the expected format, the data 
   is retrieved and returned. 
 
-Examples
 
   Suppose that you needed to determine the center of some 
   reference frame.  The following code fragment illustrates 
   how to use this routine to determine this information. 
 
      frinfo_c ( frcode, &cent, &frclss, &clssid, &found ); 
 
      if ( found ) 
      {
         printf ( "The center of reference frame %\n"
                  "has body ID code: %d\n",
                  frcode, 
                  cent                               );
      }
      else
      {
         printf ( "There is insufficient data for frame %s\n", frcode ); 
      }

 
-Restrictions
 
   None. 
 
-Author_and_Institution
 
    N.J. Bachman    (JPL)
    W.L. Taber      (JPL) 
 
-Literature_References
 
    None. 
 
-Version

    -CSPICE Version 1.2.0, 22-JUL-1999 (NJB)  
 
        Now uses logical local variable for found flag.
        
    -CSPICE Version 1.1.0, 16-MAY-1999 (NJB)  
 
        Changed name of argument "class" to frclss for C++  
        compatibility.
 
    -CSPICE Version 1.0.0, 21-JUN-1998 (NJB) (WLT)
 
        Based on SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
 
-Index_Entries
 
      Fetch reference frame attributes
 
-&
*/

{ /* Begin frinfo_c */


   /*
   Local variables
   */
   logical                 fnd;
   
   
   /*
   Participate in error tracing.
   */
   chkin_c ( "frinfo_c" );


   frinfo_ ( ( integer  * ) &frcode,
             ( integer  * ) cent,
             ( integer  * ) frclss,
             ( integer  * ) clssid,
             ( logical  * ) &fnd     );


   *found = fnd;

   chkout_c ( "frinfo_c" );

} /* End frinfo_c */

