/*

-Procedure spkw05_c ( Write SPK segment, type 5 )

-Abstract
 
   Write an SPK segment of type 5 given a time-ordered set of 
   discrete states and epochs, and the gravitational parameter 
   of a central body. 
 
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
   SPC 
   NAIF_IDS 
 
-Keywords
 
   EPHEMERIS 
 
*/
   #include <string.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    spkw05_c
   

   void spkw05_c ( SpiceInt                handle,
                   SpiceInt                body,
                   SpiceInt                center,
                   ConstSpiceChar        * frame,
                   SpiceDouble             first,
                   SpiceDouble             last,
                   ConstSpiceChar        * segid,
                   SpiceDouble             gm,
                   SpiceInt                n,
                   ConstSpiceDouble        states [][6],
                   ConstSpiceDouble        epochs []      )
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     I   Handle of an SPK file open for writing. 
   body       I   Body code for ephemeris object. 
   center     I   Body code for the center of motion of the body. 
   frame      I   The reference frame of the states. 
   first      I   First valid time for which states can be computed. 
   last       I   Last valid time for which states can be computed. 
   segid      I   Segment identifier. 
   gm         I   Gravitational parameter of central body. 
   n          I   Number of states and epochs. 
   states     I   States. 
   epochs     I   Epochs. 
 
-Detailed_Input
 
   handle      is the file handle of an SPK file that has been 
               opened for writing. 
 
   body        is the NAIF ID for the body whose states are 
               to be recorded in an SPK file. 
 
   center      is the NAIF ID for the center of motion associated 
               with BODY. 
 
   frame       is the reference frame that states are referenced to, 
               for example "J2000". 
 
   first       are the bounds on the ephemeris times, expressed as 
   last        seconds past J2000, for which the states can be used 
               to interpolate a state for BODY. 
 
   segid       is the segment identifier. An SPK segment identifier 
               may contain up to 40 characters. 
 
   gm          is the gravitational parameter of the central body 
               ( in units of kilometers **3 / seconds **2 ). 
 
   n           is the number of states and epochs to be stored 
               in the segment. 
 
   states      contains a time-ordered array of geometric states 
               ( x, y, z, dx/dt, dy/dt, dz/dt, in kilometers and 
               kilometers per second ) of the target body with 
               respect to the central body specified in the segment 
               descriptor. 
 
   epochs      contains the epochs (ephemeris seconds past J2000) 
               corresponding to the states in states.  Epochs must 
               form a strictly increasing sequence. 
 
-Detailed_Output
 
   None.  A type 5 segment is written to the file attached to handle. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input epochs do not form an increasing sequence, the 
      error SPICE(UNORDEREDTIMES) will be signaled. 
 
   2) If the number of states and epochs is not positive then the 
      error SPICE(NUMSTATESNOTPOS) will be signaled. 
 
   3) If FIRST is greater than LAST then the error 
      SPICE(BADDESCRTIMES) will be signaled. 
 
   4) If SEGID is more than 40 characters long, the error 
      SPICE(SEGIDTOOLONG) is signaled. 
 
   5) If SEGID contains any nonprintable characters, the error 
      SPICE(NONPRINTABLECHARS) is signaled. 
 
   6) Any file I/O problems will be detected and diagnosed by one 
      of the DAF routines called by this routine. 
 
   7) The error SPICE(EMPTYSTRING) is signaled if either input
      string does not contain at least one character, since the
      input strings cannot be converted to a Fortran-style string
      in this case.
      
   8) The error SPICE(NULLPOINTER) is signaled if either input string
      pointer is null.

-Files
 
   A new type 05 SPK segment is written to the SPK file attached 
   to handle. 
 
-Particulars
 
   This routine writes an SPK type 05 data segment to the open SPK 
   file according to the format described in the type 05 section of 
   the SPK Required Reading. The SPK file must have been opened with 
   write access. 
 
-Examples
 
   Suppose that you have states and are prepared to produce 
   a segment of type 05 in an SPK file. 
 
   The following code fragment could be used to add the new segment 
   to a previously opened SPK file attached to handle. The file must 
   have been opened with write access. 
 
      #include "SpiceUsr.h"
             .
             .
             .
      /.
      Create a segment identifier. 
      ./
      ConstSpiceChar  * segid = "MY_SAMPLE_SPK_TYPE_5_SEGMENT";
 
      /.
      Write the segment. 
      ./
      spkw05_c ( SpiceInt                handle,
                 SpiceInt                body,
                 SpiceInt                center,
                 ConstSpiceChar        * frame,
                 SpiceDouble             first,
                 SpiceDouble             last,
                 ConstSpiceChar        * segid,
                 SpiceDouble             gm,
                 SpiceInt                n,
                 SpiceDouble             states,
                 SpiceDouble             epochs  );
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   K.R. Gehringer  (JPL) 
   J.M. Lynch      (JPL) 
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 23-JUN-1999 (NJB)(KRG)(JML)(WLT)(IMU)

-Index_Entries
 
   write spk type_5 ephemeris data segment 
 
-&
*/

{ /* Begin spkw05_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "spkw05_c" );

 
   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkw05_c", frame );
   CHKFSTR ( CHK_STANDARD, "spkw05_c", segid );
 

   /*
   Write the segment.  Note that the state array DOES NOT require 
   transposition!
   */
   
   spkw05_ ( ( integer    * ) &handle,
             ( integer    * ) &body,
             ( integer    * ) &center,
             ( char       * ) frame,
             ( doublereal * ) &first,
             ( doublereal * ) &last,
             ( char       * ) segid,
             ( doublereal * ) &gm,
             ( integer    * ) &n,
             ( doublereal * ) states,
             ( doublereal * ) epochs,
             ( ftnlen       ) strlen(frame),
             ( ftnlen       ) strlen(segid)  );

   chkout_c ( "spkw05_c" );

} /* End spkw05_c */
