/*

-Procedure spkw10_c (SPK - write a type 10 segment )

-Abstract
 
   Write an SPK type 10 segment to the DAF open and attached to 
   the input handle. 
 
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
 
   SPK 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    spkw10_c
   
   void spkw10_c ( SpiceInt           handle,
                   SpiceInt           body, 
                   SpiceInt           center, 
                   ConstSpiceChar   * frame, 
                   SpiceDouble        first,
                   SpiceDouble        last,
                   ConstSpiceChar   * segid, 
                   ConstSpiceDouble   consts [8],
                   SpiceInt           n,
                   ConstSpiceDouble   elems  [],
                   ConstSpiceDouble   epochs []  )
                   
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   handle     I   The handle of a DAF file open for writing. 
   body       I   The NAIF ID code for the body of the segment. 
   center     I   The center of motion for body. 
   frame      I   The reference frame for this segment. 
   first      I   The first epoch for which the segment is valid. 
   last       I   The last  epoch for which the segment is valid. 
   segid      I   The string to use for segment identifier. 
   consts     I   The array of geophysical constants for the segment 
   n          I   The number of element/epoch pairs to be stored 
   elems      I   The collection of "two-line" element sets. 
   epochs     I   The epochs associated with the element sets. 
 
-Detailed_Input
 
   handle      is the file handle of an SPK file that has been 
               opened for writing by spcopn, dafopn, or dafopw. 
 
   body        is the NAIF ID for the body whose states are 
               to be recorded in an SPK file. 
 
   center      is the NAIF ID for the center of motion associated 
               with body. 
 
   frame       is the reference frame that states are referenced to, 
               for example "J2000". 
 
   first       are the bounds on the ephemeris times, expressed as 
   last        seconds past J2000, for which the states can be used 
               to interpolate a state for body. 
 
   segid       is the segment identifier. An SPK segment identifier 
               may contain up to 40 characters. 
 
   consts      are the geophysical constants needed for evaluation 
               of the two line elements sets.  The order of these 
               constants must be: 
 
               consts[0] = J2 gravitational harmonic for earth 
               consts[1] = J3 gravitational harmonic for earth 
               consts[2] = J4 gravitational harmonic for earth 
               consts[3] = Square root of the GM for earth where GM 
                           is expressed in earth radii cubed per 
                           minutes squared 
               consts[4] = Equatorial radius of the earth in km 
               consts[5] = Low altitude bound for atmospheric 
                           model in km 
               consts[6] = High altitude bound for atmospheric 
                           model in km 
               consts[7] = Distance units/earth radius (normally 1) 
 
   n           is the number of "two-line" element sets  and epochs 
               to be stored in the segment. 
 
   elems       contains a time-ordered array of two-line elements 
               as supplied in NORAD two-line element files.  The 
               i'th set of elements (where i ranges from 1 to n)
               should be stored as shown here: 
 
                  base = (i-1)*10 
 
                  elems ( base + 0 ) = NDT20 
                  elems ( base + 1 ) = NDD60 
                  elems ( base + 2 ) = BSTAR 
                  elems ( base + 3 ) = INCL 
                  elems ( base + 4 ) = NODE0 
                  elems ( base + 5 ) = ECC 
                  elems ( base + 6 ) = OMEGA 
                  elems ( base + 7 ) = MO 
                  elems ( base + 8 ) = NO 
                  elems ( base + 9 ) = EPOCH 
 
               The meaning of these variables is defined by the 
               format of the two-line element files available from 
               NORAD.
 
   epochs      contains the epochs (ephemeris seconds past J2000) 
               corresponding to the elements in elems.  The I'th 
               epoch must equal the epoch of the I'th element set 
               Epochs must form a strictly increasing sequence. 
 
-Detailed_Output
 
   None.       The data input is stored in an SPK segment in the 
               DAF connected to the input handle. 
 
-Parameters
 
   None. 
 
-Particulars
 
   This routine writes a type 10 SPK segment to the DAF open 
   for writing that is attached to handle.  A routine, GETELM, that 
   reads two-line element data from files distributed by 
   NORAD is available from NAIF. 
 
-Examples
 
   Suppose that you have collected the two-line element data 
   and geophysical constants as prescribed above.  The following 
   code fragment demonstrates how you could go about creating 
   a type 10 SPK segment. 
 
      #include "SpiceUsr.h"
          .
          .
          .
      /.
      Open a new SPK file using DAF and get a file handle. 
      ./
      body   = <integer code for the body>;
      center = <integer code for central body for the trajectory>; 
      frame  = "J2000";
      segid  = <string that gives the bodies name>; 
 
      fname  = "SAMPLE.SPK"; 
      ifname = "SAMPLE SPK FILE FOR PRIVATE USE"; 
      ncomch =  0;
 
      void spkopn_c ( fname, ifname, ncomch, &handle );
 
      /.
      Add the type 10 data. 
      ./
      spkw10_c ( handle, body,   center, frame,  first, last, 
                 segid,  consts, n,      elems,  epochs      ); 
 
      /.
      Close the SPK properly. 
      ./
      spkcls_c ( handle );
      
       
 
-Restrictions
 
   None. 
 
-Exceptions
 
   1) Errors in the structure or content of the inputs must be 
      diagnosed by routines called by this one. 
 
   2) File access errors are diagnosed by routines in the 
      call tree of this routine. 
 
   3) If either the input frame or segment ID string pointer is null,
      the error SPICE(NULLPOINTER) is signaled.
   
   4) If either the input frame or segment ID string is empty,
      the error SPICE(EMPTYSTRING) is signaled.
   
-Files
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.1, 30-OCT-2006 (BVS)

      Deleted "inertial" from the FRAME description in the Brief_I/O
      section of the header.

   -CSPICE Version 1.0.0, 29-JUN-1999 (NJB) (WLT)

-Index_Entries
 
   write a type_10 spk segment 
 
-&
*/

{ /* Begin spkw10_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "spkw10_c" );


   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkw10_c", frame );
   CHKFSTR ( CHK_STANDARD, "spkw10_c", segid );


   /*
   Write the segment. 
   */
   spkw10_ ( ( integer    * ) &handle,
             ( integer    * ) &body,
             ( integer    * ) &center,
             ( char       * ) frame,
             ( doublereal * ) &first,
             ( doublereal * ) &last,
             ( char       * ) segid,
             ( doublereal * ) consts,
             ( integer    * ) &n,
             ( doublereal * ) elems,
             ( doublereal * ) epochs,
             ( ftnlen       ) strlen(frame),
             ( ftnlen       ) strlen(segid)  );


   chkout_c ( "spkw10_c" );

} /* End spkw10_c */
