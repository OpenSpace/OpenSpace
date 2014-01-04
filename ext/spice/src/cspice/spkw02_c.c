/*

-Procedure spkw02_c ( Write SPK segment, type 2 )

-Abstract
 
  Write a type 2 segment to an SPK file. 
 
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
 
   NAIF_IDS 
   SPC 
   SPK 
 
-Keywords
 
   EPHEMERIS 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    spkw02_c
   

   void spkw02_c ( SpiceInt                handle,
                   SpiceInt                body,
                   SpiceInt                center,
                   ConstSpiceChar        * frame,
                   SpiceDouble             first,
                   SpiceDouble             last,
                   ConstSpiceChar        * segid,
                   SpiceDouble             intlen,
                   SpiceInt                n,
                   SpiceInt                polydg,
                   ConstSpiceDouble        cdata [],
                   SpiceDouble             btime     )

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
   intlen     I   Length of time covered by logical record. 
   n          I   Number of coefficient sets. 
   polydg     I   Chebyshev polynomial degree. 
   cdata      I   Array of Chebyshev coefficients. 
   btime      I   Begin time of first logical record. 
 
-Detailed_Input
 
   handle         DAF handle of an SPK file to which a type 2 segment 
                  is to be added.  The SPK file must be open for 
                  writing. 
 
   body           NAIF integer code for an ephemeris object whose 
                  state relative to another body is described by the 
                  segment to be created. 
 
   center         NAIF integer code for the center of motion of the 
                  object identified by body. 
 
   frame          NAIF name for a reference frame relative to which 
                  the state information for body is specified. 
 
   first, 
   last           Start and stop times of the time interval over 
                  which the segment defines the state of body. 
 
   segid          Segment identifier.  An SPK segment identifier may 
                  contain up to 40 characters. 
 
   intlen         Length of time, in seconds, covered by each set of 
                  Chebyshev polynomial coefficients (each logical 
                  record).  Each set of Chebyshev coefficients must 
                  cover this fixed time interval, intlen. 
 
   n              Number of sets of Chebyshev polynomial coefficients 
                  for coordinates (number of logical records) to be 
                  stored in the segment.  There is one set of 
                  Chebyshev coefficients for each time period. 
 
   polydg         Degree of each set of Chebyshev polynomials, i.e. 
                  the number of Chebyshev coefficients per coordinate 
                  minus one. 
 
   cdata          Array containing all the sets of Chebyshev 
                  polynomial coefficients to be placed in the 
                  segment of the SPK file.  The coefficients are 
                  stored in cdata in order as follows: 
 
                     the (degree + 1) coefficients for the first 
                     coordinate of the first logical record 
 
                     the coefficients for the second coordinate 
 
                     the coefficients for the third coordinate 
 
                     the coefficients for the first coordinate for 
                     the second logical record, ... 
 
                     and so on. 
 
 
   btime          Begin time (seconds past J2000 TDB) of first set 
                  of Chebyshev polynomial coefficients (first 
                  logical record).  first is an appropriate value 
                  for btime. 
 
-Detailed_Output
 
   None. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the number of sets of coefficients is not positive 
      SPICE(NUMCOEFFSNOTPOS) is signalled. 
 
   2) If the interval length is not positive, SPICE(INTLENNOTPOS) 
      is signalled. 
 
   3) If the integer code for the reference frame is not recognized, 
      SPICE(INVALIDREFFRAME) is signalled. 
 
   4) If segment stop time is not greater then the begin time, 
       SPICE(BADDESCRTIMES) is signalled. 
 
   5) If the start time of the first record is not less than 
      or equal to the descriptor begin time, SPICE(BADDESCRTIMES) 
      is signalled. 
 
   6) If the end time of the last record is not greater than 
      or equal to the descriptor end time, SPICE(BADDESCRTIMES) is 
      signalled. 
 
   7) The error SPICE(EMPTYSTRING) is signaled if either input
      string does not contain at least one character, since the
      input strings cannot be converted to a Fortran-style string
      in this case.
      
   8) The error SPICE(NULLPOINTER) is signaled if either input string
      pointer is null.

-Files
 
   A new type 2 SPK segment is written to the SPK file attached 
   to handle. 
 
-Particulars
 
   This routine writes an SPK type 2 data segment to the designated 
   SPK file, according to the format described in the SPK Required 
   Reading. 
 
   Each segment can contain data for only one target, central body, 
   and reference frame.  The Chebyshev polynomial degree and length 
   of time covered by each logical record are also fixed.  However, 
   an arbitrary number of logical records of Chebyshev polynomial 
   coefficients can be written in each segment.  Minimizing the 
   number of segments in an SPK file will help optimize how the SPICE 
   system accesses the file. 
 
-Examples
 
   Suppose that you have sets of Chebyshev polynomial coefficients 
   in an array CDATA pertaining to the position of the moon (NAIF ID 
   = 301), relative to the Earth-moon barycenter (NAIF ID = 3), in 
   the J2000 reference frame, and want to put these into a type 2 
   segment in an existing SPK file.  The following code could be used 
   to add one new type 2 segment.  To add multiple segments, put the 
   call to spkw02_c in a loop. 
 
      #include "SpiceUsr.h"
           .
           .
           .
           
      /.
      First open the SPK file and get a handle for it. 
      ./
      spkopa_c ( spknam, &handle ); 

      /.
      Create a segment identifier. 
      ./
      segid = "MY_SAMPLE_SPK_TYPE_2_SEGMENT";

      /.
      Write the segment. 
      ./
      spkw02_c ( handle, 301,    3,      "J2000", 
                 first,  last,   segid,  intlen, 
                 n,      polydg, cdata,  btime   ); 

      /.
      Close the file. 
      ./
      spkcls_c ( handle );
      
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL)
   K.S. Zukor     (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 21-JUL-1999 (NJB) (KSZ)

-Index_Entries
 
   write spk type_2 data segment 
 
-&
*/

{ /* Begin spkw02_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "spkw02_c" );

 
   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkw02_c", frame );
   CHKFSTR ( CHK_STANDARD, "spkw02_c", segid );
 

   /*
   Write the segment. 
   */
   
   spkw02_ ( ( integer    * ) &handle,
             ( integer    * ) &body,
             ( integer    * ) &center,
             ( char       * ) frame,
             ( doublereal * ) &first,
             ( doublereal * ) &last,
             ( char       * ) segid,
             ( doublereal * ) &intlen,
             ( integer    * ) &n,
             ( integer    * ) &polydg,
             ( doublereal * ) cdata,
             ( doublereal * ) &btime,
             ( ftnlen       ) strlen(frame),
             ( ftnlen       ) strlen(segid)  );


   chkout_c ( "spkw02_c" );

} /* End spkw02_c */
