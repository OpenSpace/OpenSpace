/*

-Procedure spkw03_c ( Write SPK segment, type 3 )

-Abstract
 
  Write a type 3 segment to an SPK file. 
 
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
   #undef    spkw03_c
   

   void spkw03_c ( SpiceInt                handle,
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
   MAXDEG     P   Maximum degree of Chebyshev expansions.
   TOLSCL     P   Scale factor used to compute time bound tolerance.
   handle     I   Handle of SPK file open for writing. 
   body       I   NAIF code for ephemeris object. 
   center     I   NAIF code for the center of motion of the body. 
   frame      I   Reference frame name. 
   first      I   Start time of interval covered by segment. 
   last       I   End time of interval covered by segment. 
   segid      I   Segment identifier. 
   intlen     I   Length of time covered by record. 
   n          I   Number of records in segment. 
   polydg     I   Chebyshev polynomial degree. 
   cdata      I   Array of Chebyshev coefficients. 
   btime      I   Begin time of first record. 
 
-Detailed_Input
 
   handle         DAF handle of an SPK file to which a type 3 segment 
                  is to be added. The SPK file must be open for 
                  writing. 
 
   body           NAIF integer code for an ephemeris object whose 
                  state relative to another body is described by the 
                  segment to be created. 
 
   center         NAIF integer code for the center of motion of the 
                  object identified by `body'. 
 
   frame          NAIF name for a reference frame relative to which 
                  the state information for `body' is specified. 
 
   first, 
   last           Start and stop times of the time interval over 
                  which the segment defines the state of `body'. 
 
   segid          Segment identifier. An SPK segment identifier may 
                  contain up to 40 characters. 
 
   intlen         Length of time, in seconds, covered by each set of 
                  Chebyshev polynomial coefficients (each logical 
                  record).  Each set of Chebyshev coefficents must 
                  cover this fixed time interval, `intlen'. 
 
   n              Number of sets of Chebyshev polynomial coefficients 
                  for coordinates and their derivatives (number of 
                  logical records) to be stored in the segment. 
                  There is one set of Chebyshev coefficients for each 
                  time period. 
 
   polydg         Degree of each set of Chebyshev polynomials. 
 
   cdata          Array containing all the sets of Chebyshev 
                  polynomial coefficients to be placed in the 
                  segment of the SPK file.  The coefficients are 
                  stored in `cdata' in order as follows: 
 
                     the (degree + 1) coefficients for the first 
                     coordinate of the first logical record 
 
                     the coefficients for the second coordinate 
 
                     the coefficients for the third coordinate 
 
                     the coefficients for the derivative of the first 
                     coordinate 
 
                     the coefficients for the derivative of the 
                     second coordinate 
 
                     the coefficients for the derivative of the third 
                     coordinate 
 
                     the coefficients for the first coordinate for 
                     the second logical record, ... 
 
                     and so on. 
 
                  The logical data records are stored contiguously:

                     +----------+
                     | Record 1 |
                     +----------+
                     | Record 2 |
                     +----------+
                         ...
                     +----------+
                     | Record N |
                     +----------+

                  The contents of an individual record are:

                     +--------------------------------------+
                     | Coeff set for X position component   |
                     +--------------------------------------+
                     | Coeff set for Y position component   |
                     +--------------------------------------+
                     | Coeff set for Z position component   |
                     +--------------------------------------+
                     | Coeff set for X velocity component   |
                     +--------------------------------------+
                     | Coeff set for Y velocity component   |
                     +--------------------------------------+
                     | Coeff set for Z velocity component   |
                     +--------------------------------------+

                 Each coefficient set has the structure:

                     +--------------------------------------+
                     | Coefficient of T_0                   |
                     +--------------------------------------+
                     | Coefficient of T_1                   |
                     +--------------------------------------+
                                       ...
                     +--------------------------------------+
                     | Coefficient of T_POLYDG              |
                     +--------------------------------------+

                  Where T_n represents the Chebyshev polynomial
                  of the first kind of degree n.
                 
 
   btime          Begin time (seconds past J2000 TDB) of first set 
                  of Chebyshev polynomial coefficients (first 
                  logical record). 
 
-Detailed_Output
 
   None. 
 
-Parameters
 
   The parameters below are declared in the Fortran include file
   spk03.inc, which is part of the Fortran SPICE Toolkit (aka
   SPICELIB). The values of those parameters are used in CSPICE code
   generated by running f2c on SPICELIB source code. They are not
   directly referenced by code in this module.
   
      
      MAXDEG         is the maximum allowed degree of the input
                     Chebyshev expansions. 
       
                     The value of MAXDEG is 27.
 
 
      TOLSCL         is a tolerance scale factor (also called a
                     "relative tolerance") used for time coverage bound
                     checking. TOLSCL is unitless. TOLSCL produces a
                     tolerance value via the formula
 
                        TOL = TOLSCL * max( abs(first), abs(last) )
 
                     where `first' and `last' are the coverage time
                     bounds of a type 3 segment, expressed as seconds
                     past J2000 TDB.
 
                     The resulting parameter TOL is used as a tolerance
                     for comparing the input segment descriptor time
                     bounds to the first and last epoch covered by the
                     sequence of time intervals defined by the inputs
                     to spkw03_c:
 
                        btime
                        intlen
                        n

                     The value of TOLSCL is 1.e-13. 
-Exceptions
 
   1)  If the number of sets of coefficients is not positive 
       SPICE(NUMCOEFFSNOTPOS) is signaled. 
 
   2)  If the interval length is not positive, SPICE(INTLENNOTPOS) 
       is signaled. 
 
   3)  If the integer code for the reference frame is not recognized, 
       SPICE(INVALIDREFFRAME) is signaled. 
 
   4)  If segment stop time is not greater then the begin time, 
       SPICE(BADDESCRTIMES) is signaled. 
 
   5)  If the input degree `polydg' is less than 0 or greater than
       MAXDEG, the error will be diagnosed by a routine in the call
       tree of this routine.
 
   6)  If the last non-blank character of `segid' occurs past index 40,
       or if `segid' contains any nonprintable characters, the error will
       be diagnosed by a routine in the call tree of this routine.
 
   7)  The error SPICE(EMPTYSTRING) is signaled if either input string
       does not contain at least one character, since the input strings
       cannot be converted to a Fortran-style string in this case.
 
   8)  The error SPICE(NULLPOINTER) is signaled if either input string
       pointer is null.

-Files
 
   A new type 3 SPK segment is written to the SPK file attached 
   to handle. 
 
-Particulars
 
   This routine writes an SPK type 3 data segment to the designated 
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
   in an array `cdata' pertaining to the position of the moon (NAIF ID 
   = 301), relative to the Earth-moon barycenter (NAIF ID = 3), in 
   the J2000 reference frame, and want to put these into a type 3 
   segment in an existing SPK file. The following code could be used 
   to add one new type 3 segment.  To add multiple segments, put the 
   call to spkw03_c in a loop. 
 
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
      segid = "MY_SAMPLE_SPK_TYPE_3_SEGMENT";

      /.
      Write the segment. 
      ./
      spkw03_c ( handle, 301,    3,      "J2000", 
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
 
   N.J. Bachman  (JPL)
   K.S. Zukor    (JPL) 
 
-Version

   -CSPICE Version 2.0.0, 09-JAN-2014 (NJB) 

       Relaxed test on relationship between the time bounds of the
       input record set (determined by `btime', `intlen', and `n') and
       the descriptor bounds `first' and `last'. Now the descriptor
       bounds may extend beyond the time bounds of the record set by a
       ratio computed using the parameter TOLSCL (see Parameters above
       for details). Added checks on input polynomial degree.

   -CSPICE Version 1.0.0, 08-MAR-2002 (EDW)

      Corrected section header typo: Example to Examples.
 
   -CSPICE Version 1.0.0, 23-JUN-1999 (NJB) (KSZ)

-Index_Entries
 
   write spk type_3 data segment 
 
-&
*/

{ /* Begin spkw03_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "spkw03_c" );
   

   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkw03_c", frame );
   CHKFSTR ( CHK_STANDARD, "spkw03_c", segid );
 

   /*
   Write the segment. 
   */
   spkw03_ ( ( integer    * ) &handle,
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


   chkout_c ( "spkw03_c" );

} /* End spkw03_c */
