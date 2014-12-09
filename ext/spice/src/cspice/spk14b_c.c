/*

-Procedure spk14b_c ( SPK, begin a type 14 segment )

-Abstract
 
   Begin a type 14 SPK segment in the SPK file associated with 
   handle. See also spk14a_c and spk14e_c. 
 
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
   #include "SpiceZmc.h"


   void spk14b_c (  SpiceInt           handle,
                    ConstSpiceChar   * segid,
                    SpiceInt           body,
                    SpiceInt           center,
                    ConstSpiceChar   * frame,
                    SpiceDouble        first,
                    SpiceDouble        last,
                    SpiceInt           chbdeg  )
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   handle     I   The handle of an SPK file open for writing. 
   segid      I   The string to use for segment identifier. 
   body       I   The NAIF ID code for the body of the segment. 
   center     I   The center of motion for body. 
   frame      I   The reference frame for this segment. 
   first      I   The first epoch for which the segment is valid. 
   last       I   The last epoch for which the segment is valid. 
   chbdeg     I   The degree of the Chebyshev Polynomial used. 
 
-Detailed_Input
 
   handle         is the file handle of an SPK file that has been 
                  opened for writing. 
 
   segid          is the segment identifier. An SPK segment identifier 
                  may contain up to 40 printing ASCII characters. 
 
   body           is the NAIF ID for the body whose states are 
                  to be recorded in an SPK file. 
 
   center         is the NAIF ID for the center of motion associated 
                  with body. 
 
   frame          is the reference frame that states are referenced to, 
                  for example "J2000". 
 
   first          is the starting epoch, in TDB seconds past J2000, for 
                  the ephemeris data to be placed into the segment. 
 
   last           is the ending epoch, in TDB seconds past J2000, for 
                  the ephemeris data to be placed into the segment. 
 
   chbdeg         is the degree of the Chebyshev Polynomials used to 
                  represent the ephemeris information stored in the 
                  segment. 
 
-Detailed_Output
 
   None.          The input data is used to create the segment summary 
                  for the segment being started in the SPK file 
                  associated with handle. 
 
                  See the Particulars section for details about the 
                  structure of a type 14 SPK segment. 
 
-Parameters
 
   None.
    
-Particulars
 
   This routine begins writing a type 14 SPK segment to the open SPK 
   file that is associated with handle. The file must have been 
   opened with write access. 
 
   This routine is one of a set of three routines for creating and 
   adding data to type 14 SPK segments. These routines are: 
 
      spk14b_c: Begin a type 14 SPK segment. This routine must be 
                called before any data may be added to a type 14 
                segment. 
 
      spk14a_c: Add data to a type 14 SPK segment. This routine may be 
                called any number of times after a call to spk14b_c to 
                add type 14 records to the SPK segment that was 
                started. 
 
      spk14e_c: End a type 14 SPK segment. This routine is called to 
                make the type 14 segment a permanent addition to the 
                SPK file. Once this routine is called, no further type 
                14 records may be added to the segment. A new segment 
                must be started. 
 
   A type 14 SPK segment consists of coefficient sets for fixed order 
   Chebyshev polynomials over consecutive time intervals, where the 
   time intervals need not all be of the same length. The Chebyshev 
   polynomials represent the position, X, Y, and Z coordinates, and 
   the velocities, dX/dt, dY/dt, and dZ/dt, of body relative to 
   center. 
 
   The ephemeris data supplied to the type 14 SPK writer is packed 
   into an array as a sequence of records, 
 
      ----------------------------------------------------- 
      | Record 1 | Record 2 | ... | Record N-1 | Record N | 
      ----------------------------------------------------- 
 
   with each record has the following format. 
 
         ------------------------------------------------ 
         |  The midpoint of the approximation interval  | 
         ------------------------------------------------ 
         |  The radius of the approximation interval    | 
         ------------------------------------------------ 
         |  chbdeg+1 coefficients for the X coordinate  | 
         ------------------------------------------------ 
         |  chbdeg+1 coefficients for the Y coordinate  | 
         ------------------------------------------------ 
         |  chbdeg+1 coefficients for the Z coordinate  | 
         ------------------------------------------------ 
         |  chbdeg+1 coefficients for the X velocity    | 
         ------------------------------------------------ 
         |  chbdeg+1 coefficients for the Y velocity    | 
         ------------------------------------------------ 
         |  chbdeg+1 coefficients for the Z velocity    | 
         ------------------------------------------------ 
 
-Examples
 
   Assume we have the following for each of the examples that 
   follow. 
 
      handle   is the handle of an SPK file opened with write 
               access. 
 
      segid    is a character string of no more than 40 characters 
               which provides a pedigree for the data in the SPK 
               segment we will create. 
 
      body     is the NAIF ID code for the body whose ephemeris 
               is to be placed into the file. 
 
      center   is the center of motion for the ephemeris of body. 
 
      reffrm   is the name of the SPICE reference frame for the 
               ephemeris. 
 
      first    is the starting epoch, in seconds past J2000, for 
               the ephemeris data to be placed into the segment. 
 
      last     is the ending epoch, in seconds past J2000, for 
               the ephemeris data to be placed into the segment. 
 
   Example 1: 
 
      For this example, we also assume that: 
 
         n        is the number of type 14 records that we want to 
                  put into a segment in an SPK file. 
 
         recrds   contains n type 14 records packaged for the SPK 
                  file. 
 
         etstrt   contains the initial epochs for each of the 
                  records contained in RECRDS, where 
 
                     etstrt[i] < etstrt[i+1], i = 0, n-2 
 
                     etstrt[1] <= first, etstrt[n-1] < last 
 
                     etstrt[i+1], i = 0, n-2, is the ending epoch for 
                     record i as well as the initial epoch for record 
                     i+1. 
 
      Then the following code fragment demonstrates how to create a 
      type 14 SPK segment if all of the data for the segment is 
      available at one time. 
 
         #include "SpiceUsr.h"
            .
            .
            .
         
         #define SPK  "example.bsp"
            
         /.
         If the segment is to be appended to an existing file, open
         that file for "append" access.  Otherwise, create a new file.
         ./
         
         if ( exists_c(SPK) )
         {
            spkopa_c ( SPK, &handle );
         } 
         else
         {
            /.
            New files are supplied with an internal file name.  
            Comment area space may be reserved at this time; the
            units are characters.
            ./
            ifname = "Sample type 14 SPK file.";
            ncomch = 1024;
            
            spkopn_c ( SPK, ifname, ncomch, &handle );
         }
          
          
         /.
         Begin the segment. 
         ./
         spk14b_c ( handle, segid, body, center, reffrm, 
                    first,  last,  chbdeg               );
       
         /.
         Add the data to the segment all at once. 
         ./
         spk14a_c ( handle, n, recrds, etstrt ); 
      
         /.
         End the segment, making the segment a permanent addition 
         to the SPK file. 
         ./
         spk14e_c ( handle ); 
         
             .
             .
             .
         /.
         After all segments have been loaded, close the SPK file.
         ./
         spkcls_c ( handle );
         
 
   Example 2: 
 
      In this example we want to add type 14 SPK records, as described
      above in the Particulars section, to the segments being written
      as they are generated.  The ability to write the records in this
      way is useful if computer memory is limited. It may also be
      convenient from a programming perspective to write the records
      one at a time.
 
      For this example, assume that we want to generate n type 14 SPK 
      records, one for each of n time intervals, writing them all to 
      the same segment in the SPK file. Let 
 
         n        be the number of type 14 records that we want to 
                  generate and put into a segment in an SPK file. 
 
         record   be an array with enough room to hold a single type 
                  14 record, i.e. record should have dimension at 
                  least 6 * (chbdeg + 1 ) + 2. 
 
         start    be an array of n times that are the beginning 
                  epochs for each of the intervals of interest. The 
                  times should be in increasing order and the start 
                  time for the first interval should equal the 
                  starting time for the segment. 
 
                     start[i] < start[i+1], i = 0, n-2 
 
                     start[0] = first 
 
         stop     be an array of n times that are the ending epochs 
                  for each of the intervals of interest. The times 
                  should be in increasing order and the stop time for 
                  interval i should equal the start time for interval 
                  i+1, i.e., we want to have continuous coverage in 
                  time across all of the records. Also, the stop time 
                  for the last interval should equal the ending time 
                  for the segment. 
 
                     stop[i]   < stop [i+1], i = 0, n-2 
 
                     stop[i]   = start[i+1], i = 0, n-2 
 
                     stop[n-1] = last 
 

         genrec( time1, time2, record ) 
 
                  be a subroutine that generates a type 14 SPK record 
                  for a time interval specified by time1 and time2. 
 

      Then the following code fragment demonstrates how to create a 
      type 14 SPK segment if all of the data for the segment is not 
      available at one time. 
 
         #include "SpiceUsr.h"
            .
            .
            .
        
         /.
         Begin the segment. 
         ./
         spk14b_c ( handle, segid, body, center, reffrm, 
                    first,  last,  chbdeg                ); 
 
        
         /.
         Generate the records and write them to the segment in the 
         SPK file one at at time. 
         ./   
         
         for ( i = 0;  i < n;  i++ )
         {
            genrec   ( start[i],    stop[i], record  ); 
            spk14a_c ( handle,   1, record,  start+i );
         }
 
         /.
         End the segment, making the segment a permanent addition 
         to the SPK file. 
         ./   
         spk14e_c ( handle );
         
 
-Restrictions
 
   The SPK file must be open with write access. 
 
   Only one segment may be written to a particular SPK file at a 
   time. All of the data for the segment must be written and the 
   segment must be ended before another segment may be started in 
   the file. 
 
-Exceptions
 
   1) If the degree of the Chebyshev Polynomial to be used for this 
      segment is negative, the error SPICE(INVALIDARGUMENT) will 
      be signaled. 
 
   2) Errors in the structure or content of the inputs other than the 
      degree of the Chebyshev Polynomial are diagnosed by routines 
      called by this one. 
 
   3) File access errors are diagnosed by routines in the call tree 
      of this routine. 
 
   4) If either the input frame or segment ID string pointer is null,
      the error SPICE(NULLPOINTER) is signaled.
   
   5) If either the input frame or segment ID string is empty,
      the error SPICE(EMPTYSTRING) is signaled.
   
-Files
 
   See handle in the Detailed_Input section. 
 
-Author_and_Institution
 
   N.J. Bachman        (JPL)
   K.R. Gehringer      (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.1, 30-OCT-2006 (BVS)

      Deleted "inertial" from the FRAME description in the Brief_I/O
      section of the header.

   -CSPICE Version 1.0.0, 29-JUL-1999 (NJB) (KRG)

-Index_Entries
 
   begin writing a type_14 spk segment 
 
-&
*/

{ /* Begin spk14b_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "spk14b_c" );


   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spk14b_c", frame );
   CHKFSTR ( CHK_STANDARD, "spk14b_c", segid );


   /*
   Call the f2c'd routine.
   */
   spk14b_ (  ( integer     * ) &handle,
              ( char        * ) segid,
              ( integer     * ) &body, 
              ( integer     * ) &center, 
              ( char        * ) frame, 
              ( doublereal  * ) &first, 
              ( doublereal  * ) &last, 
              ( integer     * ) &chbdeg,
              ( ftnlen        ) strlen(segid),
              ( ftnlen        ) strlen(frame)   );
               

   chkout_c ( "spk14b_c" );

} /* End spk14b_c */
