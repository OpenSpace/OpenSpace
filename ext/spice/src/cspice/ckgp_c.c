/*

-Procedure      ckgp_c ( C-kernel, get pointing )

-Abstract
 
   Get pointing (attitude) for a specified spacecraft clock time. 
 
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
 
   CK 
   SCLK
 
-Keywords
 
   POINTING 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void ckgp_c ( SpiceInt            inst, 
                 SpiceDouble         sclkdp, 
                 SpiceDouble         tol, 
                 ConstSpiceChar    * ref, 
                 SpiceDouble         cmat[3][3], 
                 SpiceDouble       * clkout,  
                 SpiceBoolean      * found      ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   inst       I   NAIF ID of instrument, spacecraft, or structure.
   sclkdp     I   Encoded spacecraft clock time. 
   tol        I   Time tolerance. 
   ref        I   Reference frame. 
   cmat       O   C-matrix pointing data. 
   clkout     O   Output encoded spacecraft clock time. 
   found      O   True when requested pointing is available. 
 
-Detailed_Input
 
   inst       is the NAIF integer ID for the instrument, spacecraft, or
              other structure for which pointing is being requested.
              For brevity we will refer to this object as the
              "instrument," and the frame fixed to this object as the
              "instrument frame" or "instrument-fixed" frame.
 
   sclkdp     is the encoded spacecraft clock time for which 
              pointing is being requested.  

              The CSPICE routines scencd_c and sce2c_c respectively
              convert spacecraft clock strings and ephemeris time
              to encoded spacecraft clock.  The inverse conversions
              are performed by scdecd_c and sct2e_c.
 
   tol        is a time tolerance in ticks, the units of encoded 
              spacecraft clock time.  

              The CSPICE routine sctiks_c converts a spacecraft clock
              tolerance duration from its character string
              representation to ticks.  scfmt_c performs the inverse
              conversion.
 
              The C-matrix returned by ckgp_c is the one whose time tag
              is closest to `sclkdp' and within `tol' units of
              `sclkdp'.  (More in Particulars, below.)
 
              In general, because using a non-zero tolerance 
              affects selection of the segment from which the
              data is obtained, users are strongly discouraged 
              from using a non-zero tolerance when reading CKs 
              with continuous data. Using a non-zero tolerance
              should be reserved exclusively to reading CKs with 
              discrete data because in practice obtaining data 
              from such CKs using a zero tolerance is often not 
              possible due to time round off. 

   ref        is the desired reference frame for the returned pointing.
              The returned C-matrix `cmat' gives the orientation of the
              instrument designated by `inst' relative to the frame
              designated by `ref'.  When a vector specified relative to
              frame `ref' is left-multiplied by `cmat', the vector is
              rotated to the frame associated with `inst'.  See the
              discussion of `cmat' below for details.
 
              Consult the SPICE document "Frames" for a discussion 
              of supported reference frames. 
 
-Detailed_Output
 
   cmat       is a rotation matrix that transforms the components of a
              vector expressed in the frame specified by `ref' to
              components expressed in the frame tied to the instrument,
              spacecraft, or other structure at time `clkout' (see
              below). 

              Thus, if a vector v has components x,y,z in the `ref'
              reference frame, then v has components x',y',z' in the 
              instrument fixed frame at time `clkout': 
 
                   [ x' ]     [          ] [ x ] 
                   | y' |  =  |   cmat   | | y | 
                   [ z' ]     [          ] [ z ] 
 
              If you know x', y', z', use the transpose of the 
              C-matrix to determine x, y, z as follows: 
 
                   [ x ]      [          ]T    [ x' ] 
                   | y |  =   |   cmat   |     | y' | 
                   [ z ]      [          ]     [ z' ] 
                           (Transpose of cmat) 

 
   clkout     is the encoded spacecraft clock time associated with
              the returned C-matrix. This value may differ from the
              requested time, but never by more than the input
              tolerance `tol'.
 
              The particulars section below describes the search 
              algorithm used by ckgp_c to satisfy a pointing request. 
              This algorithm determines the pointing instance 
              (and therefore the associated time value) that is 
              returned. 
 
   found      is SPICETRUE if a record was found to satisfy the
              pointing request.  `found' will be SPICEFALSE otherwise.

-Parameters
 
   None. 
 
-Exceptions
 
   1)  If a C-kernel file has not been loaded using furnsh_c prior to a
       call to this routine, an error is signaled by a routine in the
       call tree of this routine.
 
   2)  If `tol' is negative, found is set to SPICEFALSE. 
 
   3)  If `ref' is not a supported reference frame, an error is
       signaled by a routine in the call tree of this routine and
       `found' is set to SPICEFALSE.
 
-Files
 
   ckgp_c searches through files loaded by furnsh_c to locate a segment
   that satisfies the request for pointing for the instrument `inst' at
   time `sclkdp'. You must load at least one C-kernel file via furnsh_c
   prior to calling this routine.
  
-Particulars

   How the tolerance argument is used
   ==================================


   Reading a type 1 CK segment (discrete pointing instances)
   ---------------------------------------------------------

   In the diagram below
  
      - "0" is used to represent discrete pointing instances 
        (quaternions and associated time tags).

      - "( )" are used to represent the end points of the time
        interval covered by a segment in a CK file.
      
      - `sclkdp' is the time at which you requested pointing.
        The location of `sclkdp' relative to the time tags of the
        pointing instances is indicated by the "+" sign.

      - `tol' is the time tolerance specified in the pointing
        request.  The square brackets "[ ]" represent the 
        endpoints of the time interval 

           sclkdp-tol : sclkdp+tol

      - The quaternions occurring in the segment need not be
        evenly spaced in time.


   Case 1:  pointing is available
   ------------------------------

                            sclkdp 
                                 \   tol 
                                  | / 
                                  |/\ 
   Your request                [--+--] 
                               .  .  . 
   Segment      (0-----0--0--0--0--0--0---0--0------------0--0--0--0) 
                                   ^ 
                                   | 
                       ckgp_c returns this instance.


   Case 2:  pointing is not available
   ----------------------------------

                                                 sclkdp 
                                                    \   tol 
                                                     | / 
                                                     |/\ 
   Your request                                   [--+--] 
                                                  .  .  . 
   Segment      (0-----0--0--0--0--0--0---0--0--0---------0--0--0--0) 

                      
                       ckgp_c returns no pointing; the output 
                       `found' flag is set to SPICEFALSE.



   Reading a type 2, 3, 4, or 5 CK segment (continuous pointing)
   -------------------------------------------------------------

   In the diagrams below
  
      - "==" is used to represent periods of continuous pointing. 

      - "--" is used to represent gaps in the pointing coverage. 

      - "( )" are used to represent the end points of the time
        interval covered by a segment in a CK file.
      
      - `sclkdp' is the time at which you requested pointing.
        The location of `sclkdp' relative to the time tags of the
        pointing instances is indicated by the "+" sign.

      - `tol' is the time tolerance specified in the pointing
        request.  The square brackets "[ ]" represent the 
        endpoints of the time interval 

           sclkdp-tol : sclkdp+tol

      - The quaternions occurring in the periods of continuous
        pointing need not be evenly spaced in time.


   Case 1:  pointing is available at the request time
   --------------------------------------------------
 
                           sclkdp 
                                 \   tol 
                                  | / 
                                  |/\ 
   Your request                [--+--] 
                               .  .  . 
                               .  .  . 
                               .  .  . 
   Segment            (==---===========---=======----------===--) 
                                  ^ 
                                  | 

                 The request time lies within an interval where
                 continuous pointing is available. ckgp_c returns
                 pointing at the requested epoch.


   Case 2:  pointing is available "near" the request time
   ------------------------------------------------------
 
                                  sclkdp 
                                        \   tol 
                                         | / 
                                         |/\ 
   Your request                       [--+--] 
                                      .  .  . 
   Segment            (==---===========----=======---------===--) 
                                           ^ 
                                           | 

                 The request time lies in a gap:  an interval where
                 continuous pointing is *not* available.  ckgp_c 
                 returns pointing for the epoch closest to the 
                 request time `sclkdp'.

 
   Case 3:  pointing is not available
   ----------------------------------
 
                                               sclkdp 
                                                     \   tol 
                                                      | / 
                                                      |/\ 
   Your request                                    [--+--] 
                                                   .  .  . 
   Segment            (==---===========----=======---------===--) 

                       ckgp_c returns no pointing; the output 
                       `found' flag is set to SPICEFALSE.



   Tolerance and segment priority
   ==============================

   ckgp_c searches through loaded C-kernels to satisfy a pointing
   request. Last-loaded files are searched first. Individual files are
   searched in backwards order, so that between competing segments
   (segments containing data for the same object, for overlapping time
   ranges), the one closest to the end of the file has highest
   priority. 
 
   The search ends when a segment is found that can provide pointing
   for the specified instrument at a time falling within the specified
   tolerance on either side of the request time. Within that segment,
   the instance closest to the input time is located and returned.
 
   The following four cases illustrate this search procedure. Segments 
   A and B are in the same file, with segment A located further 
   towards the end of the file than segment B. Both segments A and B 
   contain discrete pointing data, indicated by the number 0.
 

   Case 1:  Pointing is available in the first segment searched.
            Because segment A has the highest priority and can
            satisfy the request, segment B is not searched.
            
 
                                sclkdp 
                                      \  tol 
                                       | / 
                                       |/\ 
   Your request                     [--+--] 
                                    .  .  . 
   Segment A          (0-----------------0--------0--0-----0) 
                                         ^ 
                                         | 
                                         | 
                             ckgp_c returns this instance 
 
   Segment B     (0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0)   



   Case 2:  Pointing is not available in the first segment searched.
            Because segment A cannot satisfy the request, segment B 
            is searched.


                           sclkdp 
                                \   tol 
                                 | / 
                                 |/\ 
   Your request               [--+--] 
                              .  .  . 
   Segment A          (0-----------------0--------0--0-----0) 
                              .  .  . 
   Segment B     (0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0) 
                                 ^ 
                                 | 
                     ckgp_c returns this instance 
  
 
   Segments that contain continuous pointing data are searched in the
   same manner as segments containing discrete pointing data. For
   request times that fall within the bounds of continuous intervals,
   ckgp_c will return pointing at the request time. When the request
   time does not fall within an interval, then a time at an endpoint of
   an interval may be returned if it is the closest time in the segment
   to the user request time and is also within the tolerance.
 
   In the following examples, segment A is located further towards the
   end of the file than segment C. Segment A contains discrete pointing
   data and segment C contains continuous data, indicated by the "="
   character.
 

   Case 3:  Pointing is not available in the first segment searched.
            Because segment A cannot satisfy the request, segment C
            is searched.

                           sclkdp 
                                 \  tol 
                                  | / 
                                  |/\ 
   Your request                [--+--] 
                               .  .  . 
                               .  .  . 
   Segment A          (0-----------------0--------0--0-----0) 
                               .  .  . 
                               .  .  . 
   Segment C          (---=============-----====--------==--) 
                                  ^ 
                                  | 
                                  | 
                       ckgp_c returns this instance 
 

   In the next case, assume that the order of segments A and C in the
   file is reversed:  A is now closer to the front, so data from
   segment C are considered first.


   Case 4:  Pointing is available in the first segment searched.
            Because segment C has the highest priority and can
            satisfy the request, segment A is not searched.

                                           sclkdp
                                          /
                                         |  tol
                                         | /
                                         |/\
   Your request                       [--+--]
                                      .  .  .
                                      .  .  .
   Segment C          (---=============-----====--------==--)
                                           ^
                                           |
                              ckgp_c returns this instance

   Segment A          (0-----------------0--------0--0-----0)
                                         ^
                                         |
                                   "Best" answer


   The next case illustrates an unfortunate side effect of using 
   a non-zero tolerance when reading multi-segment CKs with
   continuous data. In all cases when the look-up interval 
   formed using tolerance overlaps a segment boundary and 
   the request time falls within the coverage of the lower 
   priority segment, the data at the end of the higher priority
   segment will be picked instead of the data from the lower 
   priority segment.


   Case 5:  Pointing is available in the first segment searched.
            Because segment C has the highest priority and can
            satisfy the request, segment A is not searched.

                                           sclkdp
                                          /
                                         |  tol
                                         | /
                                         |/\
   Your request                       [--+--]
                                      .  .  .
                                      .  .  .
   Segment C                                (===============)
                                            ^
                                            |
                              ckgp_c returns this instance

   Segment A          (=====================)
                                         ^
                                         |
                                   "Best" answer

 
-Examples
 

   Suppose you have two C-kernel files containing pointing for the
   Voyager 2 narrow angle camera.  One file contains predict (planned)
   values, and the other contains corrected pointing for a selected
   group of images, that is, for a subset of images from the first
   file.
 
   The following example program uses ckgp_c to get C-matrices for a
   set of images whose SCLK counts (un-encoded character string
   versions) are contained in the array `sclkch'.

   If available, the program will get the corrected pointing values.
   Otherwise, predict values will be used.

   For each C-matrix, a unit pointing vector is constructed and 
   printed.

      #include <stdio.h>
      #include "SpiceUsr.h"
      
      int main ()
      {
         /.
         Constants for this program:
      
         -- The code for the Voyager 2 spacecraft clock is -32
      
         -- The code for the narrow angle camera on the Voyager 2
            spacecraft is -32001.
      
        --  Spacecraft clock times for successive Voyager images always
            differ by more than 0:0:400.  This is an acceptable
            tolerance, and must be converted to "ticks" (units of
            encoded SCLK) for input to ckgp_c.
      
         -- The reference frame we want is FK4.
      
         -- The narrow angle camera boresight defines the third
            axis of the instrument-fixed reference frame.
            Therefore, the vector ( 0, 0, 1 ) represents
            the boresight direction in the camera-fixed frame.
         ./
         
         #define   SC        -32
         #define   INST      -32001
         #define   REF       "FK4"
         #define   TOLVGR    "0:0:400"
         #define   NPICS     2
         #define   MAXCLK    30
         #define   CKPRED    "voyager2_predict.bc"
         #define   CKCORR    "voyager2_corrected.bc"
         #define   SCLK      "voyager2_sclk.tsc"
         
      
         SpiceBoolean            found;
      
         SpiceChar               sclkch  [NPICS][MAXCLK] = 
         
                                         { { "4/08966:30:768" },
                                           { "4/08970:58:768" } };
                                           
         SpiceChar               clkch   [MAXCLK];
      
         SpiceDouble             cmat    [3][3];
         SpiceDouble             clkout;
         SpiceDouble             sclkdp;
         SpiceDouble             toltik;
         SpiceDouble             vinert  [3];
      
         SpiceInt                i;
      
      
         /.
         Loading the files in this order ensures that the 
         corrected file will get searched first. 
         ./
         furnsh_c ( CKPRED ); 
         furnsh_c ( CKCORR ); 
       
         /.
         Need to load a Voyager 2 SCLK kernel to convert from 
         clock string to ticks.  Although not required for
         the Voyager spacecraft clocks, most modern spacecraft
         clocks require a leapseconds kernel to be loaded in
         addition to an SCLK kernel.
         ./
         furnsh_c ( SCLK );
          
         /.
         Convert tolerance from VGR formatted character string 
         SCLK to ticks, which are units of encoded SCLK. 
         ./
         sctiks_c ( SC, TOLVGR, &toltik ); 
      
         for ( i = 0;  i < NPICS;  i++ )
         {
         
            /.
            ckgp_c requires encoded spacecraft clock time. 
            ./
            scencd_c ( SC, sclkch[ i ], &sclkdp ); 
      
            ckgp_c ( INST,  sclkdp,  toltik, REF, 
                     cmat,  &clkout, &found       ); 
      
            if ( found )
            { 
               /.
               The boresight vector, relative to inertial coordinates, 
               is just the third row of the C-matrix. 
               ./
               vequ_c   ( cmat[2], vinert );

               scdecd_c ( SC, clkout, MAXCLK, clkch  ); 
      

               printf ( "VGR 2 SCLK time: %s\n", clkch  );

               printf ( "VGR 2 NA ISS boresight pointing vector: "
                        "%f %f %f\n", 
                        vinert[0], 
                        vinert[1], 
                        vinert[2]                               ); 
            }
            else
            {
               printf ( "Pointing not found for time %s\n", sclkch[i] ); 
            }
      
         }
         
         return ( 0 );
      } 
 
-Restrictions

   None.
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   C.H. Acton     (JPL)
   N.J. Bachman   (JPL)
   W.L. Taber     (JPL) 
   J.M. Lynch     (JPL) 
   B.V. Semenov   (JPL)
   M.J. Spencer   (JPL) 
   R.E. Thurman   (JPL) 
   I.M. Underwood (JPL) 
 
-Version
 
   -CSPICE Version 1.2.3, 03-JUN-2010 (BVS)  

      Header update: description of the tolerance and Particulars
      section were expanded to address some problems arising from
      using a non-zero tolerance.  

   -CSPICE Version 1.2.2, 29-JAN-2004 (NJB)  

      Header update:  description of input argument `ref' was
      expanded.

   -CSPICE Version 1.2.1, 27-JUL-2003 (CHA) (NJB)  

      Various header corrections were made.
   
   -CSPICE Version 1.2.0, 02-SEP-1999 (NJB)  
   
      Local type logical variable now used for found flag used in
      interface of ckgp_.
            
   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)  
   
       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly.  String checks are now done using
       the macro CHKFSTR.
       
   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)
   
      Based on SPICELIB Version 3.0.0, 19-SEP-1994 (WLT)

-Index_Entries
 
   get ck pointing 
 
-&
*/

{ /* Begin ckgp_c */


   /*
   Local variables
   */
   logical                 fnd;
  
   
   /*
   Participate in error tracing.
   */
   chkin_c ( "ckgp_c");
   
   /*
   Check the input string ref to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ckgp_c", ref );
   

   ckgp_( ( integer    * ) &inst, 
          ( doublereal * ) &sclkdp, 
          ( doublereal * ) &tol, 
          ( char       * ) ref, 
          ( doublereal * ) cmat, 
          ( doublereal * ) clkout, 
          ( logical    * ) &fnd, 
          ( ftnlen       ) strlen(ref)  );

   /*
   Assign the SpiceBoolean found flag.
   */
   
   *found = fnd;
   
   
   /*
   Transpose the C-matrix on output.
   */
   xpose_c ( cmat, cmat );
   
      
   chkout_c ( "ckgp_c" );

} /* End ckgp_c */
