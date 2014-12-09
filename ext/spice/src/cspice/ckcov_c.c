/*

-Procedure ckcov_c ( CK coverage )

-Abstract
 
   Find the coverage window for a specified object in a specified CK 
   file. 
 
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
 
   CELLS 
   DAF 
   CK 
   TIME 
   WINDOWS 
 
-Keywords
 
   POINTING 
   TIME 
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void ckcov_c ( ConstSpiceChar    * ck,
                  SpiceInt            idcode,
                  SpiceBoolean        needav,
                  ConstSpiceChar    * level,
                  SpiceDouble         tol,
                  ConstSpiceChar    * timsys,
                  SpiceCell         * cover   ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   ck         I   Name of CK file. 
   idcode     I   ID code of object. 
   needav     I   Flag indicating whether angular velocity is needed. 
   level      I   Coverage level:  "SEGMENT" OR "INTERVAL". 
   tol        I   Tolerance in ticks. 
   timsys     I   Time system used to represent coverage. 
   cover     I/O  Window giving coverage for `idcode'. 
 
-Detailed_Input
 
   ck             is the name of a C-kernel. 
    
   idcode         is the integer ID code of an object, normally a
                  spacecraft structure or instrument, for which
                  pointing data are expected to exist in the specified
                  CK file.
 
   needav         is a logical variable indicating whether only
                  segments having angular velocity are to be considered
                  when determining coverage.  When `needav' is
                  SPICETRUE, segments without angular velocity don't
                  contribute to the coverage window; when `needav' is
                  SPICEFALSE, all segments for `idcode' may contribute
                  to the coverage window.
 
 
   level          is the level (granularity) at which the coverage 
                  is examined.  Allowed values and corresponding 
                  meanings are: 
 
                     "SEGMENT"    The output coverage window contains
                                  intervals defined by the start and
                                  stop times of segments for the object
                                  designated by `idcode'.
 
                     "INTERVAL"   The output coverage window contains
                                  interpolation intervals of segments
                                  for the object designated by
                                  `idcode'.  For type 1 segments, which
                                  don't have interpolation intervals,
                                  each epoch associated with a pointing
                                  instance is treated as a singleton
                                  interval; these intervals are added
                                  to the coverage window.

                                  All interpolation intervals are
                                  considered to lie within the segment
                                  bounds for the purpose of this
                                  summary:  if an interpolation
                                  interval extends beyond the segment
                                  coverage interval, only its
                                  intersection with the segment
                                  coverage interval is considered to
                                  contribute to the total coverage.
 
   tol            is a tolerance value expressed in ticks of the
                  spacecraft clock associated with IDCODE.  Before each
                  interval is inserted into the coverage window, the
                  interval is intersected with the segment coverage
                  interval, then if the intersection is non-empty, it
                  is expanded by `tol': the left endpoint of the
                  intersection interval is reduced by `tol' and the
                  right endpoint is increased by `tol'. Adjusted
                  interval endpoints, when expressed as encoded SCLK,
                  never are less than zero ticks.  Any intervals that
                  overlap as a result of the expansion are merged.
 
                  The coverage window returned when tol > 0 indicates
                  the coverage provided by the file to the CK readers
                  ckgpav_c and ckgp_c when that value of `tol' is
                  passed to them as an input.
 
               
   timsys         is a string indicating the time system used in the
                  output coverage window.  `timsys' may have the
                  values:
  
                      "SCLK"    Elements of `cover' are expressed in 
                                encoded SCLK ("ticks"), where the 
                                clock is associated with the object 
                                designated by `idcode'. 
 
                      "TDB"     Elements of `cover' are expressed as 
                                seconds past J2000 TDB. 
 
 
   cover          is an initialized CSPICE window data structure.
                  `cover' optionally may contain coverage data on
                  input; on output, the data already present in `cover'
                  will be combined with coverage found for the object
                  designated by `idcode' in the file `ck'.
 
                  If `cover' contains no data on input, its size and
                  cardinality still must be initialized.
 
-Detailed_Output
 
   cover          is a CSPICE window data structure which represents
                  the merged coverage for `idcode'. When the coverage
                  level is "INTERVAL", this is the set of time
                  intervals for which data for `idcode' are present in
                  the file `ck', merged with the set of time intervals
                  present in `cover' on input.  The merged coverage is
                  represented as the union of one or more disjoint time
                  intervals.  The window `cover' contains the pairs of
                  endpoints of these intervals.
 
                  When the coverage level is "SEGMENT", `cover' is
                  computed in a manner similar to that described above,
                  but the coverage intervals used in the computation
                  are those of segments rather than interpolation
                  intervals within segments.  
 
                  When `tol' is > 0, the intervals comprising the
                  coverage window for `idcode' are expanded by `tol'
                  and any intervals overlapping as a result are merged.
                  The resulting window is returned in `cover'. The
                  expanded window in no case extends beyond the segment
                  bounds in either direction by more than `tol'.
 
                  The interval endpoints contained in `cover' are
                  encoded spacecraft clock times if `timsys' is "SCLK";
                  otherwise the times are converted from encoded
                  spacecraft clock to seconds past J2000 TDB.
 
                  See the Examples section below for a complete example
                  program showing how to retrieve the endpoints from
                  `cover'.
                                     
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the input file has transfer format, the error  
       SPICE(INVALIDFORMAT) is signaled. 
 
   2)  If the input file is not a transfer file but has architecture 
       other than DAF, the error SPICE(BADARCHTYPE) is signaled. 
 
   3)  If the input file is a binary DAF file of type other than 
       CK, the error SPICE(BADFILETYPE) is signaled. 
 
   4)  If the CK file cannot be opened or read, the error will 
       be diagnosed by routines called by this routine. The output 
       window will not be modified. 
 
   5)  If the size of the output window argument `cover' is 
       insufficient to contain the actual number of intervals in the 
       coverage window for `idcode', the error will be diagnosed by 
       routines called by this routine.   
 
   6)  If `tol' is negative, the error SPICE(VALUEOUTOFRANGE) is 
       signaled. 
 
   7)  If `level' is not recognized, the error SPICE(INVALIDOPTION) 
       is signaled. 
 
   8)  If `timsys' is not recognized, the error SPICE(INVALIDOPTION) 
       is signaled. 
 
   9)  If a time conversion error occurs, the error will be  
       diagnosed by a routine in the call tree of this routine. 
 
   10) If the output time system is TDB, the CK subsystem must be 
       able to map `idcode' to the ID code of the associated 
       spacecraft clock.  If this mapping cannot be performed, the 
       error will be diagnosed by a routine in the call tree of this 
       routine. 
  
   11) The error SPICE(EMPTYSTRING) is signaled if any of the input
       strings `ck', `level', or `timsys' do not contain at least one
       character, since such an input string cannot be converted to a
       Fortran-style string in this case.
      
   12) The error SPICE(NULLPOINTER) is signaled if the if any of the input
       strings `ck', `level', or `timsys' are null.


-Files
 
   This routine reads a C-kernel. 
 
   If the output time system is "TDB", then a leapseconds kernel 
   and an SCLK kernel for the spacecraft clock associated with 
   `idcode' must be loaded before this routine is called. 
 
   If the ID code of the clock associated with `idcode' is not  
   equal to  
 
      idcode / 1000 
 
   then the kernel variable  
 
      CK_<idcode>_SCLK 
  
   must be present in the kernel pool to identify the clock 
   associated with `idcode'.  This variable must contain the ID code 
   to be used for conversion between SCLK and TDB. Normally this 
   variable is provided in a text kernel loaded via furnsh_c. 
 
-Particulars
 
   This routine provides an API via which applications can determine 
   the coverage a specified CK file provides for a specified 
   object. 
 
-Examples
 
   1)  Display the interval-level coverage for each object in a
       specified CK file. Use tolerance of zero ticks. Do not request
       angular velocity. Express the results in the TDB time system.
 
       Find the set of objects in the file. Loop over the contents of
       the ID code set:  find the coverage for each item in the set and
       display the coverage.


          #include <stdio.h>
          #include "SpiceUsr.h"

          int main()
          {

             /.
             Local parameters
             ./
             #define  FILSIZ         256
             #define  MAXIV          100000
             #define  WINSIZ         ( 2 * MAXIV )
             #define  TIMLEN         51
             #define  MAXOBJ         1000

             /.
             Local variables
             ./
             SPICEDOUBLE_CELL        ( cover, WINSIZ );
             SPICEINT_CELL           ( ids,   MAXOBJ );

             SpiceChar               ck      [ FILSIZ ];
             SpiceChar               lsk     [ FILSIZ ];
             SpiceChar               sclk    [ FILSIZ ];
             SpiceChar               timstr  [ TIMLEN ];

             SpiceDouble             b;
             SpiceDouble             e;

             SpiceInt                i;
             SpiceInt                j;
             SpiceInt                niv;
             SpiceInt                obj;


             /.
             Load a leapseconds kernel and SCLK kernel for output time
             conversion.  Note that we assume a single spacecraft clock is
             associated with all of the objects in the CK.
             ./
             prompt_c ( "Name of leapseconds kernel > ", FILSIZ, lsk );
             furnsh_c ( lsk );

             prompt_c ( "Name of SCLK kernel        > ", FILSIZ, sclk );
             furnsh_c ( sclk );

             /.
             Get name of CK file.
             ./
             prompt_c ( "Name of CK file            > ", FILSIZ, ck );

             /.
             Find the set of objects in the CK file. 
             ./
             ckobj_c ( ck, &ids );

             /.
             We want to display the coverage for each object. Loop over
             the contents of the ID code set, find the coverage for
             each item in the set, and display the coverage.
             ./
             for ( i = 0;  i < card_c( &ids );  i++  )
             {
                /.
                Find the coverage window for the current object. 
                Empty the coverage window each time so we don't
                include data for the previous object.
                ./
                obj  =  SPICE_CELL_ELEM_I( &ids, i );

                scard_c ( 0,  &cover );  
                ckcov_c ( ck,          obj,  SPICEFALSE, 
                          "INTERVAL",  0.0,  "TDB",       &cover );

                /.
                Get the number of intervals in the coverage window.
                ./
                niv = wncard_c( &cover );

                /.
                Display a simple banner.
                ./
                printf ( "%s\n", "========================================" );

                printf ( "Coverage for object %ld\n", obj );

                /.
                Convert the coverage interval start and stop times to TDB
                calendar strings.
                ./
                for ( j = 0;  j < niv;  j++  )
                {
                   /.
                   Get the endpoints of the jth interval.
                   ./
                   wnfetd_c ( &cover, j, &b, &e );

                   /.
                   Convert the endpoints to TDB calendar
                   format time strings and display them.
                   ./
                   timout_c ( b, 
                              "YYYY MON DD HR:MN:SC.###### (TDB) ::TDB",  
                              TIMLEN,
                              timstr                                    );

                   printf ( "\n"
                            "Interval:  %ld\n"
                            "Start:     %s\n",
                            j,
                            timstr            );

                   timout_c ( e, 
                              "YYYY MON DD HR:MN:SC.###### (TDB) ::TDB",  
                              TIMLEN,
                              timstr                                    );
                   printf ( "Stop:      %s\n", timstr );

                }
                printf ( "%s\n", "========================================" );

             }
             return ( 0 );
          } 


   2)  Find the segment-level coverage for the object designated by
       IDCODE provided by the set of CK files loaded via a metakernel.
       (The metakernel must also specify leapseconds and SCLK kernels.)
       Use tolerance of zero ticks. Do not request angular velocity.
       Express the results in the TDB time system.
 

          #include <stdio.h>
          #include "SpiceUsr.h"

          int main()
          {

             /.
             Local parameters
             ./
             #define  FILSIZ         256
             #define  LNSIZE         81 
             #define  MAXCOV         100000
             #define  WINSIZ         ( 2 * MAXCOV )
             #define  TIMLEN         51

             /.
             Local variables
             ./
             SPICEDOUBLE_CELL        ( cover, WINSIZ );

             SpiceBoolean            found;

             SpiceChar               file    [ FILSIZ ];
             SpiceChar               idch    [ LNSIZE ];
             SpiceChar               meta    [ FILSIZ ];
             SpiceChar               source  [ FILSIZ ];
             SpiceChar               timstr  [ TIMLEN ];
             SpiceChar               type    [ LNSIZE ];

             SpiceDouble             b;
             SpiceDouble             e;

             SpiceInt                count;
             SpiceInt                handle;
             SpiceInt                i;
             SpiceInt                idcode;
             SpiceInt                niv;


             /.
             Prompt for the metakernel name; load the metakernel.
             The metakernel lists the CK files whose coverage
             for `idcode' we'd like to determine.  The metakernel
             must also specify a leapseconds kernel and an SCLK
             kernel for the clock associated with `idcode'.
             ./
             prompt_c ( "Name of metakernel > ", FILSIZ, meta );
             furnsh_c ( meta );

             /.
             Get the ID code of interest. 
             ./
             prompt_c ( "Enter ID code      > ", LNSIZE, idch );
             prsint_c ( idch,  &idcode );

             /.
             Find out how many kernels are loaded.  Loop over the
             kernels:  for each loaded CK file, add its coverage
             for `idcode', if any, to the coverage window.
             ./
             ktotal_c ( "CK", &count );

             for ( i = 0;  i < count;  i++  )
             {
                kdata_c  ( i,           "CK",     FILSIZ,  
                           LNSIZE,      FILSIZ,   file,  
                           type,        source,   &handle,     &found );

                ckcov_c  ( file,        idcode,   SPICEFALSE,
                           "SEGMENT",   0.0,      "TDB",       &cover );
             }

             /.
             Display results. 

             Get the number of intervals in the coverage window.
             ./
             niv = wncard_c( &cover );

             /.
             Display a simple banner.
             ./
             printf ( "\nCoverage for object %ld\n", idcode );

             /.
             Convert the coverage interval start and stop times to TDB
             calendar strings.
             ./
             for ( i = 0;  i < niv;  i++  )
             {
                /.
                Get the endpoints of the ith interval.
                ./
                wnfetd_c ( &cover, i, &b, &e );

                /.
                Convert the endpoints to TDB calendar
                format time strings and display them.
                ./
                timout_c ( b, 
                           "YYYY MON DD HR:MN:SC.###### (TDB) ::TDB",  
                           TIMLEN,
                           timstr                                  );

                printf ( "\n"
                         "Interval:  %ld\n"
                         "Start:     %s\n",
                         i,
                         timstr            );

                timout_c ( e, 
                           "YYYY MON DD HR:MN:SC.###### (TDB) ::TDB",  
                           TIMLEN,
                           timstr                                  );
                printf ( "Stop:      %s\n", timstr );

             }
             return ( 0 );
          }


-Restrictions
 
   1) When this routine is used to accumulate coverage for `idcode' 
      provided by multiple CK files, the inputs `needav', `level', `tol', 
      and `timsys'  must have the same values for all files in order 
      for the result to be meaningful. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.2, 01-JUL-2014 (NJB)

       Updated index entries.

   -CSPICE Version 1.0.1, 30-NOV-2007 (NJB)

       Corrected bug in first example program in header:
       program now empties result window prior to collecting
       data for each object. Updated examples to use wncard_c 
       rather than card_c. Updated second example to demonstrate
       segment-level summary capability.

   -CSPICE Version 1.0.0, 07-JAN-2005 (NJB)

-Index_Entries
 
   get coverage window for ck_object
   get coverage start and stop time for ck_object
   get coverage start and stop time for ck frame
   get coverage start and stop time for ck instrument 

-&
*/

{ /* Begin ckcov_c */


   /*
   Local variables 
   */
   logical                 need;


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return; 
   }
   chkin_c ( "ckcov_c" );

   /*
   Check the input string `ck' to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ckcov_c", ck );
   
   /*
   Check the input string `level' to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ckcov_c", level );

   /*
   Check the input string `timsys' to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ckcov_c", timsys );

   /*
   Make sure cell data type is d.p. 
   */
   CELLTYPECHK ( CHK_STANDARD, "ckcov_c", SPICE_DP, cover );

   /*
   Initialize the cell if necessary. 
   */
   CELLINIT ( cover );   

   /*
   Call the f2c'd Fortran routine.
   */
   need = needav;

   ckcov_ ( ( char       * ) ck,
            ( integer    * ) &idcode,
            ( logical    * ) &need,
            ( char       * ) level,
            ( doublereal * ) &tol,
            ( char       * ) timsys,
            ( doublereal * ) (cover->base),
            ( ftnlen       ) strlen(ck),
            ( ftnlen       ) strlen(level),
            ( ftnlen       ) strlen(timsys)  );

   /*
   Sync the output cell. 
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, cover );
   }

   chkout_c ( "ckcov_c" );

} /* End ckcov_c */
