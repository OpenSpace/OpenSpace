/*

-Procedure pckfrm_c ( PCK reference frame class ID set )

-Abstract
 
   Find the set of reference frame class ID codes of all frames  
   in a specified binary PCK file. 
 
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
   SETS 
   PCK 
 
-Keywords
 
   ORIENTATION 
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void pckfrm_c ( ConstSpiceChar  * pck,
                   SpiceCell       * ids  ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   pck        I   Name of PCK file. 
   ids       I/O  Set of frame class ID codes of frames in PCK file. 
 
-Detailed_Input
 
   pck            is the name of a binary PCK file.
 
   ids            is an initialized CSPICE set data structure. `ids'
                  optionally may contain a set of ID codes on input; on
                  output, the data already present in `ids' will be
                  combined with ID code set found for the file PCK.
 
                  If `ids' contains no data on input, its size and
                  cardinality still must be initialized.
 
-Detailed_Output
 
   ids            is a CSPICE set data structure which contains the
                  union of its contents upon input with the set of
                  reference frame class ID codes of each frame for
                  which data are present in the indicated PCK file. The
                  elements of CSPICE sets are unique; hence each ID
                  code in `ids' appears only once, even if the PCK file
                  contains multiple segments for that ID code.
 
                  See the Examples section below for a complete example
                  program showing how to retrieve the ID codes from
                  `ids'.
                                     
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the input file has transfer format, the error  
       SPICE(INVALIDFORMAT) is signaled. 
 
   2)  If the input file is not a transfer file but has architecture 
       other than DAF, the error SPICE(BADARCHTYPE) is signaled. 
 
   3)  If the input file is a binary DAF file of type other than 
       PCK, the error SPICE(BADFILETYPE) is signaled. 
 
   4)  If the PCK file cannot be opened or read, the error will 
       be diagnosed by routines called by this routine. 
 
   5)  If the size of the output set argument `ids' is insufficient to 
       contain the actual number of ID codes of frames covered by 
       the indicated PCK file, the error will be diagnosed by 
       routines called by this routine. 
 
   6)  The error SPICE(EMPTYSTRING) is signaled if the input string
       `pck' does not contain at least one character, since the input
       string cannot be converted to a Fortran-style string in this
       case.
 
   7)  The error SPICE(NULLPOINTER) is signaled if the input string
       pointer `pck' is null.

-Files
 
   None. 
 
-Particulars
 
   This routine provides an API via which applications can determine 
   the set of reference frames for which there are data in a 
   specified PCK file. 
 
-Examples
 
   1)  Display the coverage for each frame in a specified PCK file. 
       Find the set of frames in the file.  Loop over the contents 
       of the ID code set:  find the coverage for each item in the 
       set and display the coverage. 
 

          #include <stdio.h>
          #include "SpiceUsr.h"

          int main()
          {
             /.
             Local parameters
             ./
             #define  FILSIZ         256
             #define  MAXIV          1000
             #define  WINSIZ         ( 2 * MAXIV )
             #define  TIMLEN         51
             #define  MAXOBJ         1000

             /.
             Local variables
             ./
             SPICEDOUBLE_CELL        ( cover, WINSIZ );
             SPICEINT_CELL           ( ids,   MAXOBJ );

             SpiceChar               lsk     [ FILSIZ ];
             SpiceChar               pck     [ FILSIZ ];
             SpiceChar               timstr  [ TIMLEN ];

             SpiceDouble             b;
             SpiceDouble             e;

             SpiceInt                i;
             SpiceInt                j;
             SpiceInt                niv;
             SpiceInt                obj;


             /.
             Load a leapseconds kernel for output time conversion.
             PCKCOV itself does not require a leapseconds kernel.
             ./
             prompt_c ( "Name of leapseconds kernel > ", FILSIZ, lsk );
             furnsh_c ( lsk );

             /.
             Get name of PCK file.
             ./
             prompt_c ( "Name of PCK file           > ", FILSIZ, pck    );

             /.
             Find the set of frames in the PCK file. 
             ./
             pckfrm_c ( pck, &ids );

             /.
             We want to display the coverage for each frame. Loop over
             the contents of the ID code set, find the coverage for
             each item in the set, and display the coverage.
             ./
             for ( i = 0;  i < card_c( &ids );  i++  )
             {
                /.
                Find the coverage window for the current frame. 
                Empty the coverage window each time so we don't
                include data for the previous frame.
                ./
                obj  =  SPICE_CELL_ELEM_I( &ids, i );

                scard_c  ( 0,        &cover );
                pckcov_c ( pck, obj, &cover );

                /.
                Get the number of intervals in the coverage window.
                ./
                niv = wncard_c ( &cover );

                /.
                Display a simple banner.
                ./
                printf ( "%s\n", "========================================" );

                printf ( "Coverage for frame %ld\n", obj );

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
                              "YYYY MON DD HR:MN:SC.### (TDB) ::TDB",  
                              TIMLEN,
                              timstr                                  );

                   printf ( "\n"
                            "Interval:  %ld\n"
                            "Start:     %s\n",
                            j,
                            timstr            );

                   timout_c ( e, 
                              "YYYY MON DD HR:MN:SC.### (TDB) ::TDB",  
                              TIMLEN,
                              timstr                                  );
                   printf ( "Stop:      %s\n", timstr );

                }

             }
             return ( 0 );
          } 

 
-Restrictions
 
   1) If an error occurs while this routine is updating the set 
      `ids', the set may be corrupted. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 01-DEC-2007 (NJB)

-Index_Entries
 
   find frame class id codes of frames in binary pck file 
 
-&
*/

{ /* Begin pckfrm_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return; 
   }
   chkin_c ( "pckfrm_c" );


   /*
   Check the input string `pck' to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "pckfrm_c", pck );
   
   /*
   Make sure cell data type is SpiceInt. 
   */
   CELLTYPECHK ( CHK_STANDARD, "pckfrm_c", SPICE_INT, ids );

   /*
   Initialize the cell if necessary. 
   */
   CELLINIT ( ids );   

   /*
   Call the f2c'd Fortran routine.
   */
   pckfrm_ ( ( char       * ) pck,
             ( integer    * ) (ids->base),
             ( ftnlen       ) strlen(pck)   );

   /*
   Sync the output cell. 
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, ids );
   }

   chkout_c ( "pckfrm_c" );

} /* End pckfrm_c */
