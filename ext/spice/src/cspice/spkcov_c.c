/*

-Procedure spkcov_c ( SPK coverage )

-Abstract
 
   Find the coverage window for a specified ephemeris object in a 
   specified SPK file. 
 
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
   SPK 
   TIME 
   WINDOWS 
 
-Keywords
 
   EPHEMERIS 
   TIME 
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void spkcov_c ( ConstSpiceChar  * spk,
                   SpiceInt          idcode,
                   SpiceCell       * cover   ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   spk        I   Name of SPK file. 
   idcode     I   ID code of ephemeris object. 
   cover      O   Window giving coverage in `spk' for `idcode'. 
 
-Detailed_Input
 
   spk            is the name of an SPK file. 
    
   idcode         is the integer ID code of an object for which 
                  ephemeris data are expected to exist in the 
                  specified SPK file. 
 
   cover          is an initialized CSPICE window data structure. 
                  `cover' optionally may contain coverage data on 
                  input; on output, the data already present in 
                  `cover' will be combined with coverage found for the 
                  object designated by `idcode' in the file `spk'. 
 
                  If `cover' contains no data on input, its size and 
                  cardinality still must be initialized. 
                   
-Detailed_Output
 
   cover          is a CSPICE window data structure which 
                  represents the merged coverage for `idcode'. This is 
                  the set of time intervals for which data for 
                  `idcode' are present in the file `spk', merged with 
                  the set of time intervals present in `cover' on 
                  input.  The merged coverage is represented as the 
                  union of one or more disjoint time intervals. The 
                  window `cover' contains the pairs of endpoints of 
                  these intervals. 
 
                  The interval endpoints contained in `cover' are 
                  ephemeris times, expressed as seconds past J2000 
                  TDB. 
 
                  See the Examples section below for a complete 
                  example program showing how to retrieve the 
                  endpoints from `cover'. 
                                     
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the input file has transfer format, the error  
       SPICE(INVALIDFORMAT) is signaled. 
 
   2)  If the input file is not a transfer file but has architecture 
       other than DAF, the error SPICE(BADARCHTYPE) is signaled. 
 
   3)  If the input file is a binary DAF file of type other than 
       SPK, the error SPICE(BADFILETYPE) is signaled. 
 
   4)  If the SPK file cannot be opened or read, the error will 
       be diagnosed by routines called by this routine. The output 
       window will not be modified. 
       routines called by this routine. 

   5)  If the size of the output window argument `cover' is
       insufficient to contain the actual number of intervals in the
       coverage window for `idcode', the error will be diagnosed by
       routines called by this routine.
 
   6) The error SPICE(EMPTYSTRING) is signaled if the input
      string `spk' does not contain at least one character, since the
      input string cannot be converted to a Fortran-style string in
      this case.
      
   7) The error SPICE(NULLPOINTER) is signaled if the input string
      pointer `spk' is null.

-Files
 
   This routine reads an SPK file.
 
-Particulars
 
   This routine provides an API via which applications can determine 
   the coverage a specified SPK file provides for a specified 
   ephemeris object. 
 
-Examples
 

   1)  Display the coverage for each object in a specified SPK file. 
       Find the set of objects in the file.  Loop over the contents 
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
             SpiceChar               spk     [ FILSIZ ];
             SpiceChar               timstr  [ TIMLEN ];

             SpiceDouble             b;
             SpiceDouble             e;

             SpiceInt                i;
             SpiceInt                j;
             SpiceInt                niv;
             SpiceInt                obj;


             /.
             Load a leapseconds kernel for output time conversion.
             SPKCOV itself does not require a leapseconds kernel.
             ./
             prompt_c ( "Name of leapseconds kernel > ", FILSIZ, lsk );
             furnsh_c ( lsk );

             /.
             Get name of SPK file.
             ./
             prompt_c ( "Name of SPK file           > ", FILSIZ, spk    );

             /.
             Find the set of objects in the SPK file. 
             ./
             spkobj_c ( spk, &ids );

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

                scard_c  ( 0,        &cover );
                spkcov_c ( spk, obj, &cover );

                /.
                Get the number of intervals in the coverage window.
                ./
                niv = wncard_c ( &cover );

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
 

   2) Find the coverage for the object designated by `idcode' 
      provided by the set of SPK files loaded via a metakernel. 
      (The metakernel must also specify a leapseconds kernel.) 
        
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
            The metakernel lists the SPK files whose coverage
            for `idcode' we'd like to determine.  The metakernel
            must also specify a leapseconds kernel.
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
            kernels:  for each loaded SPK file, add its coverage
            for `idcode', if any, to the coverage window.
            ./
            ktotal_c ( "SPK", &count );

            for ( i = 0;  i < count;  i++  )
            {
               kdata_c  ( i,     "SPK",   FILSIZ,  LNSIZE,   FILSIZ, 
                          file,  type,    source,  &handle,  &found );

               spkcov_c ( file,  idcode,  &cover );
            }

            /.
            Display results. 

            Get the number of intervals in the coverage window.
            ./
            niv = wncard_c ( &cover );

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
                          "YYYY MON DD HR:MN:SC.### (TDB) ::TDB",  
                          TIMLEN,
                          timstr                                  );

               printf ( "\n"
                        "Interval:  %ld\n"
                        "Start:     %s\n",
                        i,
                        timstr            );

               timout_c ( e, 
                          "YYYY MON DD HR:MN:SC.### (TDB) ::TDB",  
                          TIMLEN,
                          timstr                                  );
               printf ( "Stop:      %s\n", timstr );

            }
            return ( 0 );
         }


-Restrictions
 
   1) If an error occurs while this routine is updating the window 
      `cover', the window may be corrupted. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.2, 01-JUL-2014 (NJB)

       Added new index entries.

   -CSPICE Version 1.0.1, 30-NOV-2007 (NJB)

       Corrected bug in first example program in header:
       program now empties result window prior to collecting
       data for each object. Deleted declaration of unused
       constant NAMLEN. Updated examples to use wncard_c 
       rather than card_c.

   -CSPICE Version 1.0.0, 30-DEC-2004 (NJB)

-Index_Entries
 
   get coverage window for spk_object 
   get coverage start and stop time for spk_object
   get coverage start and stop time for ephemeris_object
   get coverage start and stop time for body
 
-&
*/

{ /* Begin spkcov_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return; 
   }
   chkin_c ( "spkcov_c" );


   /*
   Check the input string `spk' to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkcov_c", spk );
   
   /*
   Make sure cell data type is d.p. 
   */
   CELLTYPECHK ( CHK_STANDARD, "spkcov_c", SPICE_DP, cover );

   /*
   Initialize the cell if necessary. 
   */
   CELLINIT ( cover );   

   /*
   Call the f2c'd Fortran routine.
   */
   spkcov_ ( ( char       * ) spk,
             ( integer    * ) &idcode,
             ( doublereal * ) (cover->base),
             ( ftnlen       ) strlen(spk)   );

   /*
   Sync the output cell. 
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, cover );
   }

   chkout_c ( "spkcov_c" );

} /* End spkcov_c */
