/*

-Procedure dafps_c ( DAF, pack summary )

-Abstract
 
   Pack (assemble) an array summary from its double precision and 
   integer components. 
 
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
 
   DAF 
 
-Keywords
 
   CONVERSION 
   FILES 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZim.h"
   #undef   dafps_c


   void dafps_c ( SpiceInt             nd,
                  SpiceInt             ni,
                  ConstSpiceDouble   * dc,
                  ConstSpiceInt      * ic,
                  SpiceDouble        * sum ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   nd         I   Number of double precision components. 
   ni         I   Number of integer components. 
   dc         I   Double precision components. 
   ic         I   Integer components. 
   sum        O   Array summary. 
 
-Detailed_Input
 
   nd          is the number of double precision components in 
               the summary to be packed. 
 
   ni          is the number of integer components in the summary. 
 
   dc          are the double precision components of the summary. 
 
   ic          are the integer components of the summary. 
 
-Detailed_Output
 
   sum         is an array summary containing the components in `dc' 
               and `ic'. This identifies the contents and location of 
               a single array within a DAF. 
 
-Parameters
 
   None. 
 
-Files
 
   None. 
 
-Exceptions
 
   Error free. 
 
   1) If ND is zero or negative, no DP components are stored. 
 
   2) If NI is zero or negative, no integer components are stored. 
 
   3) If the total size of the summary is greater than 125 double 
      precision words, some components may not be stored. 
 
-Particulars
 
   The components of array summaries are packed into double 
   precision arrays for reasons outlined in [1]. Two routines, 
   dafps_c (pack summary) and dafus_c (unpack summary) are provided 
   for packing and unpacking summaries. 
 
   The total size of the summary is 
 
           (NI - 1) 
      ND + -------- + 1 
               2 
 
   double precision words (where ND, NI are nonnegative). 
 
-Examples
 
 
   1) Replace the body ID code -999 with -1999 in every descriptor
      of an SPK file.


      #include <SpiceUsr.h>
 
      int main ( int argc,  char **argv )
      {
         #define ND              2
         #define NI              6
         #define DSCSIZ          5
         #define NEWCODE         ( -1999 )
         #define OLDCODE         ( -999  )

         SpiceBoolean            found;

         SpiceInt                handle;
         SpiceInt                ic      [ NI ];

         SpiceDouble             dc      [ ND ];
         SpiceDouble             sum     [ DSCSIZ ];

         /.
         Open for writing the SPK file specified on the command line.
         ./
         dafopw_c ( argv[1], &handle );
      
         /.
         Search the file in forward order.
         ./
         dafbfs_c ( handle );
         daffna_c ( &found );

         while ( found )
         {
            /.
            Fetch and unpack the descriptor (aka summary)
            of the current segment.
            ./
            dafgs_c ( sum  );
            dafus_c ( sum, ND, NI, dc, ic );
          
            /.
            Replace ID codes if necessary.
            ./
            if ( ic[0] == OLDCODE )
            {
               ic[0] = NEWCODE;
            }
            if ( ic[1] == OLDCODE )
            {
               ic[1] = NEWCODE;
            }

            /.
            Re-pack the descriptor; replace the descriptor
            in the file.
            ./
            dafps_c ( ND, NI, dc, ic, sum );

            dafrs_c ( sum );
 
            /.
            Find the next segment.
            ./
            daffna_c ( &found );
         }

         /.
         Close the file.
         ./
         dafcls_c ( handle );

         return ( 0 );
      }
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None.
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 23-NOV-2004 (NJB)

-Index_Entries
 
   pack daf summary 
 
-&
*/

{ /* Begin dafps_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "dafps_c" );


   dafps_ (  ( integer    * ) &nd,
             ( integer    * ) &ni,
             ( doublereal * ) dc,
             ( integer    * ) ic,
             ( doublereal * ) sum  );


   chkout_c ( "dafps_c" );

} /* End dafps_c */
