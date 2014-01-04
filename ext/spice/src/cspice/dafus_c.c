/*

-Procedure dafus_c ( DAF, unpack summary )

-Abstract
 
   Unpack an array summary into its double precision and integer 
   components. 
 
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
   #undef    dafus_c
   

   void dafus_c ( ConstSpiceDouble   sum [],
                  SpiceInt           nd,
                  SpiceInt           ni,
                  SpiceDouble        dc  [],
                  SpiceInt           ic  []  )

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   sum        I   Array summary. 
   nd         I   Number of double precision components. 
   ni         I   Number of integer components. 
   dc         O   Double precision components. 
   ic         O   Integer components. 
 
-Detailed_Input
 
   sum         is an array summary. This identifies the contents and 
               location of a single array within a DAF. 
 
   nd          is the number of double precision components in 
               the summary. 
 
   ni          is the number of integer components in the summary. 
 
-Detailed_Output
 
   dc          are the double precision components of the summary. 
 
   ic          are the integer components of the summary. 
 
-Parameters
 
    None. 
 
-Files
 
   None. 
 
-Exceptions
 
   Error free. 
 
   1) If nd is zero or negative, no double precision components 
      are returned. 
 
   2) If ni is zero or negative, no integer components are returned. 
 
   3) If the total size of the summary is greater than 125 double 
      precision words, some components may not be returned. 
 
-Particulars
 
   The components of array summaries are packed into double 
   precision arrays for reasons outlined in [1]. Two routines, 
   DAFPS (pack summary) and dafus_c (unpack summary) are provided 
   for packing and unpacking summaries. 
 
   The total size of the summary is 
 
           (ni - 1) 
      nd + -------- + 1 
               2 
 
   double precision words (where nd, ni are nonnegative). 
 
-Examples


   In the following code fragment, dafopr_c is used to open a file, 
   which is then searched for DAFs containing data for a particular 
   object.  dafus_c is used to unpack the summaries so the applicability
   of the segments can be determined.
   
 
      #include "SpiceUsr.h"
           .
           .
           .
      dafopr_c ( fname, &handle ); 
      dafbfs_c ( handle );
       
      daffna_c ( &found );
 
      while ( found ) 
      {
         dafgs_c ( sum );
         dafus_c ( sum, ND, NI, dc, ic );
 
         if ( ic[0] == target_object ) 
         { 
            . 
            . 
            . 
         }
 
         daffna_c ( &found );
      }
      
 
-Restrictions
 
   None. 
 
-Literature_References
 
   NAIF Document 167.0, "Double Precision Array Files (DAF) 
   Specification and User's Guide" 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 01-AUG-1999 (NJB), (IMU)

-Index_Entries
 
   unpack daf summary 
 
-&
*/

{ /* Begin dafus_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dafus_c" );


   dafus_ (  ( doublereal  * ) sum,
             ( integer     * ) &nd,
             ( integer     * ) &ni,
             ( doublereal  * ) dc,
             ( integer     * ) ic  );
             

   chkout_c ( "dafus_c" );

} /* End dafus_c */
