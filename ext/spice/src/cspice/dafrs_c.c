/*

-Procedure dafrs_c ( DAF, replace summary )

-Abstract
 
   Change the summary for the current array in the current DAF. 
 
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
 
   FILES 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZim.h"
   #undef   dafrs_c


   void dafrs_c ( ConstSpiceDouble  * sum ) 


/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   sum        I   New summary for current array. 
 
-Detailed_Input
 
   sum         is the new summary for the current array. This 
               replaces the existing summary. However, the addresses 
               (the final two integer components) of the original 
               summary are not changed. 
 
-Detailed_Output
 
   None. 
 
-Parameters
 
   None. 
 
-Files
 
   This routine operates on a DAF opened for write access.  A search
   must be in progress at the time this routine is called; this 
   routine replaces the descriptor of the current segment.
 
-Exceptions
 
   1)  If this routine is called when no search is in progress in the 
       the current DAF, the error SPICE(DAFNOSEARCH) is signaled. 
 
   2)  If the DAF containing the `current' array has actually been 
       closed, the error will be diagnosed by routines called by 
       this routine. 
 
   3)  If the DAF containing the `current' array is not open for 
       writing, the error will be diagnosed by routines called by 
       this routine. 
 
   4)  If no array is current in the current DAF, the error 
       SPICE(NOCURRENTARRAY) is signaled.  There is no current 
       array when a search is started by dafbfs_c or dafbbs_c, but no 
       calls to daffna_c or dafbna_ have been made yet, or whenever 
       daffna_c or daffpa_c return the value SPICEFALSE in the `found' 
       argument. 
 
-Particulars
 
   See SPICELIB umbrella routine DAFFA. 
 
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
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 23-NOV-2004 (NJB)

-Index_Entries
 
   replace daf summary 
 
-&
*/

{ /* Begin dafrs_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "dafrs_c" );

   /*
   Not much to it. 
   */
   dafrs_ ( (doublereal *) sum );


   chkout_c ( "dafrs_c" );

} /* End dafrs_c */
