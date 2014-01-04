/*

-Procedure scard_c ( Set the cardinality of a cell )

-Abstract
 
   Set the cardinality of a SPICE cell of any data type. 
 
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
 
-Keywords
 
   CELLS 
 
*/

#include "SpiceUsr.h"
#include "SpiceCel.h"
#include "SpiceZmc.h"

   void scard_c (  SpiceInt      card,   
                   SpiceCell   * cell  )
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   card       I   Cardinality of (number of elements in) the cell. 
   cell       O   The cell. 
 
-Detailed_Input
 
   card         is the cardinality of (number of elements in) the 
                cell. 
 
-Detailed_Output
 
   cell         is a SpiceCell of any data type. On output, the
                cardinality of the cell is card.  The data portion of
                the cell is left unchanged.
 
                If the cardinality is set to zero, the cell becomes a
                CSPICE set:  the cell's "is a set?" attribute becomes
                true.  The cell then can be used as an input to the
                CSPICE set routines such as insrt*_c.

-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the cardinality value supplied is less than 0 or greater 
       than the cell size, the error SPICE(INVALIDCARDINALITY) is 
       signaled. 
 
-Files
 
   None. 
 
-Particulars
 
   The set cardinality (scard_c) and set size (ssize_c) routines are
   typically used to initialize cells for subsequent use. 
 
   The set cardinality routines are also used by library routines 
   which manipulate cells (including set and window routines) to 
   reset the cardinalities of cells as they gain or lose elements. 
 
-Examples
 
   1) Declare an integer cell.  Populate the cell, then reset
      the cardinality to zero to effectively make room in the 
      cell.

         #include "SpiceUsr.h"
               .
               .
               .

         #define SIZE          10

         Spiceint              i;

         /.
         Declare a cell with room for SIZE integers.
         ./
         SPICEINT_CELL         ( icell, SIZE );


         /.
         Fill in the cell with integers 0 through 9.
         ./

         for ( i = 0;  i < SIZE;  i++ )
         {
            appndi_c ( i, &icell );
         }
               .
               .
               .
         /.
         Make room in the cell.
         ./
         scard_c ( 0, &icell );
               .
               .
               .

        
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
   C.A. Curzon     (JPL) 
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 21-AUG-2002 (NJB) (CAC) (WLT) (IMU)

-Index_Entries
 
   set the cardinality of an integer cell 
 
-&
*/
{ /* Begin scard_c */


   /*
   Use discovery check-in. 
   */
   if ( return_c() )
   {
      return;
   }


   /*
   Initialize the cell if necessary. 
   */
   CELLINIT ( cell );


   /*
   Make sure we have a valid cardinality value. 
   */
   if (  ( card < 0 ) || ( card > cell->size )  )
   {
      chkin_c  ( "scard_c"                                        );
      setmsg_c ( "Attempt to set cardinality of cell to invalid "
                 "value #.  Valid range is 0:#."                  );
      errint_c ( "#",  card                                       );
      errint_c ( "#",  cell->size                                 );
      sigerr_c ( "SPICE(INVALIDCARDINALITY)"                      );
      chkout_c ( "scard_c"                                        );
      return;
   }

   /*
   Set the cell's cardinality member.  For numeric cells, sync
   the Fortran cell's cardinality value. 
   */
   cell->card  =  card;


   if ( cell->dtype != SPICE_CHR )
   {
      zzsynccl_c ( C2F, cell );
   }
   
   /*
   The cell becomes a set if it's empty.
   */
   if ( card == 0 )
   {
      cell->isSet = SPICETRUE;
   }


} /* End scard_c */

