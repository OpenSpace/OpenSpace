/*

-Procedure appndd_c ( Append an item to a double precision cell )

-Abstract
 
   Append an item to a double precision cell. 
 
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
#include "SpiceZmc.h"


   void appndd_c ( SpiceDouble     item,
                   SpiceCell     * cell )


/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   item       I   The item to append. 
   cell      I/O  The cell to which item will be appended. 
 
-Detailed_Input
 
   item       is an double precision value which is to be appended to
              cell.
 
   cell       is a double precision SpiceCell to which item will be
              appended.
 
-Detailed_Output
 
   cell       is the input SpiceCell with item appended.  item is the 
              last member of cell. 
 
              If cell is actually a CSPICE set on input and ceases to
              qualify as a set as result of the requested append
              operation, the isSet member of cell will be set to
              SPICEFALSE.
-Parameters
 
   None. 
 
-Files
 
   None. 
 
-Exceptions
 
   1) If the input cell argument doesn't have double precision data type,
      the error SPICE(TYPEMISMATCH) is signaled.

   2) If the cell is not big enough to accommodate the addition 
      of a new element, the error SPICE(CELLTOOSMALL) is signaled. 
 
-Particulars
 
   None. 
 
-Examples
 
   1)  In the following example, the element 34 is appended to 
       the double precision cell fibNums. 

         #include "SpiceUsr.h"
                .
                .
                .
         /.
         Declare the cell with maximum number of elements MAXSIZ.
         ./
         SPICEINT_CELL ( fibNums, MAXSIZ );
                .
                .
                .
         /.
         Before appending 34, the cell contains: 

            Element 0: == 1.0
            Element 1: == 1.0
            Element 2: == 2.0
            Element 3: == 3.0 
            Element 4: == 5.0
            Element 5: == 8.0 
            Element 6: == 13.0
            Element 7: == 21.0 

         The following call appends the element 34 at index 8, and 
         updates the cardinality.
         ./
 
         appndd_c ( 34, &fibNums );
 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
  
   N.J. Bachman    (JPL)
   H.A. Neilan     (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 01-AUG-2002 (NJB) (HAN)

-Index_Entries
 
   append an item to a d.p. cell 
 
-&
*/

{ /* Begin appndd_c */


   /*
   Use discovery check-in. 
   */
   
   /*
   Make sure we're working with a DP cell. 
   */
   CELLTYPECHK ( CHK_DISCOVER, "appndd_c", SPICE_DP, cell );


   if ( cell->card == cell->size )
   {
      chkin_c  ( "appndd_c"                                        );
      setmsg_c ( "The cell cannot accommodate the addition of the "
                 "element *"                                       );
      errdp_c  ( "*", item                                         );
      sigerr_c ( "SPICE(CELLTOOSMALL)"                             );
      chkout_c ( "appndd_c"                                        );
      return;
   }


   /*
   Initialize the cell if necessary. 
   */
   CELLINIT ( cell );


   /*
   The item must be strictly greater than its predecessor, or
   the input cell is no longer a set.
   */
   if (  ( cell->isSet ) && ( cell->card > 0 )  )
   {
      if (  item  <=  SPICE_CELL_ELEM_D(cell, cell->card-1)  )
      {
         cell->isSet = SPICEFALSE;
      }
   }


   /*
   Append the item to the cell and increment the cell's cardinality.
   */
   SPICE_CELL_SET_D ( item, cell->card, cell );

   (cell->card) ++;


   /*
   Sync the cell. 
   */
   zzsynccl_c ( C2F, cell );


} /* End appndd_c */

