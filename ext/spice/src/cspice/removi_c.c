/*

-Procedure removi_c ( Remove an item from an integer set )

-Abstract
 
   Remove an item from an integer set.
 
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
 
   SETS 
 
-Keywords
 
   CELLS, SETS 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void removi_c ( SpiceInt        item,
                   SpiceCell     * set  )


/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   item       I   Item to be removed. 
   set       I/O  Removal set. 
 
-Detailed_Input
 
   item        is an item which is to be removed from the specified
               set. item may or may not already be an element of the
               set.


   set         is a CSPICE set.  set must be declared as an integer
               SpiceCell.
 
               On input, set may or may not contain the input item 
               as an element. 
 
-Detailed_Output
 
   set          on output contains the difference of the input set and
                the input item. If the item is not an element of the
                set, the set is not changed.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input set argument is a SpiceCell of type other than
      integer, the error SPICE(TYPEMISMATCH) is signaled.
 
   2) If the input set argument does not qualify as a CSPICE set, 
      the error SPICE(NOTASET) will be signaled.  CSPICE sets have
      their data elements sorted in increasing order and contain
      no duplicate data elements.
 
-Files
 
   None. 
 
-Particulars
 
   None. 
 
-Examples
 
   1) In the following example, the NAIF ID code of Pluto is removed from 
      the integer set planets and inserted into the integer set 
      asteroids. 

         #include "SpiceUsr.h"
                .
                .
                .
         /.
         Declare the sets with maximum number of elements MAXSIZ.
         ./
         SPICEINT_CELL ( planets,   MAXSIZ );
         SPICEINT_CELL ( asteroids, MAXSIZ );
                .
                .
                .
         removi_c ( 999, &planets   );
         insrti_c ( 999, &asteroids ); 


      If 999 is not an element of planets, then the contents of planets
      are not changed. Similarly, if 999 is already an element of
      asteroids, the contents of asteroids remain unchanged.
 
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
 
   -CSPICE Version 1.0.0, 07-AUG-2002 (NJB) (CAC) (WLT) (IMU)

-Index_Entries
 
   remove an item from an integer set 
 
-&
*/
{
   /*
   local variables
   */
   SpiceBoolean            inSet;
   
   SpiceInt                i;
   SpiceInt              * idata;
   SpiceInt                loc;


   /*
   Use discovery check-in. 
   */
   
   /*
   Make sure we're working with an integer cell. 
   */
   CELLTYPECHK ( CHK_DISCOVER, "removi_c", SPICE_INT, set );

   idata = (SpiceInt *) (set->data);


   /*
   Make sure the cell is really a set. 
   */
   CELLISSETCHK ( CHK_DISCOVER, "removi_c", set );


   /*
   Initialize the set if necessary. 
   */
   CELLINIT ( set );


   /*
   Is the item in the set? If not, we're done now.
   */
   loc   =  lstlei_c ( item,  set->card,  idata );

   inSet =  (  loc  >  -1  ) && ( item == idata[loc] );
 
   if ( !inSet )
   {
      return;
   }

   
   /*
   Shift the set's contents to overwrite the slot at index loc.
   */   
   for (  i = loc;   i < (set->card) - 1;   i++  )
   {
      idata[i] = idata[i+1];
   }


   /*
   Decrement the set's cardinality.
   */
   (set->card) --;


   /*
   Sync the set. 
   */
   zzsynccl_c ( C2F, set );
}

