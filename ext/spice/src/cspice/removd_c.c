/*

-Procedure removd_c ( Remove an item from a double precision set )

-Abstract
 
   Remove an item from a double precision set.
 
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


   void removd_c ( SpiceDouble     item,
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


   set         is a CSPICE set.  set must be declared as a double
               precision SpiceCell.
 
               On input, set may or may not contain the input item 
               as an element. 
 
-Detailed_Output
 
   set         on output contains the difference of the input set and
               the input item. If the item is not an element of the
               set, the set is not changed.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input set argument is a SpiceCell of type other than
      double precision, the error SPICE(TYPEMISMATCH) is signaled.
 
   2) If the input set argument does not qualify as a CSPICE set, 
      the error SPICE(NOTASET) will be signaled.  CSPICE sets have
      their data elements sorted in increasing order and contain
      no duplicate data elements.
 
-Files
 
   None. 
 
-Particulars
 
   None. 
 
-Examples
 
   1) In the following code fragment, a list of camera exposure
      durations are taken from the array expList and inserted into the
      set expDur.

      We then update the set by removing the element 30.0 and 
      inserting 20.0 in its place.


         #include "SpiceUsr.h"
                .
                .
                .
         /.
         The number of list items is NLIST.
         ./
         SpiceDouble        expList[NLIST] = 
                            { 
                               0.5, 2.0, 0.5, 30.0, 0.01, 30.0 
                            };

         /.
         Declare the set with maximum number of elements MAXSIZ.
         ./
         SPICEDOUBLE_CELL ( expDur,   MAXSIZ );
                .
                .
                .
         for ( i = 0;  i < NLIST;  i++ )
         {
            insrtd_c ( expList[i], &expDur );
         }
        
         /.
         At this point expDur contains the set

           { 0.01, 0.5, 2.0, 30.0 }

         ./
                .
                .
                .
         /. 
         Update the exposure set by replacing 30.0 with 20.0.
         ./
         removd_c ( 30.0, &expDur );
         insrtd_c ( 20.0, &expDur ); 


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
 
   remove an item from a d.p. set 
 
-&
*/
{
   /*
   local variables
   */
   SpiceBoolean            inSet;

   SpiceDouble           * ddata;
   
   SpiceInt                i;
   SpiceInt                loc;


   /*
   Use discovery check-in. 

   Make sure we're working with a double precision cell. 
   */
   CELLTYPECHK ( CHK_DISCOVER, "removd_c", SPICE_DP, set );

   ddata = (SpiceDouble *) (set->data);


   /*
   Make sure the cell is really a set. 
   */
   CELLISSETCHK ( CHK_DISCOVER, "removd_c", set );


   /*
   Initialize the set if necessary. 
   */
   CELLINIT ( set );


   /*
   Is the item in the set? If not, we're done now.
   */
   loc   =  lstled_c ( item,  set->card,  ddata );

   inSet =  (  loc  >  -1  ) && ( item == ddata[loc] );
 
   if ( !inSet )
   {
      return;
   }

   
   /*
   Shift the set's contents to overwrite the slot at index loc.
   */   
   for (  i = loc;   i < (set->card) - 1;   i++  )
   {
      ddata[i] = ddata[i+1];
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

