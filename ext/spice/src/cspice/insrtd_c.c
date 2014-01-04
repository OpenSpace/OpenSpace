/*

-Procedure insrtd_c ( Insert an item into a double precision set )

-Abstract
 
   Insert an item into a double precision set. 
 
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


   void insrtd_c ( SpiceDouble     item,
                   SpiceCell     * set  )

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   item       I   Item to be inserted. 
   set       I/O  Insertion set. 
 
-Detailed_Input
 
   item        is an item which is to be inserted into the 
               specified set. item may or may not already 
               be an element of the set. 


   set         is a CSPICE set.  set must be declared as a double
               precision SpiceCell. 

               On input, set  may or may not contain the input item 
               as an element. 
 
-Detailed_Output

   set         on output contains the union of the input set and 
               the singleton set containing the input item.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input set argument is a SpiceCell of type other than
      double precision, the error SPICE(TYPEMISMATCH) is signaled.

   2) If the insertion of the element into the set causes an excess 
      of elements, the error SPICE(SETEXCESS) is signaled. 
 
   3) If the input set argument does not qualify as a CSPICE set, 
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
 
   -CSPICE Version 2.0.0, 01-NOV-2005 (NJB)

       Long error message was updated to include size of
       set into which insertion was attempted.

   -CSPICE Version 1.0.0, 07-AUG-2002 (NJB) (CAC) (WLT) (IMU)

-Index_Entries
 
   insert an item into a d.p. set 
 
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
   */
   
   /*
   Make sure we're working with a double precision cell. 
   */
   CELLTYPECHK ( CHK_DISCOVER, "insrtd_c", SPICE_DP, set );

   ddata = (SpiceDouble *) (set->data);

   /*
   Make sure the input cell is a set.
   */
   CELLISSETCHK ( CHK_DISCOVER, "insrtd_c", set );


   /*
   Initialize the set if necessary. 
   */
   CELLINIT ( set );


   /*
   Is the item already in the set? If not, it needs to be inserted.
   */
   loc   =  lstled_c ( item,  set->card,  ddata );

   inSet =  (  loc  >  -1  ) && ( item == ddata[loc] );
 
   if ( inSet )
   {
      return;
   }

   
   /*
   It's an error if the set has no room left. 
   */

   if ( set->card == set->size )
   {
      chkin_c  ( "insrtd_c"                                       );
      setmsg_c ( "An element could not be inserted into the set "
                 "due to lack of space; set size is #."           );
      errint_c ( "#", set->size                                   );
      sigerr_c ( "SPICE(SETEXCESS)"                               );
      chkout_c ( "insrtd_c"                                       );
      return;
   }


   /*
   Make room by moving the items that come after index loc in the set. 
   Insert the item after index loc.
   */
   
   for (  i = (set->card);   i > loc+1;   i--  )
   {
      ddata[i] = ddata[i-1];
   }

   ddata[loc+1] = item;


   /*
   Increment the set's cardinality.
   */
   (set->card) ++;


   /*
   Sync the set. 
   */
   zzsynccl_c ( C2F, set );
}

