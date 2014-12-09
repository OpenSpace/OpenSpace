/*

-Procedure elemd_c ( Element of a double precision set )

-Abstract
 
   Determine whether an item is an element of a double precision set. 
 
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


   SpiceBoolean elemd_c ( SpiceDouble     item,
                          SpiceCell     * set   )

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   item       I   Item to be tested. 
   set        I   Set to be tested. 

   The function returns SPICETRUE if item is an element of set. 
 
-Detailed_Input
 
   item        is an item which may or may not be an element of 
               the input set. 
 
 
   set         is a CSPICE set.  set must be declared as a double
               precision SpiceCell.  
 
-Detailed_Output

   The function returns SPICETRUE if item is a member of the set, 
   and returns SPICEFALSE otherwise. 
 
-Parameters
 
   None. 
 
-Exceptions
  
   1) If the input set argument does not qualify as a CSPICE set, 
      the error SPICE(NOTASET) will be signaled.  CSPICE sets have
      their data elements sorted in increasing order and contain
      no duplicate data elements.

   2) If the input set does not have double precision data type, 
      the error SPICE(TYPEMISMATCH will be signaled.
 
-Files
 
   None. 
 
-Particulars
 
   This routine uses a binary search to check for the presence in the set
   of the specified item.
 
-Examples
 
   Let set contain the elements

      { -1.0, 0.0, 1.0, 3.0, 5.0 }

   The the following expressions have the value SPICETRUE
      
      elemd_c ( -1.0, &set )
      elemd_c (  0.0, &set )
      elemd_c (  3.0, &set )

   and the following expressions have the value SPICEFALSE

      elemd_c ( -2.0, &set )
      elemd_c (  2.0, &set )
      elemd_c (  6.0, &set )

-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
   C.A. Curzon     (JPL) 
   H.A. Neilan     (JPL) 
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 07-AUG-2002 (NJB) (CAC) (HAN) (WLT) (IMU)

-Index_Entries
 
   element of a d.p. set 
 
-&
*/
{

   /*
   Use discovery check-in. 

   Make sure we're working with a double precision cell. 
   */
   CELLTYPECHK_VAL ( CHK_DISCOVER, "elemd_c", SPICE_DP, set, SPICEFALSE );

   /*
   Make sure the input cell is a set.
   */
   CELLISSETCHK_VAL ( CHK_DISCOVER, "elemd_c", set, SPICEFALSE );

   /*
   Initialize the set if necessary. 
   */
   CELLINIT ( set );

   /*
   The routine bsrchd_c returns the index of the item in the set,
   or -1 if the item is not present.
   */
   return (  bsrchd_c ( item,  set->card,  set->data )  !=  -1 );
}


