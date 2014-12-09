/*

-Procedure wnfild_c ( Fill small gaps in a DP window )

-Abstract
 
   Fill small gaps between adjacent intervals of a double precision 
   window. 
 
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
 
   WINDOWS 
 
-Keywords
 
   WINDOWS 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void wnfild_c ( SpiceDouble     small,
                   SpiceCell     * window ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   small      I   Limiting measure of small gaps. 
   window    I,O  Window to be filled. 
 
-Detailed_Input

   small       is the limiting measure of the small gaps to be 
               filled. Adjacent intervals separated by gaps of 
               measure less than or equal to small are merged. 

   window      on input, is a window containing zero or more 
               intervals. 

               window must be declared as a double precision SpiceCell.
 
-Detailed_Output
 
   window      on output, is the original window, after adjacent 
               intervals separated by small gaps have been merged. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input window does not have double precision type,
      the error SPICE(TYPEMISMATCH) is signaled.

   2) If small is less than or equal to zero, this routine has 
      no effect on the window.
 
-Files
 
   None. 
 
-Particulars
 
   This routine removes small gaps between adjacent intervals 
   by merging intervals separated by gaps of measure less than 
   or equal to the limiting measure small. 
 
-Examples
 
   Let window contain the intervals 

      [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ] 

   Then the following series of calls 

      wnfild_c (  1, &window );                        (1) 
      wnfild_c (  2, &window );                        (2) 
      wnfild_c (  3, &window );                        (3) 
      wnfild_c ( 12, &window );                        (4) 

   produces the following series of windows 

      [ 1,  3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]     (1) 
      [ 1,  3 ]  [ 7, 11 ]  [ 23, 29 ]                 (2) 
      [ 1,  3 ]  [ 7, 11 ]  [ 23, 29 ]                 (3) 
      [ 1, 29 ]                                        (4) 

-Restrictions
 
    None. 
 
-Literature_References
 
    None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   H.A. Neilan     (JPL) 
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Version

   -CSPICE Version 1.0.0, 27-JUL-2007 (EDW)
 
      Changed gap size in Examples (4) from 10 to 12 to correct 
      erroneous example.

   -CSPICE Version 1.0.0, 29-JUL-2002 (NJB) (HAN) (WLT) (IMU)

-Index_Entries
 
   fill small gaps in a d.p. window 
 
-&
*/

{ /* Begin wnfild_c */


   /*
   Use discovery check-in.
 
   Make sure cell data type is d.p. 
   */
   CELLTYPECHK ( CHK_DISCOVER, "wnfild_c", SPICE_DP, window );


   /*
   Initialize the cell if necessary. 
   */
   CELLINIT ( window );
   

   /*
   Let the f2c'd routine do the work. 
   */
   wnfild_ ( (doublereal * )  &small,
             (doublereal * )  (window->base) );

   /*
   Sync the output cell. 
   */
   zzsynccl_c ( F2C, window );


} /* End wnfild_c */
