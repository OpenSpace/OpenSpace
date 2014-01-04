/*

-Procedure wninsd_c ( Insert an interval into a DP window )

-Abstract
 
   Insert an interval into a double precision window. 
 
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

   void wninsd_c ( SpiceDouble     left,
                   SpiceDouble     right,
                   SpiceCell     * window ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   left, 
   right      I   Left, right endpoints of new interval. 
   window    I,O  Input, output window. 
 
-Detailed_Input
 
   left, 
   right       are the left and right endpoints of the interval 
               to be inserted. 

   window      on input, is a CSPICE window containing zero or more 
               intervals. 
 
               window must be declared as a double precision
               SpiceCell.

-Detailed_Output
 
   window      on output, is the original window following the 
               insertion of the interval from left to right. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input window does not have double precision type,
      the error SPICE(TYPEMISMATCH) is signaled.

   2) If left is greater than right, the error SPICE(BADENDPOINTS) is 
      signaled. 
 
   3) If the insertion of the interval causes an excess of elements, 
      the error SPICE(WINDOWEXCESS) is signaled. 
 
-Files
 
   None. 
 
-Particulars
 
   This routine inserts the interval from left to right into the 
   input window. If the new interval overlaps any of the intervals 
   in the window, the intervals are merged. Thus, the cardinality 
   of the input window can actually decrease as the result of an 
   insertion. However, because inserting an interval that is 
   disjoint from the other intervals in the window can increase the 
   cardinality of the window, the routine signals an error. 
 
   No other CSPICE unary window routine can increase the number of
   intervals in the input window.

-Examples
 
    Let window contain the intervals 
 
       [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ] 
 
    Then the following series of calls 
 
       wninsd_c ( 5.0,  5.0, &window )                  (1) 
       wninsd_c ( 4.0,  8.0, &window )                  (2) 
       wninsd_c ( 0.0, 30.0, &window )                  (3) 
 
    produces the following series of windows 

       [ 1,  3 ]  [ 5,  5 ]  [  7, 11 ]  [ 23, 27 ]     (1) 
       [ 1,  3 ]  [ 4, 11 ]  [ 23, 27 ]                 (2) 
       [ 0, 30 ]                                        (3) 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
   K.R. Gehringer  (JPL) 
   H.A. Neilan     (JPL) 
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 29-JUL-2002 (NJB) (KRG) (HAN) (WLT) (IMU)

-Index_Entries
 
   insert an interval into a d.p. window 
 
-&
*/

{ /* Begin wninsd_c */


   /*
   Standard SPICE error handling. 
   */

   if ( return_c() )
   {
      return;
   }
   chkin_c ( "wninsd_c" );


   /*
   Make sure cell data type is d.p. 
   */
   CELLTYPECHK ( CHK_STANDARD, "wninsd_c", SPICE_DP, window );


   /*
   Initialize the cell if necessary. 
   */
   CELLINIT ( window );
   

   /*
   Let the f2c'd routine do the work. 
   */
   wninsd_ ( (doublereal * )  &left,
             (doublereal * )  &right,
             (doublereal * )  (window->base) );

   /*
   Sync the output cell. 
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, window );
   }


   chkout_c ( "wninsd_c" );

} /* End wninsd_c */
