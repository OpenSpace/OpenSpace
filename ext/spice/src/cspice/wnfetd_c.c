/*

-Procedure wnfetd_c ( Fetch an interval from a DP window )

-Abstract
 
   Fetch a particular interval from a double precision window. 
 
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


   void wnfetd_c ( SpiceCell    * window,
                   SpiceInt       n,
                   SpiceDouble  * left,
                   SpiceDouble  * right   ) 

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   window     I   Input window. 
   n          I   Index of interval to be fetched. 
   left, 
   right      O   Left, right endpoints of the nth interval. 

-Detailed_Input

   window      is a window containing zero or more intervals. 

               window must be declared as a double precision SpiceCell.

   n           is the index of a particular interval within the 
               window.  Indices range from 0 to N-1, where N is the
               number of intervals in the window.

-Detailed_Output

   left, 
   right       are the left and right endpoints of the nth interval 
               in the input window.   
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input window does not have double precision type,
      the error SPICE(TYPEMISMATCH) signals.

   2) If n is less than zero, the error SPICE(NOINTERVAL) is 
      signaled. 
 
   3) If the interval does not exist, i.e., n >= card_c(&window)/2, 
      the error SPICE(NOINTERVAL) signals. 
 
-Files
 
   None. 

-Particulars
 
   None. 
 
-Examples
 
   Let window contain the intervals 

      [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ] 

   This window has a cardinality of 6 ( N=3 intervals), so 'n' may have
   value 0, 1, or 2, (n < cardinality/2)

   Then the following calls 

      wnfetd_c ( &window,  0, &left, &right );    (1) 
      wnfetd_c ( &window,  1, &left, &right );    (2) 
      wnfetd_c ( &window,  2, &left, &right );    (3) 

   yield the following values of left and right 

      left         right 
      ---------    --------- 
      1            3 
      7            11 
      23           27 

-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Version

   -CSPICE Version 1.0.2, 30-JUL-2007 (EDW)

      Removed erroneous description in the Examples section 
      indicating "Undefined" as a return state after an error
      event caused by an invalid value of n.
 
   -CSPICE Version 1.0.0, 22-AUG-2002 (NJB) (WLT) (IMU)

-Index_Entries
 
   fetch an interval from a d.p. window 
 
-&
*/

{ /* Begin wnfetd_c */


   /*
   Local variables 
   */
   SpiceInt                fn;

   /*
   Participate in error tracing.
   */
   if ( return_c() )
      {
      return;
      }
   chkin_c ( "wnfetd_c" );


   /*
   Make sure the window's data type is d.p. 
   */
   CELLTYPECHK ( CHK_STANDARD, "wnfetd_c", SPICE_DP, window );


   /*
   Initialize the cell if necessary. 
   */
   CELLINIT ( window );

   /*
   Map the index to a Fortran-style index.  
   */
   fn = n + 1;

   wnfetd_ ( ( doublereal * ) window->base, 
             ( integer    * ) &fn,
             ( doublereal * ) left,
             ( doublereal * ) right        );


   chkout_c ( "wnfetd_c" );

} /* End wnfetd_c */
