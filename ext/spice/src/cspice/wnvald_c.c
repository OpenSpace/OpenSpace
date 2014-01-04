/*

-Procedure wnvald_c ( Validate a DP window )

-Abstract
 
   Form a valid double precision window from the contents 
   of a window array. 
 
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

   void wnvald_c ( SpiceInt       size,
                   SpiceInt       n,
                   SpiceCell    * window ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   size       I   Size of window. 
   n          I   Original number of endpoints. 
   window    I,O  Input, output window. 
 
-Detailed_Input
 
   size        is the size of the window to be validated. This 
               is the maximum number of endpoints that the cell 
               used to implement the window is capable of holding 
               at any one time. 
 
   n           is the original number of endpoints in the input 
               cell. 
 
   window      on input, is a (possibly uninitialized) cell array 
               containing n endpoints of (possibly unordered 
               and non-disjoint) intervals. 

               window must be declared as a double precision SpiceCell.
 
-Detailed_Output
 
   window      on output, is a window containing the union of the 
               intervals in the input cell.  
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input window does not have double precision type,
      the error SPICE(TYPEMISMATCH) is signaled.
  
   2) If the number of endpoints n is odd, the error 
      SPICE(UNMATCHENDPTS) is signaled. 
 
   3) If the number of end points of the window exceeds its size, the 
      error SPICE(WINDOWTOOSMALL) is signaled. 
 
   4) If any left endpoint is greater than the corresponding right endpoint,
      the error SPICE(BADENDPOINTS) is signaled.

-Files
 
   None. 
 
-Particulars
 
   This routine takes as input a cell array containing pairs of 
   endpoints and validates it to form a window. 
 
   On input, window is a cell of size size containing n endpoints. 
   During validation, the intervals are ordered, and overlapping 
   intervals are merged. On output, the cardinality of window is 
   the number of endpoints remaining, and window is ready for use with 
   any of the window routines. 
 
   Because validation is done in place, there is no chance of 
   overflow. 
 
-Examples
 
   The following small program 

      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main()
      {
         #define WINSIZ     20

         SPICEDOUBLE_CELL ( window, WINSIZ );

         SpiceDouble        winData [WINSIZ] = 
                            {
                               0.0,    0.0,
                              10.0,   12.0, 
                               2.0,    7.0, 
                              13.0,   15.0, 
                               1.0,    5.0, 
                              23.0,   29.0,  
                               0.0,    0.0,
                               0.0,    0.0,
                               0.0,    0.0,
                               0.0,    0.0
                            };

         SpiceInt           i;



         memmove ( (SpiceDouble *)(window.data), 
                    winData, 
                    WINSIZ * sizeof(SpiceDouble) );

         wnvald_c ( 20, 16, &window );

         printf ( "Current intervals:   %ld\n",  card_c(&window)/2  );
         printf ( "Maximum intervals:   %ld\n",  size_c(&window)/2  );
         printf ( "\nIntervals\n\n" );

         for ( i = 0;   i < card_c(&window);   i+=2 )
         {   
            printf ( "%10.6f   %10.6f\n",  
                     SPICE_CELL_ELEM_D (&window, i  ),
                     SPICE_CELL_ELEM_D (&window, i+1)  );
         }

         return ( 0 );
      } 

   produces the following output (possibly with different formatting). 


     Current intervals:   5
     Maximum intervals:   10

     Intervals

       0.000000     0.000000
       1.000000     7.000000
      10.000000    12.000000
      13.000000    15.000000
      23.000000    29.000000
 
 
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

   -CSPICE Version 1.0.2, 18-DEC-2008 (EDW)
   
      Corrected a typo in the version ID of the 08-OCT-2004
      Version entry. 1.0.0 changed to 1.0.1.
 
   -CSPICE Version 1.0.1, 08-OCT-2004 (NJB)

       Corrected typo in code example; also added "return"
       statement to code example.

   -CSPICE Version 1.0.0, 29-JUL-2002 (NJB) (HAN) (WLT) (IMU)

-Index_Entries
 
   validate a d.p. window 
 
-&
*/

{ /* Begin wnvald_c */



   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "wnvald_c" );

   /*
   Make sure cell data type is d.p. 
   */
   CELLTYPECHK ( CHK_STANDARD, "wnvald_c", SPICE_DP, window );

   /*
   Initialize the cell if necessary. 
   */
   CELLINIT ( window );

   /*
   Let the f2c'd routine do the work. 
   */
   wnvald_ ( (integer    * ) &size,
             (integer    * ) &n, 
             (doublereal * ) (window->base)  );

   /*
   Sync the output cell. 
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, window );
   }


   chkout_c ( "wnvald_c" );

} /* End wnvald_c */
