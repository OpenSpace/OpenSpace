/*

-Procedure wncard_c ( Cardinality of a double precision window )

-Abstract
 
   Return the cardinality (number of intervals) of a double
   precision window.
 
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
   
   SpiceInt wncard_c ( SpiceCell  * window ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   window     I   Input window 
 
   The function returns the window cardinality of the window. 
 
-Detailed_Input
  
   window      a window containing zero or more intervals

               'window' must be declared as a double precision SpiceCell.
 
-Detailed_Output
 
   The function returns the cardinality of (number of intervals in)
   the input window.
 
-Parameters
 
   None. 
 
-Exceptions
 
   None.

-Files
 
   None. 
 
-Particulars
 
   This function returns the value of card_c(window)/2.
 
-Examples


      /. Include needed headers. ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      #define WNSIZE          10
   
   int main()
      {


      SpiceInt                 i;
      SpiceDouble              left;
      SpiceDouble              right;
   
      SPICEDOUBLE_CELL ( window,  WNSIZE );

      wnvald_c ( WNSIZE, 0, &window );

      wninsd_c (  1.0,   3.0, &window );
      wninsd_c (  7.0,  11.0, &window );
      wninsd_c ( 23.0,  27.0, &window );
   
      for ( i=0; i<wncard_c(&window); i++)
         {
         wnfetd_c( &window, i, &left, &right );
         printf("Interval %d [%f, %f]\n", i, left, right );
         }
   
      return ( 0 );
      }
      
   The code outputs:
   
      Interval 0 [1.000000 ,  3.000000]
      Interval 1 [7.000000 , 11.000000]
      Interval 2 [23.000000, 27.000000]

-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   E.D. Wright    (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 21-AUG-2007 (EDW)

-Index_Entries
 
   cardinality of a d.p. window
 
-&
*/

{ /* Begin wncard_c */

   SpiceInt      retval;
   
   /*
   Use discovery check-in.

   Make sure cell data type is d.p. 
   */
   CELLTYPECHK_VAL( CHK_DISCOVER, 
                    "wncard_c", SPICE_DP, window, 0 );
   /*
   Initialize the cell if necessary. 
   */
   CELLINIT( window );

   retval = wncard_( (doublereal * ) (window->base) );

   return( retval );

} /* End wncard_c */

