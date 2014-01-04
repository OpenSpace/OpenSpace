/*

-Procedure intmin_c ( Smallest integer number )

-Abstract
 
   Return the value of the smallest (negative) number representable 
   in a SpiceInt variable. 
 
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
 
   None. 
 
-Keywords
 
   CONSTANTS 
 
*/

   #include "SpiceUsr.h"

   SpiceInt intmin_c () 

/*

-Brief_I/O
 
   The function returns the value of the smallest (negative) number 
   that can be represented in a SpiceInt variable. 
 
-Detailed_Input
 
   None. 
 
-Detailed_Output
 
   The function returns the value of the smallest (negative) number 
   that can be represented in an SpiceInt variable, where SpiceInt
   is a typedef defined in SpiceZdf.h. 
 
   The returned value will be less than or equal to -2147483647.
   See the Particulars section for details.
      
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Particulars
 
   The typedef SpiceInt is used throughout the CSPICE API to refer to
   integers; the precise type of integer is platform-dependent.  A
   SpiceInt always maps to the same type as does the f2c typedef 
   integer.
   
   When translating Fortran code, f2c maps Fortran variables of type
   INTEGER to C variables of type "integer," where integer is a typedef
   defined in the f2c header file f2c.h.  On all supported platforms, 
   Fortran INTEGERS occupy at least 32 bits.  On most platforms, this 
   means that the typedef integer translates to type long.  There are 
   some exceptional platforms on which an integer translates to type 
   int.  The mapping must provide compatibility with the f2c typedef
   doublereal:  integers must occupy half the storage of doublereals in 
   order for these types to correctly represent the Fortran types 
   INTEGER and DOUBLE PRECISION.
   
   On systems where the typedef integer maps to type long, the return
   value is defined by the macro LONG_MIN from the ANSI standard header
   file limits.h. According to the ANSI standard, LONG_MIN must be no 
   greater than
   
      -2147483647 
    
   This is 
   
           31
      - ( 2   - 1 )
      
   On systems where the typedef integer maps to type int, the value is
   defined by the macro INT_MIN from the ANSI standard header file 
   limits.h. According to the ANSI standard, INT_MIN must be no greater
   than
      
      -32767
    
   This is 
   
          15
      -( 2   - 1 )
            
   In practice however, the typedef integer will map to type int only
   if ints occupy at least four bytes, so the value of INT_MIN will
   actually be no greater than -2147483647.
   
 
-Examples
 
   The following code fragment illustrates the use of intmin_c. 
 
      /.
      Separate a double into integer and fractional components.
      If the integer component is out of range, avoid overflow 
      by making it as large as possible. 
      ./
      #include <math.h>
               .
               .
               .
      fract = modf ( dvalue, &integralDP ); 
      
      if (  integralDP  >  (double)intmax_c()  ) 
      { 
         ivalue = intmax_c();
      }
      else if (  integralDP  <  (double)intmin_c()  )   
      {
         ivalue = intmin_c(); 
      }
      else 
      {
         ivalue = (long)( integralDP );
      }

 
-Restrictions
 
   None. 
 
-Literature_References
 
   None.
   
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 29-JAN-1999 (NJB)

-Index_Entries
 
   smallest integer number 
 
-&
*/

{ /* Begin intmin_c */


   /*
   Static variables
   */

   static SpiceBoolean            first = SPICETRUE;
   static SpiceInt                value;



   if ( first )
   {
      value = intmin_();
      first = SPICEFALSE;
   }
   
   return ( value );
   

} /* End intmin_c */

