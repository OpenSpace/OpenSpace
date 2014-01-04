/*

-Procedure invort_c ( Invert nearly orthogonal matrices )

-Abstract
 
   Given a matrix, construct the matrix whose rows are the  
   columns of the first divided by the length squared of the 
   the corresponding columns of the input matrix. 
 
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
 
   MATRIX 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZim.h"
   #undef    invort_c


   void invort_c ( ConstSpiceDouble   m  [3][3],
                   SpiceDouble        mit[3][3] ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   m          I   A 3x3 matrix. 
   mit        I   m after transposition and scaling of rows. 
    
-Detailed_Input
 
   m          is a 3x3 matrix. 
 
-Detailed_Output
 
   mit        is the matrix obtained by transposing m and dividing 
              the rows by squares of their norms. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If any of the columns of m have zero length, the error  
      SPICE(ZEROLENGTHCOLUMN) will be signaled. 
       
   2) If any column is too short to allow computation of the 
      reciprocal of its length without causing a floating  
      point overflow, the error SPICE(COLUMNTOOSMALL) will 
      be signalled. 
       
-Files
 
   None. 
 
-Particulars
 
   Suppose that m is the matrix  
     
           -                      - 
          |   A*u    B*v     C*w   |    
          |      1      1       1  | 
          |                        | 
          |   A*u    B*v     C*w   |    
          |      2      2       2  | 
          |                        | 
          |   A*u    B*v     C*w   |   
          |      3      3       3  | 
           -                      - 
 
   where the vectors (u , u , u ),  (v , v , v ),  and (w , w , w ) 
                       1   2   3      1   2   3          1   2   3 

   are unit vectors. This routine produces the matrix: 
     
     
           -                      - 
          |   a*u    a*u     a*u   |    
          |      1      2       3  | 
          |                        | 
          |   b*v    b*v     b*v   |    
          |      1      2       3  | 
          |                        | 
          |   c*w    c*w     c*w   |   
          |      1      2       3  | 
           -                      - 
     
   where a = 1/A, b = 1/B, and c = 1/C. 
     
-Examples
 
   Suppose that you have a matrix m whose columns are orthogonal  
   and have non-zero norm (but not necessarily norm 1).  Then the  
   routine invort_c can be used to construct the inverse of m: 
      
      #include "SpiceUsr.h"
           .
           .
           .
      invort_c ( m, invers );
         
   This method is numerically more robust than calling the 
   routine invert_c. 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   W.L. Taber     (JPL) 
   N.J. Bachman   (JPL)

-Version
 
   -CSPICE Version 1.0.0, 02-JAN-2002 (WLT) (NJB)

-Index_Entries
 
   Transpose a matrix and invert the lengths of the rows 
   Invert a pseudo orthogonal matrix
 
-&
*/

{ /* Begin invort_c */

   /*
   Local variables
   */
   SpiceDouble             temp[3][3];


   /*
   Participate in error tracing. 
   */
   chkin_c ( "invort_c" );

   /*
   Transpose the input matrix to obtain a Fortran-style matrix.
   */
   xpose_c ( m, temp );

   invort_ ( (SpiceDouble * )temp,
             (SpiceDouble * )mit  );

   /*
   Transpose the output matrix to obtain a C-style matrix.
   */
   xpose_c ( mit, mit );


   chkout_c ( "invort_c" );

} /* End invort_c */
