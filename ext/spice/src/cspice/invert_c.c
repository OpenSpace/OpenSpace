/*

-Procedure invert_c ( Invert a 3x3 matrix )

-Abstract
 
   Generate the inverse of a 3x3 matrix. 
 
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
 
   MATRIX,  MATH 
 
*/
   #include <math.h>
   #include "SpiceUsr.h"
   #undef    invert_c
   

   void invert_c ( ConstSpiceDouble  m1  [3][3],
                   SpiceDouble       mout[3][3] ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   m1         I   Matrix to be inverted. 
   mout       O   Inverted matrix (m1)**-1.  If m1 is singular, then 
                  mout will be the zero matrix.   mout can 
                  overwrite m1. 
 
-Detailed_Input
 
   m1    An arbitrary 3x3 matrix.  The limits on the size of 
         elements of m1 are determined by the process of calculating 
         the cofactors of each element of the matrix.  For a 3x3 
         matrix this amounts to the differencing of two terms, each 
         of which consists of the multiplication of two matrix 
         elements.  This multiplication must not exceed the range 
         of double precision numbers or else an overflow error will 
         occur. 
 
-Detailed_Output
 
   mout  is the inverse of m1 and is calculated explicitly using 
         the matrix of cofactors.  mout is set to be the zero matrix 
         if m1 is singular. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) No internal checking on the input matrix m1 is performed except on 
      the size of its determinant.  Thus it is possible to generate a 
      floating point overflow or underflow in the process of 
      calculating the matrix of cofactors.
 
   2) If the determinant is less than 10**-16, the matrix is deemed to 
      be singular and the output matrix is filled with zeros.
      
-Particulars
 
   A temporary matrix is used to compute the result, so the output 
   matrix may overwrite the input matrix.   
 
-Examples
 
   Suppose that m1 is given by the following matrix equation: 
 
           | 0   -1    0 | 
      m1 = | 0.5  0    0 |   
           | 0    0    1 |     
 
   If invert_c is called as shown 
   
      invert_c (m1, m1); 
 
   then m1 will be set to be: 
 
           | 0    2    0 | 
      m1 = |-1    0    0 | 
           | 0    0    1 | 
 
-Restrictions
 
   The input matrix must be such that generating the cofactors will 
   not cause a floating point overflow or underflow.  The 
   strictness of this condition depends, of course, on the computer 
   installation and the resultant maximum and minimum values of 
   double precision numbers. 
 
-Files
 
   None 
 
-Author_and_Institution
 
   W.M. Owen       (JPL) 
 
-Literature_References
 
   None 
 
-Version
 
  -CSPICE Version 1.0.0, 13-SEP-1999 (NJB) (WMO)

-Index_Entries
 
   invert a 3x3_matrix 
 
-&
*/

{ /* Begin invert_c */

   /*
   Local constants
   */

   #define  SINGULAR_DET     1.e-16



   /*
   Local variables
   */
   SpiceInt                i;

   SpiceDouble             invdet;
   SpiceDouble             mdet;
   SpiceDouble             mtemp[3][3];


   /*
   Find the determinant of m1 and check for singularity.
   */
   
   mdet = det_c(m1);
   
   if ( fabs(mdet) < SINGULAR_DET ) 
   {
   
      /*
      The matrix is considered to be singular.
      */

      for ( i = 0;  i < 9;  i++ )
      {
         *( (SpiceDouble*)mout+i ) = 0.;
      }

      return;
   }
   
   
   /*
   Get the cofactors of each element of m1.
   */
   mtemp[0][0] =  ( m1[1][1]*m1[2][2] - m1[2][1]*m1[1][2] );
   mtemp[0][1] = -( m1[0][1]*m1[2][2] - m1[2][1]*m1[0][2] );
   mtemp[0][2] =  ( m1[0][1]*m1[1][2] - m1[1][1]*m1[0][2] );
   mtemp[1][0] = -( m1[1][0]*m1[2][2] - m1[2][0]*m1[1][2] );
   mtemp[1][1] =  ( m1[0][0]*m1[2][2] - m1[2][0]*m1[0][2] );
   mtemp[1][2] = -( m1[0][0]*m1[1][2] - m1[1][0]*m1[0][2] );
   mtemp[2][0] =  ( m1[1][0]*m1[2][1] - m1[2][0]*m1[1][1] );
   mtemp[2][1] = -( m1[0][0]*m1[2][1] - m1[2][0]*m1[0][1] );
   mtemp[2][2] =  ( m1[0][0]*m1[1][1] - m1[1][0]*m1[0][1] );

   /*
   Multiply the cofactor matrix by 1/mdet to obtain the inverse matrix.
   */
     
   invdet = 1. / mdet;
  
   vsclg_c ( invdet, (SpiceDouble *)mtemp, 9, (SpiceDouble *)mout );


} /* End invert_c */

