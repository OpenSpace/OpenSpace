/*

-Procedure vtmv_c ( Vector transpose times matrix times vector, 3 dim )

-Abstract
 
    Multiply the transpose of a 3-dimensional column vector, 
    a 3x3 matrix, and a 3-dimensional column vector. 
 
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
 
    MATRIX,  VECTOR 
 
*/

   #include "SpiceUsr.h"
   #undef    vtmv_c

   SpiceDouble vtmv_c ( ConstSpiceDouble v1     [3],
                        ConstSpiceDouble matrix [3][3],
                        ConstSpiceDouble v2     [3] ) 

/*

-Brief_I/O
 
    VARIABLE  I/O  DESCRIPTION 
    --------  ---  -------------------------------------------------- 
     v1        I     3 dimensional double precision column vector. 
     matrix    I     3x3 double precision matrix. 
     v2        I     3 dimensional double precision column vector. 
 
     The function returns the result of (v1**t * matrix * v2 ). 
 
-Detailed_Input
 
    v1         This may be any 3-dimensional, double precision 
               column vector. 
 
    matrix     This may be any 3x3, double precision matrix. 
 
    v2         This may be any 3-dimensional, double precision 
               column vector. 
 
-Detailed_Output
 
    the function returns the double precision value of the equation 
    (v1**t * matrix * v2 ). 
 
    Notice that vtmv_c is actually the dot product of the vector 
    resulting from multiplying the transpose of v1 and matrix and the 
    vector v2. 
 
-Parameters
 
    None. 
 
-Particulars
 
    This routine implements the following vector/matrix/vector 
    multiplication: 
 
                          T 
       vtmv_c = |   v1   | |          |  |  | 
                           |  matrix  |  |v2| 
                           |          |  |  | 
 
    v1 is a column vector which becomes a row vector when transposed. 
    v2 is a column vector. 
 
    No checking is performed to determine whether floating point 
    overflow has occurred. 
 
-Examples
 
    if  v1 = | 2.0 |   matrix = |  0.0  1.0  0.0 | 
             |     |            |                | 
             | 4.0 |            | -1.0  0.0  0.0 | 
             |     |            |                | 
             | 6.0 |            |  0.0  0.0  1.0 | 
 
        v2 = | 1.0 | 
             |     | 
             | 1.0 | 
             |     | 
             | 1.0 | 
 
    then function value is equal to 4.0. 
 
-Restrictions
 
    None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
    None. 
 
-Author_and_Institution
 
    W.M. Owen       (JPL) 
    E.D. Wright     (JPL)
 
-Literature_References
 
    None. 
 
-Version
 
   -CSPICE Version 1.0.0, 1-JUL-1999

-Index_Entries
 
   3-dimensional vector_transpose times matrix times vector 
 
-&
*/

{ /* Begin vtmv_c */


  /*
   Local variables
   */
   SpiceInt          k;
   SpiceInt          l;
   SpiceDouble       val = 0.;
      
   for ( k = 0; k < 3; k++ )
      {
      for ( l = 0; l < 3; l++ )
         {
         val += v1[k] * matrix[k][l] * v2[l];
         }
      }

   return val;

} /* End vtmv_c */
