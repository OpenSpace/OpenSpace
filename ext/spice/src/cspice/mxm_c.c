/*

-Procedure mxm_c ( Matrix times matrix, 3x3 )

-Abstract

   Multiply two 3x3 matrices.

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
   #include "SpiceZmc.h"
   #undef    mxm_c
   

   void mxm_c ( ConstSpiceDouble   m1  [3][3],
                ConstSpiceDouble   m2  [3][3],
                SpiceDouble        mout[3][3] )

/*

-Brief_I/O

   VARIABLE  I/O              DESCRIPTION
   --------  ---  --------------------------------------------------
   m1        i   3x3 double precision matrix.
   m2        i   3x3 double precision matrix.
   mout      o   3x3 double precision matrix. mout is the product
                 m1*m2.

-Detailed_Input

   m1         is an arbitrary 3x3 double precision matrix.

   m2         is an arbitrary 3x3 double precision matrix.

-Detailed_Output

   mout       is a 3x3 double precision matrix. mout is the product
              m1*m2. mout may overwrite either m1 or m2.

-Parameters

   None.

-Particulars

   The code reflects precisely the following mathematical expression

   For each value of the subscripts i and j from 1 to 3:

   mout(i,j) = summation from k=1 to 3 of  ( m1(i,k) * m2(k,j) )

   The intermediate results of the operation above are buffered in a
   temporary matrix which is later moved to the output matrix.
   Thus, to save space in the calling program, mout can be actually
   be m1 or m2 if desired without interfering with the computations.

-Examples

   Let m1 = |  1.  1.  0. |
            |             |
            | -1.  1.  0. |
            |             |
            |  0.  0.  1. |


   and m2 = |  1.  0.  0. |
            |             |
            |  0.  1.  1. |
            |             |
            |  0. -1.  1. |

   then the call

      mxm_c ( m1, m2, mout );

   produces the matrix

   mout = |  1.  1.  1. |
          |             |
          | -1.  1.  1. |
          |             |
          |  0. -1.  1. |


-Restrictions

   None.

-Exceptions

   Error free.

-Files

   None.

-Author_and_Institution

   E.D. Wright     (JPL)
   W.M. Owen       (JPL)

-Literature_References

   None.

-Version

   -CSPICE Version 1.0.0, 16-APR-1999 (EDW)

-Index_Entries

   matrix times matrix 3x3_case

-&
*/


{ /* Begin mxm_c */

   /*
   Local variables
   */

   SpiceInt     i;
   SpiceInt     j;
   SpiceDouble  mtemp[3][3];


   for ( i = 0; i <= 2; ++i)
      {

      for ( j = 0; j <= 2; ++j)
         {
         mtemp[i][j] = m1[i][0] * m2[0][j] +
                       m1[i][1] * m2[1][j] +
                       m1[i][2] * m2[2][j];
         }

      }


   /*
   Copy the results from the temporary matrix to the return matrix.
   */
   MOVED ( mtemp, 9, mout );


} /* End mxm_c */
