/*

-Procedure  mtxv_c ( Matrix transpose times vector, 3x3 )

-Abstract

   mtxv_c multiplies the transpose of a 3x3 matrix on the left with
   a vector on the right.

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

   VECTOR,  MATRIX

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #undef    mtxv_c
   

   void mtxv_c ( ConstSpiceDouble     m1  [3][3],
                 ConstSpiceDouble     vin [3],
                 SpiceDouble          vout[3]   )

/*

-Brief_I/O

   VARIABLE  I/O              DESCRIPTION
   --------  ---  --------------------------------------------------
   m1         I   3x3 double precision matrix.
   vin        I   3-dimensional double precision vector.
   vout       O   3-dimensional double precision vector. vout is
                  the product m1**t * vin.

-Detailed_Input

   m1         is an arbitrary 3x3 double precision matrix.
              typically, m1 will be a rotation matrix since
              then its transpose is its inverse (but this is NOT
              a requirement).

   vin        is an arbitrary 3-dimensional double precision
              vector.

-Detailed_Output

   vout       is a 3-dimensional double precision vector. vout is
              the product vout = (m1**t)  x (vin). vout can
              overwrite vin.

-Parameters

   None.

-Particulars

   The intermediate results of the operation performed by this routine
   are buffered in a temporary vector which is later moved to the output
   vector.  Thus vout can be actually vin if desired without
   interfering with the computation.

-Examples

   Typically the matrix m1 will be a rotation matrix.  Because
   the transpose of an orthogonal matrix is equivalent to its
   inverse, applying the rotation to the vector is accomplished by
   multiplying the vector by the transpose of the matrix.

          -1
   let  m1   * vin = vout. If m1 is an orthogonal matrix,
   then  (m1**t) * vin = vout.


      If m1 = |  1.  1.  0. |   and  vin = |  5. |
              |             |              |     |
              | -1.  1.  0. |              | 10. |
              |             |              |     |
              |  0.  0.  1. |              | 15. |


   then the call

      mtxv_c ( m1, vin, vout )

   produces the vector


      vout = | -5. |
             |     |
             | 15. |
             |     |
             | 15. |


-Restrictions

   The user is responsible for checking the magnitudes of the
   elements of m1 and vin so that a floating point overflow does
   not occur.

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

   -CSPICE Version 1.0.1, 10-NOV-2006   (EDW)

      Added Parameters section header. 

   -CSPICE Version 1.0.0, 16-APR-1999 (EDW)

-Index_Entries

   matrix_transpose times 3-dimensional vector

-&
*/


{ /* Begin mtxv_c */


   /*
   Local variables
   */

   SpiceInt                 i;
   SpiceDouble              vtemp[3];


   for ( i = 0; i <= 2; i++ )
      {
      vtemp[i] = m1[0][i]*vin[0] + m1[1][i]*vin[1] + m1[2][i]*vin[2];
      }


   /* Move the computed result to the output array. */

   MOVED ( vtemp, 3, vout );


} /* End mtxv_c */
