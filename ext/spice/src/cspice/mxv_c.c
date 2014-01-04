/*

-Procedure mxv_c ( Matrix times vector, 3x3 )

-Abstract

   Multiply a 3x3 double precision matrix with a 3-dimensional
   double precision vector.

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
   #include "SpiceZmc.h"
   #undef    mxv_c
   

   void mxv_c ( ConstSpiceDouble    m1  [3][3],
                ConstSpiceDouble    vin [3],
                SpiceDouble         vout[3]    )

/*

-Brief_I/O

   VARIABLE  I/O              DESCRIPTION
   --------  ---  --------------------------------------------------
   m1        I   3x3 double precision matrix.
   vin       I   3-dimensional double precision vector.
   vout      O   3-dimensinoal double precision vector. vout is
                 the product m1*vin.

-Detailed_Input

   m1         is an arbitrary 3x3 double precision matrix.

   vin        is an arbitrary 3-dimensional double precision vector.

-Detailed_Output

   vout       is a 3-dimensional double precision vector. vout is
              the product m1 * v. vout may overwrite vin.

-Parameters

   None.

-Particulars

   The intermediate results of the operation performed by this routine
   are buffered in a temporary vector which is later moved to the output
   vector.  Thus vout can be actually be vin if desired without
   interfering with the computation.

-Examples

   Let

      m1 = |  0.  1.  0. |   and  vin = | 1. |
           |             |              |    |
           | -1.  0.  0. |              | 2. |
           |             |              |    |
           |  0.  0.  1. |              | 3. |

   Then the call

      mxv_c ( m1, vin, vout );

   produces the vector

      vout = |  2. |
             |     |
             | -1. |
             |     |
             |  3. |


-Restrictions

   The user is responsible for checking the magnitudes of the
   elements of matrix and vin so that a floating point overflow does
   not occur.

-Exceptions

   Error free.

-Files

   None.

-Author_and_Institution

   Ed Wright       (JPL)
   W.M. Owen       (JPL)

-Literature_References

   None.

-Version

   -CSPICE Version 1.0.0, 16-APR-1999 (EDW)

-Index_Entries

   matrix times 3-dimensional vector

-&
*/


{ /* Begin mxv_c */

   /*
   Local variables
   */

   SpiceInt                 i;
   SpiceDouble              vtemp[3];


   for ( i = 0; i <= 2; i++ )
      {
      vtemp[i] = m1[i][0]*vin[0] + m1[i][1]*vin[1] + m1[i][2]*vin[2];
      }


   /* Move the computed result to the output array. */

   MOVED ( vtemp, 3, vout );


} /* End of mxv_c */

