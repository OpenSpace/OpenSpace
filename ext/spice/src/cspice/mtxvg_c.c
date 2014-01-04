/*

-Procedure  mtxvg_c ( Matrix transpose times vector, general dimension )

-Abstract

   Multiply the transpose of a matrix and a vector of arbitrary size.

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
   #include <stdlib.h>
   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #include "SpiceZfc.h"
   #include "SpiceZim.h"
   #undef    mtxvg_c


   void mtxvg_c ( const void   * m1,
                  const void   * v2,
                  SpiceInt       ncol1,
                  SpiceInt       nr1r2,
                  void         * vout   )
/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   m1         I   Left-hand matrix to be multiplied.
   v2         I   Right-hand vector to be multiplied.
   ncol1      I   Column dimension of m1 and length of vout.
   nr1r2      I   Row dimension of m1 and length of v2.
   vout       O   Product vector m1 transpose * v2.

-Detailed_Input

   m1         is a double precision matrix of arbitrary size which
              forms the left-hand matrix of the multiplication.

   v2         is a double precision vector on the right of the
              multiplication.

   ncol1      is the column dimension of m1 and length of vout.

   nr1r2      is the row dimension of m1 and length of v2.

-Detailed_Output

   vout       is the double precision vector which results from
              the multiplication

                            t
                 vout = (m1)  x  v2

              where the superscript t denotes the transpose of a matrix.
              vout has length ncol1.

              vout may overwrite m1 or v2.  Note that this capability
              does not exist in the Fortran version of SPICELIB; in the
              Fortran version, the output must not overwrite either
              input.

-Parameters

   None.

-Particulars

    The code reflects precisely the following mathematical expression

       For each value of the subscript i from 1 to ncol1,

       vout(i) = Summation from k=1 to nr1r2 of  ( m1(k,i) * v2(k) )

-Examples

   1)  Suppose that

               | 1  2 |
          m1 = | 1  3 |
               | 1  4 |


       and that

               | 1 |
          v2 = | 2 |
               | 3 |


       Then calling mxvg_c as shown

          mtxvg_c ( m1, v2, 2, 3, vout );


       will yield the following vector value for vout:

          vout = | 6  |
                 | 20 |

-Restrictions

   1) The user is responsible for checking the magnitudes of the
      elements of m1 and v2 so that a floating point overflow does
      not occur.

-Exceptions

   Error free.

-Files

   None.

-Author_and_Institution

   N.J. Bachman    (JPL)
   W.M. Owen       (JPL)

-Literature_References

   None.

-Version

   -CSPICE Version 1.2.0, 28-AUG-2001 (NJB)

      Const-qualified input arrays.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

      Corrected a comment describing the local macro INDEX.   Made
      miscellaneous code format corrections.

      Based on SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)

-Index_Entries

   matrix transpose times n-dimensional vector

-&
*/

{ /* Begin mxvg_c */

   /*
   Local macros

   We'd like to be able to refer to the elements of the input and output
   matrices using normal subscripts, for example, m1[2][3].  Since the
   compiler doesn't know how to compute index offsets for the array
   arguments, which have user-adjustable size, we must compute the
   offsets ourselves.  To make syntax a little easier to read (we hope),
   we'll use macros to do the computations.

   The macro INDEX(width, i,j) computes the index offset from the array
   base of the element at position [i][j] in a 2-dimensional matrix
   having the number of columns indicated by width.  For example, if
   the input matrix m1 has 2 rows and 3 columns, the element at position
   [0][1] would be indicated by

      m1[ INDEX(3,0,1) ]

   */

   #define INDEX( width, row, col )     ( (row)*(width) + (col) )


   /*
   Local variables
   */
   SpiceDouble                        innerProduct;
   SpiceDouble                       *tmpvec;
   SpiceDouble                       *loc_m1;
   SpiceDouble                       *loc_v2;

   SpiceInt                           row;
   SpiceInt                           i;

   size_t                             size;


   /*
   Allocate space for a temporary copy of the output vector, which
   has ncol1 rows.
   */
   size     =  (size_t) ( ncol1 * sizeof(SpiceDouble) );

   tmpvec   =  (SpiceDouble *) malloc ( size );

   if ( tmpvec == (SpiceDouble *)0 )
   {
      chkin_c  ( "mtxvg_c"                                         );
      setmsg_c ( "An attempt to create a temporary vector failed." );
      sigerr_c ( "SPICE(MEMALLOCFAILED)"                           );
      chkout_c ( "mtxvg_c"                                         );
      return;
   }

   /*
   Cast the input pointers to pointers to SpiceDoubles.  Note:  the
   original variables are pointers to void so that callers may
   supply the array names as arguments without casting them to
   SpiceDoubles.  The naked array name is considered by the compiler
   to be an incompatible pointer type with (SpiceDouble *), so we
   can't simply declare the arguments to be (SpiceDouble *).  On the
   other hand, every pointer type can be cast to (void *).
   */

   loc_m1 = (SpiceDouble *) m1;
   loc_v2 = (SpiceDouble *) v2;


   /*
   Compute the product.  The vector element at position (row) is
   the inner product of the column of m1 having index row and v2.
   We compute index offsets using the macro INDEX.
   */

   for ( row = 0;  row < ncol1;  row++ )
   {

      innerProduct = 0.0;

      for ( i = 0;  i < nr1r2;  i++ )
      {
         innerProduct  +=  loc_m1[ INDEX(ncol1, i, row  ) ] * loc_v2[i];
      }

      tmpvec [ row ]  =  innerProduct;
   }

   /*
   Move the result from tmpvec into vout.
   */
   MOVED ( tmpvec, ncol1, vout );

   /*
   Free the temporary vector.
   */
   free ( tmpvec );


} /* End mtxvg_c */
