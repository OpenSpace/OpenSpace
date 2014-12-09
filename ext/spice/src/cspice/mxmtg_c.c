/*

-Procedure  mxmtg_c ( Matrix times matrix transpose, general dimension )

-Abstract

   Multiply a matrix and the transpose of a matrix, both of
   arbitrary size.

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
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef    mxmtg_c


   void mxmtg_c ( const void   * m1,
                  const void   * m2,
                  SpiceInt       nrow1,
                  SpiceInt       nc1c2,
                  SpiceInt       nrow2,
                  void         * mout  )
/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   m1         I   Left-hand matrix to be multiplied.
   m2         I   Right-hand matrix whose transpose is to be multiplied
   nrow1      I   Row dimension of m1 and row dimension of mout.
   nc1c2      I   Column dimension of m1 and column dimension of m2.
   nrow2      I   Row dimension of m2 and column dimension of mout.
   mout       O   Product matrix.

-Detailed_Input

   m1         may be any double precision matrix of arbitrary size.

   m2         may be any double precision matrix of arbitrary size.
              The number of columns in m2 must match the number of
              columns in m1.

   nrow1      is the number of rows in both m1 and mout.

   nc1c2      i the number of columns in m1 and (by necessity) the
              number of columns of m2.

   nrow2      is the number of rows in both m2 and the number of columns
              in mout.

-Detailed_Output

   mout       is the product matrix given by

                                   t
                 mout = (m1) x (m2)


              where the superscript "t" denotes the transpose matrix.
              This is a double precision matrix of dimension nrow1 x
              nrow2.

              mout may overwrite m1 or m2.  Note that this capability
              does not exist in the Fortran version of SPICELIB; in the
              Fortran version, the output must not overwrite either
              input.

-Parameters

   None.

-Particulars

   The code reflects precisely the following mathematical expression

      For each value of the subscript i from 1 to nrow1, and j from 1
      to nrow2:

      mout(i,j) = summation from k=1 to nc1c2 of  ( m1(i,k) * m2(j,k) )

   Notice that the order of the subscripts of m2 are reversed from
   what they would be if this routine merely multiplied m1 and m2.
   It is this transposition of subscripts that makes this routine
   multiply m1 and the TRANPOSE of m2.

-Examples

   1)  Let m1 =

          | 1.0  2.0  3.0 |
          |               |
          | 3.0  2.0  1.0 |

       Let m2 =

          | 1.0  2.0  0.0 |
          |               |
          | 2.0  1.0  2.0 |
          |               |
          | 1.0  2.0  0.0 |
          |               |
          | 2.0  1.0  2.0 |

       Here

          nrow1   = 2
          nc1c2   = 3
          nrow2   = 4


       so the call

          mxmtg_c ( m1, m2, nrow1, nc1c2, nrow2, mout );


       produces the matrix


          mout = | 5.0  10.0  5.0  10.0 |
                 |                      |
                 | 7.0  10.0  7.0  10.0 |


-Restrictions

   No error checking is performed to prevent numeric overflow or
   underflow.

   No error checking is performed to determine if the input and
   output matrices have, in fact, been correctly dimensioned.

   The user is responsible for checking the magnitudes of the
   elements of m1 and m2 so that a floating point overflow does
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

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

      Based on SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)

-Index_Entries

   matrix times matrix_transpose n-dimensional_case

-&
*/

{ /* Begin mxmtg_c */



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
   SpiceDouble            innerProduct;
   SpiceDouble            *tmpmat;
   SpiceDouble            *loc_m1;
   SpiceDouble            *loc_m2;

   SpiceInt                col;
   SpiceInt                nelts;

   SpiceInt                row;
   SpiceInt                i;

   size_t                  size;


   /*
   Allocate space for a temporary copy of the output matrix, which
   has nrow1 rows and nc1c2 columns.
   */
   nelts    =  nrow1 * nrow2;

   size     =  (size_t) ( nelts * sizeof(SpiceDouble) );

   tmpmat   =  (SpiceDouble *) malloc ( size );

   if ( tmpmat == (SpiceDouble *)0 )
   {
      chkin_c  ( "mxmtg_c"                                         );
      setmsg_c ( "An attempt to create a temporary matrix failed." );
      sigerr_c ( "SPICE(MEMALLOCFAILED)"                           );
      chkout_c ( "mxmtg_c"                                         );
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
   loc_m2 = (SpiceDouble *) m2;


   /*
   Compute the product.  The matrix element at position (row,col) is
   the inner product of the row of m1 having index row and the
   row of m2 having index col.  We compute index offsets using
   the macro INDEX.
   */

   for ( row = 0;  row < nrow1;  row++ )
   {

      for ( col = 0;  col < nrow2;  col++ )
      {
         innerProduct = 0.0;

         for ( i = 0;  i < nc1c2;  i++ )
         {
            innerProduct  +=    loc_m1[ INDEX(nc1c2, row, i) ]
                              * loc_m2[ INDEX(nc1c2, col, i) ];
         }

         tmpmat [ INDEX( nrow2, row, col ) ]  =  innerProduct;
      }
   }

   /*
   Move the result from tmpmat into mout.
   */
   MOVED ( tmpmat, nelts, mout );

   /*
   Free the temporary matrix.
   */
   free ( tmpmat );


} /* End mxmtg_c */
