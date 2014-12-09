/*

-Procedure unormg_c ( Unit vector and norm, general dimension )

-Abstract

   Normalize a double precision vector of arbitrary dimension and
   return its magnitude.

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

   VECTOR

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #undef    unormg_c


   void unormg_c ( ConstSpiceDouble  * v1,
                   SpiceInt            ndim,
                   SpiceDouble       * vout,
                   SpiceDouble       * vmag )
/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
    v1        I     Vector to be normalized.
    ndim      I     Dimension of v1 (and also vout).
    vout      O     Unit vector v1 / |v1|.
                    If v1 = 0, vout will also be zero.
                    vout can overwrite v1.
    vmag      O     Magnitude of v1, that is, |v1|.

-Detailed_Input

   v1      This variable may contain any vector of arbitrary
           dimension, including the zero vector.
   ndim    This is the dimension of v1 and vout.

-Detailed_Output

   vout    This variable contains the unit vector in the direction
           of v1.  If v1 is the zero vector, then vout will also be
           the zero vector.

   vmag    This is the magnitude of v1.

-Parameters

   None.

-Particulars

   unormg_c references a function called vnormg_c (which itself is
   numerically stable) to calculate the norm of the input vector v1.
   If the norm is equal to zero, then each component of the output
   vector vout is set to zero.  Otherwise, vout is calculated by
   dividing v1 by the norm.  No error detection or correction is
   implemented.

-Examples

   The following table shows how selected v1 implies vout and mag.

   v1                    ndim   vout                   mag
   -----------------------------------------------------------------
   (5, 12)               2      (5/13, 12/13)          13
   (1D-7, 2D-7, 2D-7)    3      (1/3, 2/3, 2/3)        3D-7

-Restrictions

   No error checking is implemented in this subroutine to guard
   against numeric overflow.

-Exceptions

   1)  If ndim is not physically realistic, greater than zero, a
       BADDIMENSION error is flagged.

-Files

   None.

-Author_and_Institution

   W.M. Owen       (JPL)
   W.L. Taber      (JPL)
   E.D. Wright     (JPL)

-Literature_References

   None.

-Version

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

      Made input vector const.  Converted check-in style to discovery.

   -CSPICE Version 1.0.0, 31-MAR-1998   (EDW)

-Index_Entries

   n-dimensional unit vector and norm

-&
*/

{ /* Begin unormg_c */

   /*
   Local variables
   */
   SpiceInt                i;



   /*
   Use discovery check-in.
   */



   /* Check ndim is cool.  Dimension is positive definite. */
   
   if ( ndim <= 0 )
      {
      
      chkin_c    ( "unormg_c"                                    );
      SpiceError ( "Vector dimension less than or equal to zero",
                   "BADDIMENSION"                                );
      chkout_c   ( "unormg_c"                                    );
      return;
      
      }



   /* Get the magnitude of the vector. */
   
   *vmag = vnormg_c ( v1, ndim );


   /*
   If vmag is nonzero, then normalize.  Note that this process is
   numerically stable: overflow could only happen if vmag were small,
   but this could only happen if each component of v1 were also small.
   In fact, the magnitude of any vector is never less than the
   magnitude of any component.
   */

   if ( *vmag > 0. )
      {

      for ( i = 0; i < ndim; i++ )
         {
         vout[i] = v1[i]/ (*vmag);
         }

      }
   else
      {

      for ( i = 0; i < ndim ; i++ );
         {
         vout[i] = 0.;
         }

     }


} /* End unormg_c */
