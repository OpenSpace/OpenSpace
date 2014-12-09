/*

-Procedure vdotg_c ( Vector dot product, general dimension )

-Abstract

   Compute the dot product of two vectors of arbitrary dimension.

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
   #undef    vdotg_c
   

   SpiceDouble vdotg_c ( ConstSpiceDouble   * v1,
                         ConstSpiceDouble   * v2,
                         SpiceInt             ndim )
/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
    v1        I     First vector in the dot product.
    v2        I     Second vector in the dot product.
    ndim      I     Dimension of v1 and v2.

   The function returns the value of the dot product of v1 and v2.

-Detailed_Input

   v1      This may be any double precision vector of arbitrary
           dimension.

   v2      This may be any double precision vector of arbitrary
           dimension.

-Detailed_Output

   The function returns the value of the dot product of v1 and v2.

-Parameters

   None.

-Particulars

   vdotg_c calculates the dot product of v1 and v2 by a simple
   application of the definition.  No error checking is
   performed to prevent or recover from numeric overflow.

-Examples

   Suppose that given two n-dimensional vectors, we want to change
   one of the vectors until the two vectors are perpendicular.
   The following code fragment demonstrates the use of vdot_c to do
   so.

    dot = vdotg_c ( v1, v2, ndim );

    while ( dot != 0. )
       {

         /. change one of the vectors ./
                  ....

        dot = vdotg_c ( v1, v2, ndim );
       }


-Restrictions

   The user is responsible for determining that the vectors v1 and
   v2 are not so large as to cause numeric overflow.  In most cases
   this won't present a problem.

-Exceptions

   1)  If ndim is not physically realistic, greater than zero, a
       BADDIMENSION error is signaled.  The value 0. is returned.

-Files

   None.

-Author_and_Institution

   W.M. Owen       (JPL)
   E.D. Wright     (JPL)

-Literature_References

   None.

-Version

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

      Made input vectors const.  Converted check-in style to discovery.

   -CSPICE Version 1.0.0, 31-MAR-1998   (EDW)

-Index_Entries

   dot product of n-dimensional vectors

-&
*/

{ /* Begin vdotg_c */

   /*
   Local variables
   */

   SpiceInt                i;
   SpiceDouble             dot;


   /*
   Use discovery check-in.
   */


   /* Initialize dot to zero. */
   
   dot  = 0.;


   /* Check ndim is cool.  Dimension is positive definite. */
   
   if ( ndim <= 0 )
      {
      
      chkin_c    ( "vdotg_c"                                      );
      SpiceError ( "Vector dimension less than or equal to zero",
                   "BADDIMENSION"                                 );
      chkout_c   ( "vdotg_c"                                      );
      return     ( 0.                                             );
      
      }


   /* Do the calculation.  Not very involved. */
   
   for ( i = 0; i < ndim; i++ )
      {
      dot += v1[i] * v2[i];
      }


   /* Return the value. */

   return dot;
   

} /* End vdotg_c */
