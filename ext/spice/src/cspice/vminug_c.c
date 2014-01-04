/*

-Procedure vminug_c ( Minus V, "-V", general dimension )

-Abstract

   Negate a double precision vector of arbitrary dimension.

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
   #undef   vminug_c

   void vminug_c ( ConstSpiceDouble  * vin,
                   SpiceInt            ndim,
                   SpiceDouble       * vout )

/*

-Brief_I/O

    VARIABLE  I/O  DESCRIPTION
    --------  ---  --------------------------------------------------
     vin       I   ndim-dimensional double precision vector to
                   be negated.
     ndim      I   Dimension of vin (and also vout).
     vout      O   ndim-dimensional double precision vector equal to
                   -vin.

-Detailed_Input

    vin      double precision vector of arbitrary size.

    ndim     the dimension of vin and vout.

-Detailed_Output

    vout    a double precision vector which contains the negation
            of vin. vout may overwrite vin.

-Parameters

    None.

-Particulars

    For each value of the index i from 1 to ndim, vminug_c negates vin
    by the expression:

    vout[i] = - vin[i];

-Examples

    Let vin = [ -10.0, 15.0, -5.0, 20.0 ]

    The call

    vminug_c ( vin, 4, vin )

    negates all of the components of the vector VIN, and overwrites
    the original components. The vector VIN then contains the
    components

    vin = [ 10.0, -15.0, 5.0, -20.0 ]

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

   -CSPICE Version 1.0.0, 29-JUN-1999

-Index_Entries

   negate an n-dimensional vector

-&
*/

{ /* Begin vminug_c */

   /*
   Local variables
   */
   SpiceInt       i;


   /* Do it.  This isn't rocket science. */
   for ( i = 0; i < ndim; i++ )
      {
      vout[i] = -vin[i];
      }


} /* End vminug_c */
