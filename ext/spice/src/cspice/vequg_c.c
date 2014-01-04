/*

-Procedure vequg_c ( Vector equality, general dimension )

-Abstract

   Make one double precision vector of arbitrary dimension equal
   to another.

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

   ASSIGNMENT
   VECTOR

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #undef    vequg_c


   void vequg_c ( ConstSpiceDouble  * vin,
                  SpiceInt            ndim,
                  SpiceDouble       * vout )
/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
    vin       I   ndim-dimensional double precision vector.
    ndim      I   Dimension of vin (and also vout).
    vout      O   ndim-dimensional double precision vector set
                  equal to vin.

-Detailed_Input

   vin      is a double precision vector of arbitrary dimension.

   ndim     is the number of components of vin.

-Detailed_Output

   vout     is a double precision vector set equal to vin.

-Parameters

   None.

-Particulars

   The code simply sets each component of vout equal to the
   corresponding component of vin.

-Examples

   Let state be a state vector. Set abstat equal to state.

   vequg_c  ( state, 6, abstate );

   Note that this routine may be used in place of MOVED, which
   sets each output array element equal to the corresponding
   input array element.

-Restrictions

   None.

-Exceptions

   1)  If ndim is not physically realistic, greater than zero, a
       BADDIMENSION error is flagged.

-Files

   None.

-Author_and_Institution

   W.M. Owen       (JPL)
   E.D. Wright     (JPL)

-Literature_References

   None.

-Version

   -CSPICE Version 1.0.0, 23-AUG-1999   (EDW) (NJB)

-Index_Entries

   assign an n-dimensional vector to another

-&
*/

{ /* Begin vequg_c */


   /*
   Use discovery check-in.
   */


   /* Check ndim is cool.  Dimension is positive definite. */

   if ( ndim <= 0 )
      {

      chkin_c    ( "vequg_c"                                      );
      SpiceError ( "Vector dimension less than or equal to zero",
                   "BADDIMENSION"                                 );
      chkout_c   ( "vequg_c"                                     );
      return;

      }


   /* Do the equality thing. */

   MOVED ( vin, ndim, vout );


} /* End vequg_c */

