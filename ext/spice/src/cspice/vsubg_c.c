/*

-Procedure vsubg_c ( Vector subtraction, general dimension )

-Abstract

    Compute the difference between two double precision vectors of
    arbitrary dimension.

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
   #undef    vsubg_c
   

   void vsubg_c ( ConstSpiceDouble  * v1,
                  ConstSpiceDouble  * v2,
                  SpiceInt            ndim,
                  SpiceDouble       * vout )
/*

-Brief_I/O

    VARIABLE  I/O  DESCRIPTION
    --------  ---  --------------------------------------------------
     v1        I     First vector (minuend).
     v2        I     Second vector (subtrahend).
     ndim      I     Dimension of v1, v2, and vout.
     vout      O     Difference vector, v1 - v2.
                     vout can overwrite either v1 or v2.

-Detailed_Input

    v1      is a double precision vector of arbitrary dimension which
            is the minuend (i.e. first or left-hand member) in the
            vector subtraction.

    v2      is a double precision vector of arbitrary dimension which
            is the subtrahend (i.e. second or right-hand member) in
            the vector subtraction.

    ndim    is the dimension of v1 and v2 (and vout).

-Detailed_Output

    vout    is a double precision vector containing the difference
            v1 - v2.

-Parameters

   None.

-Particulars

    For each value of the index i from 0 to (ndim - 1), this subroutine
    performs the following subtraction:

       vout(i) = v1(i) - v2(i)

    No error checking is performed to guard against numeric overflow
    or underflow.  vout may overwrite v1 or v2.

-Examples

    The following table shows the results of vsubg_c from various
    inputs.

     v1                v2             ndim         vout
    -----------------------------------------------------------------
    (1, 2, 3, 4)     ( 1, 1, 1, 1 )    4         ( 0, 1, 2, 3 )
    (1, 2, 3, 4)     (-1,-2,-3,-4 )    4         ( 2, 4, 6, 8 )
    (1, 2, 3, 4)     (-1, 2,-3, 4 )    4         ( 2, 0, 6, 0 )

-Restrictions

    No error checking is performed to guard against numeric overflow.
    The programmer is thus required to insure that the values in v1
    and v2 are reasonable and will not cause overflow.

    It is assumed the proper amount of memory has been allocated for
    v1, v2 and vout.

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

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

      Made input vectors const.

   -CSPICE Version 1.0.0, 05-MAR-1998 (EDW)

-Index_Entries

   n-dimensional vector subtraction

-&
*/

{ /* Begin vsubg_c */

   /*
   Local variables
   */

   SpiceInt                i;


   /* Do the calculation.  Not very involved. */

   for ( i = 0; i < ndim; i++ )
      {
      vout[i] = v1[i] - v2[i];
      }


} /* End vsubg_c */
