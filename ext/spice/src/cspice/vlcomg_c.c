/*

-Procedure vlcomg_c ( Vector linear combination, general dimension )

-Abstract

   Compute a vector linear combination of two double precision
   vectors of arbitrary dimension.

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
   #undef   vlcomg_c

   void vlcomg_c ( SpiceInt            n,
                   SpiceDouble         a,
                   ConstSpiceDouble *  v1,
                   SpiceDouble         b,
                   ConstSpiceDouble *  v2,
                   SpiceDouble      *  sum )

/*

-Brief_I/O

    VARIABLE  I/O  DESCRIPTION
    --------  ---  --------------------------------------------------
    n          I   Dimension of vector space
    a          I   Coefficient of v1
    v1         I   Vector in n-space
    b          I   Coefficient of v2
    v2         I   Vector in n-space
    sum        O   Linear Vector Combination a*v1 + b*v2

-Detailed_Input

    n   This variable contains the dimension of the v1, v2 and sum.
    a   This double precision variable multiplies v1.
    v1  This is an arbitrary, double precision n-dimensional vector.
    b   This double precision variable multiplies v2.
    v2  This is an arbitrary, double precision n-dimensional vector.

-Detailed_Output

    sum   is an arbitrary, double precision n-dimensional vector
          which contains the linear combination a*v1 + b*v2.

-Parameters

    None.

-Particulars

    For each index from 1 to n, this routine implements in C
    code the expression:

    sum[i] = a*v1[i] + b*v2[i]

    No error checking is performed to guard against numeric overflow.

-Examples

    We can easily use this routine to perform vector projections
    to 2-planes in n-space.  Let x be an arbitray n-vector
    and let u and v be orthonormal n-vectors spanning the plane
    of interest.  The projection of x onto this 2-plane, projuv can
    be obtained by the following code fragment.

       vlcomg_c ( n, vdot_c(x,u,n), u, vdot_c(x,v,n), v, projuv );

-Restrictions

    No error checking is performed to guard against numeric overflow
    or underflow.  The user is responsible for insuring that the
    input values are reasonable.

-Exceptions

   Error free.

-Files

    None

-Author_and_Institution

    W.L. Taber      (JPL)

-Literature_References

    None

-Version

   -CSPICE Version 1.0.0, 30-JUN-1999

-Index_Entries

   linear combination of two n-dimensional vectors

-&
*/

{ /* Begin vlcomg_c */

   /*
   Local variables
   */
   SpiceInt       i;


   /* A simple loop to do the work. */
   for ( i = 0; i < n; i++ )
      {
      sum[i] = a*v1[i] + b*v2[i];
      }


} /* End vlcomg_c */
