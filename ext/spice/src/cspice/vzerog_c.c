/*

-Procedure vzerog_c ( Is a vector the zero vector?---general dim. )

-Abstract

   Indicate whether a general-dimensional vector is the zero vector.

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

   MATH
   VECTOR

*/

   #include "SpiceUsr.h"
   #undef    vzerog_c

   SpiceBoolean vzerog_c ( ConstSpiceDouble * v, SpiceInt ndim )

/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   v          I   Vector to be tested.
   ndim       I   Dimension of v.

   The function returns the value SPICETRUE if and only if v is the
   zero vector.

-Detailed_Input

   v,
   ndim           are, respectively, a vector and its dimension.

-Detailed_Output

   The function returns the value SPICETRUE if and only if v is the
   zero vector.

-Parameters

   None.

-Exceptions

   Error free.

   1)   When ndim is non-positive, this function returns the value
        SPICEFALSE  (A vector of non-positive dimension cannot be the
        zero vector.)

-Files

   None.

-Particulars

   This function has the same truth value as the logical expression

      ( vnormg_c ( v, ndim )  ==  0. )

   Replacing the above expression by

      vzerog_c ( v, ndim );

   has several advantages:  the latter expresses the test more
   clearly, looks better, and doesn't go through the work of scaling,
   squaring, taking a square root, and re-scaling (all of which
   vnormg_c must do) just to find out that a vector is non-zero.

   A related function is vzero_c, which accepts three-dimensional
   vectors.

-Examples

   1)  When testing whether a vector is the zero vector, one
       normally constructs tests like

          if (  vnormg_c ( v, ndim )  ==  0.  )
             {
                      .
                      .
                      .

       These can be replaced with the code

          if (  vzerog_c ( v, ndim )  )
             {
                      .
                      .
                      .

   2)  Make sure that a `unit' quaternion is non-zero before
       converting it to a rotation matrix.

          if (  vzerog_c ( q, 4 )  )
             {

             [ handle error ]

          else
             {
             vhatg_c ( q, 4, q )
             q2m_c   ( q, m )
                      .
                      .
                      .

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman    (JPL)
   I.M. Underwood  (JPL)
   E.D. Wright     (JPL)

-Version

   -CSPICE Version 1.0.0, 29-JUN-1999

-Index_Entries

   test whether an n-dimensional vector is the zero vector

-&
*/

{ /* Begin vzerog_c */


   /*
   Local variables.
   */
   SpiceInt       i;

   /* ndim must be at least 1. */
   if ( ndim < 1 )
      {
      return SPICEFALSE;
      }


   /* Check for any non-zero entries.  If they exist, test fails. */
   for ( i=0; i < ndim; i++ )
      {
      if ( v[i] != 0. )
         {
         return SPICEFALSE;
         }
      }


   /* If we are here, the vector is zero. */
   return SPICETRUE;



} /* End vzerog_c */
