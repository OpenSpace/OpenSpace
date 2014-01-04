/*

-Procedure vaddg_c ( Vector addition, general dimension )

-Abstract

    Add two vectors of arbitrary dimension.

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
   #undef   vaddg_c

   void vaddg_c ( ConstSpiceDouble  * v1,
                  ConstSpiceDouble  * v2,
                  SpiceInt            ndim,
                  SpiceDouble       * vout )

/*

-Brief_I/O

    VARIABLE  I/O  DESCRIPTION
    --------  ---  --------------------------------------------------
     v1        I     First vector to be added.
     v2        I     Second vector to be added.
     ndim      I     Dimension of v1, v2, and vout.
     vout      O     Sum vector, v1 + v2.
                     vout can overwrite either v1 or v2.

-Detailed_Input

    v1      This may be any double precision vector of arbitrary
            dimension.

    v2      Likewise.

    ndim    the dimension of v1, v2 and vout.

-Detailed_Output

    vout   This is vector sum of v1 and v2. vout may overwrite either
           v1 or v2.

-Parameters

   None.

-Particulars

    This routine simply performs addition between components of v1
    and v2.  No checking is performed to determine whether floating
    point overflow has occurred.

-Examples

    The following table shows the output vout as a function of the
    the input v1 and v2 from the subroutine vaddg_c.

    v1                  v2                  ndim   vout
    -----------------------------------------------------------------
    [1.0, 2.0, 3.0]     [4.0, 5.0, 6.0]     3      [5.0,  7.0, 9.0]
    [1e-7,1e23]         [1e24, 1e23]        2      [1e24, 2e23]

-Restrictions

    The user is required to determine that the magnitude each
    component of the vectors is within the appropriate range so as
    not to cause floating point overflow.

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

   -CSPICE Version 1.0.1  07-NOV-2003  (EDW)

      Corrected a mistake in the second example's value
      for VOUT, i.e. replaced [1D24, 2D23, 0.0] with
      [1e24, 2e23].

   -CSPICE Version 1.0.0, 29-JUN-1999

-Index_Entries

   n-dimensional vector addition

-&
*/

{ /* Begin vaddg_c */

   /*
   Local variables
   */
   SpiceInt       i;


   /* 
   Do it.  This isn't rocket science. 
   */
   for ( i = 0; i < ndim; i++ )
      {
      vout[i] = v1[i] + v2[i];
      }


} /* End vaddg_c */
