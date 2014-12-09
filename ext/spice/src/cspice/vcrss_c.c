/*

-Procedure vcrss_c ( Vector cross product, 3 dimensions )

-Abstract

   Compute the cross product of two 3-dimensional vectors.

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
   #undef    vcrss_c
   

   void vcrss_c ( ConstSpiceDouble   v1[3],
                  ConstSpiceDouble   v2[3],
                  SpiceDouble        vout[3] )
/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I     Left hand vector for cross product.
   v2         I     Right hand vector for cross product.
   vout       O     Cross product v1xv2.
                    vout can overwrite either v1 or v2.

-Detailed_Input

   v1      This may be any 3-dimensional vector.  Typically, this
           might represent the (possibly unit) vector to a planet,
           sun, or a star which defines the orientation of axes of
           some coordinate system.

   v2      Ditto.

-Detailed_Output

   vout    This variable represents the cross product of v1 and v2.
           vout may overwrite v1 or v2.

-Parameters

   None.

-Particulars

   vcrss_c calculates the three dimensional cross product of two
   vectors according to the definition.  The cross product is stored
   in a buffer vector until the calculation is complete.  Thus vout
   may overwrite v1 or v2 without interfering with intermediate
   computations.

   If v1 and v2 are large in magnitude (taken together, their
   magnitude surpasses the limit allow by the computer) then it may
   be possible to generate a floating point overflow from an
   intermediate computation even though the actual cross product
   may be well within the range of double precision numbers.
   vcrss_c does NOT check the magnitude of v1 or v2 to insure that
   overflow will not occur.

-Examples

   v1                  v2                  vout (=v1Xv2)
   -----------------------------------------------------------------
   (0, 1, 0)           (1, 0, 0)           (0, 0, -1)
   (5, 5, 5)           (-1, -1, -1)        (0, 0, 0)

-Restrictions

   No checking of v1 or v2 is done to prevent floating point
   overflow. The user is required to determine that the magnitude
   of each component of the vectors is within an appropriate range
   so as not to cause floating point overflow. In almost every case
   there will be no problem and no checking actually needs to be
   done.

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

   -CSPICE Version 1.0.1, 06-MAR-1998 (EDW)
   
      Minor header correction.  Added use of MOVED.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries

   vector cross product

-&
*/

{ /* Begin vcrss_c */

   /*
   Local variables
   */

   SpiceDouble  vtemp[3];


   /*
   Calculate the cross product of v1 and v2, store in vtemp.
   */

   vtemp[0] = v1[1]*v2[2] - v1[2]*v2[1];
   vtemp[1] = v1[2]*v2[0] - v1[0]*v2[2];
   vtemp[2] = v1[0]*v2[1] - v1[1]*v2[0];


   /*
   Now move the result into vout.
   */

   MOVED ( vtemp, 3, vout );


} /* End vcrss_c */
