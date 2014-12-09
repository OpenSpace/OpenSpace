/*

-Procedure      vscl_c ( Vector scaling, 3 dimensions )

-Abstract
 
   Multiply a scalar and a 3-dimensional double precision vector. 
 
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
   #undef    vscl_c


   void vscl_c ( SpiceDouble        s,
                 ConstSpiceDouble   v1[3],
                 SpiceDouble        vout[3] ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
    s         I     Scalar to multiply a vector. 
    v1        I     Vector to be multiplied. 
    vout      O     Product vector, s*v1. vout can overwrite v1. 
 
-Detailed_Input
 
   s    This is a double precision scalar used to multiply the 
        vector v1. 
 
   v1   This is a 3-dimensional, double precision vector which is 
        to be scaled by s. 
 
-Detailed_Output
 
   vout   This is a 3-dimensional, double precision vector which 
          is the scalar multiple of v1.  vout = s*v1. 
 
-Parameters
 
   None. 
 
-Particulars
 
   vscl_c multiplies each component of v1 by s to form the respective 
   components of vout.  No error checking is performed. 
 
-Examples
 
   The following table shows the output vout as a function of the 
   the inputs v1, and s from the subroutine vscl_c. 
 
   v1                   s         vout 
   ------------------------------------------------------- 
   (1, -2, 0)          -1       (-1, 2, 0) 
   (0,  0, 0)           5       ( 0, 0, 0) 
 
-Restrictions
 
   The user is responsible for insuring that no floating point 
   overflow occurs from multiplying s by any component of v1. 
   No error recovery or reporting scheme is incorporated in this 
   subroutine. 
 
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

      Made input vector const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries
 
   3-dimensional vector scaling 
 
-&
*/

{ /* Begin vscl_c */

   vout[0] = s * v1[0];
   vout[1] = s * v1[1];
   vout[2] = s * v1[2];


} /* End vscl_c */
