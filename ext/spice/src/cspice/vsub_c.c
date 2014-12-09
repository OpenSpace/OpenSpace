/*

-Procedure vsub_c ( Vector subtraction, 3 dimensions )

-Abstract
 
   Compute the difference between two 3-dimensional, double 
   precision vectors. 
 
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
   #undef    vsub_c


   void vsub_c ( ConstSpiceDouble   v1[3],
                 ConstSpiceDouble   v2[3],
                 SpiceDouble        vout[3] ) 
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   v1         I     First vector (minuend). 
   v2         I     Second vector (subtrahend). 
   vout       O     Difference vector, v1 - v2. vout can overwrite 
                    either v1 or v2. 
 
-Detailed_Input
 
   v1    This can be any 3-dimensional, double precision vector. 
 
   v2    Ditto. 
 
-Detailed_Output
 
   vout   This is a 3-dimensional, double precision vector which 
           represents the vector difference, v1 - v2. 
 
-Parameters
 
   None. 
 
-Particulars
 
   This routine simply performs subtraction between components of v1 
   and v2.  No checking is performed to determine whether floating 
   point overflow has occurred. 
 
-Examples
 
   The following table shows the output vout as a function of the 
   the input v1 and v2 from the subroutine vsub_c. 
 
   v1                  v2              ---> vout 
   --------------      --------------       -------------- 
   [1.0 , 2.0, 3.0]    [4.0 , 5.0 , 6.0]    [-3.0 , -3.0, -3.0] 
   [1e-7,1e23, 0.0]    [1e24, 1e23, 0.0]    [-1e24,  0.0,  0.0]
 
-Restrictions
 
   The user is required to determine that the magnitude each 
   component of the vectors is within the appropriate range so as 
   not to cause floating point overflow.  No error recovery or 
   reporting scheme is incorporated in this subroutine. 
 
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

    -CSPICE Version 1.1.1, 07-NOV-2003 (EDW)

      Corrected a mistake in the second example's value
      for VOUT, i.e. replaced [1D24, 2D23, 0.0] with
      [-1e24, 0.0, 0.0].

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

      Made input vectors const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries
 
   3-dimensional vector subtraction 
 
-&
*/

{ /* Begin vsub_c */


   vout[0] = v1[0] - v2[0];
   vout[1] = v1[1] - v2[1];
   vout[2] = v1[2] - v2[2];


} /* End vsub_c */
