/*

-Procedure vlcom3_c ( Vector linear combination, 3 dimensions )

-Abstract
 
   This subroutine computes the vector linear combination 
   a*v1 + b*v2 + c*v3 of double precision, 3-dimensional vectors. 
 
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
   #undef    vlcom3_c


   void vlcom3_c ( SpiceDouble        a, 
                   ConstSpiceDouble   v1 [3], 
                   SpiceDouble        b, 
                   ConstSpiceDouble   v2 [3], 
                   SpiceDouble        c, 
                   ConstSpiceDouble   v3 [3], 
                   SpiceDouble        sum[3]  ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   a          I   Coefficient of v1 
   v1         I   Vector in 3-space 
   b          I   Coefficient of v2 
   v2         I   Vector in 3-space 
   c          I   Coefficient of v3 
   v3         I   Vector in 3-space 
   sum        O   Linear Vector Combination a*v1 + b*v2 + c*v3 
 
-Detailed_Input
 
   a     is a double precision number. 
 
   v1    is a double precision 3-dimensional vector. 
 
   b     is a double precision number. 
 
   v2    is a double precision 3-dimensional vector. 
 
   c     is a double precision number. 
 
   v3    is a double precision 3-dimensional vector. 
 
-Detailed_Output
 
   sum   is a double precision 3-dimensional vector which contains 
         the linear combination a*v1 + b*v2 + c*v3 
 
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Particulars
 
   For each index from 0 to 2, this routine implements in FORTRAN 
   code the expression: 
 
   sum[i] = a*v1[i] + b*v2[i] + c*v3[i] 
 
   No error checking is performed to guard against numeric overflow. 
 
-Examples
 
   Often one has the components (a,b,c) of a vector in terms 
   of a basis v1, v2, v3.  The vector represented by (a,b,c) can 
   be obtained immediately from the call 
 
   vlcom3_c ( a, v1, b, v2, c, v3,   VECTOR ) 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   W.L. Taber      (JPL) 
   E.D. Wright     (JPL)
 
-Version
 
   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

      Made input vectors const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries
 
   linear combination of three 3-dimensional vectors 
 
-&
*/

{ /* Begin vlcom3_c */


   /* This really doesn't require a degree in rocket science */
 
   sum[0] = a*v1[0] + b*v2[0] + c*v3[0];
   sum[1] = a*v1[1] + b*v2[1] + c*v3[1];
   sum[2] = a*v1[2] + b*v2[2] + c*v3[2];


} /* End vlcom3_c */
