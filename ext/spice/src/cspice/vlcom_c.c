/*

-Procedure      vlcom_c ( Vector linear combination, 3 dimensions )

-Abstract
 
   Compute a vector linear combination of two double precision, 
   3-dimensional vectors. 
 
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
   #undef    vlcom_c


   void vlcom_c ( SpiceDouble        a, 
                  ConstSpiceDouble   v1[3], 
                  SpiceDouble        b, 
                  ConstSpiceDouble   v2[3], 
                  SpiceDouble        sum[3] ) 
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   a          I   Coefficient of v1 
   v1         I   Vector in 3-space 
   b          I   Coefficient of v2 
   v2         I   Vector in 3-space 
   sum        O   Linear Vector Combination a*v1 + b*v2 
 
-Detailed_Input
 
   a     This double precision variable multiplies v1. 
   v1    This is an arbitrary, double precision 3-dimensional 
          vector. 
   b     This double precision variable multiplies v2. 
   v2    This is an arbitrary, double precision 3-dimensional 
          vector. 
 
-Detailed_Output
 
   sum   is an arbitrary, double precision 3-dimensional vector 
          which contains the linear combination a*v1 + b*v2. 
 
-Parameters
 
   None. 
 
-Particulars
 
   For each index from 0 to 2, this routine implements in C 
   code the expression: 
 
   sum[i] = a*v1[i] + b*v2[i] 
 
   No error checking is performed to guard against numeric overflow. 
 
-Examples
 
   To generate a sequence of points on an ellipse with major 
   and minor axis vectors major and minor, one could use the 
   following code fragment 
 
          step = twopi_c()/ n; 
          ang  = 0.0;

          for ( i = 0; i < n; i++ )
             { 
             vlcom_c ( cos(ang),major,  sin(ang),minor,  point );
 
             do something with the ellipse point just constructed 

             ang = ang + step;
             }
 
   As a second example, suppose that u and v are orthonormal vectors 
   that form a basis of a plane. Moreover suppose that we wish to 
   project a vector x onto this plane, we could use the following 
   call inserts this projection into proj. 
 
          vlcom_c ( vdot_c(x,v),v,   vdot_c(x,u),u,    proj ) 
 
 
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
   E.D. Wright     (JPL)
 
-Literature_References
 
   None 
 
-Version
 
   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

      Made input vectors const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries
 
   linear combination of two 3-dimensional vectors 
 
-&
*/

{ /* Begin vlcom_c */

   /* This really doesn't require a degree in rocket science */
 
   sum[0] = a*v1[0] + b*v2[0];
   sum[1] = a*v1[1] + b*v2[1];
   sum[2] = a*v1[2] + b*v2[2];


} /* End vlcom_c */
