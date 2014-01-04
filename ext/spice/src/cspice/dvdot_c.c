/*

-Procedure dvdot_c ( Derivative of Vector Dot Product, 3-D )

-Abstract

   Compute the derivative of the dot product of two double
   precision position vectors.

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
   DERIVATIVE

*/

   #include "SpiceUsr.h"
   #undef   dvdot_c

   SpiceDouble dvdot_c ( ConstSpiceDouble s1[6],
                         ConstSpiceDouble s2[6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   s1         I   First state vector in the dot product.
   s2         I   Second state vector in the dot product.

   The function returns the derivative of the dot product <s1,s2>

-Detailed_Input

   s1      Any state vector.  The components are in order
           (x, y, z, dx/dt, dy/dt, dz/dt )

   s2      Any state vector.

-Detailed_Output

   The function returns the derivative of the dot product of the
   position portions of the two state vectors s1 and s2.

-Parameters

   None.

-Files

   None.

-Exceptions

   Error free.

-Particulars

   Given two state vectors s1 and s2 made up of position and
   velocity components (p1,v1) and (p2,v2) respectively,
   dvdot_c calculates the derivative of the dot product of p1 and p2,
   i.e. the time derivative

         d
         -- < p1, p2 > = < v1, p2 > + < p1, v2 >
         dt

   where <,> denotes the dot product operation.

-Examples

   Suppose that given two state vectors (s1 and s2)whose position
   components are unit vectors, and that we need to compute the
   rate of change of the angle between the two vectors.

   We know that the Cosine of the angle (theta) between the vectors is
   given by

      cosine(theta) = vdot_c(s1,s2)

   Thus by the chain rule, the derivative of the angle is given
   by:

      sine(theta) dtheta/dt = dvdot_c(s1,s2)

   Thus for values of theta away from zero we can compute

   dtheta/dt as

   dtheta = dvdot_c(s1,s2) / sqrt ( 1 - vdot_c(s1,s2)**2 )

   Note if the position components of s1 and s2 are parallel, the
   derivative of the  angle between the positions does not
   exist.  Any code that computes the derivative of the angle
   between two position vectors should account for the case
   when the position components are parallel.

-Restrictions

   The user is responsible for determining that the states s1 and
   s2 are not so large as to cause numeric overflow.  In most cases
   this won't present a problem.

-Author_and_Institution

   W.L. Taber      (JPL)
   E.D. Wright     (JPL)

-Literature_References

   None.

-Version

   -CSPICE Version 1.0.0, 7-JUL-1999

-Index_Entries

   Compute the derivative of a dot product

-&
*/

{ /* Begin dvdot_c */

   return (  s1[0]*s2[3] + s1[1]*s2[4] + s1[2]*s2[5]
           + s1[3]*s2[0] + s1[4]*s2[1] + s1[5]*s2[2] );

} /* End dvdot_c */
