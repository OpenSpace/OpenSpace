/*

-Procedure  vdot_c ( Vector dot product, 3 dimensions )

-Abstract

   Compute the dot product of two double precision, 3-dimensional
   vectors.

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

   #include <math.h>
   #include "SpiceUsr.h"
   #undef    vdot_c
   

   SpiceDouble vdot_c ( ConstSpiceDouble   v1[3], 
                        ConstSpiceDouble   v2[3] )
/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   First vector in the dot product.
   v2         I   Second vector in the dot product.

   The function returns the value of the dot product of v1 and v2.
 
-Detailed_Input

    v1      This may be any 3-dimensional, double precision vector.
 
    v2      This may be any 3-dimensional, double precision vector.

-Detailed_Output

   The function returns the value of the dot product of v1 and v2.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   vdot_c calculates the dot product of v1 and v2 by a simple
   application of the definition.  No error checking is performed to
   prevent numeric overflow.

-Examples

   Suppose that given two position vectors, we want to change
   one of the positions until the two vectors are perpendicular.
   The following code fragment demonstrates the use of vdot_c to do so.
 
       dot = vdot_c ( v1, v2 )
 
       while ( fabs(dot) > tolerance )
       {
          [ CHANGE ONE OF THE POSITION VECTORS ]
          
          dot = vdot_c ( v1, v2 )
       }


-Restrictions

   The user is responsible for determining that the vectors v1 and
   v2 are not so large as to cause numeric overflow.  In most cases
   this won't present a problem.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman       (JPL)
   W.L. Taber         (JPL)
   W.M. Owen          (JPL)

-Version

   -CSPICE Version 1.0.2, 16-JAN-2008   (EDW)

      Corrected typos in header titles:
      
      Detailed Input to Detailed_Input
      Detailed Output to Detailed_Output
      
   -CSPICE Version 1.0.1, 12-NOV-2006 (EDW)

      Added Parameters section header.

   -CSPICE Version 1.0.0, 16-APR-1999 (EDW)

-Index_Entries

   dot product 3-dimensional vectors

-&
*/

{  /* Begin vdot_c */


   return ( v1[0]*v2[0] + v1[1]*v2[1] + v1[2]*v2[2] );


} /* End vdot_c */
