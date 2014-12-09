/*

-Procedure  vnorm_c ( Vector norm, 3 dimensions )

-Abstract

   Compute the magnitude of a double precision, 3-dimensional vector.

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
   #include "SpiceZmc.h"
   #undef    vnorm_c


   SpiceDouble vnorm_c ( ConstSpiceDouble v1[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   Vector whose magnitude is to be found.

   The function returns the norm of v1.
 
-Detailed_Input

   v1             may be any 3-dimensional, double precision vector.
 
-Detailed_Output

   The function returns the magnitude of v1 calculated in a numerically 
   stable way.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   vnorm_c takes care to avoid overflow while computing the norm of the
   input vector v1.  vnorm_c finds the component of v1 whose magnitude 
   is the largest.  Calling this magnitude v1max, the norm is computed 
   using the formula
 
       vnorm_c  =  v1max *  ||  (1/v1max) * v1  ||
       
   where the notation ||x|| indicates the norm of the vector x.

-Examples

   The following table show the correlation between various input
   vectors v1 and vnorm_c:

   v1                                    vnorm_c
   -----------------------------------------------------------------
   ( 1.e0,    2.e0,   2.e0    )          3.e0
   ( 5.e0,    12.e0,  0.e0    )          13.e0
   ( -5.e-17, 0.0e0,  12.e-17 )          13.e-17
       
-Restrictions

   None.
   
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

   -CSPICE Version 1.0.0, 16-APR-1999 (NJB)

-Index_Entries

   norm of 3-dimensional vector
   
-&
*/

{  /* Begin vnorm_c */

   /*
   Local variables
   */
   SpiceDouble                        normSqr;
   SpiceDouble                        tmp0;
   SpiceDouble                        tmp1;
   SpiceDouble                        tmp2;
   SpiceDouble                        v1max;


   /*
   Determine the maximum component of the vector.
   */
   v1max = MaxAbs(  v1[0],   MaxAbs( v1[1], v1[2] )   );
   
   
   /*
   If the vector is zero, return zero; otherwise normalize first.
   Normalizing helps in the cases where squaring would cause overflow
   or underflow.  In the cases where such is not a problem it not worth
   it to optimize further.
   */ 
   
   if ( v1max == 0.0 ) 
   {
      return ( 0.0 );
   }
   else
   {
      tmp0     =  v1[0]/v1max;
      tmp1     =  v1[1]/v1max;
      tmp2     =  v1[2]/v1max;
      
      normSqr  =  tmp0*tmp0 + tmp1*tmp1 + tmp2*tmp2;
        
      return (  v1max * sqrt( normSqr )  );
   }
 
 
} /* End vnorm_c */
