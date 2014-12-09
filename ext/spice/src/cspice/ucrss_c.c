/*

-Procedure ucrss_c ( Unitized cross product, 3x3 )

-Abstract
 
   Compute the normalized cross product of two 3-vectors. 
 
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
   #undef    ucrss_c


   void ucrss_c ( ConstSpiceDouble   v1[3],
                  ConstSpiceDouble   v2[3], 
                  SpiceDouble        vout[3] ) 
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   v1         I     Left vector for cross product. 
   v2         I     Right vector for cross product. 
   vout       O     Normalized cross product (v1xv2) / |v1xv2|. 
 
-Detailed_Input
 
   v1   A 3-vector. 
 
   v2   A 3-vector. 
 
-Detailed_Output
 
   vout is the result of the computation (v1xv2)/|v1xv2| 
 
-Parameters
 
   None. 
 
-Particulars
 
   None. 
 
-Examples
 
   To get a unit normal to the plane spanned by two vectors 
   v1 and v2. Simply call 
 
       ucrss_c ( v1, v2, normal );
 
-Restrictions
 
   None. 
 
-Exceptions
 
   Error free. 
 
   1) If the cross product of v1 and v2 yields the zero-vector, then 
      the zero-vector is returned instead of a vector of unit length. 
 
-Files
 
   None. 
 
-Author_and_Institution
 
   W.M. Owen       (JPL) 
   W.L. Taber      (JPL) 
 
-Literature_References
 
   None 
 
-Version
 
   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

      Made input vectors const.
      
   -CSPICE Version 1.0.0, 08-FEB-1998   (EDW)

-Index_Entries
 
   unitized cross product 
 
-&
*/

{ /* Begin ucrss_c */


   /*
   Local variables
   */

   SpiceDouble     vcross [ 3 ];
   SpiceDouble     vmag;
 
   SpiceDouble     maxv1;
   SpiceDouble     maxv2;
 
   SpiceDouble     tv1 [ 3 ];
   SpiceDouble     tv2 [ 3 ];
 

   /*
   Get the biggest component of each of the two vectors.
   */

   maxv1 = MaxAbs( v1[0], v1[1] );
   maxv1 = MaxAbs( maxv1, v1[2] );

   maxv2 = MaxAbs( v2[0], v2[1] );
   maxv2 = MaxAbs( maxv2, v2[2] );
 
 
   /*
   Scale v1 and v2 by 1/maxv1 and 1/maxv2 respectively
   */

   if ( maxv1 != 0. )
      {
      tv1[0] = v1[0]/maxv1;
      tv1[1] = v1[1]/maxv1;
      tv1[2] = v1[2]/maxv1;
      }
   else
      {
      tv1[0] = 0.00;
      tv1[1] = 0.00;
      tv1[2] = 0.00;
      }



   if ( maxv2 != 0. )
      {
      tv2[0] = v2[0]/maxv2;
      tv2[1] = v2[1]/maxv2;
      tv2[2] = v2[2]/maxv2;
      }
   else
      {
      tv2[0] = 0.00;
      tv2[1] = 0.00;
      tv2[2] = 0.00;
      }
 

   /*
   Calculate the cross product of v1 and v2
   */

   vcross[0] = tv1[1]*tv2[2] - tv1[2]*tv2[1];
   vcross[1] = tv1[2]*tv2[0] - tv1[0]*tv2[2];
   vcross[2] = tv1[0]*tv2[1] - tv1[1]*tv2[0];


   /*
   Get the magnitude of vcross and normalize it
   */

   vmag = vnorm_c( vcross );
 
   if ( vmag > 0. )
      {
      vout[0] = vcross[0] / vmag;
      vout[1] = vcross[1] / vmag;
      vout[2] = vcross[2] / vmag;
      }
   else
      {
      vout[0] = 0.0;
      vout[1] = 0.0;
      vout[2] = 0.0;
      }


} /* End ucrss_c */
