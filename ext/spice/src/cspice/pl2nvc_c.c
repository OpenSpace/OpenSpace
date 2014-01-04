/*

-Procedure pl2nvc_c ( Plane to normal vector and constant )

-Abstract
 
   Return a unit normal vector and constant that define a specified 
   plane. 
 
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
 
   PLANES 
 
-Keywords
 
   GEOMETRY 
   MATH 
   PLANE 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #undef    pl2nvc_c
   
   
   void pl2nvc_c ( ConstSpicePlane   * plane,
                   SpiceDouble         normal[3],
                   SpiceDouble       * constant  ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   plane      I   A CSPICE plane. 
   normal, 
   constant   O   A normal vector and constant defining the 
                  geometric plane represented by plane. 
 
-Detailed_Input
 
   plane          is a CSPICE plane. 
 
-Detailed_Output
 
   normal, 
   constant       are, respectively, a unit normal vector and 
                  constant that define the geometric plane 
                  represented by plane.  Let the symbol < a, b > 
                  indicate the inner product of vectors a and b; 
                  then the geometric plane is the set of vectors x 
                  in three-dimensional space that satisfy 
 
                     < x,  normal >  =  constant. 
 
                  normal is a unit vector.  constant is the distance of 
                  the plane from the origin; 
 
                     constant * normal 
 
                  is the closest point in the plane to the origin. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
   1)  The input plane MUST have been created by one of the CSPICE 
       routines 
 
          nvc2pl_c ( Normal vector and constant to plane ) 
          nvp2pl_c ( Normal vector and point to plane    ) 
          psv2pl_c ( Point and spanning vectors to plane ) 
 
       Otherwise, the results of this routine are unpredictable. 
 
-Files
 
   None. 
 
-Particulars
 
   CSPICE geometry routines that deal with planes use the `plane' 
   data type to represent input and output planes.  This data type 
   makes the subroutine interfaces simpler and more uniform. 
 
   The CSPICE routines that produce CSPICE planes from data that 
   define a plane are: 
 
      nvc2pl_c ( Normal vector and constant to plane ) 
      nvp2pl_c ( Normal vector and point to plane    ) 
      psv2pl_c ( Point and spanning vectors to plane ) 
 
   The CSPICE routines that convert CSPICE planes to data that 
   define a plane are: 
 
      pl2nvc_c ( Plane to normal vector and constant ) 
      pl2nvp_c ( Plane to normal vector and point    ) 
      pl2psv_c ( Plane to point and spanning vectors ) 
 
-Examples
 
   1)  Given a point in a plane and a normal vector, find the distance 
       of the plane from the origin.  We make a `plane' from the point
       and normal, then convert the plane to a unit normal and constant.
       The constant is the distance of the plane from the origin. 
 
          nvp2pl_c ( normal, point,  &plane    ); 
          pl2nvc_c ( &plane, normal, &constant ); 
 
 
   2)  Apply a linear transformation represented by the matrix m to 
       a plane represented by the normal vector n and the constant c. 
       Find a normal vector and constant for the transformed plane. 

          /. 
          Make a CSPICE plane from n and c, and then find a 
          point in the plane and spanning vectors for the 
          plane.  n need not be a unit vector. 
          ./ 
          nvc2pl_c ( n,       c,     &plane         ); 
          pl2psv_c ( &plane,  point,  span1,  span2 );
           
 
          /.
          Apply the linear transformation to the point and 
          spanning vectors.  All we need to do is multiply 
          these vectors by m, since for any linear 
          transformation T, 
           
                T ( point  +  t1 * span1     +  t2 * span2 ) 
      
             =  T (point)  +  t1 * T(span1)  +  t2 * T(span2), 
            
          which means that T(point), T(span1), and T(span2) 
          are a point and spanning vectors for the transformed 
          plane. 
          ./
      
          mxv_c ( m, point, tpoint ); 
          mxv_c ( m, span1, tspan1 ); 
          mxv_c ( m, span2, tspan2 ); 
 
          /.
          Make a new CSPICE plane tplane from the 
          transformed point and spanning vectors, and find a 
          unit normal and constant for this new plane. 
          ./
          
          psv2pl_c ( tpoint,   tspan1,  tspan2,   &tplane ); 
          pl2nvc_c ( &tplane,  tn,      &tc               ); 
          

 
-Restrictions
 
   None. 
 
-Literature_References
 
   [1] `Calculus and Analytic Geometry', Thomas and Finney. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.1, 06-FEB-2003 (EDW)

       Trivial correction to header docs.

   -CSPICE Version 1.0.0, 05-MAR-1999 (NJB)

-Index_Entries
 
   plane to normal vector and constant 
 
-&
*/

{ /* Begin pl2nvc_c */

   
   /*
   Unpack the plane.
   */

   MOVED ( plane->normal, 3, normal );
   
   *constant = plane->constant;


} /* End pl2nvc_c */

