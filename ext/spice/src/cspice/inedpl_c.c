/*

-Procedure inedpl_c ( Intersection of ellipsoid and plane )

-Abstract
 
   Find the intersection of a triaxial ellipsoid and a plane. 
 
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
 
   ELLIPSES 
   PLANES 
 
-Keywords
 
   ELLIPSE 
   ELLIPSOID 
   GEOMETRY 
   MATH 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    inedpl_c
   

   void inedpl_c ( SpiceDouble           a,
                   SpiceDouble           b,
                   SpiceDouble           c,
                   ConstSpicePlane     * plane,
                   SpiceEllipse        * ellipse,
                   SpiceBoolean        * found    )
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   a          I   Length of ellipsoid semi-axis lying on the x-axis. 
   b          I   Length of ellipsoid semi-axis lying on the y-axis. 
   c          I   Length of ellipsoid semi-axis lying on the z-axis. 
   plane      I   Plane that intersects ellipsoid. 
   ellipse    O   Intersection ellipse, when found is SPICETRUE.
   found      O   Flag indicating whether ellipse was found. 
 
-Detailed_Input
 
   a, 
   b, 
   c              are the lengths of the semi-axes of a triaxial 
                  ellipsoid.  The ellipsoid is centered at the 
                  origin and oriented so that its axes lie on the 
                  x, y and z axes.  a, b, and c are the lengths of 
                  the semi-axes that point in the x, y, and z 
                  directions respectively. 
 
   plane          is a CSPICE plane. 
 
-Detailed_Output
 
   ellipse        is the CSPICE ellipse formed by the intersection 
                  of the input plane and ellipsoid. ellipse will 
                  represent a single point if the ellipsoid and 
                  plane are tangent. 
 
                  If the intersection of the ellipsoid and plane is 
                  empty, ellipse is not modified. 
 
 
   found          is SPICETRUE if and only if the intersection of the 
                  ellipsoid and plane is non-empty. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If any of the lengths of the semi-axes of the input ellipsoid 
       are non-positive, the error SPICE(DEGENERATECASE) is 
       signalled. ellipse is not modified. found is set to SPICEFALSE.
 
   2)  If the input plane in invalid, the error will be diagnosed by 
       routines called by this routine. ellipse is not modified. 
       found is set to SPICEFALSE.
 
   3)  If the input plane and ellipsoid are very nearly tangent, 
       roundoff error may cause this routine to give unreliable 
       results. 
 
   4)  If the input plane and ellipsoid are precisely tangent, the 
       intersection is a single point.  In this case, the output 
       ellipse is degenerate, but found will still have the value 
       SPICETRUE.  You must decide whether this output makes sense for 
       your application. 
 
-Files
 
   None. 
 
-Particulars
 
   An ellipsoid and a plane can intersect in an ellipse, a single 
   point, or the empty set. 
 
-Examples
 
   1)  Suppose we wish to find the limb of a body, as observed from 
       location loc in body-fixed coordinates.  The CSPICE routine 
       edlimb_c solves this problem.  Here's how inedpl_c is used in 
       that solution. 
 
       We assume loc is outside of the body. The body is modelled as 
       a triaxial ellipsoid with semi-axes of length a, b, and c. 
       The notation 
 
          < x, y > 
 
       indicates the inner product of the vectors x and y. 
 
       The limb lies on the plane defined by 
 
          < x,  n >  =  1, 
 
       where the vector n is defined as 
 
                      2              2              2
          ( loc[0] / a ,   loc[1] / b ,   loc[2] / c  )  
 
       The assignments 
 
          n[0] = loc[0] / (a*a);
          n[1] = loc[1] / (b*b);
          n[2] = loc[2] / (c*c);
 
       and the calls 
 
          nvc2pl_c ( n,  1.0,  &plane ); 
 
          inedpl_c ( a,  b,  c,  &plane,  &limb, &found ); 
 
          el2cgv_c ( limb, center, smajor, sminor );
 
       will return the center and semi-axes of the limb. 
 
 
       How do we know that  < x, n > = 1  for all x on the limb? 
       This is because all limb points x satisfy 
 
          < loc - x, surfnm(x) >  =  0, 
 
       where surfnm(x) is any surface normal at x.  surfnm(x) is 
       parallel to the vector 
 
                         2            2            2
          v = (  x[0] / a ,   x[1] / b ,   x[2] / c   ) 
 
       so we have 
 
          < loc - x, v >  =  0, 
 
          < loc, v >      =  < x, v >  =  1  (from the original 
                                              ellipsoid 
                                              equation)
       and finally 
 
          < x, n >  =  1
          
       where n is as defined above.
 
 
 
   2)  Suppose we wish to find the terminator of a body.  We can 
       make a fair approximation to the location of the terminator 
       by finding the limb of the body as seen from the vertex of 
       the umbra; then the problem is essentially the same as in 
       example 1.  Let VERTEX be this location.  We make the 
       assignments 
 
          p[0] =   vertex[0] / (a*a);
          p[1] =   vertex[1] / (b*b);
          p[2] =   vertex[2] / (c*c);
 
       and then make the calls 
 
          nvc2pl_c ( p,  1.0,  &plane ); 
 
          inedpl_c ( a,  b,  c,  &plane, &term, &found ); 
 
       The CSPICE ellipse term represents the terminator of the 
       body. 
 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.5, 06-FEB-2003 (EDW)

      Corrected a typo in the header documentation,
      input variable 'ellipse' not 'ellips'

   -CSPICE Version 1.0.0, 13-JUN-1999 (NJB)

-Index_Entries
 
   intersection of ellipsoid and plane 
 
-&
*/

{ /* Begin inedpl_c */

   /*
   Local variables
   */

   SpiceDouble             center  [3];
   SpiceDouble             dist;
   SpiceDouble             dstort  [3];
   SpiceDouble             invdst  [3];
   SpiceDouble             maxrad;
   SpiceDouble             point   [3];
   SpiceDouble             rcircl;
   SpiceDouble             span1   [3];
   SpiceDouble             span2   [3];
   SpiceDouble             vec1    [3];
   SpiceDouble             vec2    [3];

   SpiceInt                i;

   SpicePlane              dplane;
   
   
   /*
   Participate in error tracing.
   */

   chkin_c ( "inedpl_c" );


   /*
   We don't want to worry about flat ellipsoids:
   */
   if (         ( a <= 0. )            
          ||    ( b <= 0. )            
          ||    ( c <= 0. )   )    
   {
   
      *found = SPICEFALSE;

      setmsg_c ( "semi-axes: a = #,  b = #,  c = #."  );
      errdp_c  ( "#", a                               );
      errdp_c  ( "#", b                               );
      errdp_c  ( "#", c                               );
      sigerr_c ( "SPICE(DEGENERATECASE)"              );
      chkout_c ( "inedpl_c"                           );
      return;
   }
 
 
   /*
   This algorithm is partitioned into a series of steps:


   1)  Identify a linear transformation that maps the input
       ellipsoid to the unit sphere.  We'll this mapping the
       `distortion' mapping.  Apply the distortion mapping to both
       the input plane and ellipsoid.  The image of the plane under
       this transformation will be a plane.

   2)  Find the intersection of the transformed plane and the unit
       sphere.

   3)  Apply the inverse of the distortion mapping to the
       intersection ellipse to find the undistorted intersection
       ellipse.



   Step 1:

   Find the image of the ellipsoid and plane under the distortion
   matrix.  Since the image of the ellipsoid is the unit sphere,
   only the plane transformation requires any work.

   If the input plane is too far from the origin to possibly
   intersect the ellipsoid, return now.  This can save us
   some numerical problems when we scale the plane and ellipsoid.

   The point returned by PL2PSV is the closest point in PLANE
   to the origin, so its norm gives the distance of the plane
   from the origin.
   */
   
   pl2psv_c  ( plane, point, span1, span2 );

   maxrad  =  MaxAbs ( a, b      );
   maxrad  =  MaxAbs ( c, maxrad );

 
   if ( vnorm_c(point) > maxrad )
   {
      *found = SPICEFALSE; 
      chkout_c ( "inedpl_c" );
      return; 
   }
 
 
   /*
   The distortion matrix and its inverse are

      +-               -+        +-               -+
      |  1/a   0    0   |        |   a    0    0   |
      |   0   1/b   0   |,       |   0    b    0   |.
      |   0    0   1/c  |        |   0    0    c   |
      +-               -+        +-               -+

   We declare them with length three, since we are going to make
   use of the diagonal elements only.
   */
   
   dstort[0] = 1. / a;
   dstort[1] = 1. / b;
   dstort[2] = 1. / c;
 
   invdst[0] =  a;
   invdst[1] =  b;
   invdst[2] =  c;
   
 
   /*
   Apply the distortion mapping to the input plane.  Applying
   the distortion mapping to a point and two spanning vectors that
   define the input plane yields a point and two spanning vectors
   that define the distorted plane.
   */
   
   for ( i = 0;  i < 3;  i++ )
   {
      point[i]  =  dstort[i]  *  point[i];
      span1[i]  =  dstort[i]  *  span1[i];
      span2[i]  =  dstort[i]  *  span2[i];
   }
 
   psv2pl_c ( point, span1, span2, &dplane );
 
 
   /*
   Step 2:

   Find the intersection of the distorted plane and unit sphere.


   The intersection of the distorted plane and the unit sphere
   may be a circle, a point, or the empty set.  The distance of the
   plane from the origin determines which type of intersection we
   have.  If we represent the distorted plane by a unit normal
   vector and constant, the size of the constant gives us the
   distance of the plane from the origin.  If the distance is greater
   than 1, the intersection of plane and unit sphere is empty. If
   the distance is equal to 1, we have the tangency case.

   The routine PL2PSV always gives us an output point that is the
   closest point to the origin in the input plane.  This point is
   the center of the intersection circle.  The spanning vectors
   returned by PL2PSV, after we scale them by the radius of the
   intersection circle, become an orthogonal pair of vectors that
   extend from the center of the circle to the circle itself.  So,
   the center and these scaled vectors define the intersection
   circle.
   */
   
   pl2psv_c  ( &dplane, center, vec1, vec2 );

   dist = vnorm_c ( center );

   if ( dist > 1. )
   {
      *found = SPICEFALSE;
      chkout_c ( "inedpl_c" );
      return;
   }
 
 
   /*
   Scale the generating vectors by the radius of the intersection
   circle.
   */
   
   rcircl =  sqrt (   brcktd_c ( 1. - dist*dist,  0.,  1. )  );

   vscl_c ( rcircl, vec1, vec1 );
   vscl_c ( rcircl, vec2, vec2 );
 
 
   /*
   Step 3:

   Apply the inverse distortion to the intersection circle to find
   the actual intersection ellipse.
   */
   
   for ( i = 0;  i < 3;  i++ )
   {
      center[i]  =  invdst[i]  *  center[i];
      vec1[i]    =  invdst[i]  *  vec1[i];
      vec2[i]    =  invdst[i]  *  vec2[i];
   }

 
   /*
   Make an ellipse from the center and generating vectors.
   */
   cgv2el_c ( center, vec1, vec2, ellipse );

   *found  =  SPICETRUE;
 
 
   chkout_c ( "inedpl_c" );

} /* End inedpl_c */
