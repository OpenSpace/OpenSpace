/*

-Procedure nvc2pl_c ( Normal vector and constant to plane )

-Abstract
 
   Make a CSPICE plane from a normal vector and a constant. 
 
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
   #undef    nvc2pl_c
   

   void nvc2pl_c ( ConstSpiceDouble     normal[3],
                   SpiceDouble          constant,
                   SpicePlane        *  plane     ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   normal, 
   constant   I   A normal vector and constant defining a plane. 
   plane      O   A CSPICE plane structure representing the plane. 
 
-Detailed_Input
 
   normal, 
   constant       are, respectively, a normal vector and constant
                  defining a plane.  normal need not be a unit vector.
                  Let the symbol < a, b > indicate the inner product of
                  vectors a and b; then the geometric plane is the set
                  of vectors x in three-dimensional space that satisfy
 
                     < x,  normal >  =  constant. 
 
-Detailed_Output
 
   plane          is a CSPICE plane structure that represents the 
                  geometric plane defined by normal and constant. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the input vector normal is the zero vector, the error 
       SPICE(ZEROVECTOR) is signalled. 
 
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
 
   Any of these last three routines may be used to convert this 
   routine's output, plane, to another representation of a 
   geometric plane. 
 
-Examples
 
   1)  Apply a linear transformation represented by the matrix M to 
       a plane represented by the normal vector N and the constant C. 
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
           
                T ( POINT  +  t1 * SPAN1     +  t2 * SPAN2 ) 
      
             =  T (POINT)  +  t1 * T(SPAN1)  +  t2 * T(SPAN2), 
            
          which means that T(POINT), T(SPAN1), and T(SPAN2) 
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
          
          psv2pl_c ( tpoint,   tspan1,  tspan2,  &tplane ); 
          pl2nvc_c ( &tplane,  tn,      &tc              ); 
          
 
-Restrictions
 
   No checking is done to prevent arithmetic overflow. 
 
-Literature_References
 
   [1] `Calculus and Analytic Geometry', Thomas and Finney. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.1, 02-NOV-2009 (NJB)

      Corrected header typo.

   -CSPICE Version 1.0.0, 01-MAR-1999 (NJB)

-Index_Entries
 
   normal vector and constant to plane 
 
-&
*/

{ /* Begin nvc2pl_c */


   /*
   Local variables
   */
   SpiceDouble             mag;



   /*
   This routine checks in only if an error is discovered.
   */

   if ( return_c () ) 
   {
      return;
   }
 
   unorm_c ( normal, plane->normal, &mag );
 
 
   /*
   The normal vector must be non-zero.
   */
   if ( mag == 0. ) 
   {
      chkin_c  ( "nvc2pl_c"                          );
      setmsg_c ( "plane's normal must be non-zero."  );
      sigerr_c ( "SPICE(ZEROVECTOR)"                 );
      chkout_c ( "nvc2pl_c"                          );
      return;
   }
 
 
   /*
   To find the plane constant corresponding to the unitized normal
   vector, we observe that

      < x, normal > = constant,

   so

      < x, normal / || normal || >   =   constant / || normal ||

   */
   
   
   plane->constant = constant / mag;
   
 
   /*
   The constant should be the distance of the plane from the
   origin.  If the constant is negative, negate both it and the
   normal vector.
   */
      
   if ( plane->constant  <  0. ) 
   {
      plane->constant  =   - (plane->constant);
      
      vminus_c ( plane->normal, plane->normal );
   }
 

} /* End nvc2pl_c */

