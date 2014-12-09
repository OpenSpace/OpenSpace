/*

-Procedure vprjp_c ( Vector projection onto plane )

-Abstract
 
   Project a vector onto a specified plane, orthogonally. 
 
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
   VECTOR 
 
*/

   #include "SpiceUsr.h"
   #undef    vprjp_c


   void vprjp_c ( ConstSpiceDouble    vin   [3],
                  ConstSpicePlane   * plane,
                  SpiceDouble         vout  [3] ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   vin        I   Vector to be projected. 
   plane      I   A CSPICE plane onto which vin is projected. 
   vout       O   Vector resulting from projection. 
 
-Detailed_Input
 
   vin            is a 3-vector that is to be orthogonally projected 
                  onto a specified plane. 
 
   plane          is a CSPICE plane that represents the geometric 
                  plane onto which vin is to be projected. 
 
-Detailed_Output
 
   vout           is the vector resulting from the orthogonal 
                  projection of vin onto plane.  vout is the closest 
                  point in the specified plane to vin. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  Invalid input planes are diagnosed by the routine pl2nvc_c, 
       which is called by this routine. 
 
-Files
 
   None. 
 
-Particulars
 
   Projecting a vector v orthogonally onto a plane can be thought of 
   as finding the closest vector in the plane to v.  This `closest 
   vector' always exists; it may be coincident with the original 
   vector. 
 
   Two related routines are vprjpi_c, which inverts an orthogonal 
   projection of a vector onto a plane, and vproj_c, which projects 
   a vector orthogonally onto another vector. 
 
-Examples
 
   1)   Find the closest point in the ring plane of a planet to a 
        spacecraft located at positn (in body-fixed coordinates). 
        Suppose the vector normal is normal to the ring plane, and 
        that origin, which represents the body center, is in the 
        ring plane.  Then we can make a `plane' with the code 
 
           pnv2pl_c ( origin, normal, &plane ); 
 
        can find the projection by making the call 
 
           vprjp_c ( positn, &plane, proj ); 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   [1] `Calculus and Analytic Geometry', Thomas and Finney. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 05-MAR-1999 (NJB)

-Index_Entries
 
   vector projection onto plane 
 
-&
*/

{ /* Begin vprjp_c */


   /*
   Local variables
   */
   SpiceDouble             constant;
   SpiceDouble             normal    [3];


   /*
   Participate in error tracing.
   */

   if ( return_c() ) 
   {
      return;
   }
   
   chkin_c ( "vprjp_c" );


   /*
   Obtain a unit vector normal to the input plane, and a constant
   for the plane.
   */
   pl2nvc_c ( plane, normal, &constant );
 
   
   /*
   Let the notation < a, b > indicate the inner product of vectors
   a and b.

   vin differs from its projection onto plane by some multiple of
   normal.  That multiple is


             < vin - vout, normal >                 *  normal

      =   (  < vin, normal > - < vout, normal >  )  *  normal

      =   (  < vin, normal > - const             )  *  normal


   Subtracting this multiple of normal from vin yields vout.
   */
 
   vlcom_c (  1.0,
              vin,
              constant - vdot_c ( vin, normal ),
              normal,
              vout                              );
 
 
   chkout_c ( "vprjp_c" );

} /* End vprjp_c */

