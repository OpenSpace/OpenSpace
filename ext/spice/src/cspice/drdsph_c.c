/*

-Procedure drdsph_c ( Derivative of rectangular w.r.t. spherical )

-Abstract
 
   This routine computes the Jacobian of the transformation from 
   spherical to rectangular coordinates. 
 
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
 
   COORDINATES 
   DERIVATIVES 
   MATRIX 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"


   void drdsph_c ( SpiceDouble    r,
                   SpiceDouble    colat,
                   SpiceDouble    lon,
                   SpiceDouble    jacobi[3][3] )  
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   r          I   Distance of a point from the origin. 
   colat      I   Angle of the point from the positive z-axis. 
   lon        I   Angle of the point from the xy plane. 
   jacobi     O   Matrix of partial derivatives. 
 
-Detailed_Input
 
   r          Distance of a point from the origin. 
 
   colat      Angle between the point and the positive z-axis, in 
              radians. 
 
   lon        Angle of the point from the xz plane in radians. 
              The angle increases in the counterclockwise sense 
              about the +z axis. 
 
-Detailed_Output
 
   jacobi     is the matrix of partial derivatives of the conversion 
              between spherical and rectangular coordinates,  
              evaluated at the input coordinates.  This matrix has  
              the form 
 
                  .-                                 -. 
                  |  dx/dr     dx/dcolat     dx/dlon  | 
                  |                                   | 
                  |  dy/dr     dy/dcolat     dy/dlon  | 
                  |                                   | 
                  |  dz/dr     dz/dcolat     dz/dlon  | 
                  `-                                 -' 
 
             evaluated at the input values of r, lon and lat. 
             Here x, y, and z are given by the familiar formulae 
 
                 x = r*cos(lon)*sin(colat) 
                 y = r*sin(lon)*sin(colat) 
                 z = r*cos(colat) 
 
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Particulars
 
   It is often convenient to describe the motion of an object in 
   the spherical coordinate system.  However, when performing 
   vector computations its hard to beat rectangular coordinates. 
 
   To transform states given with respect to spherical coordinates 
   to states with respect to rectangular coordinates, one uses
   the Jacobian of the transformation between the two systems. 
 
   Given a state in spherical coordinates 
 
      ( r, colat, lon, dr, dcolat, dlon ) 
 
   the velocity in rectangular coordinates is given by the matrix 
   equation: 
                  t          |                                   t 
      (dx, dy, dz)   = jacobi|              * (dr, dcolat, dlon ) 
                             |(r,colat,lon) 
 
   This routine computes the matrix  
 
            | 
      jacobi| 
            |(r,colat,lon) 
 
-Examples
 
   Suppose that one has a model that gives the radius, colatitude  
   and longitude as a function of time (r(t), colat(t), lon(t)),  
   for which the derivatives ( dr/dt, dcolat/dt, dlon/dt ) are 
   computable. 
 
   To find the velocity of the object in bodyfixed rectangular 
   coordinates, one simply multiplies the Jacobian of the 
   transformation from spherical to rectangular coordinates  
   (evaluated at r(t), colat(t), lon(t) ) by the vector of  
   derivatives of the spherical coordinates. 
 
   In code this looks like:

      #include "SpiceUsr.h"
            .
            .
            .
      /.
      Load the derivatives of r, colat, and lon into the 
      spherical velocity vector sphv. 
      ./
      sphv[0] = dr_dt     ( t );
      sphv[1] = dcolat_dt ( t );
      sphv[2] = dlon_dt   ( t );
 
      /.
      Determine the Jacobian of the transformation from 
      cylindrical to rectangular at the coordinates at the 
      given cylindrical coordinates at time t. 
      ./
      drdsph_c ( r(t), colat(t), lon(t), jacobi );
 
      /.
      Multiply the Jacobian on the left by the spherical 
      velocity to obtain the rectangular velocity recv. 
      ./
      mxv_c ( jacobi, sphv, recv );

-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   W.L. Taber     (JPL) 
   I.M. Underwood (JPL) 
   N.J. Bachman   (JPL)
 
-Version
 
   -CSPICE Version 1.0.0, 19-JUL-2001 (WLT) (IMU) (NJB)

-Index_Entries
 
   Jacobian of rectangular w.r.t. spherical coordinates 
 
-&
*/

{ /* Begin drdsph_c */


   /*
   Don't participate in error tracing; the underlying routine is 
   error-free.
   */
   drdsph_ ( (doublereal *) &r,
             (doublereal *) &colat,
             (doublereal *) &lon,
             (doublereal *) jacobi  );

   /*
   Transpose the Jacobian to create a C-style matrix.
   */
   xpose_c ( jacobi, jacobi );


} /* End drdsph_c */
