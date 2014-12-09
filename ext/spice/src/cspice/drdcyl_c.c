/*

-Procedure drdcyl_c (Derivative of rectangular w.r.t. cylindrical)

-Abstract
 
   This routine computes the Jacobian of the transformation from 
   cylindrical to rectangular coordinates. 
 
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


   void drdcyl_c ( SpiceDouble    r,
                   SpiceDouble    lon,
                   SpiceDouble    z,
                   SpiceDouble    jacobi[3][3] ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   r          I   Distance of a point from the origin. 
   lon        I   Angle of the point from the xz plane in radians. 
   z          I   Height of the point above the xy plane. 
   jacobi     O   Matrix of partial derivatives. 
 
-Detailed_Input
 
   r          Distance of the point of interest from z axis. 
 
   lon        Cylindrical angle (in radians) of the point of 
              interest from xz plane.  The angle increases in the  
              counterclockwise sense about the +z axis. 
 
   z          Height of the point above xy plane. 
 
-Detailed_Output
 
   jacobi     is the matrix of partial derivatives of the conversion 
              between cylindrical and rectangular coordinates.  It 
              has the form 
 
                 .-                               -. 
                 |  dx/dr     dx/dlon     dx/dz    | 
                 |                                 | 
                 |  dy/dr     dy/dlon     dy/dz    | 
                 |                                 | 
                 |  dz/dr     dz/dlon     dz/dz    | 
                 `-                               -' 
 
              evaluated at the input values of r, lon and z. 
              Here x,y, and z are given by the familiar formulae 
 
                 x = r*cos(lon) 
                 y = r*sin(lon) 
                 z = z 
 
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Particulars
 
   It is often convenient to describe the motion of an object in 
   the cylindrical coordinate system.  However, when performing 
   vector computations its hard to beat rectangular coordinates. 
 
   To transform states given with respect to cylindrical coordinates 
   to states with respect to rectangular coordinates, one uses 
   the Jacobian of the transformation between the two systems. 
 
   Given a state in cylindrical coordinates 
 
      ( r, lon, z, dr, dlon, dz ) 
 
   the velocity in rectangular coordinates is given by the matrix 
   equation: 
                  t          |                          t 
      (dx, dy, dz)   = jacobi|          * (dr, dlon, dz) 
                             |(r,lon,z) 
 
   This routine computes the matrix  
 
            | 
      jacobi| 
            |(r,lon,z) 
 
-Examples
 
   Suppose that one has a model that gives radius, longitude and 
   height as a function of time (r(t), lon(t), z(t)) for 
   which the derivatives ( dr/dt, dlon/dt, dz/dt ) are computable. 
 
   To find the corresponing velocity in bodyfixed rectangular 
   coordinates, one simply multiplies the Jacobian of the 
   transformation from cylindrical to rectangular coordinates  
   (evaluated at r(t), lon(t), z(t) ) by the vector of derivatives  
   of the cylindrical coordinates. 
 
   In code this looks like: 
 
      #include "SpiceUsr.h"
            .
            .
            .
      /.
      Load the derivatives of r, lon, and z into the 
      cylindrical velocity vector sphv. 
      ./
      cylv[0] = dr_dt   ( t );
      cylv[1] = dlon_dt ( t );
      cylv[2] = dz_dt   ( t );
 
      /.
      Determine the Jacobian of the transformation from 
      cylindrical to rectangular at the coordinates at the 
      given cylindrical coordinates at time t. 
      ./
      drdcyl_c ( r(t), lon(t), z(t), jacobi );
 
      /.
      Multiply the Jacobian on the left by the cylindrical 
      velocity to obtain the rectangular velocity recv. 
      ./
      mxv_c ( jacobi, cylv, recv );
 
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
 
   Jacobian of rectangular w.r.t. cylindrical coordinates 
 
-&
*/

{ /* Begin drdcyl_c */

   /*
   Don't participate in error tracing; the underlying routine is 
   error-free.
   */
   drdcyl_ ( (doublereal *) &r,
             (doublereal *) &lon,
             (doublereal *) &z,
             (doublereal *) jacobi  );

   /*
   Transpose the Jacobian to create a C-style matrix.
   */
   xpose_c ( jacobi, jacobi );

} /* End drdcyl_c */
