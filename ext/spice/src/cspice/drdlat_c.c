/*

-Procedure drdlat_c ( Derivative of rectangular w.r.t. latitudinal )

-Abstract
 
   Compute the Jacobian of the transformation from latitudinal to 
   rectangular coordinates. 
 
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

   void drdlat_c ( SpiceDouble   r,
                   SpiceDouble   lon,
                   SpiceDouble   lat,
                   SpiceDouble   jacobi[3][3] ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   radius     I   Distance of a point from the origin. 
   lon        I   Angle of the point from the XZ plane in radians. 
   lat        I   Angle of the point from the XY plane in radians. 
   jacobi     O   Matrix of partial derivatives. 
 
-Detailed_Input
  
    radius     Distance of a point from the origin. 
 
    lon        Angle of the point from the XZ plane in radians. 
               The angle increases in the counterclockwise sense
               about the +Z axis.
                
    lat        Angle of the point from the XY plane in radians. 
               The angle increases in the direction of the +Z axis.

-Detailed_Output
 
   jacobi     is the matrix of partial derivatives of the conversion 
              between latitudinal and rectangular coordinates. It has 
              the form 
 
                  .-                                -. 
                  |  dx/dr     dx/dlon     dx/dlat   | 
                  |                                  | 
                  |  dy/dr     dy/dlon     dy/dlat   | 
                  |                                  | 
                  |  dz/dr     dz/dlon     dz/dlat   | 
                  `-                                -' 
 
             evaluated at the input values of r, lon and lat. 
             Here x, y, and z are given by the familiar formulae 
 
                 x = r * cos(lon) * cos(lat) 
                 y = r * sin(lon) * cos(lat) 
                 z = r *            sin(lat). 
 
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Particulars
 
   It is often convenient to describe the motion of an object 
   in latitudinal coordinates. It is also convenient to manipulate 
   vectors associated with the object in rectangular coordinates. 
 
   The transformation of a latitudinal state into an equivalent 
   rectangular state makes use of the Jacobian of the 
   transformation between the two systems. 
 
   Given a state in latitudinal coordinates, 
 
        ( r, lon, lat, dr, dlon, dlat ) 
 
   the velocity in rectangular coordinates is given by the matrix 
   equation 
                  t          |                               t 
      (dx, dy, dz)   = jacobi|             * (dr, dlon, dlat) 
                             |(r,lon,lat) 
                                           
   This routine computes the matrix  
 
            |
      jacobi| 
            |(r,lon,lat) 
 
-Examples
 
   Suppose you have a model that gives radius, longitude, and 
   latitude as functions of time (r(t), lon(t), lat(t)), and 
   that the derivatives (dr/dt, dlon/dt, dlat/dt) are computable. 
   To find the velocity of the object in rectangular coordinates, 
   multiply the Jacobian of the transformation from latitudinal 
   to rectangular (evaluated at r(t), lon(t), lat(t)) by the 
   vector of derivatives of the latitudinal coordinates. 
 
   This is illustrated by the following code fragment. 
 
      #include "SpiceUsr.h"
            .
            .
            .

      /.
      Load the derivatives of r, lon and lat into the 
      latitudinal velocity vector latv. 
      ./
      latv[0] = dr_dt   ( t );
      latv[1] = dlon_dt ( t );
      latv[2] = dlat_dt ( t );
 
      /.
      Determine the Jacobian of the transformation from 
      latitudinal to rectangular coordinates, using the latitudinal 
      coordinates at time t. 
      ./ 
      drdlat_c ( r(t), lon(t), lat(t), jacobi );
 
      /.
      Multiply the Jacobian by the latitudinal velocity to 
      obtain the rectangular velocity recv. 
      ./ 
      mxv_c ( jacobi, latv, recv );
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   W.L. Taber     (JPL) 
   N.J. Bachman   (JPL)
 
-Version
 
   -CSPICE Version 1.0.0, 20-JUL-2001 (WLT) (NJB)

-Index_Entries

   Jacobian of rectangular w.r.t. latitudinal coordinates

-&
*/

{ /* Begin drdlat_c */

   /*
   Don't participate in error tracing; the underlying routine is 
   error-free.
   */
   drdlat_ ( (doublereal *) &r,
             (doublereal *) &lon,
             (doublereal *) &lat,
             (doublereal *) jacobi  );

   /*
   Transpose the Jacobian to create a C-style matrix.
   */
   xpose_c ( jacobi, jacobi );

} /* End drdlat_c */
