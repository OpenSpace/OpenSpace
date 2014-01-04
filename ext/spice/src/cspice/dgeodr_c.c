/*

-Procedure dgeodr_c ( Derivative of geodetic w.r.t. rectangular )

-Abstract
 
   This routine computes the Jacobian of the transformation from 
   rectangular to geodetic coordinates. 
 
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


   void dgeodr_c ( SpiceDouble   x,
                   SpiceDouble   y,
                   SpiceDouble   z,
                   SpiceDouble   re,
                   SpiceDouble   f,
                   SpiceDouble   jacobi[3][3] ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   X          I   X-coordinate of point. 
   Y          I   Y-coordinate of point. 
   Z          I   Z-coordinate of point. 
   RE         I   Equatorial radius of the reference spheroid. 
   F          I   Flattening coefficient. 
   JACOBI     O   Matrix of partial derivatives. 
 
-Detailed_Input
  
   x, 
   y, 
   z          are the rectangular coordinates of the point at 
              which the Jacobian of the map from rectangular 
              to geodetic coordinates is desired. 
 
   re         Equatorial radius of the reference spheroid. 
 
   f          Flattening coefficient = (re-rp) / re,  where rp is 
              the polar radius of the spheroid.  (More importantly 
              rp = re*(1-f).) 

-Detailed_Output
 
   jacobi     is the matrix of partial derivatives of the conversion 
              between rectangular and geodetic coordinates.  It 
              has the form 
 
                  .-                            -. 
                  |  dlon/dx   dlon/dy  dlon/dz  | 
                  |  dlat/dx   dlat/dy  dlat/dz  | 
                  |  dalt/dx   dalt/dy  dalt/dz  | 
                  `-                            -' 
 
             evaluated at the input values of x, y, and z. 
 
-Parameters
 
   None. 
 
-Exceptions
 
    1) If the input point is on the z-axis (x and y = 0), the 
       Jacobian is undefined.  The error SPICE(POINTONZAXIS) 
       will be signaled. 
 
    2) If the flattening coefficient is greater than or equal to 
       one, the error SPICE(VALUEOUTOFRANGE) is signaled. 
 
    3) If the equatorial radius is not positive, the error 
       SPICE(BADRADIUS) is signaled. 
 
-Files
 
   None. 
 
-Particulars
 
   When performing vector calculations with velocities it is 
   usually most convenient to work in rectangular coordinates. 
   However, once the vector manipulations have been performed, 
   it is often desirable to convert the rectangular representations 
   into geodetic coordinates to gain insights about phenomena 
   in this coordinate frame. 
 
   To transform rectangular velocities to derivatives of coordinates 
   in a geodetic system, one uses the Jacobian of the transformation 
   between the two systems. 
 
   Given a state in rectangular coordinates 
 
      ( x, y, z, dx, dy, dz ) 
 
   the velocity in geodetic coordinates is given by the matrix  
   equation: 
                        t          |                     t 
      (dlon, dlat, dalt)   = jacobi|       * (dx, dy, dz) 
                                   |(x,y,z) 
 
   This routine computes the matrix  
 
            | 
      jacobi| 
            |(x, y, z) 
 
 
-Examples
 
   Suppose one is given the bodyfixed rectangular state of an object 
   (x(t), y(t), z(t), dx(t), dy(t), dz(t)) as a function of time t. 
 
   To find the derivatives of the coordinates of the object in 
   bodyfixed geodetic coordinates, one simply multiplies the 
   Jacobian of the transformation from rectangular to geodetic 
   coordinates (evaluated at x(t), y(t), z(t)) by the rectangular  
   velocity vector of the object at time t. 
 
   In code this looks like: 
 
      #include "SpiceUsr.h"
          .
          .
          .

      /.
      Load the rectangular velocity vector vector recv. 
      ./ 
      recv[0] = dx_dt ( t );
      recv[1] = dy_dt ( t );
      recv[2] = dz_dt ( t );
 
      /. 
      Determine the Jacobian of the transformation from 
      rectangular to geodetic coordinates at the rectangular 
      coordinates at time t. 
      ./ 
      dgeodr_c ( x(t), y(t), z(t), re, f, jacobi );
 
      /.
      Multiply the Jacobian on the right by the rectangular 
      velocity to obtain the geodetic coordinate derivatives  
      geov. 
      ./ 
      mxv_c ( jacobi, recv, geov );
 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   W.L. Taber     (JPL) 
   N.J. Bachman   (JPL)
 
-Version

   -CSPICE Version 1.0.0, 18-JUL-2001 (WLT) (NJB)
 
-Index_Entries
 
   Jacobian of geodetic  w.r.t. rectangular coordinates 
 
-&
*/

{ /* Begin dgeodr_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "dgeodr_c" );


   dgeodr_ ( (doublereal *) &x,
             (doublereal *) &y,
             (doublereal *) &z,
             (doublereal *) &re,
             (doublereal *) &f,
             (doublereal *) jacobi  );

   /*
   Transpose the Jacobian to create a C-style matrix.
   */
   xpose_c ( jacobi, jacobi );


   chkout_c ( "dgeodr_c" );

} /* End dgeodr_c */
