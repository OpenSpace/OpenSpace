/*

-Procedure raxisa_c ( Rotation axis of a matrix )

-Abstract
 
   Compute the axis of the rotation given by an input matrix 
   and the angle of the rotation about that axis. 
 
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
 
   ROTATION 
 
-Keywords
 
   ANGLE,  MATRIX,  ROTATION 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef raxisa_c
   

   void raxisa_c ( ConstSpiceDouble     matrix[3][3],
                   SpiceDouble          axis  [3],
                   SpiceDouble        * angle       ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   matrix     I   3x3 rotation matrix in double precision. 
   axis       O   Axis of the rotation. 
   angle      O   Angle through which the rotation is performed. 
 
-Detailed_Input
 
   matrix     is a 3x3 rotation matrix in double precision. 
 
-Detailed_Output
 
   axis       is a unit vector pointing along the axis of the rotation.
              In other words, `axis' is a unit eigenvector of the input
              matrix, corresponding to the eigenvalue 1. If the input
              matrix is the identity matrix, `axis' will be the vector
              (0, 0, 1). If the input rotation is a rotation by pi
              radians, both `axis' and -axis may be regarded as the
              axis of the rotation.
 
   angle      is the angle between `v' and matrix*v for any non-zero
              vector `v' orthogonal to `axis'.  `angle' is given in
              radians.  The angle returned will be in the range from 0
              to pi radians.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input matrix is not a rotation matrix (a fairly
      loose tolerance is used to check this) a routine in the
      call tree of this routine will signal an error indicating
      the problem.

   2) If the input matrix is the identity matrix, this routine
      returns an angle of 0.0, and an axis of ( 0.0, 0.0, 1.0 ).
      
-Files
 
   None. 
 
-Particulars

   Every rotation matrix has an axis `a' such any vector `v'
   parallel to that axis satisfies the equation

      v = matrix * v

   This routine returns a unit vector `axis' parallel to the axis of
   the input rotation matrix.  Moreover for any vector `w' orthogonal
   to the axis of the rotation, the two vectors

       axis, 
       w x (matrix*w)

      (where "x" denotes the cross product operation)

   will be positive scalar multiples of one another (at least
   to within the ability to make such computations with double
   precision arithmetic, and under the assumption that `matrix'
   does not represent a rotation by zero or pi radians).

   The angle returned will be the angle between `w' and matrix*w
   for any vector orthogonal to `axis'.

   If the input matrix is a rotation by 0 or pi radians some
   choice must be made for the axis returned.  In the case of
   a rotation by 0 radians, `axis' is along the positive z-axis.
   In the case of a rotation by 180 degrees, two choices are
   possible.  The choice made this routine is unspecified.

-Examples
 
    This routine can be used to numerically approximate the 
    instantaneous angular velocity vector of a rotating object. 
 
    Suppose that r(t) is the rotation matrix whose columns 
    represent the inertial pointing vectors of the bodyfixed 
    axes of an object at time t. 
 
    Then the angular velocity vector points along the vector 
    given by: 
                           T 
       limit  axis( r(t+h)r ) 
       h-->0 
 
    And the magnitude of the angular velocity at time t is given by: 
 
                           T 
       d angle ( r(t+h)r(t) ) 
       ----------------------   at   h = 0 
       dh 
 
    Thus to approximate the angular velocity vector the following 
    code fragment will do 
 
       [ Load t      into the double precision variable t 
         Load h      into the double precision variable h 
         Load r(t+h) into the 3 by 3 double precision array rth 
         Load r(t)   into the 3 by 3 double precision array rt 
          . 
          . 
          . 
       ]
       
       /.
                                                    T
       Compute the infinitesimal rotation r(t+h)r(t)  
       ./
       mxmt_c ( rth, rt, infrot );

       /.
       Compute the axis and angle of the infinitesimal rotation.
       /.
       raxisa_c ( infrot, axis, &angle );

       /.
       Scale axis to get the angular velocity vector.
       ./
       vscl_c ( angle/h, axis, angvel ); 
 
 
-Restrictions
 
    1) If the input matrix is not a rotation matrix but is close enough
       to pass the tests this routine performs on it, no error will be
       signaled, but the results may have poor accuracy.
 
    2) The input matrix is taken to be an object that acts on (rotates)
       vectors---it is not regarded as a coordinate transformation.  To
       find the axis and angle of a coordinate transformation, input
       the transpose of that matrix to this routine.
 
-Author_and_Institution
 
    N.J. Bachman    (JPL)
    W.L. Taber      (JPL) 
    F.S. Turner     (JPL)  
  
-Literature_References
 
    None.

-Version
 
   -CSPICE Version 1.0.1, 05-JAN-2005 (NJB) (WLT) (FST)

       Various header updates were made to reflect changes
       made to the underlying SPICELIB Fortran code.  
       Miscellaneous header corrections were made as well.    

   -CSPICE Version 1.0.0, 31-MAY-1999 (WLT) (NJB)

-Index_Entries
 
   rotation axis of a matrix 
 
-&
*/

{ /* Begin raxisa_c */

   /*
   Local variables
   */
   SpiceDouble             tmpmat[3][3];

   
   /*
   Error free: no error tracing.
   */
 
   /*
   Transpose the input matrix to put it in column-major order.
   */
   
   xpose_c ( matrix, tmpmat );

   raxisa_ (  ( doublereal * ) tmpmat,
              ( doublereal * ) axis,
              ( doublereal * ) angle  );
 
} /* End raxisa_c */

