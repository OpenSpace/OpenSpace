/*

-Procedure axisar_c ( Axis and angle to rotation )

-Abstract
 
   Construct a rotation matrix that rotates vectors by a specified 
   angle about a specified axis. 
 
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
 
   MATRIX 
   ROTATION 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef axisar_c


   void axisar_c ( ConstSpiceDouble  axis   [3],
                   SpiceDouble       angle,
                   SpiceDouble       r      [3][3]  ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   axis       I   Rotation axis. 
   angle      I   Rotation angle, in radians. 
   r          O   Rotation matrix corresponding to axis and angle. 
 
-Detailed_Input
 
   axis, 
   angle          are, respectively, a rotation axis and a rotation 
                  angle.  axis and angle determine a coordinate 
                  transformation whose effect on any vector v is to 
                  rotate v by angle radians about the vector axis. 
 
-Detailed_Output
 
   r              is a rotation matrix representing the coordinate 
                  transformation determined by axis and angle:  for 
                  each vector v, r*v is the vector resulting from 
                  rotating v by angle radians about axis. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
   1)  If axis is the zero vector, the rotation generated is the 
       identity.  This is consistent with the specification of vrotv. 
 
-Files
 
   None. 
 
-Particulars
 
   axisar_c can be thought of as a partial inverse of raxisa_c.  
   axisar_c is really is a `left inverse':  the code fragment 
 
      raxisa_c ( r,    axis,  &angle ); 
      axisar_c ( axis, angle, r      ); 
 
   preserves r, except for round-off error, as long as r is a 
   rotation matrix. 
 
   On the other hand, the code fragment 
 
      axisar_c ( axis, angle, r      ); 
      raxisa_c ( r,    axis,  &angle ); 
 
   preserves axis and angle, except for round-off error, only if 
   angle is in the range (0, pi).  So axisar_c is a right inverse 
   of raxisa_c only over a limited domain. 
 
-Examples
 
   1)  A matrix that rotates vectors by pi/2 radians about the z-axis 
       can be found using the code fragment 
 
          axis[0] = 0. 
          axis[1] = 0. 
          axis[2] = 1. 
 
          axisar_c ( axis, halfpi_c(), r ); 
 
       The returned matrix r will equal 
 
          +-               -+ 
          |  0    -1     0  | 
          |                 | 
          |  1     0     0  |. 
          |                 | 
          |  0     0     1  | 
          +-               -+ 
 
 
   2)  Linear interpolation between two rotation matrices: 
 
          Let r(t) be a time-varying rotation matrix; r could be 
          a C-matrix describing the orientation of a spacecraft 
          structure.  Given two points in time t1 and t2 at which 
          r(t) is known, and given a third time t3, where 
 
             t1  <  t3  <  t2, 
 
          we can estimate r(t3) by linear interpolation.  In other 
          words, we approximate the motion of r by pretending that 
          r rotates about a fixed axis at a uniform angular rate 
          during the time interval [t1, t2].  More specifically, we 
          assume that each column vector of r rotates in this 
          fashion.  This procedure will not work if r rotates through 
          an angle of pi radians or more during the time interval 
          [t1, t2]; an aliasing effect would occur in that case. 
 
          If we let 
 
             r1 = r(t1) 
             r2 = r(t2), and 
 
                         -1 
             q  = r2 * r1  , 
 
          then the rotation axis and angle of q define the rotation 
          that each column of r(t) undergoes from time t1 to time 
          t2.  Since r(t) is orthogonal, we can find q using the 
          transpose of r1.  We find the rotation axis and angle via 
          raxisa_c. 
 
             mxmt_c   ( r2,   r1,    q      ); 
             raxisa_c ( q,    axis,  &angle ); 
 
          Find the fraction of the total rotation angle that r 
          rotates through in the time interval [t1, t3]. 
 
             frac = ( t3 - t1 )  /  ( t2 - t1 ) 
 
          Finally, find the rotation delta that r(t) undergoes 
          during the time interval [t1, t3], and apply that rotation 
          to r1, yielding r(t3), which we'll call r3. 
 
             axisar_c ( axis,   frac * angle,  delta  ); 
             mxm_c    ( delta,  r1,            r3     ); 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 18-JUN-1999 (NJB)

-Index_Entries
 
   axis and angle to rotation 
 
-&
*/

{ /* Begin axisar_c */



   /*
   Error free:  no error tracing required.
   */

   axisar_ (  ( doublereal * ) axis,
              ( doublereal * ) &angle,
              ( doublereal * ) r       );

   /*
   Transpose the output matrix to put it in row-major order.
   */
   
   xpose_c ( r, r );
   

} /* End axisar_c */
