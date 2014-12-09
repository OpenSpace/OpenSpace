/*

-Procedure  rotvec_c ( Transform a vector via a rotation )

-Abstract
 
   Transform a vector to a new coordinate system rotated by angle 
   radians about axis iaxis.  This transformation rotates v1 by 
   -angle radians about the specified axis.
 
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
 
   ROTATION,  VECTOR 
 
*/

   #include "SpiceUsr.h"
   #include <math.h>
   #undef    rotvec_c


   void rotvec_c ( ConstSpiceDouble  v1    [3],
                   SpiceDouble       angle, 
                   SpiceInt          iaxis, 
                   SpiceDouble       vout  [3] ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   v1        I    Vector whose coordinate system is to be rotated. 
   angle     I    Angle of rotation in radians. 
   iaxis     I    Axis of rotation (X=1, Y=2, Z=3). 
   vout      O    Resulting vector [angle]      * v1 expressed in 
                                          iaxis 
                  the new coordinate system. vout can overwrite v1. 
 
-Detailed_Input
 
   v1      This is a vector (typically representing a vector fixed 
           in inertial space) which is to be expressed in another 
           coordinate system.  The vector remains fixed but the 
           coordinate system changes. 
 
   angle   The angle given in radians, through which the rotation 
           is performed. 
 
   iaxis   The index of the axis of rotation.  The X, Y, and Z 
           axes have indices 1, 2 and 3 respectively. 
 
-Detailed_Output
 
   vout    This is the vector expressed in the new coordinate system 
           specified by the angle of rotation and axis. If 
           [angle]       represents the rotation matrix described by 
                  iaxis 
           the angle and axis, (refer to the routine ROTATE) 
           then vout = [angle]      * v1 
                              iaxis 
 
-Parameters
 
   None. 
 
-Particulars
 
   A rotation about the first, i.e. x-axis, is described by 
 
   |  1        0          0      | 
   |  0   cos(theta) sin(theta)  | 
   |  0  -sin(theta) cos(theta)  | 
 
   A rotation about the second, i.e. y-axis, is described by 
 
   |  cos(theta)  0  -sin(theta)  | 
   |      0       1        0      | 
   |  sin(theta)  1   cos(theta)  | 
 
   A rotation about the third, i.e. z-axis, is described by 
 
   |  cos(theta) sin(theta)   0   | 
   | -sin(theta) cos(theta)   0   | 
   |       0          0       1   | 
 
   rotvec_c decides which form is appropriate according to the value 
   of iaxis and applies the rotation to the input vector. 
 
-Examples
 
   Suppose that 
   v1 = (1.414, 0, 0), angle = PI/4, iaxis = 3 
   then after calling rotvec_c according to 
 
          rotvec_c (v1, angle, iaxis, vout) 
 
   vout will be equal to (1, -1, 0). 
 
-Restrictions
 
   None 
 
-Exceptions 
 
   1) If the axis index is not in the range 1 to 3 it will be treated 
      the same as that integer 1, 2, or 3 that is congruent to it mod 
      3. 
 
-Files
 
   None 
 
-Author_and_Institution
 
   W.M. Owen       (JPL) 
   W.L. Taber      (JPL) 
 
-Literature_References
 
   None 
 
-Version
 
   -CSPICE Version 1.1.1, 04-OCT-1999 (NJB)
 
      Procedure line and abstract and were changed to dispel the
      impression that the input vector is rotated by +angle
      radians about the specified axis.
 
   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

      Made input vector const.

   -CSPICE Version 1.0.0, 08-FEB-1998   (EDW)

-Index_Entries
 
   rotate a vector 
 
-&
*/

{ /* Begin rotvec_c */

   /*
   Local constants
   */

   static SpiceInt indexs[5] = { 3,1,2,3,1 };


   /*
   Local variables
   */

   SpiceDouble     sn;
   SpiceDouble     cn;
   SpiceDouble     temp [3];

   SpiceInt        tmp;
   SpiceInt        i1;
   SpiceInt        i2;
   SpiceInt        i3;


   /* Get the sine and cosine of angle */

   sn = sin(angle);
   cn = cos(angle);


   /*
   Get indices for axes. The first index is for the axis of rotation. 
   The next two axes follow in right hand order (XYZ).  First get the
   non-negative value of iaxis mod 3.
   */

   tmp = ( ( iaxis % 3 + 3) % 3 );
   i1  = indexs[tmp]     - 1;
   i2  = indexs[tmp + 1] - 1;
   i3  = indexs[tmp + 2] - 1;


   /* The coordinate along the axis of rotation does not change. */

   temp[0] = v1[i1];
   temp[1] = cn * v1[i2] + sn * v1[i3];
   temp[2] =-sn * v1[i2] + cn * v1[i3];


   /*  Move the buffered vector to the output */

   vout[i1] = temp[0];
   vout[i2] = temp[1];
   vout[i3] = temp[2];


} /* End rotvec_c */
