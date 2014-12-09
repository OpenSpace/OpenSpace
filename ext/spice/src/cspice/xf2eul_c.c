/*

-Procedure xf2eul_c ( State transformation to Euler angles )

-Abstract
 
   Convert a state transformation matrix to Euler angles and their 
   derivatives with respect to a specified set of axes. 
 
   The companion routine eul2xf_c converts Euler angles and their 
   derivatives with respect to a specified set of axes to a state 
   transformation matrix. 
 
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
   PCK 
 
-Keywords
 
   ANGLES 
   STATE 
   DERIVATIVES 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef    xf2eul_c
   
 
   void xf2eul_c ( ConstSpiceDouble     xform  [6][6],
                   SpiceInt             axisa,
                   SpiceInt             axisb,
                   SpiceInt             axisc,
                   SpiceDouble          eulang [6],
                   SpiceBoolean       * unique         ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   xform      I   A state transformation matrix. 
   axisa      I   Axis A of the Euler angle factorization. 
   axisb      I   Axis B of the Euler angle factorization. 
   axisc      I   Axis C of the Euler angle factorization. 
   eulang     O   An array of Euler angles and their derivatives. 
   unique     O   Indicates if eulang is a unique representation. 
 
-Detailed_Input
 
   xform       is a state transformation from some frame frame1 to 
               another frame frame2.  Pictorially, xform has the 
               structure shown here. 
 
                    [       |        ] 
                    |  r    |    0   | 
                    |       |        | 
                    |-------+--------| 
                    |       |        | 
                    | dr/dt |    r   | 
                    [       |        ] 
 
               where r is a rotation that varies with respect to time 
               and dr/dt is its time derivative. 
 
               More specifically, if s1 is the state of some object 
               in frame1, then s2, the state of the same object 
               relative to frame2 is given by 
 
                  s2 = xform*s1 
 
               where '*' denotes the matrix vector product. 
 
   axisa       are the axes desired for the factorization of r. 
   axisb       All must be in the range from 1 to 3.  Moreover 
   axisc       it must be the case that axisa and axisb are distinct 
               and that axisb and axisc are distinct. 
 
               Every rotation matrix can be represented as a product 
               of three rotation matrices about the principal axes 
               of a reference frame. 
 
                   r =  [ alpha ]     [ beta ]     [ gamma ] 
                                 axisa        axisb         axisc 
 
               The value 1 corresponds to the X axis. 
               The value 2 corresponds to the Y axis. 
               The value 3 corresponds to the Z axis. 
 
-Detailed_Output
 
   eulang      is the set of Euler angles corresponding to the 
               specified factorization. 
 
               If we represent r as shown here: 
 
                   r =  [ alpha ]     [ beta ]     [ gamma ] 
                                 axisa        axisb         axisc 
 
               then 
 
 
                  eulang[0] = alpha 
                  eulang[1] = beta 
                  eulang[2] = gamma 
                  eulang[3] = dalpha/dt 
                  eulang[4] = dbeta/dt 
                  eulang[5] = dgamma/dt 
 
               The range of alpha and gamma is (-pi, pi]. 
 
               The range of beta depends on the exact set of 
               axes used for the factorization.  For 
               factorizations in which the first and third axes 
               are the same, the range of beta is [0, pi]. 
 
               For factorizations in which the first and third 
               axes are different, the range of beta is 
               [-pi/2, pi/2]. 
 
               For rotations such that alpha and gamma are not 
               uniquely determined, alpha and dalpha/dt will 
               always be set to zero; gamma and dgamma/dt are 
               then uniquely determined. 
 
   unique      is a logical that indicates whether or not the 
               values in eulang are uniquely determined.  If 
               the values are unique then unique will be set to 
               SPICETRUE.  If the values are not unique and some 
               components ( eulang[0] and eulang[3] ) have been set 
               to zero, then unique will have the value SPICEFALSE. 
 
 
-Parameters
 
   None. 
 
-Exceptions
 
   All erroneous inputs are diagnosed by routines in the call 
   tree to this routines.  These include 
 
   1)   If any of axisa, axisb, or axisc do not have values in 
 
           { 1, 2, 3 }, 
 
        then the error SPICE(INPUTOUTOFRANGE) is signaled. 
 
   2)   An arbitrary rotation matrix cannot be expressed using 
        a sequence of Euler angles unless the second rotation axis 
        differs from the other two.  If axisb is equal to axisc or 
        axisa, then the error SPICE(BADAXISNUMBERS) is signaled. 
 
   3)   If the input matrix r is not a rotation matrix, the error 
        SPICE(NOTAROTATION) is signaled. 
 
   4)   If eulang[0] and eulang[2] are not uniquely determined, 
        EULANG[0] is set to zero, and EULANG[2] is determined. 
 
-Files
 
   None. 
 
-Particulars
 
   A word about notation:  the symbol 
 
      [ x ] 
           i 
 
   indicates a coordinate system rotation of x radians about the 
   ith coordinate axis.  To be specific, the symbol 
 
      [ x ] 
           1 
 
   indicates a coordinate system rotation of x radians about the 
   first, or x-, axis; the corresponding matrix is 
 
      +-                    -+ 
      |  1      0       0    | 
      |                      | 
      |  0    cos(x)  sin(x) |. 
      |                      | 
      |  0   -sin(x)  cos(x) | 
      +-                    -+ 
 
   Remember, this is a COORDINATE SYSTEM rotation by x radians; this 
   matrix, when applied to a vector, rotates the vector by -x 
   radians, not x radians.  Applying the matrix to a vector yields 
   the vector's representation relative to the rotated coordinate 
   system. 
 
   The analogous rotation about the second, or y-, axis is 
   represented by 
 
      [ x ] 
           2 
 
   which symbolizes the matrix 
 
      +-                    -+ 
      | cos(x)   0   -sin(x) | 
      |                      | 
      |  0       1      0    |, 
      |                      | 
      | sin(x)   0    cos(x) | 
      +-                    -+ 
 
   and the analogous rotation about the third, or z-, axis is 
   represented by 
 
      [ x ] 
           3 
 
   which symbolizes the matrix 
 
      +-                    -+ 
      |  cos(x)  sin(x)   0  | 
      |                      | 
      | -sin(x)  cos(x)   0  |. 
      |                      | 
      |  0        0       1  | 
      +-                    -+ 
 
 
   The input matrix is assumed to be the product of three 
   rotation matrices, each one of the form 
 
      +-                    -+ 
      |  1      0       0    | 
      |                      | 
      |  0    cos(r)  sin(r) |     (rotation of r radians about the 
      |                      |      x-axis), 
      |  0   -sin(r)  cos(r) | 
      +-                    -+ 
 
 
      +-                    -+ 
      | cos(s)   0   -sin(s) | 
      |                      | 
      |  0       1      0    |     (rotation of s radians about the 
      |                      |      y-axis), 
      | sin(s)   0    cos(s) | 
      +-                    -+ 
 
   or 
 
      +-                    -+ 
      |  cos(t)  sin(t)   0  | 
      |                      | 
      | -sin(t)  cos(t)   0  |     (rotation of t radians about the 
      |                      |      z-axis), 
      |  0        0       1  | 
      +-                    -+ 
 
   where the second rotation axis is not equal to the first or 
   third.  Any rotation matrix can be factored as a sequence of 
   three such rotations, provided that this last criterion is met. 
 
   This routine is related to the routine eul2xf_c which produces 
   a state transformation from an input set of axes, Euler angles 
   and derivatives. 
 
   The two function calls shown here will not change xform except for 
   round off errors. 
 
      xf2eul_c ( xform,  axisa, axisb, axisc, eulang, &unique );
      eul2xf_c ( eulang, axisa, axisb, axisc, xform           ); 
 
   On the other hand the two calls 
 
      eul2xf_c ( eulang, axisa, axisb, axisc, xform           ); 
      xf2eul_c ( xform,  axisa, axisb, axisc, eulang, &unique );
 
   will leave eulang unchanged only if the components of eulang 
   are in the range produced by xf2eul_c and the Euler representation 
   of the rotation component of xform is unique within that range. 
 
 
-Examples
 
   Suppose that you wish to determine the rate of change of 
   the right ascension and declination of the pole of Jupiter, 
   from the state transformation matrix that transforms J2000 
   states to object fixed states. 
 
   Using this routine with the routine sxform_c you can determine 
   these instanteous rates. 
 
   Recall that the rotation component of tsipm is given by 
 
      [w]  [halfpi_c-dec] [ra+halfpi_c] 
         3               1             3 
 
 
   Thus the calls
 
      sxform_c ( "J2000", "IAU_JUPITER", et, tsipm   );
      xf2eul_c (  tsipm,  3, 1, 3,   eulang, &unique );
 
   yield the following: 
 
      eulang[0] is  w 
      eulang[1] is  pi/2 - dec 
      eulang[2] is  ra   + pi/2 
      eulang[3] is  d w/dt 
      eulang[4] is -d dec/dt 
      eulang[5] is  d ra/dt 
 
   Hence: 
 
      d dec/dt = -eulang[4] 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   W.L. Taber      (JPL) 
   N.J. Bachman    (JPL)
   
-Version
 
   -CSPICE Version 1.0.1, 05-MAR-2008 (NJB)

      Fixed typo (missing double quote character) in code example. 
      Corrected order of header sections.

   -CSPICE Version 1.0.0, 15-JUN-1999 (WLT) (NJB)

-Index_Entries
 
   Euler angles and derivatives from state transformation 
 
-&
*/


{ /* Begin xf2eul_c */

   /*
   Local variables
   */
   logical                 u;
   SpiceDouble             fXform[6][6];


   /*
   Participate in error tracing.
   */
   chkin_c ( "xf2eul_c" );


   /*
   Convert the state transformation matrix to column-major order.
   The let the f2c'd routine do the real work.
   */
   
   xpose6_c ( xform, fXform );
   
   xf2eul_ (  ( doublereal * ) fXform, 
              ( integer    * ) &axisa, 
              ( integer    * ) &axisb, 
              ( integer    * ) &axisc, 
              ( doublereal * ) eulang, 
              ( logical    * ) &u     ); 


   *unique  =  u;
   
   chkout_c ( "xf2eul_c" );

} /* End xf2eul_c */
