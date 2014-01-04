/*

-Procedure m2eul_c ( Matrix to Euler angles )

-Abstract

   Factor a rotation matrix as a product of three rotations about
   specified coordinate axes.

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

   ANGLE
   MATRIX
   ROTATION
   TRANSFORMATION

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef    m2eul_c
   

   void  m2eul_c ( ConstSpiceDouble    r[3][3],
                   SpiceInt            axis3,
                   SpiceInt            axis2,
                   SpiceInt            axis1,
                   SpiceDouble       * angle3,
                   SpiceDouble       * angle2,
                   SpiceDouble       * angle1  )
/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   r          I   A rotation matrix to be factored.
   axis3,
   axis2,
   axis1      I   Numbers of third, second, and first rotation axes.
   angle3,
   angle2,
   angle1     O   Third, second, and first Euler angles, in radians.

-Detailed_Input

   r              is a 3x3 rotation matrix that is to be factored as
                  a product of three rotations about a specified
                  coordinate axes.  The angles of these rotations are
                  called `Euler angles'.

   axis3,
   axis2,
   axis1          are the indices of the rotation axes of the
                  `factor' rotations, whose product is r.  r is
                  factored as

                     r = [ angle3 ]     [ angle2 ]     [ angle1 ]   .
                                  axis3          axis2          axis1

                  The axis numbers must belong to the set {1, 2, 3}.
                  The second axis number MUST differ from the first
                  and third axis numbers.

                  See the Particulars section below for details
                  concerning this notation.

-Detailed_Output

   angle3,
   angle2,
   angle1         are the Euler angles corresponding to the matrix
                  r and the axes specified by axis3, axis2, and
                  axis1.  These angles satisfy the equality

                     r = [ angle3 ]     [ angle2 ]     [ angle1 ]
                                 axis3           axis2          axis1


                  See the Particulars section below for details
                  concerning this notation.

                  The range of angle3 and angle1 is (-pi, pi].

                  The range of angle2 depends on the exact set of
                  axes used for the factorization.  For
                  factorizations in which the first and third axes
                  are the same,

                     r = [R]  [S]  [T] ,
                            a    b    a

                  the range of angle2 is [0, pi].


                  For factorizations in which the first and third
                  axes are different,

                     r = [R]  [S]  [T] ,
                            a    b    c

                  the range of angle2 is [-pi/2, pi/2].

                  For rotations such that angle3 and angle1 are not
                  uniquely determined, angle3 will always be set to
                  zero; angle1 is then uniquely determined.

-Parameters

   None.

-Exceptions

   1)   If any of axis3, axis2, or axis1 do not have values in

           { 1, 2, 3 },

        then the error SPICE(INPUTOUTOFRANGE) is signalled.

   2)   An arbitrary rotation matrix cannot be expressed using
        a sequence of Euler angles unless the second rotation axis
        differs from the other two.  If axis2 is equal to axis3 or
        axis1, then then error SPICE(BADAXISNUMBERS) is signalled.

   3)   If the input matrix r is not a rotation matrix, the error
        SPICE(NOTAROTATION) is signalled.

   4)   If angle3 and angle1 are not uniquely determined, angle3
        is set to zero, and angle1 is determined.

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

   This routine is related to the CSPICE routine EUL2M, which
   produces a rotation matrix, given a sequence of Euler angles.
   This routine is a `right inverse' of EUL2M:  the sequence of
   calls

      m2eul_c ( r,  axis3,   axis2,   axis1,
                    angle3,  angle2,  angle1     );

      eul2m_c (     angle3,  angle2,  angle1,
                    axis3,   axis2,   axis1,   r );

   preserves r, except for round-off error.


   On the other hand, the sequence of calls

      eul2m_c ( angle3,  angle2,  angle1,
                axis3,   axis2,   axis1,   r );

      m2eul_c ( r,  axis3,   axis2,   axis1,
                    angle3,  angle2,  angle1 );


   preserve angle3, angle2, and angle1 only if these angles start
   out in the ranges that m2eul_c's outputs are restricted to.

-Examples

   1)  Conversion of instrument pointing from a matrix representation
       to Euler angles:

       Suppose we want to find camera pointing in alpha, delta, and
       kappa, given the inertial-to-camera coordinate transformation


       ticam =

   +-                                                               -+
   |  0.49127379678135830  0.50872620321864170  0.70699908539882417  |
   |                                                                 |
   | -0.50872620321864193 -0.49127379678135802  0.70699908539882428  |
   |                                                                 |
   |  0.70699908539882406 -0.70699908539882439  0.01745240643728360  |
   +-                                                               -+


       We want to find angles alpha, delta, kappa such that

          ticam  =  [ kappa ]  [ pi/2 - delta ]  [ pi/2 + alpha ] .
                             3                 1                 3

       The code fragment

          m2eul_c ( ticam, 3, 1, 3, &kappa, &ang2, &ang1 );

          alpha  =  ang1       - halfpi_c();
          delta  =  halfpi_c() - ang2;

       calculates the desired angles.  If we wish to make sure that
       alpha, delta, and kappa are in the ranges [0, 2pi),
       [-pi/2, pi/2], and [0, 2pi) respectively, we may add the code

          if ( alpha < 0. )
             {
             alpha = alpha + twopi_c();
             }

          if ( kappa < 0. )
             {
             kappa = kappa + twopi_c();
             }

       Note that delta is already in the correct range, since ang2
       is in the range [0, pi] when the first and third input axes
       are equal.

       If we wish to print out the results in degrees, we might
       use the code

          printf ( "Alpha = %25.17f\n"
                   "Delta = %25.17f\n"
                   "Kappa = %25.17f\n",
                   dpr_c() * alpha,
                   dpr_c() * delta,
                   dpr_c() * kappa     );


       We should see something like

          Alpha =     315.00000000000000000
          Delta =       1.00000000000000000
          Kappa =      45.00000000000000000

       possibly formatted a little differently, or degraded slightly
       by round-off.


   2)  Conversion of instrument pointing angles from a non-J2000,
       not necessarily inertial frame to J2000-relative RA, Dec,
       and Twist.

       Suppose that we have pointing for some instrument expressed as

          [ gamma ]  [ beta ]  [ alpha ]
                   3         2          3

       with respect to some coordinate system S.  For example, S
       could be a spacecraft-fixed system.

       We will suppose that the transformation from J2000
       coordinates to system S coordinates is given by the rotation
       matrix j2s.

       The rows of j2s are the unit basis vectors of system S, given
       in J2000 coordinates.

       We want to express the pointing with respect to the J2000
       system as the sequence of rotations

          [ kappa ]  [ pi/2 - delta ]  [ pi/2 + alpha ] .
                   3                 1                 3

       First, we use subroutine eul2m_c to form the transformation
       from system S to instrument coordinates s2inst.

          eul2m_c ( gamma, beta, alpha, 3, 2, 3, s2inst );

       Next, we form the transformation from J2000 to instrument
       coordinates j2inst.

          mxm_c ( s2inst, j2s, j2inst );

       Finally, we express j2inst using the desired Euler angles, as
       in the first example:

          m2eul_c ( j2inst, 3, 1, 3, &twist, &ang2, &ang3 );

          ra   =  ang3       - halfpi_c();
          dec  =  halfpi_c() - ang2;

       If we wish to make sure that ra, dec, and twist are in
       the ranges [0, 2pi), [-pi/2, pi/2], and [0, 2pi)
       respectively, we may add the code

          if ( ra < 0. )
             {
             ra = ra + twopi_c();
             }

          if ( twist < 0. )
             {
             twist = twist + twopi_c();
             }

       Note that dec is already in the correct range, since ang2
       is in the range [0, pi] when the first and third input axes
       are equal.

       Now ra, dec, and twist express the instrument pointing
       as RA, Dec, and Twist, relative to the J2000 system.

       A warning note:  more than one definition of RA, Dec, and
       Twist is extant.  Before using this example in an application,
       check that the definition given here is consistent with that
       used in your application.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman   (JPL)

-Version

   -CSPICE Version 1.3.1, 13-OCT-2004 (NJB)

      Fixed header typo.

   -CSPICE Version 1.3.0, 21-OCT-1998 (NJB)

      Made input matrix const.

   -CSPICE Version 1.2.0, 13-FEB-1998 (EDW)

      Minor corrections to header.

   -CSPICE Version 1.2.0, 08-FEB-1998 (NJB)

      Removed local variables used for temporary capture of outputs.

   -CSPICE Version 1.0.0 25-OCT-1997 (NJB)

      Based on SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)

-Index_Entries

   matrix to euler angles

-&
*/

{ /* Begin m2eul_c */

   /*
   Local variables
   */
   SpiceDouble             loc_r[3][3];


   /*
   Participate in error tracing.
   */
   chkin_c ( "m2eul_c" );


   /*
   Transpose the input matrix to put it in column-major order.
   */
   xpose_c ( r, loc_r );


   /*
   Call the f2c'd version of m2eul:
   */
   m2eul_ ( (doublereal *) loc_r,
            (integer    *) &axis3,
            (integer    *) &axis2,
            (integer    *) &axis1,
            (doublereal *) angle3,
            (doublereal *) angle2,
            (doublereal *) angle1  );


   chkout_c ( "m2eul_c" );

} /* End m2eul_c */
