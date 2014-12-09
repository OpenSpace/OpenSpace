/*

-Procedure qxq_c ( Quaternion times quaternion )

-Abstract
 
   Multiply two quaternions. 
    
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
 
   MATH 
   POINTING 
   ROTATION 
 
*/

   #include "SpiceUsr.h"
   #undef   qxq_c


   void qxq_c ( ConstSpiceDouble    q1   [4],
                ConstSpiceDouble    q2   [4],
                SpiceDouble         qout [4]  ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   q1         I   First SPICE quaternion factor. 
   q2         I   Second SPICE quaternion factor. 
   qout       O   Product of `q1' and `q2'. 
 
-Detailed_Input
 
   q1             is a 4-vector representing a SPICE-style quaternion.
                  See the discussion of "Quaternion Styles" in the
                  Particulars section below.

                  Note that multiple styles of quaternions are in use.
                  This routine will not work properly if the input
                  quaternions do not conform to the SPICE convention.
 
   q2             is a second SPICE-style quaternion. 
 
-Detailed_Output
 
   qout           is 4-vector representing the quaternion product  
 
                     q1 * q2 
 
                  Representing q(i) as the sums of scalar (real) 
                  part s(i) and vector (imaginary) part v(i) 
                  respectively, 
 
                     q1 = s1 + v1 
                     q2 = s2 + v2 
 
                  qout has scalar part s3 defined by 
 
                     s3 = s1 * s2 - <v1, v2> 
 
                  and vector part v3 defined by 
 
                     v3 = s1 * v2  +  s2 * v1  +  v1 x v2 
 
                  where the notation < , > denotes the inner 
                  product operator and x indicates the cross 
                  product operator. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Particulars

     
   Quaternion Styles
   -----------------

   There are different "styles" of quaternions used in
   science and engineering applications. Quaternion styles
   are characterized by

      - The order of quaternion elements

      - The quaternion multiplication formula

      - The convention for associating quaternions
        with rotation matrices

   Two of the commonly used styles are

      - "SPICE"

         > Invented by Sir William Rowan Hamilton
         > Frequently used in mathematics and physics textbooks

      - "Engineering"

         > Widely used in aerospace engineering applications


   CSPICE function interfaces ALWAYS use SPICE quaternions.
   Quaternions of any other style must be converted to SPICE
   quaternions before they are passed to CSPICE functions.


   Relationship between SPICE and Engineering Quaternions
   ------------------------------------------------------

   Let M be a rotation matrix such that for any vector V,

      M*V

   is the result of rotating V by theta radians in the
   counterclockwise direction about unit rotation axis vector A.
   Then the SPICE quaternions representing M are

      (+/-) (  cos(theta/2),
               sin(theta/2) A(1),
               sin(theta/2) A(2),
               sin(theta/2) A(3)  )

   while the engineering quaternions representing M are

      (+/-) ( -sin(theta/2) A(1),
              -sin(theta/2) A(2),
              -sin(theta/2) A(3),
               cos(theta/2)       )

   For both styles of quaternions, if a quaternion q represents
   a rotation matrix M, then -q represents M as well.

   Given an engineering quaternion

      QENG   = ( q0,  q1,  q2,  q3 )

   the equivalent SPICE quaternion is

      QSPICE = ( q3, -q0, -q1, -q2 )


   Associating SPICE Quaternions with Rotation Matrices
   ----------------------------------------------------

   Let FROM and TO be two right-handed reference frames, for
   example, an inertial frame and a spacecraft-fixed frame. Let the
   symbols

      V    ,   V
       FROM     TO

   denote, respectively, an arbitrary vector expressed relative to
   the FROM and TO frames. Let M denote the transformation matrix
   that transforms vectors from frame FROM to frame TO; then

      V   =  M * V
       TO         FROM

   where the expression on the right hand side represents left
   multiplication of the vector by the matrix.

   Then if the unit-length SPICE quaternion q represents M, where

      q = (q0, q1, q2, q3)

   the elements of M are derived from the elements of q as follows:

        +-                                                         -+
        |           2    2                                          |
        | 1 - 2*( q2 + q3 )   2*(q1*q2 - q0*q3)   2*(q1*q3 + q0*q2) |
        |                                                           |
        |                                                           |
        |                               2    2                      |
    M = | 2*(q1*q2 + q0*q3)   1 - 2*( q1 + q3 )   2*(q2*q3 - q0*q1) |
        |                                                           |
        |                                                           |
        |                                                   2    2  |
        | 2*(q1*q3 - q0*q2)   2*(q2*q3 + q0*q1)   1 - 2*( q1 + q2 ) |
        |                                                           |
        +-                                                         -+

   Note that substituting the elements of -q for those of q in the
   right hand side leaves each element of M unchanged; this shows
   that if a quaternion q represents a matrix M, then so does the
   quaternion -q.

   To map the rotation matrix M to a unit quaternion, we start by
   decomposing the rotation matrix as a sum of symmetric
   and skew-symmetric parts:

                                      2
      M = [ I  +  (1-cos(theta)) OMEGA  ] + [ sin(theta) OMEGA ]

                   symmetric                   skew-symmetric


   OMEGA is a skew-symmetric matrix of the form

                 +-             -+
                 |  0   -n3   n2 |
                 |               |
       OMEGA  =  |  n3   0   -n1 |
                 |               |
                 | -n2   n1   0  |
                 +-             -+

   The vector N of matrix entries (n1, n2, n3) is the rotation axis
   of M and theta is M's rotation angle.  Note that N and theta
   are not unique.

   Let

      C = cos(theta/2)
      S = sin(theta/2)

   Then the unit quaternions Q corresponding to M are

      Q = +/- ( C, S*n1, S*n2, S*n3 )

   The mappings between quaternions and the corresponding rotations
   are carried out by the CSPICE routines

      q2m_c {quaternion to matrix}
      m2q_c {matrix to quaternion}

   m2q_c always returns a quaternion with scalar part greater than
   or equal to zero.


   SPICE Quaternion Multiplication Formula
   ---------------------------------------

   Given a SPICE quaternion

      Q = ( q0, q1, q2, q3 )

   corresponding to rotation axis A and angle theta as above, we can
   represent Q using "scalar + vector" notation as follows:

      s =   q0           = cos(theta/2)

      v = ( q1, q2, q3 ) = sin(theta/2) * A

      Q = s + v

   Let Q1 and Q2 be SPICE quaternions with respective scalar
   and vector parts s1, s2 and v1, v2:

      Q1 = s1 + v1
      Q2 = s2 + v2

   We represent the dot product of v1 and v2 by

      <v1, v2>

   and the cross product of v1 and v2 by

      v1 x v2

   Then the SPICE quaternion product is

      Q1*Q2 = s1*s2 - <v1,v2>  + s1*v2 + s2*v1 + (v1 x v2)

   If Q1 and Q2 represent the rotation matrices M1 and M2
   respectively, then the quaternion product

      Q1*Q2

   represents the matrix product

      M1*M2

 
-Examples
 
   1)  Let qid, qi, qj, qk be the "basis" quaternions 
 
          qid  =  ( 1, 0, 0, 0 ) 
          qi   =  ( 0, 1, 0, 0 ) 
          qj   =  ( 0, 0, 1, 0 ) 
          qk   =  ( 0, 0, 0, 1 ) 
 
       respectively.  Then the calls 
 
          qxq_c ( qi, qj, ixj );
          qxq_c ( qj, qk, jxk );
          qxq_c ( qk, qi, kxi );
 
       produce the results 
 
          ixj == qk 
          jxk == qi 
          kxi == qj 
 
       All of the calls 
 
          qxq_c ( qi, qi, qout );
          qxq_c ( qj, qj, qout );
          qxq_c ( qk, qk, qout );
 
       produce the result 
 
          qout  ==  -qid
 
       For any quaternion Q, the calls 
 
          qxq_c ( qid, q,   qout );
          qxq_c ( q,   qid, qout );
 
       produce the result 
 
          qout  ==  q 
 
 
 
   2)  Composition of rotations:  let `cmat1' and `cmat2' be two 
       C-matrices (which are rotation matrices).  Then the 
       following code fragment computes the product cmat1 * cmat2: 
 
 
          /. 
          Convert the C-matrices to quaternions. 
          ./
          m2q_c ( cmat1, q1 );
          m2q_c ( cmat2, q2 );
 
          /.
          Find the product. 
          ./ 
          qxq_c ( q1, q2, qout ); 
 
          /.
          Convert the result to a C-matrix. 
          ./ 
          q2m_c ( qout, cmat3 );
 
          /.
          Multiply `cmat1' and `cmat2' directly. 
          ./ 
          mxm_c ( cmat1, cmat2, cmat4 );
 
          /.
          Compare the results.  The difference `diff' of 
          `cmat3' and `cmat4' should be close to the zero 
          matrix. 
          ./ 
          vsubg_c ( 9, cmat3, cmat4, diff );

 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
 
-Version
 
   -CSPICE Version 1.0.1, 27-FEB-2008 (NJB)

      Updated header; added information about SPICE 
      quaternion conventions.

   -CSPICE Version 1.0.0, 27-OCT-2005 (NJB)

-Index_Entries
 
   quaternion times quaternion 
   multiply quaternion by quaternion 
-&
*/

{ /* Begin qxq_c */

   /*
   Local variables
   */
   SpiceDouble             cross[3];


   /*
   This routine is error free.
   */

   /*
   Assign the scalar portion of the product `vout'. 
   */
   qout[0]  =  q1[0]*q2[0] - vdot_c( q1+1, q2+1 );

   /*
   Compute the cross product term of the vector component of
   vout.
   */
   vcrss_c ( q1+1, q2+1, cross );

   /*
   Assign the vector portion of the product `vout'. 
   */
   vlcom3_c ( q1[0],   q2+1,  
              q2[0],   q1+1,  
              1.0,     cross,   qout+1 );


} /* End qxq_c */
