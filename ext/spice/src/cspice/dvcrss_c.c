/*

-Procedure dvcrss_c ( Derivative of Vector cross product )

-Abstract

   Compute the cross product of two 3-dimensional vectors 
   and the derivative of this cross product. 

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

   VECTOR 
   DERIVATIVE
   MATH

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #undef   dvcrss_c

   void dvcrss_c ( ConstSpiceDouble s1  [6],
                   ConstSpiceDouble s2  [6],
                   SpiceDouble      sout[6] ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   s1        I   Left hand state for cross product and derivative. 
   s2        I   Right hand state for cross product and derivative. 
   sout      O   State associated with cross product of positions. 
 
-Detailed_Input
 
   s1       This may be any state vector.  Typically, this 
            might represent the apparent state of a planet or the 
            Sun, which defines the orientation of axes of 
            some coordinate system. 
 
   s2       A state vector. 
 
-Detailed_Output
 
   sout     This variable represents the state associated with the 
            cross product of the position components of 's1' and 's2.' 
            In otherwords if s1 = (P1,V1) and s2 = (P2,V2) then 
            'sout' is ( P1xP2, d/dt{ P1xP2 } ) 
 
            'sout' may overwrite 's1' or 's2'. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
   1) If 's1' and 's2'  are large in magnitude (taken together, 
      their magnitude surpasses the limit allow by the 
      computer) then it may be possible to generate a 
      floating point overflow from an intermediate 
      computation even though the actual cross product and 
      derivative may be well within the range of double 
      precision numbers. 
 
      dvcrss_c does NOT check the magnitude of 's1' or 's2'  to 
      insure that overflow will not occur. 
 
-Files
 
   None. 
 
-Particulars
 
   dvcrss_c calculates the three-dimensional cross product of two 
   vectors and the derivative of that cross product according to 
   the definition.  The components of this state are stored 
   in a local buffer vector until the calculation is complete. 
   Thus sout may overwrite 's1' or 's2'  without interfering with 
   intermediate computations. 
 
-Examples
 
          s1                    s2                   sout 
   ----------------------------------------------------------------- 
   (0, 1, 0, 1, 0, 0)  ( 1,  0,  0, 1, 0, 0)  (0, 0, -1, 0,  0, -1 ) 
   (5, 5, 5, 1, 0, 0)  (-1, -1, -1, 2, 0, 0)  (0, 0,  0, 0, 11,-11 ) 
 
-Restrictions
 
   None. 
     
-Literature_References
 
   None.

-Author_and_Institution
 
   W.L. Taber      (JPL) 
   E.D. Wright     (JPL)
 
-Version
 
   -CSPICE Version 1.0.0, 23-NOV-2009 (EDW)

-Index_Entries
 
   Compute the derivative of a cross product 
 
-&
*/

{ /* Begin dvcrss_c */

   /*
   Local variables
   */

   SpiceDouble vtemp [3];
   SpiceDouble dvtmp1[6];
   SpiceDouble dvtmp2[6];

   /*
   Calculate the cross product of 's1' and 's2', store it in 'vtemp'.
   */
   vcrss_c (s1, s2, vtemp );

   /*
   Calculate the two components of the derivative of s1 x s2.
   */
   vcrss_c ( &(s1[3]), s2,       dvtmp1 );
   vcrss_c ( s1,       &(s2[3]), dvtmp2 );

   /*
   Put all of the pieces into 'sout'.
   */
   vequ_c ( vtemp, sout );
   vadd_c ( dvtmp1, dvtmp2, &(sout[3]));

} /* End dvcrss_c */

