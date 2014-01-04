/*

-Procedure ducrss_c ( Unit Normalized Cross Product and Derivative )

-Abstract
 
   Compute the unit vector parallel to the cross product of 
   two 3-dimensional vectors and the derivative of this unit vector. 
 
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

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #undef   ducrss_c

   void ducrss_c ( ConstSpiceDouble s1  [6],
                   ConstSpiceDouble s2  [6],
                   SpiceDouble      sout[6] ) 

/*

-Brief_I/O
 
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   s1        I   Left hand state for cross product and derivative. 
   s2        I   Right hand state for cross product and derivative. 
   sout      O   Unit vector and derivative of the cross product. 
 
-Detailed_Input

   s1       This may be any state vector.  Typically, this 
            might represent the apparent state of a planet or the 
            Sun, which defines the orientation of axes of 
            some coordinate system. 
 
   s2       Any state vector. 
 
-Detailed_Output
 
   sout     This variable represents the unit vector parallel to the 
            cross product of the position components of 's1' and 's2' 
            and the derivative of the unit vector. 
 
            If the cross product of the position components is 
            the zero vector, then the position component of the 
            output will be the zero vector.  The velocity component 
            of the output will simply be the derivative of the 
            cross product of the position components of 's1' and 's2'. 
 
            'sout' may overwrite 's1' or 's2'. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
   1) If the position components of 's1' and 's2' cross together to 
      give a zero vector, the position component of the output 
      will be the zero vector.  The velocity component of the 
      output will simply be the derivative of the cross product 
      of the position vectors. 
 
   2) If 's1' and 's2' are large in magnitude (taken together, 
      their magnitude surpasses the limit allowed by the 
      computer) then it may be possible to generate a 
      floating point overflow from an intermediate 
      computation even though the actual cross product and 
      derivative may be well within the range of double 
      precision numbers. 

-Files
 
   None. 

-Particulars
 
   ducrss_c calculates the unit vector parallel to the cross product 
   of two vectors and the derivative of that unit vector. 
   The results of the computation may overwrite either of the 
   input vectors. 
 
-Examples
 
   One often constructs non-inertial coordinate frames from 
   apparent positions of objects.  However, if one wants to convert 
   states in this non-inertial frame to states in an inertial 
   reference frame, the derivatives of the axes of the non-inertial 
   frame are required.  For example consider an Earth meridian 
   frame defined as follows. 
 
      The z-axis of the frame is defined to be the vector 
      normal to the plane spanned by the position vectors to the 
      apparent Sun and to the apparent body as seen from an observer. 
 
      Let 'sun' be the apparent state of the Sun and let 'body' be the 
      apparent state of the body with respect to the observer.  Then 
      the unit vector parallel to the z-axis of the Earth meridian 
      system and its derivative are given by the call: 
 
      ducrss_c ( sun, body, zzdot );
 
-Restrictions
 
   No checking of 's1' or 's2' is done to prevent floating point 
   overflow. The user is required to determine that the magnitude 
   of each component of the states is within an appropriate range 
   so as not to cause floating point overflow. In almost every case 
   there will be no problem and no checking actually needs to be 
   done. 

-Literature_References

   None.

-Author_and_Institution
 
   N.J. Bachman    (JPL) 
   W.L. Taber      (JPL) 
   E.D. Wright     (JPL)
 
-Version
 
   -CSPICE Version 1.0.0, 23-NOV-2009 (EDW)

-Index_Entries
 
   Compute a unit cross product and its derivative 
 
-&
*/

{ /* Begin ducrss_c */

   /*
   Local variables
   */

   SpiceDouble  tmpsta[6];

   /*
   Not much to this. Just get the cross product and its derivative.
   Using that, get the associated unit vector and its derivative.
   */
   dvcrss_c ( s1, s2, tmpsta );
   dvhat_c  ( tmpsta, sout   );

} /* End ducrss_c */


