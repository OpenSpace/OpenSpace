/*

-Procedure el2cgv_c ( Ellipse to center and generating vectors )

-Abstract
 
   Convert a CSPICE ellipse to a center vector and two generating 
   vectors.  The selected generating vectors are semi-axes of the 
   ellipse. 
 
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
 
   ELLIPSES 
 
-Keywords
 
   ELLIPSE 
   GEOMETRY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #undef    el2cgv_c
   

   void el2cgv_c ( ConstSpiceEllipse   * ellipse,
                   SpiceDouble           center[3],
                   SpiceDouble           smajor[3],
                   SpiceDouble           sminor[3]  ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   ellipse    I   A CSPICE ellipse. 
   center, 
   smajor, 
   sminor     O   Center and semi-axes of ellipse. 
 
-Detailed_Input
 
   ellipse        is a CSPICE ellipse. 
 
-Detailed_Output
 
   center, 
   smajor, 
   sminor         are, respectively, a center vector, a semi-major 
                  axis vector, and a semi-minor axis vector that 
                  generate the input ellipse.  This ellipse is the 
                  set of points 
 
                     center + cos(theta) smajor + sin(theta) sminor 
 
                  where theta ranges over the interval (-pi, pi]. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Particulars
 
   CSPICE ellipses serve to simplify calling sequences and reduce 
   the chance for error in declaring and describing argument lists 
   involving ellipses. 
 
   The set of ellipse conversion routines is 
 
      cgv2el_c ( Center and generating vectors to ellipse ) 
      el2cgv_c ( Ellipse to center and generating vectors ) 
 
   A word about the output of this routine:   the semi-major axis of 
   an ellipse is a vector of largest possible magnitude in the set 
 
      cos(theta) vec1  +  sin(theta) vec2, 
 
   where theta is in the interval (-pi, pi].  There are two such 
   vectors; they are additive inverses of each other. The semi-minor 
   axis is an analogous vector of smallest possible magnitude.  The 
   semi-major and semi-minor axes are orthogonal to each other.  If 
   smajor and sminor are choices of semi-major and semi-minor axes, 
   then the input ellipse can also be represented as the set of 
   points 
 
 
      center + cos(theta) smajor + sin(theta) sminor 
 
   where theta ranges over the interval (-pi, pi]. 
 
 
-Examples
 
   1)  Find the semi-axes of the limb of an ellipsoid. 
 
          #include "SpiceUsr.h"
                  .
                  .
                  .
          /.
          Our viewing location is viewpt.  The radii of the 
          ellipsoid are a, b, and c. 
          ./
          edlimb_c ( a, b, c, viewpt, &limb );

          el2cgv_c ( &limb, center, smajor, sminor ); 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 12-JUN-1999 (NJB)

-Index_Entries
 
   ellipse to center and generating vectors 
 
-&
*/

{ /* Begin el2cgv_c */

   /*
   Error free.
   */


   MOVED ( ellipse->center,    3, center );
   MOVED ( ellipse->semiMajor, 3, smajor );
   MOVED ( ellipse->semiMinor, 3, sminor );
   

} /* End el2cgv_c */

