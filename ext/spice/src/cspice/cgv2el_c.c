/*

-Procedure cgv2el_c ( Center and generating vectors to ellipse )

-Abstract
 
   Form a CSPICE ellipse from a center vector and two generating 
   vectors. 
 
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
   #undef    cgv2el_c


   void cgv2el_c ( ConstSpiceDouble    center[3],
                   ConstSpiceDouble    vec1  [3],
                   ConstSpiceDouble    vec2  [3],
                   SpiceEllipse      * ellipse   ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   center, 
   vec1, 
   vec2       I   Center and two generating vectors for an ellipse. 
   ellipse    O   The CSPICE ellipse defined by the input vectors. 
 
-Detailed_Input
 
   center, 
   vec1, 
   vec2           are a center and two generating vectors defining 
                  an ellipse in three-dimensional space.  The 
                  ellipse is the set of points 
 
                     center  +  cos(theta) vec1  +  sin(theta) vec2 
 
                  where theta ranges over the interval (-pi, pi]. 
                  vec1 and vec2 need not be linearly independent. 
 
-Detailed_Output
 
   ellipse        is the CSPICE ellipse defined by the input 
                  vectors. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If vec1 and vec2 are linearly dependent, ellips will be 
       degenerate.  CSPICE ellipses are allowed to represent 
       degenerate geometric ellipses. 
 
-Files
 
   None. 
 
-Particulars
 
   CSPICE ellipses serve to simplify calling sequences and reduce 
   the chance for error in declaring and describing argument lists 
   involving ellipses. 
 
   The set of ellipse conversion routines is 
 
      cgv2el_c ( Center and generating vectors to ellipse ) 
      el2cgv_c ( Ellipse to center and generating vectors ) 
 
-Examples
 
   1)  Find the intersecton of an ellipse with a plane.  The ellipse 
       is defined by the vectors center, vec1, and vec2.  The plane 
       is defined by the normal vector n and the constant c. 
 
          #include "SpiceUsr.h"
                    .
                    .
                    .
          /.
          Make a CSPICE ellipse.  Make a plane while we're at it. 
          ./
          cgv2el_c ( center, vec1, vec2,  &ellipse ); 
          nvc2pl_c ( n,      c,           &plane   ); 

          /.
          Find the intersection of the ellipse and plane. 
          nxpts is the number of intersection points; xpt1 
          and xpt2 are the points themselves. 
          ./
          inelpl_c ( &ellipse, &plane, &nxpts, xpt1, xpt2 );
 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 05-MAR-1999 (NJB)

-Index_Entries
 
   center and generating vectors to ellipse 
 
-&
*/

{ /* Begin cgv2el_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "cgv2el_c" );

   /*
   The center of the ellipse is held in the first three elements.
   */
   MOVED ( center, 3, ellipse->center );

   /*
   Find the semi-axes of the ellipse.  These may be degenerate.
   */
   saelgv_c ( vec1, vec2, ellipse->semiMajor, ellipse->semiMinor );
   
   
   chkout_c ( "cgv2el_c" );

} /* End cgv2el_c */

