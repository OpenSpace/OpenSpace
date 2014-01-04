/*

-Procedure radrec_c ( Range, RA and DEC to rectangular coordinates )

-Abstract
 
   Convert from range, right ascension, and declination to rectangular
   coordinates.
 
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
 
   CONVERSION,   COORDINATES 
 
*/

   #include "SpiceUsr.h"

   void radrec_c ( SpiceDouble range, 
                   SpiceDouble ra, 
                   SpiceDouble dec, 
                   SpiceDouble rectan[3] ) 
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  --------------------------------------------------- 
   range      I   Distance of a point from the origin. 
   ra         I   Right ascension of point in radians. 
   dec        I   Declination of point in radians. 
   rectan     O   Rectangular coordinates of the point.
 
-Detailed_Input
 
   range      is the distance of the point from the origin.  Output
              units are the same as the units associated with `range.'
 
   ra         is the right ascension of the input point:  the angular
              distance measured toward the east from the prime meridian
              to the meridian containing the input point. The direction
              of increasing right ascension is from the +X axis towards
              the +Y axis.

              The range (i.e., the set of allowed values) of 
              `ra' is unrestricted.  Units are radians.

   dec        is the declination of the point.  This is the angular 
              distance from the XY plane to the point.  

              The range of `dec' is unrestricted.  Units are radians.
 
-Detailed_Output
 
   rectan     is the array containing the rectangular coordinates of 
              the point.  The output units associated with `rectan'
              are those associated with the input `range.'

-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Particulars

   None.
 
-Examples
 
   The following code fragment converts right ascension and 
   declination from the B1950 reference frame to the J2000 frame.
 
      #include "SpiceUsr.h"

      SpiceDouble      ra;
      SpiceDouble      dec; 
      SpiceDouble      r;
      SpiceDouble      rotab  [ 3 ][ 3 ]; 
      SpiceDouble      oldvec [ 3 ];
      SpiceDouble      newvec [ 3 ];


      radrec_c ( 1.0, ra, dec, oldvec );

      pxform_c ( "B1950", "J2000", 0.0, rotab );

      mxv_c    ( rotab,   oldvec, newvec );
      recrad_c ( newvec,  &r,     &ra,    &dec ); 
 
 
-Restrictions
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   H.A. Neilan     (JPL) 
   E.D. Wright     (JPL)
 
-Literature_References
 
   "Celestial Mechanics, A Computational Guide for the Practitioner" 
   by Laurence G. Taff 
 
-Version
 
   -CSPICE Version 1.0.2, 28-JUL-2003 (NJB) 
   
      Various header corrections were made.

   -CSPICE Version 1.0.1, 13-APR-2000 (NJB) 
   
      Made some minor updates and corrections in the code example.
      
   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries
 
   range ra and dec to rectangular coordinates 
   right_ascension and declination to rectangular 
 
-&
*/

{ /* Begin radrec_c */

   /*
   There isn't much to say or do...
   */

   latrec_c ( range, ra, dec, rectan );


} /* End radrec_c */
