/*

-Procedure sphcyl_c ( Spherical to cylindrical coordinates )

-Abstract
 
    This routine converts from spherical coordinates to cylindrical 
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
 
   CONVERSION,  COORDINATES 
 
*/

   #include <math.h>
   #include "SpiceUsr.h"


   void sphcyl_c ( SpiceDouble     radius,
                   SpiceDouble     colat,
                   SpiceDouble     slon,
                   SpiceDouble   * r,
                   SpiceDouble   * lon,
                   SpiceDouble   * z ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  ------------------------------------------------- 
   radius     I   Distance of point from origin. 
   colat      I   Polar angle (co-latitude in radians) of point. 
   slon       I   Azimuthal angle (longitude) of point (radians). 
   r          O   Distance of point from z axis. 
   lon        O   angle (radians) of point from XZ plane. 
   z          O   Height of point above XY plane. 
 
-Detailed_Input
 
   radius     Distance of the point from origin. 
 
   colat      Polar angle (co-latitude in radians) of the point. 
 
   slon       Azimuthal angle (longitude) of the point (radians). 
 
-Detailed_Output
 
   r          Distance of the point of interest from z axis. 
 
   lon        cylindrical angle (radians) of the point from the 
               XZ plane. 
 
   z          Height of the point above XY plane. 
 
-Parameters
 
   None. 
 
-Particulars
 
   This returns the cylindrical coordinates of a point whose 
   position is input through spherical coordinates. 
 
-Examples
 
 
   Other than the obvious conversion between coordinate systems 
   this routine could be used to obtain the axial projection 
   from a sphere to a cylinder about the z-axis that contains 
   the equator of the sphere.  The following code fragment 
   illustrates this idea. 
 
         sphcyl_c ( radius, colat,  lon, r,  lon, z ) 
         r = radius 
 
   r,  lon, and z now contain the coordinates of the projected 
   point. Such a projection is valuable because it preserves the 
   areas between regions on the sphere and their projections to the 
   cylinder. 
 
-Restrictions
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Author_and_Institution
 
   W.L. Taber      (JPL) 
   E.D. Wright     (JPL)
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries
 
   spherical to cylindrical coordinates 
 
-&
*/

{ /* Begin sphcyl_c */

   /*
   Local variables
   */

   SpiceDouble    rr;
   SpiceDouble    zz;

   /*
   Convert to cylindrical coordinates, storing the results in
   temporary variables.
   */

   rr = radius * sin( colat );
   zz = radius * cos( colat );


   /* Move the results to the output variables. */

   *lon = slon;
   *r   = rr;
   *z   = zz;


} /* End sphcyl_c */
