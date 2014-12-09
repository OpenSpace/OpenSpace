/*

-Procedure drdgeo_c ( Derivative of rectangular w.r.t. geodetic )

-Abstract
 
   This routine computes the Jacobian of the transformation from 
   geodetic to rectangular coordinates. 
 
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
 
   COORDINATES 
   DERIVATIVES 
   MATRIX 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void drdgeo_c ( SpiceDouble    lon,
                   SpiceDouble    lat,
                   SpiceDouble    alt,
                   SpiceDouble    re,
                   SpiceDouble    f,
                   SpiceDouble    jacobi[3][3] ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   lon        I   Geodetic longitude of point (radians). 
   lat        I   Geodetic latitude of point (radians). 
   alt        I   Altitude of point above the reference spheroid. 
   re         I   Equatorial radius of the reference spheroid. 
   f          I   Flattening coefficient. 
   jacobi     O   Matrix of partial derivatives. 
 
-Detailed_Input
 
   lon        Geodetic longitude of point (radians). 
 
   lat        Geodetic latitude  of point (radians). 
 
   alt        Altitude of point above the reference spheroid. 
 
   re         Equatorial radius of the reference spheroid. 
 
   f          Flattening coefficient = (re-rp) / re,  where rp is 
              the polar radius of the spheroid.  (More importantly 
              rp = re*(1-f).) 
 
-Detailed_Output
 
   jacobi     is the matrix of partial derivatives of the conversion 
              between geodetic and rectangular coordinates.  It 
              has the form 
 
                 .-                             -. 
                 |  dx/dlon   dx/dlat  dx/dalt   | 
                 |  dy/dlon   dy/dlat  dy/dalt   | 
                 |  dz/dlon   dz/dlat  dz/dalt   | 
                 `-                             -' 
 
              evaluated at the input values of lon, lat and alt. 
 
              The formulae for computing x, y, and z from 
              geodetic coordinates are given below. 
 
                 x = [alt +        re/g(lat,f)]*cos(lon)*cos(lat)


                 y = [alt +        re/g(lat,f)]*sin(lon)*cos(lat)

                                    2
                 z = [alt + re*(1-f) /g(lat,f)]*         sin(lat)

              where

                 re is the polar radius of the reference spheroid.

                 f  is the flattening factor (the polar radius is
                 obtained by multiplying the equatorial radius by 1-f).

                 g( lat, f ) is given by

                        2             2     2
                    sqrt ( cos (lat) + (1-f) * sin (lat) )

-Parameters
 
   None. 
 
-Exceptions
 
   1) If the flattening coefficient is greater than or equal to 
      one, the error SPICE(VALUEOUTOFRANGE) is signaled. 
 
   2) If the equatorial radius is non-positive, the error
      SPICE(BADRADIUS) is signaled.
 
-Files
 
   None. 
 
-Particulars
 
   It is often convenient to describe the motion of an object in 
   the geodetic coordinate system.  However, when performing 
   vector computations its hard to beat rectangular coordinates. 
 
   To transform states given with respect to geodetic coordinates 
   to states with respect to rectangular coordinates, one makes use 
   of the Jacobian of the transformation between the two systems. 
 
   Given a state in geodetic coordinates 
 
        ( lon, lat, alt, dlon, dlat, dalt ) 
 
   the velocity in rectangular coordinates is given by the matrix 
   equation: 
 
                  t          |                                 t 
      (dx, dy, dz)   = jacobi|             * (dlon, dlat, dalt) 
                             |(lon,lat,alt) 
 
 
   This routine computes the matrix  
 
            | 
      jacobi| 
            |(lon,lat,alt) 
 
-Examples
 
   Suppose that one has a model that gives radius, longitude and 
   latitude as a function of time (lon(t), lat(t), alt(t) ) for 
   which the derivatives ( dlon/dt, dlat/dt, dalt/dt ) are 
   computable. 
 
   To find the velocity of the object in bodyfixed rectangular 
   coordinates, one simply multiplies the Jacobian of the 
   transformation from geodetic to rectangular coordinates, 
   evaluated at (lon(t), lat(t), alt(t) ), by the vector of  
   derivatives of the geodetic coordinates. 
 
   In code this looks like: 
 
      #include "SpiceUsr.h"
           .
           .
           .
      /. 
      Load the derivatives of lon, lat, and alt into the 
      geodetic velocity vector GEOV. 
      ./ 
      geov[0] = dlon_dt ( t );
      geov[1] = dlat_dt ( t );
      geov[2] = dalt_dt ( t );
 
      /.
      Determine the Jacobian of the transformation from 
      geodetic to rectangular coordinates at the geodetic  
      coordinates of time t. 
      ./
      drdgeo_c ( lon(t), lat(t), alt(t), re, f, jacobi ); 
 
      /.
      Multiply the Jacobian on the right by the geodetic 
      velocity to obtain the rectangular velocity recv. 
      ./
      mxv_c ( jacobi, geov, recv );
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   W.L. Taber     (JPL) 
   N.J. Bachman   (JPL)

-Version
 
   -CSPICE Version 1.0.0, 20-JUL-2001 (WLT) (NJB)

-Index_Entries
 
   Jacobian of rectangular w.r.t. geodetic coordinates 
 
-&
*/

{ /* Begin drdgeo_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "drdgeo_c" );


   drdgeo_ ( (doublereal *) &lon,
             (doublereal *) &lat,
             (doublereal *) &alt,
             (doublereal *) &re,
             (doublereal *) &f,
             (doublereal *) jacobi  );

   /*
   Transpose the Jacobian to create a C-style matrix.
   */
   xpose_c ( jacobi, jacobi );


   chkout_c ( "drdgeo_c" );
   
} /* End drdgeo_c */
