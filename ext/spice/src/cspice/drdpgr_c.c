/*

-Procedure drdpgr_c ( Derivative of rectangular w.r.t. planetographic )

-Abstract
 
   This routine computes the Jacobian matrix of the transformation 
   from planetographic to rectangular coordinates. 
 
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
   #include "SpiceZmc.h"


   void drdpgr_c ( ConstSpiceChar  * body,
                   SpiceDouble       lon,
                   SpiceDouble       lat,
                   SpiceDouble       alt,
                   SpiceDouble       re,
                   SpiceDouble       f,
                   SpiceDouble       jacobi[3][3] ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   body       I   Name of body with which coordinates are associated. 
   lon        I   Planetographic longitude of a point (radians). 
   lat        I   Planetographic latitude of a point (radians). 
   alt        I   Altitude of a point above reference spheroid. 
   re         I   Equatorial radius of the reference spheroid. 
   f          I   Flattening coefficient. 
   jacobi     O   Matrix of partial derivatives. 
 
-Detailed_Input
 
   body       Name of the body with which the planetographic 
              coordinate system is associated. 
 
              `body' is used by this routine to look up from the 
              kernel pool the prime meridian rate coefficient giving 
              the body's spin sense.  See the Files and Particulars 
              header sections below for details. 
 
   lon        Planetographic longitude of the input point.  This is 
              the angle between the prime meridian and the meridian 
              containing the input point.  For bodies having 
              prograde (aka direct) rotation, the direction of 
              increasing longitude is positive west:  from the +X 
              axis of the rectangular coordinate system toward the 
              -Y axis.  For bodies having retrograde rotation, the 
              direction of increasing longitude is positive east: 
              from the +X axis toward the +Y axis. 
 
              The earth, moon, and sun are exceptions: 
              planetographic longitude is measured positive east for 
              these bodies. 
 
              The default interpretation of longitude by this 
              and the other planetographic coordinate conversion 
              routines can be overridden; see the discussion in 
              Particulars below for details. 
 
              Longitude is measured in radians. On input, the range 
              of longitude is unrestricted. 
 
   lat        Planetographic latitude of the input point.  For a 
              point P on the reference spheroid, this is the angle 
              between the XY plane and the outward normal vector at 
              P. For a point P not on the reference spheroid, the 
              planetographic latitude is that of the closest point 
              to P on the spheroid. 
 
              Latitude is measured in radians.  On input, the 
              range of latitude is unrestricted.  
 
   alt        Altitude of point above the reference spheroid. 
              Units of `alt' must match those of `re'. 
 
   re         Equatorial radius of a reference spheroid.  This 
              spheroid is a volume of revolution:  its horizontal 
              cross sections are circular.  The shape of the 
              spheroid is defined by an equatorial radius `re' and 
              a polar radius `rp'.  Units of `re' must match those of  
              `alt'. 
 
   f          Flattening coefficient =  
 
                 (re-rp) / re 
 
              where `rp' is the polar radius of the spheroid, and the 
              units of `rp' match those of `re'. 
 
-Detailed_Output
 
   JACOBI     is the matrix of partial derivatives of the conversion 
              from planetographic to rectangular coordinates.  It 
              has the form 
 
                 .-                              -. 
                 |  DX/DLON   DX/DLAT   DX/DALT   | 
                 |  DY/DLON   DY/DLAT   DY/DALT   | 
                 |  DZ/DLON   DZ/DLAT   DZ/DALT   | 
                 `-                              -' 
 
              evaluated at the input values of `lon', `lat' and `alt'. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the body name `body' cannot be mapped to a NAIF ID code, 
      and if `body' is not a string representation of an integer, 
      the error SPICE(IDCODENOTFOUND) will be signaled. 
 
   2) If the kernel variable   
 
         BODY<ID code>_PGR_POSITIVE_LON 
 
      is present in the kernel pool but has a value other 
      than one of 
 
          'EAST' 
          'WEST' 
 
      the error SPICE(INVALIDOPTION) will be signaled.  Case 
      and blanks are ignored when these values are interpreted. 
 
   3) If polynomial coefficients for the prime meridian of `body' 
      are not available in the kernel pool, and if the kernel 
      variable BODY<ID code>_PGR_POSITIVE_LON is not present in 
      the kernel pool, the error SPICE(MISSINGDATA) will be signaled. 
       
   4) If the equatorial radius is non-positive, the error 
      SPICE(VALUEOUTOFRANGE) is signaled. 
 
   5) If the flattening coefficient is greater than or equal to one, 
      the error SPICE(VALUEOUTOFRANGE) is signaled. 

   6) The error SPICE(EMPTYSTRING) is signaled if the input
      string `body' does not contain at least one character, since the
      input string cannot be converted to a Fortran-style string in
      this case.
      
   7) The error SPICE(NULLPOINTER) is signaled if the input string
      pointer `body' is null.
 
-Files
 
   This routine expects a kernel variable giving body's prime 
   meridian angle as a function of time to be available in the 
   kernel pool.  Normally this item is provided by loading a PCK 
   file.  The required kernel variable is named  
 
      BODY<body ID>_PM  
 
   where <body ID> represents a string containing the NAIF integer  
   ID code for `body'.  For example, if `body' is "JUPITER", then  
   the name of the kernel variable containing the prime meridian  
   angle coefficients is  
 
      BODY599_PM 
 
   See the PCK Required Reading for details concerning the prime 
   meridian kernel variable. 
 
   The optional kernel variable  
    
      BODY<body ID>_PGR_POSITIVE_LON 
 
   also is normally defined via loading a text kernel. When this 
   variable is present in the kernel pool, the prime meridian 
   coefficients for `body' are not required by this routine. See the 
   Particulars section below for details. 
 
-Particulars
 
   It is often convenient to describe the motion of an object in the 
   planetographic coordinate system.  However, when performing 
   vector computations it's hard to beat rectangular coordinates. 
 
   To transform states given with respect to planetographic 
   coordinates to states with respect to rectangular coordinates, 
   one makes use of the Jacobian of the transformation between the 
   two systems. 
 
   Given a state in planetographic coordinates 
 
      ( lon, lat, alt, dlon, dlat, dalt ) 
 
   the velocity in rectangular coordinates is given by the matrix 
   equation: 
 
                  t          |                                  t 
      (dx, dy, dz)   = jacobi|              * (dlon, dlat, dalt) 
                             |(lon,lat,alt) 
 
 
   This routine computes the matrix  
 
            | 
      jacobi| 
            |(lon,lat,alt) 
 
 
   In the planetographic coordinate system, longitude is defined 
   using the spin sense of the body.  Longitude is positive to the 
   west if the spin is prograde and positive to the east if the spin 
   is retrograde.  The spin sense is given by the sign of the first 
   degree term of the time-dependent polynomial for the body's prime 
   meridian Euler angle "W":  the spin is retrograde if this term is 
   negative and prograde otherwise.  For the sun, planets, most 
   natural satellites, and selected asteroids, the polynomial 
   expression for W may be found in a SPICE PCK kernel. 
 
   The earth, moon, and sun are exceptions: planetographic longitude 
   is measured positive east for these bodies. 
 
   If you wish to override the default sense of positive longitude 
   for a particular body, you can do so by defining the kernel 
   variable 
 
      BODY<body ID>_PGR_POSITIVE_LON 
 
   where <body ID> represents the NAIF ID code of the body. This 
   variable may be assigned either of the values 
 
      'WEST' 
      'EAST' 
 
   For example, you can have this routine treat the longitude 
   of the earth as increasing to the west using the kernel 
   variable assignment 
 
      BODY399_PGR_POSITIVE_LON = 'WEST' 
       
   Normally such assignments are made by placing them in a text 
   kernel and loading that kernel via furnsh_c. 
 
   The definition of this kernel variable controls the behavior of 
   the CSPICE planetographic routines 
 
      pgrrec_c 
      recpgr_c 
      dpgrdr_c 
      drdpgr_c 
 
   It does not affect the other CSPICE coordinate conversion 
   routines. 
 
-Examples
 
   Numerical results shown for this example may differ between 
   platforms as the results depend on the SPICE kernels used as 
   input and the machine specific arithmetic implementation. 
 

     Find the planetographic state of the earth as seen from 
     Mars in the J2000 reference frame at January 1, 2005 TDB. 
     Map this state back to rectangular coordinates as a check. 


           #include <stdio.h>
           #include "SpiceUsr.h"

           int main()
        {
           /.
           Local variables 
           ./
           SpiceDouble             alt;
           SpiceDouble             drectn [3];
           SpiceDouble             et;
           SpiceDouble             f;
           SpiceDouble             jacobi [3][3];
           SpiceDouble             lat;
           SpiceDouble             lon;
           SpiceDouble             lt;
           SpiceDouble             pgrvel [3];
           SpiceDouble             radii  [3];
           SpiceDouble             re;
           SpiceDouble             rectan [3];
           SpiceDouble             rp;
           SpiceDouble             state  [6];

           SpiceInt                n;


           /.
           Load a PCK file containing a triaxial
           ellipsoidal shape model and orientation
           data for Mars.
           ./
           furnsh_c ( "pck00008.tpc" );

           /.
           Load an SPK file giving ephemerides of earth and Mars.
           ./
           furnsh_c ( "de405.bsp" );

           /.
           Load a leapseconds kernel to support time conversion.
           ./
           furnsh_c ( "naif0007.tls" );

           /.
           Look up the radii for Mars.  Although we
           omit it here, we could first call badkpv_c
           to make sure the variable BODY499_RADII
           has three elements and numeric data type.
           If the variable is not present in the kernel
           pool, bodvrd_c will signal an error.
           ./
           bodvrd_c ( "MARS", "RADII", 3, &n, radii );

           /.
           Compute flattening coefficient.
           ./
           re  =  radii[0];
           rp  =  radii[2];
           f   =  ( re - rp ) / re;

           /.
           Look up the geometric state of earth as seen from Mars at
           January 1, 2005 TDB, relative to the J2000 reference
           frame.
           ./
           str2et_c ( "January 1, 2005 TDB", &et);

           spkezr_c ( "Earth", et,    "J2000", "LT+S",    
                      "Mars",  state, &lt              );

           /.
           Convert position to planetographic coordinates.
           ./
           recpgr_c ( "mars", state, re, f, &lon, &lat, &alt );

           /.
           Convert velocity to planetographic coordinates.
           ./

           dpgrdr_c ( "MARS",  state[0],  state[1],  state[2],    
                      re,      f,         jacobi               );

           mxv_c ( jacobi, state+3, pgrvel );


           /.
           As a check, convert the planetographic state back to
           rectangular coordinates.
           ./
           pgrrec_c ( "mars", lon, lat, alt, re, f, rectan );
           drdpgr_c ( "mars", lon, lat, alt, re, f, jacobi );

           mxv_c ( jacobi, pgrvel, drectn );

           printf ( "\n"
                    "Rectangular coordinates:\n"
                    "\n"
                    "  X (km)                 = %18.9e\n"
                    "  Y (km)                 = %18.9e\n"
                    "  Z (km)                 = %18.9e\n"
                    "\n"
                    "Rectangular velocity:\n"
                    "\n"
                    "  dX/dt (km/s)           = %18.9e\n"
                    "  dY/dt (km/s)           = %18.9e\n"
                    "  dZ/dt (km/s)           = %18.9e\n"
                    "\n"
                    "Ellipsoid shape parameters:\n"
                    "\n"
                    "  Equatorial radius (km) = %18.9e\n"
                    "  Polar radius      (km) = %18.9e\n"
                    "  Flattening coefficient = %18.9e\n"
                    "\n"
                    "Planetographic coordinates:\n"
                    "\n"
                    "  Longitude (deg)        = %18.9e\n"
                    "  Latitude  (deg)        = %18.9e\n"
                    "  Altitude  (km)         = %18.9e\n"
                    "\n"
                    "Planetographic velocity:\n"
                    "\n"
                    "  d Longitude/dt (deg/s) = %18.9e\n"
                    "  d Latitude/dt  (deg/s) = %18.9e\n"
                    "  d Altitude/dt  (km/s)  = %18.9e\n"
                    "\n"
                    "Rectangular coordinates from inverse mapping:\n"
                    "\n"
                    "  X (km)                 = %18.9e\n"
                    "  Y (km)                 = %18.9e\n"
                    "  Z (km)                 = %18.9e\n"
                    "\n"
                    "Rectangular velocity from inverse mapping:\n"
                    "\n"
                    "  dX/dt (km/s)           = %18.9e\n"
                    "  dY/dt (km/s)           = %18.9e\n"
                    "  dZ/dt (km/s)           = %18.9e\n"
                    "\n",
                    state [0],
                    state [1],
                    state [2],
                    state [3],
                    state [4],
                    state [5],
                    re,
                    rp,
                    f,
                    lon / rpd_c(),
                    lat / rpd_c(),
                    alt,
                    pgrvel[0]/rpd_c(),
                    pgrvel[1]/rpd_c(),
                    pgrvel[2],
                    rectan [0],
                    rectan [1],
                    rectan [2],
                    drectn [0],
                    drectn [1],
                    drectn [2]                );

           return ( 0 );
        }

     Output from this program should be similar to the following
     (rounding and formatting differ across platforms):


        Rectangular coordinates:

          X (km)                 =    1.460397325e+08
          Y (km)                 =    2.785466068e+08
          Z (km)                 =    1.197503153e+08

        Rectangular velocity:

          dX/dt (km/s)           =   -4.704288238e+01
          dY/dt (km/s)           =    9.070217780e+00
          dZ/dt (km/s)           =    4.756562739e+00

        Ellipsoid shape parameters:

          Equatorial radius (km) =    3.396190000e+03
          Polar radius      (km) =    3.376200000e+03
          Flattening coefficient =    5.886007556e-03

        Planetographic coordinates:

          Longitude (deg)        =    2.976676591e+02
          Latitude  (deg)        =    2.084450403e+01
          Altitude  (km)         =    3.365318254e+08

        Planetographic velocity:

          d Longitude/dt (deg/s) =   -8.357386316e-06
          d Latitude/dt  (deg/s) =    1.593493548e-06
          d Altitude/dt  (km/s)  =   -1.121443268e+01

        Rectangular coordinates from inverse mapping:

          X (km)                 =    1.460397325e+08
          Y (km)                 =    2.785466068e+08
          Z (km)                 =    1.197503153e+08

        Rectangular velocity from inverse mapping:

          dX/dt (km/s)           =   -4.704288238e+01
          dY/dt (km/s)           =    9.070217780e+00
          dZ/dt (km/s)           =    4.756562739e+00

 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
   W.L. Taber     (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 26-DEC-2004 (NJB) (WLT)

-Index_Entries
 
   Jacobian of rectangular w.r.t. planetographic coordinates 
 
-&
*/

{ /* Begin drdpgr_c */


   /*
   Participate in error tracing.
   */
   if ( return_c()  )
   {
      return; 
   }
   chkin_c ( "drdpgr_c" );


   /*
   Check the input string body to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "drdpgr_c", body );
   

   /*
   Call the f2c'd Fortran routine.
   */
   drdpgr_ ( ( char       * ) body,
             ( doublereal * ) &lon,
             ( doublereal * ) &lat,
             ( doublereal * ) &alt,
             ( doublereal * ) &re,
             ( doublereal * ) &f,
             ( doublereal * ) jacobi,
             ( ftnlen       ) strlen(body)  );

   /*
   Convert Jacobian matrix to row-major order. 
   */
   xpose_c ( jacobi, jacobi );


   chkout_c ( "drdpgr_c" );

} /* End drdpgr_c */
