/*

-Procedure xfmsta_c ( Transform state between coordinate systems )

-Abstract
 
   Transform a state between coordinate systems.
 
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
 
   CONVERSION 
   COORDINATE
   EPHEMERIS
   STATE 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void xfmsta_c ( ConstSpiceDouble     input_state[6],
                   ConstSpiceChar     * input_coord_sys,
                   ConstSpiceChar     * output_coord_sys,
                   ConstSpiceChar     * body,
                   SpiceDouble          output_state[6]  )
                   
/*

-Brief_I/O
 
   VARIABLE         I/O  DESCRIPTION
   --------         ---  -------------------------------------------
   input_state       I   Input state.
   input_coord_sys   I   Current (input) coordinate system.
   output_coord_sys  I   Desired (output) coordinate system.
   body              I   Name or NAIF ID of body with which
                         coordinates are associated (if applicable).
   output_state      O   Converted output state.

-Detailed_Input

   input_state       is a state vector in the input (`input_coord_sys')
                     coordinate system representing position and velocity.
   
                     All angular measurements must be in radians.
   
                     Note: body radii values taken from the kernel
                     pool are used when converting to or from geodetic or
                     planetographic coordinates. It is the user's
                     responsibility to verify the distance inputs are in
                     the same units as the radii in the kernel pool,
                     typically kilometers.
   
   input_coord_sys   is the name of the coordinate system that the input
                     state vector (`input_state') is currently in.
   
                     `input_coord_sys' may be any of the following:
   
                           'RECTANGULAR'
                           'CYLINDRICAL'
                           'LATITUDINAL'
                           'SPHERICAL'
                           'GEODETIC'
                           'PLANETOGRAPHIC'

                     Leading spaces, trailing spaces, and letter case
                     are ignored. For example, ' cyLindRical  ' would
                     be accepted.

   output_coord_sys  is the name of the coordinate system that the state
                     should be converted to.
   
                     Please see the description of `input_coord_sys'
                     for details.
   
   body              is the name or NAIF ID of the body associated with the
                     planetographic or geodetic coordinate system.
          
                     If neither of the coordinate system choices are
                     geodetic or planetographic, `body' may be an empty
                     string (' ').
          
                     Examples of accepted body names or IDs are:
                              'Earth'
                              '399'
         
                     Leading spaces, trailing spaces, and letter case are
                     ignored.

-Detailed_Output

   output_state      is the state vector that has been converted to the
                     output coordinate system (`output_coord_sys').

-Parameters

   None.
   
-Exceptions

   1)  If either the input or output coordinate system is not
       recognized, the error SPICE(COORDSYSNOTREC) is signaled.
   
   2)  If the input body name cannot be converted to a NAIF ID
       (applies to geodetic and planetographic coordinate
       systems), the error 'SPICE(IDCODENOTFOUND)' is signaled.
   
   3)  If the input state `input_state' is not valid, meaning the position
       but not the velocity is along the z-axis, the error
       'SPICE(INVALIDSTATE)' is signaled.
   
       Note: If both the input position and velocity are along
       the z-axis and the output coordinate system is not
       rectangular, the velocity can still be calculated even
       though the Jacobian is undefined. This case will not
       signal an error. An example of the input position and
       velocity along the z-axis is below.
   
                     Term    Value
                     -----   ------
                       x       0
                       y       0
                       z       z
                     dx/dt     0
                     dy/dt     0
                     dz/dt   dz_dt
   
   4)  If either the input or output coordinate system is 
       geodetic or planetographic and at least one of the body's
       radii is less than or equal to zero, the error 
       SPICE(INVALIDRADIUS) will be signaled.
   
   5)  If either the input or output coordinate system is
       geodetic or planetographic and the difference of the
       equatorial and polar radii divided by the equatorial radius
       would produce numeric overflow, the error
       'SPICE(INVALIDRADIUS)' will be signaled.
   
   6)  If the product of the Jacobian and velocity components
       may lead to numeric overflow, the error 'SPICE(NUMERICOVERFLOW)'
       is signaled.
   

-Files

   SPK, PCK, CK, and FK kernels may be required.
   
   If the input or output coordinate systems are either geodetic or
   planetographic, a PCK providing the radii of the body
   name `body' must be loaded via furnsh_c.
   
   Kernel data are normally loaded once per program run, NOT every
   time this routine is called.

-Particulars

   Input Order
   -------------------------------------------
   
      The input and output states will be structured by the
      following descriptions.
      
      For rectangular coordinates, the state vector is the following
      in which X, Y, and Z are the rectangular position components and
      DX, DY, and DZ are the time derivatives of each position component.
              ISTATE = ( X, Y, Z, DX, DY, DZ )
      
      For cylindrical coordinates, the state vector is the following
      in which R is the radius, LONG is the longitude, Z is the
      height, and DR, DLONG, and DZ are the time derivatives of each
      position component.
              ISTATE = ( R, LONG, Z, DR, DLONG, DZ )
      
      For latitudinal coordinates, the state vector is the following
      in which R is the radius, LONG is the longitude, LAT is the
      latitude, and DR, DLONG, and DLAT are the time derivatives of
      each position component.
              ISTATE = ( R, LONG, LAT, DR, DLONG, DLAT )
      
      For spherical coordinates, the state vector is the following in
      which R is the radius, COLAT is the colatitude, LONG is the
      longitude, and DR, DCOLAT, and DLONG are the time derivatives of
      each position component.
              ISTATE = ( R, COLAT, LONG, DR, DCOLAT, DLONG )
      
      For geodetic coordinates, the state vector is the following in
      which LONG is the longitude, LAT is the latitude, ALT is the
      altitude, and DLONG, DLAT, and DALT are the time derivatives of
      each position component.
              ISTATE = ( LONG, LAT, ALT, DLONG, DLAT, DALT )
      
      For planetographic coordinates, the state vector is the
      following in which LONG is the longitude, LAT is the latitude,
      ALT is the altitude, and DLONG, DLAT, and DALT are the time
      derivatives of each position component.
              ISTATE = ( LONG, LAT, ALT, DLONG, DLAT, DALT )

   Input Boundaries
   -------------------------------------------
   
      There are intervals the input angles must fall within if
      the input coordinate system is not rectangular. These
      intervals are provided below.
      
         Input variable    Input meaning   Input interval [rad]
         --------------    -------------   ------------------------
             LONG           Longitude        0     <= LONG  <  2*pi
             LAT            Latitude        -pi/2  <= LAT   <= pi/2
             COLAT          Colatitude       0     <= COLAT <= pi

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Find the apparent state of Phoebe as seen by CASSINI in the
      J2000 frame at 2004 Jun 11 19:32:00. Transform the state
      from rectangular to latitudinal coordinates. For verification,
      transform the state back from latitudinal to rectangular
      coordinates.
   
      Use the meta-kernel shown below to load the required SPICE
      kernels.
   
         KPL/MK
   
         File name: xfmsta_ex1.tm
   
         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.
   
         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.
   
         The names and contents of the kernels referenced
         by this meta-kernel are as follows:
   
                File name                     Contents
                ---------                     --------
                cpck05Mar2004.tpc             Planet orientation and
                                              radii
                naif0009.tls                  Leapseconds
                020514_SE_SAT105.bsp          Satellite ephemeris for
                                              Saturn
                030201AP_SK_SM546_T45.bsp     CASSINI ephemeris
                981005_PLTEPH-DE405S.bsp      Planetary ephemeris
   
   
         \begindata
   
         KERNELS_TO_LOAD = ( 'naif0009.tls'  ,
                             '020514_SE_SAT105.bsp'  ,
                             '030201AP_SK_SM546_T45.bsp'  ,
                             '981005_PLTEPH-DE405S.bsp',
                             'cpck05Mar2004.tpc'   )

         End of meta-kernel

      Example code begins here.
      
         #include <stdio.h>
         #include "SpiceUsr.h"

         int main()
         {

            /.
            Constants 
            ./

            #define METAKR "xfmsta_ex1.tm"

            /.
            Local variables

            `et' is the ephemeris time (TDB) corresponding to the
            observation.
            ./
            SpiceDouble             et;
            SpiceDouble             lt;

            /.
            `state_rec' is the state of Phoebe with respect to CASSINI
            in rectangular coordinates. `state_lat' is the state
            transformed in latitudinal coordinates. `state_rec2' is the
            state transformed back into rectangular coordinates from
            latitudinal.
            ./
            SpiceDouble             state_rec  [6];
            SpiceDouble             state_lat  [6];
            SpiceDouble             state_rec2 [6];

            /.
            The required kernels must be loaded.
            ./
            furnsh_c ( METAKR );

            /.
            Calculate the state at 2004 Jun 11 19:32:00 UTC.
            ./
            str2et_c ( "2004-JUN-11-19:32:00", &et );

            /.
            Calculate the apparent state of Phoebe as seen by
            Cassini in the J2000 frame.
            ./
            spkezr_c ( "phoebe",  et, "iau_phoebe", "lt+s",
                       "cassini", state_rec, &lt );

            /.
            Transform the state from rectangular to latitudinal.
            Notice that since neither the input nor output
            coordinate frames are 'geodetic' or 'planetographic',
            the input for the body name is a blank string.
            ./
            xfmsta_c ( state_rec, "rectangular", "latitudinal", 
                       " ", state_lat );

            /.
            Transform the state back to rectangular from latitudinal.
            The result should be very close to `state_rec'.
            ./
            xfmsta_c ( state_lat, "latitudinal", "rectangular", 
                       " ", state_rec2 );

            /.
            Report the results.
            ./
            printf ( "\n"
                     "Phoebe as seen by Cassini - rectangular\n"
                     "  Position [km]:\n"
                     "    %16.6f %16.6f %16.6f\n"
                     "  Velocity [km/s]:\n"
                     "    %16.6f %16.6f %16.6f\n\n"
                     "Phoebe as seen by Cassini - latitudinal\n"
                     "  Position [km, rad, rad]:\n"
                     "    %16.6f %16.6f %16.6f\n"
                     "  Velocity [km/s, rad/s, rad/s]:\n"
                     "    %16.6f %16.6f %16.6f\n\n"
                     "Verification:\n"
                     "Phoebe as seen by Cassini - rectangular\n"
                     "  Position [km]:\n"
                     "    %16.6f %16.6f %16.6f\n"
                     "  Velocity [km/s]:\n"
                     "    %16.6f %16.6f %16.6f\n", 
                     state_rec [0], state_rec [1], state_rec [2],
                     state_rec [3], state_rec [4], state_rec [5],
                     state_lat [0], state_lat [1], state_lat [2],
                     state_lat [3], state_lat [4], state_lat [5],
                     state_rec2[0], state_rec2[1], state_rec2[2],
                     state_rec2[3], state_rec2[4], state_rec2[5] );

            return(0);

         }
   
      When this program was executed on a PC/Linux/gcc platform, the
      output was:

         Phoebe as seen by Cassini - rectangular
           Position [km]:
                 -1982.639762      -934.530471      -166.562595
           Velocity [km/s]:
                     3.970832        -3.812496        -2.371663

         Phoebe as seen by Cassini - latitudinal
           Position [km, rad, rad]:
                  2198.169858        -2.701121        -0.075846
           Velocity [km/s, rad/s, rad/s]:
                    -1.780939         0.002346        -0.001144

         Verification:
         Phoebe as seen by Cassini - rectangular
           Position [km]:
                 -1982.639762      -934.530471      -166.562595
           Velocity [km/s]:
                     3.970832        -3.812496        -2.371663

   2) Transform a given state from cylindrical to planetographic
      coordinates with respect to Earth. 
   
         #include <stdio.h>
         #include "SpiceUsr.h"

         int main()
         {

            /.
            Local variables

            `state_cyl' is the state in cylindrical coordinates. 
            `state_pln' is the state transformed into planetographic
            coordinates. `state_cyl2' is the state transformed
            back into cylindrical coordinates from planetographic.
            ./
            SpiceDouble     state_cyl  [6] = {1, 0.5, 0.5, 0.2, 0.1, -0.2};
            SpiceDouble     state_pln  [6];
            SpiceDouble     state_cyl2 [6];


            /.
            The required kernels must be loaded.
            ./
            furnsh_c ( "cpck05Mar2004.tpc" );

            /.
            Transform the state from cylindrical to planetographic.
            Note that since one of the coordinate systems is
            planetographic, the body name must be input.
            ./
            xfmsta_c ( state_cyl, "cylindrical", "planetographic", 
                       "earth", state_pln );

            /.
            Transform the state back to cylindrical from planetographic.
            The result should be very close to `state_cyl'.
            ./
            xfmsta_c ( state_pln, "planetographic", "cylindrical", 
                       "earth", state_cyl2 );

            /.
            Report the results.
            ./
            printf ( "\n"
                     "Cylindrical state\n"
                     "  Position [km, rad, km]:\n"
                     "    %16.6f %16.6f %16.6f\n"
                     "  Velocity [km/s, rad/s, km/s]:\n"
                     "    %16.6f %16.6f %16.6f\n\n"
                     "Planetographic state\n"
                     "  Position  [rad, rad, km]:\n"
                     "    %16.6f %16.6f %16.6f\n"
                     "  Velocity [rad/s, rad/s, km/s]:\n"
                     "    %16.6f %16.6f %16.6f\n\n"
                     "Verification:  Cylindrical state\n"
                     "  Position [km, rad, km]:\n"
                     "    %16.6f %16.6f %16.6f\n"
                     "  Velocity [km/s, rad/s, km/s]:\n"
                     "    %16.6f %16.6f %16.6f\n", 
                     state_cyl [0], state_cyl [1], state_cyl [2],
                     state_cyl [3], state_cyl [4], state_cyl [5],
                     state_pln [0], state_pln [1], state_pln [2],
                     state_pln [3], state_pln [4], state_pln [5],
                     state_cyl2[0], state_cyl2[1], state_cyl2[2],
                     state_cyl2[3], state_cyl2[4], state_cyl2[5] );

            return(0);

         }
   
      When this program was executed on a PC/Linux/gcc platform, the
      output was:

         Cylindrical state
           Position [km, rad, km]:
                     1.000000         0.500000         0.500000
           Velocity [km/s, rad/s, km/s]:
                     0.200000         0.100000        -0.200000

         Planetographic state
           Position  [rad, rad, km]:
                     0.500000         1.547727     -6356.238467
           Velocity [rad/s, rad/s, km/s]:
                     0.100000        -0.004721        -0.195333

         Verification:  Cylindrical state
           Position [km, rad, km]:
                     1.000000         0.500000         0.500000
           Velocity [km/s, rad/s, km/s]:
                     0.200000         0.100000        -0.200000

-Restrictions
 
   None.
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   S.C. Krening   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 30-JAN-2011 (SCK)

-Index_Entries
 
   state transformation between coordinate systems
   convert state

-&
*/

{ /* Begin xfmsta_c */

   /*
   Static local variables 
   */

   /*
   Local variables 
   */

   
   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "xfmsta_c" );

   /*
   Check the input strings to make sure the pointers are non-null
   and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "xfmsta_c", input_coord_sys  );
   CHKFSTR ( CHK_STANDARD, "xfmsta_c", output_coord_sys );
   CHKFSTR ( CHK_STANDARD, "xfmsta_c", body             );

   /*
   Call the f2c'd Fortran routine. Use explicit type casts for every
   type defined by f2c.
   */
   xfmsta_ ( ( doublereal * )  input_state,
             ( char       * )  input_coord_sys,
             ( char       * )  output_coord_sys,
             ( char       * )  body,
             ( doublereal * )  output_state,  
             ( ftnlen       )  strlen(input_coord_sys),
             ( ftnlen       )  strlen(output_coord_sys),
             ( ftnlen       )  strlen(body)              );
 
 
   chkout_c ( "xfmsta_c" );
 

} /* End xfmsta_c */
