/*

-Procedure et2lst_c ( ET to Local Solar Time )

-Abstract
 
   Given an ephemeris epoch, compute the local solar time for 
   an object on the surface of a body at a specified longitude. 
 
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
 
   TIME
 
-Keywords
 
   TIME 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void et2lst_c ( SpiceDouble        et,
                   SpiceInt           body,
                   SpiceDouble        lon,
                   ConstSpiceChar   * type,
                   SpiceInt           timlen,
                   SpiceInt           ampmlen,
                   SpiceInt         * hr,
                   SpiceInt         * mn,
                   SpiceInt         * sc,
                   SpiceChar        * time,
                   SpiceChar        * ampm ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   et         I   Epoch in seconds past J2000 epoch. 
   body       I   ID-code of the body of interest. 
   lon        I   Longitude of surface point (RADIANS). 
   type       I   Type of longitude "PLANETOCENTRIC", etc. 
   timlen     I   Available room in output time string.
   ampmlen    I   Available room in output `ampm' string.
   hr         O   Local hour on a "24 hour" clock. 
   mn         O   Minutes past the hour. 
   sc         O   Seconds past the minute. 
   time       O   String giving local time on 24 hour clock. 
   ampm       O   String giving time on A.M./ P.M. scale. 
 
-Detailed_Input
 
   `et'         is the epoch expressed in TDB seconds past 
              the J2000 epoch at which a local time is desired. 
 
   body       is the NAIF ID-code of a body on which local 
              time is to be measured. 
 
   lon        is the longitude (either planetocentric or 
              planetographic) in radians of the site on the 
              surface of body for which local time should be 
              computed. 
 
   type       is the form of longitude supplied by the variable 
              lon.   Allowed values are "PLANETOCENTRIC" and 
              "PLANETOGRAPHIC".  Note the case of the letters 
              in type is insignificant.  Both "PLANETOCENTRIC" 
              and "planetocentric" are recognized.  Leading and
              trailing blanks in type are not significant.

   timlen     The maximum allowed length of the output time string.
              This length must large enough to hold the time string
              plus the terminator.  If the output string is expected to
              have x characters, timlen needs to be x + 1.
 
   ampmlen    The maximum allowed length of the output `ampm' string.
              This length must large enough to hold the apmpm string
              plus the terminator.  If the output string is expected to
              have x characters, ampmlen needs to be x + 1.

-Detailed_Output 
 
   hr         is the local "hour" of the site specified at the epoch
              `et'. Note that an "hour" of local time does not have the
              same duration as an hour measured by conventional clocks.
              It is simply a representation of an angle.  See
              Particulars for a more complete discussion of the meaning
              of local time.
 
   mn         is the number of "minutes" past the hour of the local
              time of the site at the epoch `et'. Again note that a
              "local minute" is not the same as a minute you would
              measure with conventional clocks.
 
   sc         is the number of "seconds" past the minute of the local
              time of the site at the epoch `et'.  Again note that a
              "local second" is not the same as a second you would
              measure with conventional clocks.
 
   time       is a string expressing the local time on a "24 hour"
              local clock.
 
   ampm       is a string expressing the local time on a "12 hour"
              local clock together with the traditional AM/PM label to
              indicate whether the sun has crossed the local zenith
              meridian.

-Parameters
 
   None. 
 
-Exceptions
 
   1) This routine defines local solar time for any point on the 
      surface of the Sun to be 12:00:00 noon. 
 
   2) If the type of the coordinates is not recognized, the 
      error SPICE(UNKNOWNSYSTEM) will be signaled. 
 
   3) If the bodyfixed frame to associate with body cannot be 
      determined, the error SPICE(CANTFINDFRAME) is signaled. 
 
   4) If insufficient data are available to compute the 
      location of the sun in bodyfixed coordinates, the 
      error will be diagnosed by a routine called by this one. 
 
   5) If the input type string is empty, the error SPICE(EMPTYSTRING)
      will be signaled.

   6) If any of the routine's string arguments have null pointers, the
      error SPICE(NULLPOINTER) will be signaled.

   7) If either of the output strings are too short to accommodate
      at least one character of data in addition to a terminating
      null character, the error SPICE(STRINGTOOSHORT) will be 
      signaled.

-Files
 
   Suitable SPK and PCK files must be loaded prior to calling this
   routine so that the bodyfixed position of the sun relative to `body'
   can be computed.
 
   When the input longitude is planetographic, the default
   interpretation of this value can be overridden using the optional
   kernel variable
 
      BODY<body ID>_PGR_POSITIVE_LON
 
   which is normally defined via loading a text kernel.

-Particulars
 
   This routine returns the local solar time at a user 
   specified location on a user specified body. 
 
   Let SUNLNG be the planetocentric longitude (in degrees) of 
   the sun as viewed from the center of the body of interest. 
 
   Let SITLNG be the planetocentric longitude (in degrees) of 
   the site for which local time is desired. 
 
   We define local time to be 12 + (SITLNG - SUNLNG)/15 
 
   (where appropriate care is taken to map ( SITLNG - SUNLNG ) 
   into the range from -180 to 180). 
 
   Using this definition, we see that from the point of view 
   of this routine, local solar time is simply a measure of angles 
   between meridians on the surface of a body.  Consequently, 
   this routine is not appropriate for computing "local times" 
   in the sense of Pacific Standard Time.   For computing times 
   relative to standard time zones on earth, see the routines 
   timout_c and str2et_c. 

   Regarding planetographic longitude
   ----------------------------------

   In the planetographic coordinate system, longitude is defined using
   the spin sense of the body.  Longitude is positive to the west if
   the spin is prograde and positive to the east if the spin is
   retrograde.  The spin sense is given by the sign of the first degree
   term of the time-dependent polynomial for the body's prime meridian
   Euler angle "W":  the spin is retrograde if this term is negative
   and prograde otherwise.  For the sun, planets, most natural
   satellites, and selected asteroids, the polynomial expression for W
   may be found in a SPICE PCK kernel.

   The earth, moon, and sun are exceptions: planetographic longitude
   is measured positive east for these bodies.

   If you wish to override the default sense of positive planetographic
   longitude for a particular body, you can do so by defining the
   kernel variable

      BODY<body ID>_PGR_POSITIVE_LON

   where <body ID> represents the NAIF ID code of the body. This
   variable may be assigned either of the values

      'WEST'
      'EAST'

   For example, you can have this routine treat the longitude of the
   earth as increasing to the west using the kernel variable assignment

      BODY399_PGR_POSITIVE_LON = 'WEST'

   Normally such assignments are made by placing them in a text kernel
   and loading that kernel via furnsh_c.

-Examples
 
   The following code fragment illustrates how you could print the
   local time at a site on Mars with planetographic longitude 326.17
   deg E at epoch et.
 
   Convert the longitude to radians, set the type of the longitude and
   make up a mnemonic for MARS's ID-code.
 
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main ()
      {
         /.
         In a user's program, the LSK, PCK and SPK files named 
         below should be replaced with names of applicable
         files.
         ./
         #define LSK             "naif0007.tls"
         #define PCK             "pck00007.tpc"
         #define SPK             "de405s.bsp"

         #define TIMLEN          51
         #define AMPMLEN         51
         #define MARS            499
         #define TYPE            "PLANETOGRAPHIC"

         SpiceChar               ampm  [ AMPMLEN ];
         SpiceChar               time  [ TIMLEN  ];

         SpiceDouble             dlon;
         SpiceDouble             et;
         SpiceDouble             rlon;

         SpiceInt                hr;
         SpiceInt                mn;
         SpiceInt                sc;

         furnsh_c ( PCK );
         furnsh_c ( SPK );
         furnsh_c ( LSK );

         dlon  =  326.17;
         rlon  =  dlon * rpd_c();
 
         str2et_c ( "2002 sep 2 00:00:00", &et );

         et2lst_c ( et,  MARS, rlon, TYPE, TIMLEN, AMPMLEN, 
                    &hr, &mn,  &sc,  time, ampm             );

         printf ( "The local time at Mars %6.2f degrees E "
                  "planetographic longitude is: %s\n",  
                   dlon,  ampm                              );

         return ( 0 );
      }



-Restrictions
 
   This routine relies on being able to determine the name of the
   bodyfixed frame associated with body through the frames subsystem.
   If the body specified is NOT one of the nine planets or their
   satellites, you will need to load an appropriate frame definition
   kernel that contains the relationship between the body ID and the
   bodyfixed frame name.  See the FRAMES Required Reading for more
   details on specifying this relationship.
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 2.0.0, 03-NOV-2005 (NJB)
 
      Bug fix:  treatment of planetographic longitude has been
      updated to be consistent with the SPICE planetographic/
      rectangular coordinate conversion routines.  The effect of
      this change is that the default sense of positive longitude
      for the moon is now east; also, the default sense of positive
      planetographic longitude now may be overridden for any body
      (see Particulars above).

   -CSPICE Version 1.0.0, 02-SEP-2002 (NJB) (WLT)

-Index_Entries
 
   Compute the local time for a point on a body. 
 
-&
*/

{ /* Begin et2lst_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "et2lst_c" );

   /*
   Check the input type argument. 
   */
   CHKFSTR ( CHK_STANDARD, "et2lst_c", type );

   /*
   Check the output arguments. 
   */
   CHKOSTR ( CHK_STANDARD, "et2lst_c", time, timlen  );
   CHKOSTR ( CHK_STANDARD, "et2lst_c", ampm, ampmlen );


   et2lst_ ( ( doublereal * ) &et,
             ( integer    * ) &body,
             ( doublereal * ) &lon,
             ( char       * ) type,
             ( integer    * ) hr,
             ( integer    * ) mn,
             ( integer    * ) sc,
             ( char       * ) time,
             ( char       * ) ampm, 
             ( ftnlen       ) strlen(type),
             ( ftnlen       ) timlen-1,
             ( ftnlen       ) ampmlen-1 );

   /*
   Convert the output strings from Fortran to C style. 
   */
   F2C_ConvertStr ( timlen,  time );
   F2C_ConvertStr ( ampmlen, ampm );


   chkout_c ( "et2lst_c" );

} /* End et2lst_c */

