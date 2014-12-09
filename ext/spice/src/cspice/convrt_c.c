/*

-Procedure convrt_c ( Convert Units )

-Abstract
 
    Take a measurement X, the units associated with 
    X, and units to which X should be converted; return Y --- 
    the value of the measurement in the output units. 
 
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
 
    CONVERSION, UNITS 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   

   void convrt_c ( SpiceDouble       x,
                   ConstSpiceChar  * in,
                   ConstSpiceChar  * out,
                   SpiceDouble     * y    ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  ------------------------------------------------- 
   x          I   Number representing a measurement in some units. 
   in         I   The units in which x is measured. 
   out        I   Desired units for the measurement. 
   y          O   The measurment in the desired units. 
 
-Detailed_Input
 
   x          is a number representing a measurement in the units 
              specified by in. 

   in         represents the units associated with a measurement x. 
              Acceptable units are: 

              Angles:                 "RADIANS" 
                                      "DEGREES" 
                                      "ARCMINUTES" 
                                      "ARCSECONDS" 
                                      "HOURANGLE" 
                                      "MINUTEANGLE" 
                                      "SECONDANGLE" 

              Metric Distances:       "METERS" 
                                      "KM" 
                                      "CM" 
                                      "MM" 

              English Distances:      "FEET" 
                                      "INCHES" 
                                      "YARDS" 
                                      "STATUTE_MILES" 
                                      "NAUTICAL_MILES" 

              Astrometric Distances:  "AU" 
                                      "PARSECS" 
                                      "LIGHTSECS" 
                                      "LIGHTYEARS" julian lightyears 

              Time:                   "SECONDS" 
                                      "MINUTES" 
                                      "HOURS" 
                                      "DAYS" 
                                      "JULIAN_YEARS" 
                                      "TROPICAL_YEARS" 
                                      "YEARS" (same as julian years) 


              The case of the string in is not significant.


   out        represents the units desired for the measurement x. 
              See the description of in. 
 
              The case of the string out is not significant.
              
              
-Detailed_Output
 
   y          is the input measurement converted to the desired units. 
 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input units, output units, or both input and 
      output units are not recognized, the error 
      SPICE(UNITSNOTREC) is signaled. 

   2) If the units being converted between are incompatible, the 
      error SPICE(INCOMPATIBLEUNITS) is signaled. 
 
-Files
 
   None. 
 
-Particulars
 
   This routine converts a measurement x given in units specified by 
   in to the equivalent value y in units specified by out. 

   If a unit is not recognized, an error message is produced that 
   indicates which one was not recognized. 

   If input and output units are incompatible (for example angle 
   and distance units) and error message will be produced stating 
   the requested units and associated types. 
 
-Examples
 
   To convert 1 meter to statute miles and feet you could make the
   calls
   

      convrt_c ( 1.0,   "meters",        "statute_miles", &miles ); 
      convrt_c ( miles, "statute_miles", "feet",          &feet  ); 

   or 

      convrt_c ( 1.0,   "METERS",        "STATUTE_MILES", &miles ); 
      convrt_c ( 1.0,   "METERS",        "FEET",          &feet  ); 
 
 
-Restrictions
 
   You should make sure that your units are appropriate for the 
   measurement. This routine does not do any checking for over- 
   flow. Something like 

         convrt_c ( 10.0e302, "LIGHTYEARS", "MM", &y ); 

   will cause a floating point overflow. 

   Some of the units are not "defined" quantities.  In such a case 
   a best estimate is provided as of the date of the current version 
   of this routine.  Those estimated quantities are: 

      1 AU    --- the astronomical unit. The value was taken from 
                  the JPL ephemeris DE125. This value is an
                  approximation and should not be used for
                  high-accuracy work. It agrees with the value used in
                  the JPL planetary ephemeris DE430 (149597870.700 km)
                  at the 100m level.

      The tropical year is the time from equinox to equinox.  This 
      varies slightly with time. 

      1 PARSEC --- is dependent upon the value of the astronomical 
                   unit. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   C.A. Curzon     (JPL) 
   H.A. Neilan     (JPL) 
   W.M. Owen       (JPL) 
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
   
 
-Version
 
   -CSPICE Version 1.0.0, 17-MAY-1999 (NJB)(CAC)(HAN)(WMO)(WLT)(IMU)

-Index_Entries
 
   convert units 
 
-&
*/

{ /* Begin convrt_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "convrt_c" );


   /*
   Check the column name to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "convrt_c", in  );
   CHKFSTR ( CHK_STANDARD, "convrt_c", out );
   

   /*
   Call the f2c'd Fortran routine.
   */
   
   convrt_ (  ( doublereal * )  &x,
              ( char       * )  in,
              ( char       * )  out,
              ( doublereal * )  y,
              ( ftnlen       )  strlen(in),
              ( ftnlen       )  strlen(out)  );


   chkout_c ( "convrt_c" );

} /* End convrt_c */

