/*

-Procedure occult_c ( find occultation type at time )

-Abstract

   Determines the occultation condition (not occulted, partially,
   etc.) of one target relative to another target as seen by
   an observer at a given time.

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
 
   SPK
   TIME
   KERNEL
 
-Keywords
 
   GEOMETRY
   OCCULTATION
   ELLIPSOID

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include <stdio.h>

   void occult_c ( ConstSpiceChar   * target1,
                   ConstSpiceChar   * shape1,
                   ConstSpiceChar   * frame1,
                   ConstSpiceChar   * target2,
                   ConstSpiceChar   * shape2,
                   ConstSpiceChar   * frame2,
                   ConstSpiceChar   * abcorr,
                   ConstSpiceChar   * observer,
                   SpiceDouble        et,
                   SpiceInt         * occult_code )


/*

-Brief_I/O

   VARIABLE    I/O  DESCRIPTION
   --------    ---  -------------------------------------------
   target1      I   Name or ID of first target.
   shape1       I   Type of shape model used for first target.
   frame1       I   Body-fixed, body-centered frame for first body.
   target2      I   Name or ID of second target.
   shape2       I   Type of shape model used for second target.
   frame2       I   Body-fixed, body-centered frame for second body.
   abcorr       I   Aberration correction flag.
   observer     I   Name or ID of the observer.
   et           I   Time of the observation (seconds past J2000).
   occult_code  O   Occultation identification code.
 
-Detailed_Input
 
   target1    is the name of the first target body. Both object
              names and NAIF IDs are accepted. For example, both
              'Moon' and '301' are accepted.
   
   shape1     is a string indicating the geometric model used to
              represent the shape of the front target body. The
              supported options are:
   
                 'ELLIPSOID'     Use a triaxial ellipsoid model
                                 with radius values provided via the
                                 kernel pool. A kernel variable
                                 having a name of the form
   
                                    'BODYnnn_RADII'
   
                                 where nnn represents the NAIF
                                 integer code associated with the
                                 body, must be present in the kernel
                                 pool. This variable must be
                                 associated with three numeric
                                 values giving the lengths of the
                                 ellipsoid's X, Y, and Z semi-axes.
   
                 'POINT'         Treat the body as a single point.
                                 When a point target is specified,
                                 the occultation type must be
                                 set to 'ANY'.
   
              At least one of the target bodies `target1' or `target2' must
              be modeled as an ellipsoid.
   
              Case and leading or trailing blanks are not
              significant in the string.
   
   frame1     is the name of the body-fixed, body-centered reference
              frame associated with the first target body. Examples
              of such names are 'IAU_SATURN' (for Saturn) and
              'ITRF93' (for the Earth).
   
              If the first target body is modeled as a point, `frame1'
              should be left blank (Ex: ' ').
   
              Case and leading or trailing blanks bracketing a
              non-blank frame name are not significant in the string.
   
   target2    is the name of the second target body. See the description
              of `target1' above for more details.
   
   shape2     is the shape specification for the body designated
              by `target2'. See the description of `shape1' above for
              details.
   
   frame2     is the name of the body-fixed, body-centered reference
              frame associated with the second target body. See the
              description of `frame1' above for more details.
   
   abcorr     indicates the aberration corrections to be applied to
              the state of each target body to account for one-way
              light time. Stellar aberration corrections are
              ignored if specified, since these corrections don't
              improve the accuracy of the occultation determination.
   
              See the header of the SPICE routine spkezr_c for a
              detailed description of the aberration correction
              options. For convenience, the options supported by
              this routine are listed below:
   
                 'NONE'     Apply no correction.
   
                 'LT'       "Reception" case: correct for
                            one-way light time using a Newtonian
                            formulation.
   
                 'CN'       "Reception" case: converged
                            Newtonian light time correction.
   
                 'XLT'      "Transmission" case: correct for
                            one-way light time using a Newtonian
                            formulation.
   
                 'XCN'      "Transmission" case: converged
                            Newtonian light time correction.
   
              Case and blanks are not significant in the string
              `abcorr'.
   
   observer   is the name of the body from which the occultation
              is observed. See the description of `target1' above for 
              more details.
   
   et         is the observation time in seconds past the J2000
              epoch.
 
 
-Detailed_Output
 
   occult_code   is an integer occultation code indicating the geometric
                 relationship of the three bodies.
      
                 The meaning of the sign of `occult_code' is given below.
      
                     Code sign          Meaning
                     ---------          ------------------------------
                        > 0             The second ellipsoid is
                                        partially or fully occulted
                                        by the first.
                     
                        < 0             The first ellipsoid is
                                        partially of fully
                                        occulted by the second.
                     
                        = 0             No occultation.
      
                 Possible `occult_code' values and meanings are given below.
                 The variable names indicate the type of occultation
                 and which target is in the back. For example, 
                 SPICE_OCCULT_TOTAL1 represents a total occultation in which the
                 first target is in the back (or occulted by) the second
                 target.
      
                     Name                Code    Meaning
                     ------              -----   ------------------------------
                     SPICE_OCCULT_TOTAL1  -3     Total occultation of first
                                                 target by second.
      
                     SPICE_OCCULT_ANNLR1  -2     Annular occultation of first
                                                 target by second. The second
                                                 target does not block the limb
                                                 of the first.
      
                     SPICE_OCCULT_PARTL1  -1     Partial occultation of first
                                                 target by second target.
      
                     SPICE_OCCULT_NOOCC    0     No occultation or transit:
                                                 both objects are completely
                                                 visible to the observer.
      
                     SPICE_OCCULT_PARTL2   1     Partial occultation of second
                                                 target by first target.
      
                     SPICE_OCCULT_ANNLR2   2     Annular occultation of
                                                 second target by first.
      
                     SPICE_OCCULT_TOTAL2   3     Total occultation of second
                                                 target by first.

-Parameters
 
   None.
 
-Exceptions
 
   1)  If the target or observer body names input by the user are
       not recognized, the error will be diagnosed by a routine in
       the call tree of this routine.
   
   2)  If the input shapes are not accepted, the error will be
       diagnosed by a routine in the call tree of this routine.
   
   3)  If both input shapes are points, the error will be
       diagnosed by a routine in the call tree of this routine.
   
   4)  If the radii of a target body modeled as an ellipsoid cannot
       be determined by searching the kernel pool for a kernel
       variable having a name of the form
   
          'BODYnnn_RADII'
   
       where nnn represents the NAIF integer code associated with
       the body, the error will be diagnosed by a routine in the
       call tree of this routine.
   
   5)  If any of the target or observer bodies (`target1', `target2', or
       `observer') are the same, the error will be diagnosed
       by a routine in the call tree of this routine.
   
   6)  If the loaded kernels provide insufficient data to
       compute any required state vector, the deficiency will
       be diagnosed by a routine in the call tree of this routine.
   
   7)  If an error occurs while reading an SPK or other kernel,
       the error will be diagnosed by a routine in the call tree
       of this routine.
   
   8)  Invalid aberration correction specifications will be
       diagnosed by a routine in the call tree of this routine.
 
-Files
 
   Appropriate SPICE kernels must be loaded by the calling program
   before this routine is called.
   
   The following data are required:
   
      - SPK data: the calling application must load ephemeris data
        for the targets and observer for the specified input time.
        If aberration corrections are used, the states of the target
        bodies and of the observer relative to the solar system
        barycenter must be calculable from the available ephemeris
        data. Typically ephemeris data are made available by loading
        one or more SPK files via furnsh_c.
   
      - PCK data: bodies modeled as triaxial ellipsoids must have
        semi-axis lengths provided by variables in the kernel pool.
        Typically these data are made available by loading a text
        PCK file via furnsh_c.
   
   Kernel data are normally loaded once per program run, NOT every
   time this routine is called.
 
-Particulars
 
   For many purposes, modeling extended bodies as triaxial
   ellipsoids is adequate for determining whether one body is
   occulted by another as seen from a specified observer.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Find whether MRO is occulted by Mars as seen by
      the DSS-13 ground station at a few specific
      times.

         KPL/MK
         
         File: mro_ex_occult.tm
         
         This is the meta-kernel file for the example problem for
         the subroutine occult_c. These kernel files can be found in
         the NAIF archives.
         
         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.
         
         The names and contents of the kernels referenced
         by this meta-kernel are as follows:
         
               File name                       Contents
               ---------                       --------
               de421.bsp                       Planetary ephemeris
               earthstns_itrf93_050714.bsp     DSN station ephemeris
               pck00010.tpc                    Planet orientation and
                                               radii
               earth_000101_120409_120117.bpc  High precision Earth
                                               orientation
               mro_psp_rec.bsp                 MRO ephemeris
               naif0010.tls                    Leapseconds
               earth_topo_050714.tf            Topocentric reference
                                               frames for
                                               DSN stations
         
         \begindata
         
         KERNELS_TO_LOAD = ( 'de421.bsp',
                             'earthstns_itrf93_050714.bsp',
                             'pck00010.tpc',
                             'earth_000101_120409_120117.bpc',
                             'mro_psp_rec.bsp',
                             'naif0010.tls',
                             'earth_topo_050714.tf' )
         \begintext

      Example code begins here.
      
         #include exit
         #include "SpiceUsr.h"
         
         int main()
         {
            /.
            Constants
            
            `abcorr' is the desired light time and stellar
            aberration correction setting.
            
            METAKR is the name of the meta-kernel.
            ./ 
            #define ABCORR "CN+S"
            #define METAKR "mro_ex_occult.tm"
            #define TIMLEN  41
            
            /.
            Local variables  
            ./
            SpiceChar             * abcorr      = "CN";
            SpiceChar             * time_format = "YYYY-MON-DD HR:MN ::UTC-8";
            SpiceChar             * observer    = "DSS-13";
            SpiceChar             * out_format  = "%s %s %s %s wrt %s\n";
            SpiceChar             * shape1      = "point";
            SpiceChar             * shape2      = "ellipsoid";
            SpiceChar             * target1     = "MRO";
            SpiceChar             * target2     = "Mars";
            SpiceChar               time[TIMLEN];
            
            static SpiceChar      * output_char[4] =
                                    {
                                       "totally occulted by", 
                                       "transited by",
                                       "partially occulted by", 
                                       "not occulted by"
                                    };
            
            SpiceDouble             et;
            SpiceDouble             et_start;
            SpiceDouble             et_stop;
            
            SpiceInt                dt = 1000;
            SpiceInt                occult_code;
            
            /.
            Load kernel files via the meta-kernel.
            ./
            furnsh_c ( METAKR );
            
            /.
            Calculate the type of occultation that
            corresponds to time ET.
            ./
            str2et_c ( "2012-JAN-5 1:15:00 UTC", &et_start );
            str2et_c ( "2012-JAN-5 2:50:00 UTC", &et_stop  );
            
            et = et_start;
            while ( et < et_stop ) {
               /.
               Calculate the type of occultation that
               corresponds to time ET.
               ./
               occult_c ( target1, shape1,  " ",
                          target2, shape2,  "IAU_MARS", 
                          abcorr,  observer, et, &occult_code );
                          
               /.
               Output the results.
               ./
               timout_c ( et, time_format, TIMLEN, time );
               
               switch ( occult_code ) {
               
                  case SPICE_OCCULT_TOTAL1:
         
                     printf( out_format, time, target1, output_char[0],
                                         target2, observer );
                     break;
                  
                  case SPICE_OCCULT_ANNLR1:
                  
                     printf( out_format, time, target1, output_char[1],
                                         target2, observer );
                     break;
                  
                  case SPICE_OCCULT_PARTL1:
                  
                     printf( out_format, time, target1, output_char[2],
                                         target2, observer );
                     break;
                  
                  case SPICE_OCCULT_NOOCC:
                  
                     printf( out_format, time, target1, output_char[3],
                                         target2, observer );
                     break;
                  
                  case SPICE_OCCULT_PARTL2:
                  
                     printf( out_format, time, target2, output_char[2],
                                         target1, observer );
                     break;
                  
                  case SPICE_OCCULT_ANNLR2:
                  
                     printf( out_format, time, target2, output_char[1],
                                         target1, observer );
                     break;
                  
                  case SPICE_OCCULT_TOTAL2:
                  
                     printf( out_format, time, target2, output_char[0],
                                         target1, observer );
                     break;
                  
                  default:
                  
                     printf( "Bad occultation code: %d", occult_code );
                     break;
                     
               }
               /.
               Increment the time.
               ./
               et = et + dt;
            }
            
            return 0;
         }

      When this program was executed on a Linux/PC gcc platform, the
      output was:
   
         2012-JAN-04 17:15 Mars transited by MRO wrt DSS-13
         2012-JAN-04 17:31 MRO not occulted by Mars wrt DSS-13
         2012-JAN-04 17:48 MRO totally occulted by Mars wrt DSS-13
         2012-JAN-04 18:04 MRO totally occulted by Mars wrt DSS-13
         2012-JAN-04 18:21 MRO not occulted by Mars wrt DSS-13
         2012-JAN-04 18:38 Mars transited by MRO wrt DSS-13

-Restrictions
 
   None.
 
-Literature_References
 
   None. 
   
-Author_and_Institution

   S.C. Krening  (JPL)
   N.J. Bachman  (JPL)

-Version

 -CSPICE Version 1.0.0  1-FEB-2012 (SCK) (NJB)

-Index_Entries

   occultation type at a specified time

-&
*/

{ /* Begin occult_c */

   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "occult_c" );

   /*
   Check the input strings to make sure the pointers are non-null
   and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "occult_c", target1  );
   CHKFSTR ( CHK_STANDARD, "occult_c", shape1   );
   CHKFSTR ( CHK_STANDARD, "occult_c", frame1   );
   CHKFSTR ( CHK_STANDARD, "occult_c", target2  );
   CHKFSTR ( CHK_STANDARD, "occult_c", shape2   );
   CHKFSTR ( CHK_STANDARD, "occult_c", frame2   );
   CHKFSTR ( CHK_STANDARD, "occult_c", abcorr   );
   CHKFSTR ( CHK_STANDARD, "occult_c", observer );

   /*
   Call the f2c'd routine.
   */
   occult_ ( ( char       * ) target1,
             ( char       * ) shape1,
             ( char       * ) frame1,
             ( char       * ) target2,
             ( char       * ) shape2,
             ( char       * ) frame2,
             ( char       * ) abcorr,
             ( char       * ) observer,
             ( doublereal * ) &et,
             ( integer    * ) occult_code,
             ( ftnlen       ) strlen(target1),
             ( ftnlen       ) strlen(shape1),
             ( ftnlen       ) strlen(frame1),
             ( ftnlen       ) strlen(target2),
             ( ftnlen       ) strlen(shape2),
             ( ftnlen       ) strlen(frame2),
             ( ftnlen       ) strlen(abcorr),
             ( ftnlen       ) strlen(observer) );

   chkout_c ( "occult_c" );  

}
