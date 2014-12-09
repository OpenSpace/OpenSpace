/*

-Procedure phaseq_c ( Phase angle quantity between bodies centers )

-Abstract

   Compute the apparent phase angle for a target, observer,
   illuminator set of ephemeris objects.

-Disclaimer

   THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
   CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
   GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
   ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
   PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
   TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
   WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
   PARTICULAR USE OR PURPOSE (AS set_c FORTH IN UNITED STATES UCC
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

   PHASE ANGLE
   EPHEMERIS
   GEOMETRY
   SEARCH

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef phaseq_c

   SpiceDouble phaseq_c ( SpiceDouble       et,
                          ConstSpiceChar  * target,
                          ConstSpiceChar  * illmn,
                          ConstSpiceChar  * obsrvr,
                          ConstSpiceChar  * abcorr )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   et         I   Ephemeris seconds past J2000 TDB.
   target     I   Target body name.
   illmn      I   Illuminating body name.
   obsrvr     I   Observer body.
   abcorr     I   Aberration correction flag.
   retval     O   Value of phase angle.

-Detailed_Input

   et         the time in ephemeris seconds past J2000 TDB at which
              to compute the phase angle.

   target     the name of the target body. Optionally, you may
              supply a string containing the integer ID code
              for the object. For example both "MOON" and "301"
              are legitimate strings that indicate the Moon is the
              target body.

              Case and leading or trailing blanks are not significant
              in the string `target'.

   illmn      the name of the illuminating body. Optionally, you may
              supply a string containing the integer ID code
              for the object. For example both "SUN" and "10"
              are legitimate strings that indicate the sun is the
              illuminating body.

              Case and leading or trailing blanks are not significant
              in the string `illmn'.

              In most cases, `illmn' is the sun.

   obsrvr     the name of the observer body. Optionally, you may
              supply a string containing the integer ID code
              for the object. For example both "MOON" and "301"
              are legitimate strings that indicate the Moon is the
              observer body.

              Case and leading or trailing blanks are not significant
              in the string `obsrvr'.

   abcorr     the string description of the aberration corrections to
              apply to the state evaluations to account for one-way
              light time and stellar aberration.

              This routine accepts only reception mode aberration
              corrections. See the header of spkezr_c for a detailed
              description of the aberration correction options.
              For convenience, the appropriate aberration options are
              listed below:

                 "NONE"     Apply no correction. Returns the "true"
                            geometric state.

                 "LT"       "Reception" case:  correct for
                            one-way light time using a Newtonian
                            formulation.

                 'LT+S'     "Reception" case:  correct for
                            one-way light time and stellar
                            aberration using a Newtonian
                            formulation.

                 "CN"       "Reception" case:  converged
                            Newtonian light time correction.

                 "CN+S"     "Reception" case:  converged
                            Newtonian light time and stellar
                            aberration corrections.

              Case and leading or trailing blanks are not significant
              in the string `abcorr'.

-Detailed_Output

   phaseq     is the optionally light-time corrected phase angle
              between `target' and `illmn' as observed from `obsrvr'.
              The range of `phaseq' is [0, pi] radians.

-Parameters

   None.

-Exceptions

   1) The error SPICE(IDCODENOTFOUND) signals if the body name to
      SPICE ID look-up fails for any of the `target', `illmn', or
      `obsrvr' names.

   2) The error SPICE(INVALIDOPTION) signals if the aberration
      correct, `abcorr', indicates a transmission based correction.

   3) The error SPICE(BODIESNOTDISTINCT) signals if the `target',
      `illmn', and `obsrvr' are not unique.

-Files

   None.

-Particulars

   This routine returns the phase angle using the location of the
   bodies (if point objects) or the centers of the bodies (if finite
   bodies).


                       ILLMN      OBS
       ILLMN as seen      ^       /
       from TARG at       |      /
       ET - LT.           |     /
                         >|..../< phase angle
                          |   /
                        . |  /
                      .   | /
                     .    |v     TARG as seen from OBS
               SEP   .   TARG    at ET
                      .  /
                        /
                       v

        PI = SEP + PHASE

        so

        PHASE = PI - SEP

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

      Use the meta-kernel shown below to load the required SPICE
      kernels.

         KPL/MK

         File name: standard.tm

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
            de421.bsp                     Planetary ephemeris
            pck00009.tpc                  Planet orientation and
                                          radii
            naif0009.tls                  Leapseconds

         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'pck00009.tpc',
                                'naif0009.tls'  )

         \begintext

   Example:

      Determine the time intervals from December 1, 2006 UTC to
      January 31, 2007 UTC for which the sun-moon-earth configuration
      phase angle satisfies the relation conditions with respect to a
      reference value of .57598845 radians (the phase angle at
      January 1, 2007 00:00:00.000 UTC, 33.001707 degrees). Also
      determine the time intervals corresponding to the local maximum and
      minimum phase angles, and the absolute maximum and minimum phase
      angles during the search interval. The configuration defines the
      sun as the illuminator, the moon as the target, and the earth as
      the observer.

      #include <stdio.h>
      #include "SpiceUsr.h"

      #define  TIMFMT  "YYYY MON DD HR:MN:SC.###"
      #define  NINTVL  5000
      #define  TIMLEN  41
      #define  NLOOPS  7

      int main()
         {

         /.
         Local variables
         ./
         SpiceChar               begstr [ TIMLEN ];
         SpiceChar               endstr [ TIMLEN ];

         SPICEDOUBLE_CELL      ( cnfine, 2 );
         SPICEDOUBLE_CELL      ( result, NINTVL*2 );

         SpiceDouble             adjust;
         SpiceDouble             et0;
         SpiceDouble             et1;
         SpiceDouble             phaseq;
         SpiceDouble             refval;
         SpiceDouble             start;
         SpiceDouble             step;
         SpiceDouble             stop;
         SpiceInt                i;
         SpiceInt                j;

         /.
         Define the values for target, observer, illuminator, and
         aberration correction.
         ./

         ConstSpiceChar * target = "moon";
         ConstSpiceChar * illmn = "sun";
         ConstSpiceChar * abcorr = "lt+s";
         ConstSpiceChar * obsrvr = "earth";

         ConstSpiceChar * relate [NLOOPS] = { "=",
                                              "<",
                                              ">",
                                              "LOCMIN",
                                              "ABSMIN",
                                              "LOCMAX",
                                              "ABSMAX",
                                            };

         /.
         Load kernels.
         ./
         furnsh_c ( "standard.tm" );

         /.
         Store the time bounds of our search interval in
         the confinement window.
         ./
         str2et_c ( "2006 DEC 01", &et0 );
         str2et_c ( "2007 JAN 31", &et1 );

         wninsd_c ( et0, et1, &cnfine );

         /.
         Search using a step size of 1 day (in units of seconds).
         The reference value is 0.57598845 radians. We're not using the
         adjustment feature, so we set ADJUST to zero.
         ./
         step   = spd_c();
         refval = 0.57598845;
         adjust = 0.0;

         for ( j = 0;  j < NLOOPS;  j++ )
            {

            printf ( "Relation condition: %s\n",  relate[j] );

            /.
            Perform the search. The SPICE window `result' contains
            the set of times when the condition is met.
            ./
            gfpa_c ( target,    illmn,  abcorr, obsrvr,
                     relate[j], refval,  adjust, step,
                     NINTVL,    &cnfine, &result        );

            /.
            Display the results.
            ./
            if ( wncard_c(&result) == 0 )
               {
               printf ( "Result window is empty.\n\n" );
               }
            else
               {

               for ( i = 0;  i < wncard_c(&result);  i++ )
                  {

                  /.
                  Fetch the endpoints of the Ith interval
                  of the result window.
                  ./
                  wnfetd_c ( &result, i, &start, &stop );

                  phaseq = phaseq_c ( start, target, illmn, obsrvr, abcorr );

                  timout_c ( start, TIMFMT, TIMLEN, begstr );
                  printf ( "Start time = %s %16.9f\n", begstr, phaseq );

                  phaseq = phaseq_c ( stop, target, illmn, obsrvr, abcorr );

                  timout_c ( stop, TIMFMT, TIMLEN, endstr );
                  printf ( "Stop time  = %s %16.9f\n", endstr, phaseq );
                  }

               printf("\n");

               }

            }

         return ( 0 );
         }

   The program outputs:

      Relation condition: =
      Start time = 2006 DEC 02 13:31:34.414      0.575988450
      Stop time  = 2006 DEC 02 13:31:34.414      0.575988450
      Start time = 2006 DEC 07 14:07:55.470      0.575988450
      Stop time  = 2006 DEC 07 14:07:55.470      0.575988450
      Start time = 2006 DEC 31 23:59:59.997      0.575988450
      Stop time  = 2006 DEC 31 23:59:59.997      0.575988450
      Start time = 2007 JAN 06 08:16:25.512      0.575988450
      Stop time  = 2007 JAN 06 08:16:25.512      0.575988450
      Start time = 2007 JAN 30 11:41:32.557      0.575988450
      Stop time  = 2007 JAN 30 11:41:32.557      0.575988450

      Relation condition: <
      Start time = 2006 DEC 02 13:31:34.414      0.575988450
      Stop time  = 2006 DEC 07 14:07:55.470      0.575988450
      Start time = 2006 DEC 31 23:59:59.997      0.575988450
      Stop time  = 2007 JAN 06 08:16:25.512      0.575988450
      Start time = 2007 JAN 30 11:41:32.557      0.575988450
      Stop time  = 2007 JAN 31 00:00:00.000      0.468279091

      Relation condition: >
      Start time = 2006 DEC 01 00:00:00.000      0.940714974
      Stop time  = 2006 DEC 02 13:31:34.414      0.575988450
      Start time = 2006 DEC 07 14:07:55.470      0.575988450
      Stop time  = 2006 DEC 31 23:59:59.997      0.575988450
      Start time = 2007 JAN 06 08:16:25.512      0.575988450
      Stop time  = 2007 JAN 30 11:41:32.557      0.575988450

      Relation condition: LOCMIN
      Start time = 2006 DEC 05 00:16:50.317      0.086121423
      Stop time  = 2006 DEC 05 00:16:50.317      0.086121423
      Start time = 2007 JAN 03 14:18:31.977      0.079899769
      Stop time  = 2007 JAN 03 14:18:31.977      0.079899769

      Relation condition: ABSMIN
      Start time = 2007 JAN 03 14:18:31.977      0.079899769
      Stop time  = 2007 JAN 03 14:18:31.977      0.079899769

      Relation condition: LOCMAX
      Start time = 2006 DEC 20 14:09:10.392      3.055062862
      Stop time  = 2006 DEC 20 14:09:10.392      3.055062862
      Start time = 2007 JAN 19 04:27:54.600      3.074603891
      Stop time  = 2007 JAN 19 04:27:54.600      3.074603891

      Relation condition: ABSMAX
      Start time = 2007 JAN 19 04:27:54.600      3.074603891
      Stop time  = 2007 JAN 19 04:27:54.600      3.074603891

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   E.D. Wright    (JPL)

-Version

   -CSPICE Version 1.0.0, 08-MAR-2012 (EDW)

-Index_Entries

   compute the phase of two objects wrt an illumination source

-&
*/

{ /* Begin phaseq_c */

   /*
   Local variables
   */
   SpiceDouble       retval;

   /*
   Give the function an initial value:
   */
   retval = 0.0;

   /*
   Participate in error tracing.
   */
   if ( return_c()  )
      {
      return ( retval );
      }

   chkin_c ( "phaseq_c" );

   /*
   Check the input strings to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR_VAL ( CHK_STANDARD, "phaseq_c", target, retval );
   CHKFSTR_VAL ( CHK_STANDARD, "phaseq_c", illmn, retval );
   CHKFSTR_VAL ( CHK_STANDARD, "phaseq_c", obsrvr, retval );
   CHKFSTR_VAL ( CHK_STANDARD, "phaseq_c", abcorr, retval );


   /*
   Call the f2c'd routine for the grunt work.
   */
   retval = phaseq_ ( (doublereal *) &et,
                      (char       *) target,
                      (char       *) illmn,
                      (char       *) obsrvr,
                      (char       *) abcorr,
                      (ftnlen      ) strlen(target),
                      (ftnlen      ) strlen(illmn),
                      (ftnlen      ) strlen(obsrvr),
                      (ftnlen      ) strlen(abcorr));

   chkout_c ( "phaseq_c" );

   return(retval);

} /* End phaseq_c */
