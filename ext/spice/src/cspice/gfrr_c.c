/*

-Procedure gfrr_c (GF, range rate search )

-Abstract

   Determine time intervals for which a specified constraint
   on the observer-target range rate is met.

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

   GF.REQ
   NAIF_IDS.REQ
   SPK.REQ
   TIME.REQ
   WINDOWS.REQ

-Keywords

   EVENT
   GEOMETRY
   EPHEMERIS
   SEARCH
   WINDOW

*/

   #include <stdlib.h>
   #include "SpiceGF.h"
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "zzalloc.h"

   void gfrr_c ( ConstSpiceChar     * target,
                 ConstSpiceChar     * abcorr,
                 ConstSpiceChar     * obsrvr,
                 ConstSpiceChar     * relate,
                 SpiceDouble          refval,
                 SpiceDouble          adjust,
                 SpiceDouble          step,
                 SpiceInt             nintvls,
                 SpiceCell          * cnfine,
                 SpiceCell          * result  )

/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   SPICE_GF_CNVTOL   P   Convergence tolerance
   target            I   Name of the target body.
   abcorr            I   Aberration correction flag.
   obsrvr            I   Name of the observing body.
   relate            I   Relational operator.
   refval            I   Reference value.
   adjust            I   Adjustment value for absolute extrema searches.
   step              I   Step size used for locating extrema and roots.
   nintvls           I   Workspace window interval count.
   cnfine           I-O  SPICE window to which the search is confined.
   result            O   SPICE window containing results.

-Detailed_Input

   target      is the name of a target body. The target body is
               an ephemeris object; its trajectory is given by
               SPK data.

               The string `target' is case-insensitive, and leading
               and trailing blanks in `target' are not significant.
               Optionally, you may supply a string containing the
               integer ID code for the object. For example both
               "MOON" and "301" are legitimate strings that indicate
               the Moon is the target body.

               The target and observer define a position vector which
               points from the observer to the target; the time derivative
               length of this vector is the "range rate" that serves as
               the subject of the search performed by this routine.


   abcorr      indicates the aberration corrections to be applied to
               the observer-target state vector to account for
               one-way light time and stellar aberration.

               Any aberration correction accepted by the SPICE
               routine spkezr_c is accepted here. See the header
               of spkezr_c for a detailed description of the
               aberration correction options. For convenience,
               the options are listed below:

                  "NONE"     Apply no correction.

                  "LT"       "Reception" case:  correct for
                             one-way light time using a Newtonian
                             formulation.

                  "LT+S"     "Reception" case:  correct for
                             one-way light time and stellar
                             aberration using a Newtonian
                             formulation.

                  "CN"       "Reception" case:  converged
                             Newtonian light time correction.

                  "CN+S"     "Reception" case:  converged
                             Newtonian light time and stellar
                             aberration corrections.

                  "XLT"      "Transmission" case:  correct for
                             one-way light time using a Newtonian
                             formulation.

                  "XLT+S"    "Transmission" case:  correct for
                             one-way light time and stellar
                             aberration using a Newtonian
                             formulation.

                  "XCN"      "Transmission" case:  converged
                             Newtonian light time correction.

                  "XCN+S"    "Transmission" case:  converged
                             Newtonian light time and stellar
                             aberration corrections.

               Case and blanks are not significant in the string
               `abcorr'.

   obsrvr      is the name of the observing body. The observing body is
               an ephemeris object; its trajectory is given by SPK
               data. `obsrvr' is case-insensitive, and leading and
               trailing blanks in `obsrvr' are not significant.
               Optionally, you may supply a string containing the
               integer ID code for the object. For example both "MOON"
               and "301" are legitimate strings that indicate the Moon
               is the observer.

   relate      is a relational operator used to define a constraint
               on observer-target range rate. The result window found
               by this routine indicates the time intervals where
               the constraint is satisfied. Supported values of
               `relate' and corresponding meanings are shown below:

                  ">"      Distance is greater than the reference
                           value `refval'.

                  "="      Distance is equal to the reference
                           value `refval'.

                  "<"      Distance is less than the reference
                           value `refval'.


                 "ABSMAX"  Distance is at an absolute maximum.

                 "ABSMIN"  Distance is at an absolute  minimum.

                 "LOCMAX"  Distance is at a local maximum.

                 "LOCMIN"  Distance is at a local minimum.

              The caller may indicate that the region of interest
              is the set of time intervals where the quantity is
              within a specified distance of an absolute extremum.
              The argument `adjust' (described below) is used to
              specify this distance.

              Local extrema are considered to exist only in the
              interiors of the intervals comprising the confinement
              window:  a local extremum cannot exist at a boundary
              point of the confinement window.

              Case is not significant in the string `relate'.

    refval    is the reference value used together with the argument
              `relate' to define an equality or inequality to be
              satisfied by the range rate between the specified target
              and observer. See the discussion of `relate' above for
              further information.

              The units of `refval' are km/sec.

   adjust     is a parameter used to modify searches for absolute
              extrema: when `relate' is set to "ABSMAX" or "ABSMIN" and
              `adjust' is set to a positive value, gfdist_c will find
              times when the observer-target range rate is within
              `adjust' km/sec of the specified extreme value.

              If `adjust' is non-zero and a search for an absolute
              minimum `min' is performed, the result window contains
              time intervals when the observer-target range rate has
              values between `min' and min+adjust.

              If the search is for an absolute maximum `max', the
              corresponding range is from max-adjust to `max'.

              `adjust' is not used for searches for local extrema,
              equality or inequality conditions.

   step       is the step size to be used in the search. `step' must
              be short enough for a search using this step size
              to locate the time intervals where the specified
              range rate function is monotone increasing or
              decreasing. However, `step' must not be *too* short, or
              the search will take an unreasonable amount of time.

              The choice of `step' affects the completeness but not
              the precision of solutions found by this routine; the
              precision is controlled by the convergence tolerance.
              See the discussion of the parameter SPICE_GF_CNVTOL for
              details.

              `step' has units of TDB seconds.

   nintvls    is a parameter specifying the number of intervals that
              can be accommodated by each of the dynamically allocated
              windows used internally by this routine. `nintvls' should
              be at least as large as the number of intervals within
              the search region on which the specified range rate
              function is monotone increasing or decreasing. See
              the Examples section below for code examples illustrating
              the use of this parameter.

   cnfine     is a SPICE window that confines the time period over
              which the specified search is conducted. `cnfine' may
              consist of a single interval or a collection of
              intervals.

              In some cases the confinement window can be used to
              greatly reduce the time period that must be searched
              for the desired solution. See the Particulars section
              below for further discussion.

              See the Examples section below for a code example
              that shows how to create a confinement window.

-Detailed_Output

   cnfine     is the input confinement window, updated if necessary
              so the control area of its data array indicates the
              window's size and cardinality. The window data are
              unchanged.


   result     is the window of intervals, contained within the
              confinement window `cnfine', on which the specified
              constraint is satisfied.

              If `result' is non-empty on input, its contents will be
              discarded before 'gfrr_c' conducts its search.

              `result' must be declared with sufficient size to capture
              the full set of time intervals within the search region
              on which the specified constraint is satisfied.

              If the search is for local extrema, or for absolute
              extrema with `adjust' set to zero, then normally each
              interval of `result' will be a singleton: the left and
              right endpoints of each interval will be identical.

              If no times within the confinement window satisfy the
              constraint, `result' will be returned with a cardinality
              of zero.

-Parameters

   SPICE_GF_CNVTOL

              is the convergence tolerance used for finding endpoints
              of the intervals comprising the result window.
              SPICE_GF_CNVTOL is used to determine when binary searches
              for roots should terminate: when a root is bracketed
              within an interval of length SPICE_GF_CNVTOL, the root is
              considered to have been found.

              The accuracy, as opposed to precision, of roots found
              by this routine depends on the accuracy of the input
              data. In most cases, the accuracy of solutions will be
              inferior to their precision.

              SPICE_GF_CNVTOL is declared in the header file SpiceGF.h.

-Exceptions

   1)  In order for this routine to produce correct results,
       the step size must be appropriate for the problem at hand.
       Step sizes that are too large may cause this routine to miss
       roots; step sizes that are too small may cause this routine
       to run unacceptably slowly and in some cases, find spurious
       roots.

       This routine does not diagnose invalid step sizes, except
       that if the step size is non-positive, an error is signaled
       by a routine in the call tree of this routine.

   2)  Due to numerical errors, in particular,

          - Truncation error in time values
          - Finite tolerance value
          - Errors in computed geometric quantities

       it is *normal* for the condition of interest to not always be
       satisfied near the endpoints of the intervals comprising the
       result window.

       The result window may need to be contracted slightly by the
       caller to achieve desired results. The SPICE window routine
       wncond_c can be used to contract the result window.

   3)  If an error (typically cell overflow) occurs while performing
       window arithmetic, the error will be diagnosed by a routine
       in the call tree of this routine.

   4)  If the relational operator `relate' is not recognized, an
       error is signaled by a routine in the call tree of this
       routine.

   5)  If the aberration correction specifier contains an
       unrecognized value, an error is signaled by a routine in the
       call tree of this routine.

   6)  If 'adjust' is negative, the error SPICE(VALUEOUTOFRANGE) will
       signal from a routine in the call tree of this routine.

       A non-zero value for 'adjust' when 'relate' has any value other than
       "ABSMIN" or "ABSMAX" causes the error SPICE(INVALIDVALUE) to
       signal from a routine in the call tree of this routine.

   7)  If either of the input body names do not map to NAIF ID
       codes, an error is signaled by a routine in the call tree of
       this routine.

   8)  If required ephemerides or other kernel data are not
       available, an error is signaled by a routine in the call tree
       of this routine.

   9)  If the workspace interval count is less than 1, the error
       SPICE(VALUEOUTOFRANGE) will be signaled.

   10) If the required amount of workspace memory cannot be
       allocated, the error SPICE(MALLOCFAILURE) will be
       signaled.

   11) If any input string argument pointer is null, the error
       SPICE(NULLPOINTER) will be signaled.

   12) If any input string argument is empty, the error
       SPICE(EMPTYSTRING) will be signaled.

   13) If either input cell has type other than SpiceDouble,
       the error SPICE(TYPEMISMATCH) is signaled.

-Files

   Appropriate kernels must be loaded by the calling program before
   this routine is called.

   The following data are required:

      - SPK data: ephemeris data for target and observer for the
        time period defined by the confinement window must be
        loaded. If aberration corrections are used, the states of
        target and observer relative to the solar system barycenter
        must be calculable from the available ephemeris data.
        Typically ephemeris data are made available by loading one
        or more SPK files via furnsh_c.

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   This routine determines if the caller-specified constraint condition
   on the geometric event (range rate) is satisfied for any time intervals
   within the confinement window 'cnfine'. If one or more such time
   intervals exist, those intervals are added to the 'result' window.

   This routine provides a simpler, but less flexible interface
   than does the routine gfevnt_c for conducting the searches for
   observer-target range rate value events. Applications that require
   support for progress reporting, interrupt handling, non-default step
   or refinement functions, or non-default convergence tolerance should
   call gfevnt_c rather than this routine.

   Below we discuss in greater detail aspects of this routine's
   solution process that are relevant to correct and efficient
   use of this routine in user applications.


   The Search Process
   ==================

   Regardless of the type of constraint selected by the caller, this
   routine starts the search for solutions by determining the time
   periods, within the confinement window, over which the specified
   range rate function is monotone increasing and monotone decreasing.
   Each of these time periods is represented by a SPICE window. Having
   found these windows, all of the range rate function's local extrema
   within the confinement window are known. Absolute extrema then can
   be found very easily.

   Within any interval of these "monotone" windows, there will be at
   most one solution of any equality constraint. Since the boundary
   of the solution set for any inequality constraint is contained in
   the union of

      - the set of points where an equality constraint is met
      - the boundary points of the confinement window

   the solutions of both equality and inequality constraints can be
   found easily once the monotone windows have been found.


   Step Size
   =========

   The monotone windows (described above) are found via a two-step
   search process. Each interval of the confinement window is
   searched as follows: first, the input step size is used to
   determine the time separation at which the sign of the rate of
   change of range rate  will be sampled. Starting at
   the left endpoint of an interval, samples will be taken at each
   step. If a change of sign is found, a root has been bracketed; at
   that point, the time at which the range rate is zero can be
   found by a refinement process, for example, via binary search.

   Note that the optimal choice of step size depends on the lengths
   of the intervals over which the range rate function is monotone:
   the step size should be shorter than the shortest of these
   intervals (within the confinement window).

   The optimal step size is *not* necessarily related to the lengths
   of the intervals comprising the result window. For example, if
   the shortest monotone interval has length 10 days, and if the
   shortest result window interval has length 5 minutes, a step size
   of 9.9 days is still adequate to find all of the intervals in the
   result window. In situations like this, the technique of using
   monotone windows yields a dramatic efficiency improvement over a
   state-based search that simply tests at each step whether the
   specified constraint is satisfied. The latter type of search can
   miss solution intervals if the step size is longer than the
   shortest solution interval.

   Having some knowledge of the relative geometry of the target and
   observer can be a valuable aid in picking a reasonable step size.
   In general, the user can compensate for lack of such knowledge by
   picking a very short step size; the cost is increased computation
   time.

   Note that the step size is not related to the precision with which
   the endpoints of the intervals of the result window are computed.
   That precision level is controlled by the convergence tolerance.


   Convergence Tolerance
   =====================

   As described above, the root-finding process used by this routine
   involves first bracketing roots and then using a search process to
   locate them.  "Roots" include times when extrema are attained and
   times when the geometric quantity function is equal to a reference
   value or adjusted extremum. All endpoints of the intervals comprising
   the result window are either endpoints of intervals of the confinement
   window or roots.

   Once a root has been bracketed, a refinement process is used to
   narrow down the time interval within which the root must lie.
   This refinement process terminates when the location of the root
   has been determined to within an error margin called the
   "convergence tolerance." The convergence tolerance used by this
   routine is set via the parameter SPICE_GF_CNVTOL.

   The value of SPICE_GF_CNVTOL is set to a "tight" value so that the
   tolerance doesn't limit the accuracy of solutions found by this
   routine. In general the accuracy of input data will be the limiting
   factor.

   The user may change the convergence tolerance from the default
   SPICE_GF_CNVTOL value by calling the routine gfstol_c, e.g.

      gfstol_c( tolerance value in seconds )

   Call gfstol_c prior to calling this routine. All subsequent
   searches will use the updated tolerance value.

   Searches over time windows of long duration may require use of
   larger tolerance values than the default: the tolerance must be
   large enough so that it, when added to or subtracted from the
   confinement window's lower and upper bounds, yields distinct time
   values.

   Setting the tolerance tighter than SPICE_GF_CNVTOL is unlikely to be
   useful, since the results are unlikely to be more accurate.
   Making the tolerance looser will speed up searches somewhat,
   since a few convergence steps will be omitted. However, in most
   cases, the step size is likely to have a much greater effect
   on processing time than would the convergence tolerance.


   The Confinement Window
   ======================

   The simplest use of the confinement window is to specify a time
   interval within which a solution is sought. However, the
   confinement window can, in some cases, be used to make searches
   more efficient. Sometimes it's possible to do an efficient search
   to reduce the size of the time period over which a relatively
   slow search of interest must be performed.

   Consider the following example: suppose one wishes to find the
   times when the range rate between Io and the Earth attains a global
   minimum over some (lengthy) time interval. There is one local
   minimum every few days. The required step size for this search
   must be smaller than the shortest interval on which the range rate
   is monotone increasing or decreasing; this step size will be less
   than half the average time between local minima. However, we know
   that a global minimum can't occur when the Jupiter-Sun-Earth
   angle is greater than 90 degrees. We can use a step size of a
   half year to find the time period, within our original time
   interval, during which this angle is less than 90 degrees; this
   time period becomes the confinement window for our Earth-Io
   range rate search. This way we've used a quick (due to the large
   step size) search to cut out about half of the search period over
   which we must perform a slower search using a small step size.

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

      Determine the time windows from January 1, 2007 UTC to
      April 1, 2007 UTC for which the sun-moon range rate satisfies the
      relation conditions with respect to a reference value of
      0.3365 km/s radians (this range rate known to occur within the
      search interval). Also determine the time windows corresponding
      to the local maximum and minimum range rate, and the absolute
      maximum and minimum range rate during the search interval.

      #include <stdio.h>
      #include <stdlib.h>
      #include <string.h>

      #include "SpiceUsr.h"

      #define       MAXWIN    20000
      #define       TIMFMT    "YYYY-MON-DD HR:MN:SC.###"
      #define       TIMLEN    41
      #define       NLOOPS    7

      int main( int argc, char **argv )
         {

         /.
         Create the needed windows. Note, one window
         consists of two values, so the total number
         of cell values to allocate is twice
         the number of intervals.
         ./
         SPICEDOUBLE_CELL ( result, 2*MAXWIN );
         SPICEDOUBLE_CELL ( cnfine, 2        );

         SpiceDouble       begtim;
         SpiceDouble       endtim;
         SpiceDouble       step;
         SpiceDouble       adjust;
         SpiceDouble       refval;
         SpiceDouble       beg;
         SpiceDouble       end;

         SpiceChar         begstr [ TIMLEN ];
         SpiceChar         endstr [ TIMLEN ];

         SpiceChar       * target = "MOON";
         SpiceChar       * abcorr = "NONE";
         SpiceChar       * obsrvr = "SUN";

         SpiceInt          count;
         SpiceInt          i;
         SpiceInt          j;

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
         furnsh_c( "standard.tm" );

         /.
         Store the time bounds of our search interval in
         the cnfine confinement window.
         ./
         str2et_c( "2007 JAN 01", &begtim );
         str2et_c( "2007 APR 01", &endtim );

         wninsd_c ( begtim, endtim, &cnfine );

         /.
         Search using a step size of 1 day (in units of seconds).
         The reference value is .3365 km/s. We're not using the
         adjustment feature, so we set 'adjust' to zero.
         ./
         step   = spd_c();
         adjust = 0.;
         refval = .3365;

         for ( j = 0;  j < NLOOPS;  j++ )
            {

            printf ( "Relation condition: %s \n",  relate[j] );

            /.
            Perform the search. The SPICE window 'result' contains
            the set of times when the condition is met.
            ./
            gfrr_c ( target,
                     abcorr,
                     obsrvr,
                     relate[j],
                     refval,
                     adjust,
                     step,
                     MAXWIN,
                     &cnfine,
                     &result );

            count = wncard_c( &result );

            /.
            Display the results.
            ./
            if (count == 0 )
               {
               printf ( "Result window is empty.\n\n" );
               }
            else
               {
               for ( i = 0;  i < count;  i++ )
                  {

                  /.
                  Fetch the endpoints of the Ith interval
                  of the result window.
                  ./
                  wnfetd_c ( &result, i, &beg, &end );

                  timout_c ( beg, TIMFMT, TIMLEN, begstr );
                  timout_c ( end, TIMFMT, TIMLEN, endstr );

                  printf ( "Start time, drdt = %s \n", begstr );
                  printf ( "Stop time,  drdt = %s \n", endstr );

                  }

               }

            printf("\n");

            }

         return( 0 );
         }


   The program outputs:

      Relation condition: =
      Start time, drdt = 2007-JAN-02 00:35:19.574
      Stop time,  drdt = 2007-JAN-02 00:35:19.574
      Start time, drdt = 2007-JAN-19 22:04:54.899
      Stop time,  drdt = 2007-JAN-19 22:04:54.899
      Start time, drdt = 2007-FEB-01 23:30:13.428
      Stop time,  drdt = 2007-FEB-01 23:30:13.428
      Start time, drdt = 2007-FEB-17 11:10:46.540
      Stop time,  drdt = 2007-FEB-17 11:10:46.540
      Start time, drdt = 2007-MAR-04 15:50:19.929
      Stop time,  drdt = 2007-MAR-04 15:50:19.929
      Start time, drdt = 2007-MAR-18 09:59:05.959
      Stop time,  drdt = 2007-MAR-18 09:59:05.959

      Relation condition: <
      Start time, drdt = 2007-JAN-02 00:35:19.574
      Stop time,  drdt = 2007-JAN-19 22:04:54.899
      Start time, drdt = 2007-FEB-01 23:30:13.428
      Stop time,  drdt = 2007-FEB-17 11:10:46.540
      Start time, drdt = 2007-MAR-04 15:50:19.929
      Stop time,  drdt = 2007-MAR-18 09:59:05.959

      Relation condition: >
      Start time, drdt = 2007-JAN-01 00:00:00.000
      Stop time,  drdt = 2007-JAN-02 00:35:19.574
      Start time, drdt = 2007-JAN-19 22:04:54.899
      Stop time,  drdt = 2007-FEB-01 23:30:13.428
      Start time, drdt = 2007-FEB-17 11:10:46.540
      Stop time,  drdt = 2007-MAR-04 15:50:19.929
      Start time, drdt = 2007-MAR-18 09:59:05.959
      Stop time,  drdt = 2007-APR-01 00:00:00.000

      Relation condition: LOCMIN
      Start time, drdt = 2007-JAN-11 07:03:58.988
      Stop time,  drdt = 2007-JAN-11 07:03:58.988
      Start time, drdt = 2007-FEB-10 06:26:15.439
      Stop time,  drdt = 2007-FEB-10 06:26:15.439
      Start time, drdt = 2007-MAR-12 03:28:36.404
      Stop time,  drdt = 2007-MAR-12 03:28:36.404

      Relation condition: ABSMIN
      Start time, drdt = 2007-JAN-11 07:03:58.988
      Stop time,  drdt = 2007-JAN-11 07:03:58.988

      Relation condition: LOCMAX
      Start time, drdt = 2007-JAN-26 02:27:33.766
      Stop time,  drdt = 2007-JAN-26 02:27:33.766
      Start time, drdt = 2007-FEB-24 09:35:07.816
      Stop time,  drdt = 2007-FEB-24 09:35:07.816
      Start time, drdt = 2007-MAR-25 17:26:56.150
      Stop time,  drdt = 2007-MAR-25 17:26:56.150

      Relation condition: ABSMAX
      Start time, drdt = 2007-MAR-25 17:26:56.150
      Stop time,  drdt = 2007-MAR-25 17:26:56.150

-Restrictions

   1) The kernel files to be used by this routine must be loaded
      (normally using the CSPICE routine furnsh_c) before this
      routine is called.

   2) This routine has the side effect of re-initializing the
      range rate quantity utility package. Callers may themselves
      need to re-initialize the range rate quantity utility
      package after calling this routine.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman   (JPL)
   E.D. Wright    (JPL)

-Version

   -CSPICE Version 1.0.1, 28-FEB-2013 (NJB) (EDW)

      Header was updated to discuss use of gfstol_c.

      Edit to comments to correct search description.

      Edits to Example section, proper description of "standard.tm"
      meta kernel.

   -CSPICE Version 1.0.0, 26-AUG-2009 (EDW) (NJB)

-Index_Entries

 GF range rate search

-&
*/

{ /* Begin gfrr_c */

   /*
   Local variables
   */
   doublereal            * work;

   static SpiceInt         nw = SPICE_GF_NWRR;
   SpiceInt                nBytes;

   /*
   Participate in error tracing.
   */

   chkin_c ( "gfrr_c" );

   /*
   Make sure cell data types are d.p.
   */
   CELLTYPECHK2 ( CHK_STANDARD, "gfrr_c", SPICE_DP, cnfine, result );

   /*
   Initialize the input cells if necessary.
   */
   CELLINIT2 ( cnfine, result );

   /*
   Check the input strings to make sure each pointer is non-null
   and each string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gfrr_c", target );
   CHKFSTR ( CHK_STANDARD, "gfrr_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "gfrr_c", obsrvr );
   CHKFSTR ( CHK_STANDARD, "gfrr_c", relate );

   /*
   Check the workspace size; some mallocs have a violent
   dislike for negative allocation amounts. To be safe,
   rule out a count of zero intervals as well.
   */

   if ( nintvls < 1 )
      {
      setmsg_c ( "The specified workspace interval count # was "
                 "less than the minimum allowed value of one (1)." );
      errint_c ( "#",  nintvls                              );
      sigerr_c ( "SPICE(VALUEOUTOFRANGE)"                   );
      chkout_c ( "gfrr_c"                                   );
      return;
      }

   /*
   Allocate the workspace. 'nintvls' indicates the maximum number of
   intervals returned in 'result'. An interval consists of
   two values.
   */

   nintvls = 2 * nintvls;

   nBytes  = ( nintvls + SPICE_CELL_CTRLSZ ) * nw * sizeof(SpiceDouble);

   work    = (doublereal *) alloc_SpiceMemory( nBytes );

   if ( !work )
      {
      setmsg_c ( "Workspace allocation of # bytes failed due to "
                 "malloc failure"                               );
      errint_c ( "#",  nBytes                                   );
      sigerr_c ( "SPICE(MALLOCFAILED)"                          );
      chkout_c ( "gfrr_c"                                       );
      return;
      }

   /*
   Let the f2'd routine do the work.
   */

   gfrr_( ( char          * ) target,
          ( char          * ) abcorr,
          ( char          * ) obsrvr,
          ( char          * ) relate,
          ( doublereal    * ) &refval,
          ( doublereal    * ) &adjust,
          ( doublereal    * ) &step,
          ( doublereal    * ) (cnfine->base),
          ( integer       * ) &nintvls,
          ( integer       * ) &nw,
          ( doublereal    * ) work,
          ( doublereal    * ) (result->base),
          ( ftnlen          ) strlen(target),
          ( ftnlen          ) strlen(abcorr),
          ( ftnlen          ) strlen(obsrvr),
          ( ftnlen          ) strlen(relate) );

   /*
   De-allocate the workspace.
   */
   free_SpiceMemory( work );

   /*
   Sync the output cell.
   */
   if ( !failed_c() )
      {
      zzsynccl_c ( F2C, result ) ;
      }

   ALLOC_CHECK;

   chkout_c ( "gfrr_c" );

} /* End gfrr_c */
