/*

-Procedure gfposc_c (GF, observer-target vector coordinate search)

-Abstract

     Determine time intervals for which a coordinate of an
     observer-target position vector satisfies a numerical constraint.

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

   SEPARATION
   GEOMETRY
   SEARCH
   EVENT

*/

   #include <stdlib.h>
   #include "SpiceUsr.h"
   #include "SpiceGF.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "zzalloc.h"

   void gfposc_c ( ConstSpiceChar     * target,
                   ConstSpiceChar     * frame,
                   ConstSpiceChar     * abcorr,
                   ConstSpiceChar     * obsrvr,
                   ConstSpiceChar     * crdsys,
                   ConstSpiceChar     * coord,
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
   SPICE_GF_CNVTOL
              P   Convergence tolerance.
   target     I   Name of the target body
   frame      I   Name of the reference frame for coordinate calculations
   abcorr     I   Aberration correction flag
   obsrvr     I   Name of the observing body
   crdsys     I   Name of the coordinate system containing COORD
   coord      I   Name of the coordinate of interest
   relate     I   Operator that either looks for an extreme value
                  (max, min, local, absolute) or compares the
                  coordinate value and refval
   refval     I   Reference value
   adjust     I   Adjustment value for absolute extrema searches
   step       I   Step size used for locating extrema and roots
   nintvls    I   Workspace window interval count
   cnfine    I-O  SPICE window to which the search is restricted
   result     O   SPICE window containing results

-Detailed_Input

   target     the string name of a target body.  Optionally, you may
              supply the integer ID code for the object as an
              integer string.  For example both 'MOON' and '301'
              are legitimate strings that indicate the moon is the
              target body.

              The target and observer define a position vector
              that points from the observer to the target.

   frame      the string name of the reference frame in which to perform
              state look-ups and coordinate calculations.

              The SPICE frame subsystem must recognize the 'frame' name.

   abcorr     the string description of the aberration corrections to apply
              to the state evaluations to account for one-way light time
              and stellar aberration.

              This routine accepts the same aberration corrections as does
              the SPICE routine SPKEZR. See the header of SPKEZR for a
              detailed description of the aberration correction options.
              For convenience, the options are listed below:

                  'NONE'     Apply no correction.

                  'LT'       "Reception" case:  correct for
                             one-way light time using a Newtonian
                             formulation.

                  'LT+S'     "Reception" case:  correct for
                             one-way light time and stellar
                             aberration using a Newtonian
                             formulation.

                  'CN'       "Reception" case:  converged
                             Newtonian light time correction.

                  'CN+S'     "Reception" case:  converged
                             Newtonian light time and stellar
                             aberration corrections.

                  'XLT'      "Transmission" case:  correct for
                             one-way light time using a Newtonian
                             formulation.

                  'XLT+S'    "Transmission" case:  correct for
                             one-way light time and stellar
                             aberration using a Newtonian
                             formulation.

                  'XCN'      "Transmission" case:  converged
                             Newtonian light time correction.

                  'XCN+S'    "Transmission" case:  converged
                             Newtonian light time and stellar
                             aberration corrections.

              The abcorr string lacks sensitivity to case, and to embedded,
              leading and trailing blanks.

   obsrvr     the string naming the observing body. Optionally, you
              may supply the ID code of the object as an integer
              string. For example, both 'EARTH' and '399' are
              legitimate strings to supply to indicate the
              observer is Earth.

   crdsys     the string name of the coordinate system for which the
              coordinate of interest is a member.

   coord      the string name of the coordinate of interest in crdsys.

              The supported coordinate systems and coordinate names are:

              Coordinate System (CRDSYS)    Coordinates (COORD)      Range

                 'RECTANGULAR'                  'X'
                                                'Y'
                                                'Z'

                 'LATITUDINAL'                  'RADIUS'
                                                'LONGITUDE'        (-Pi,Pi]
                                                'LATITUDE'         [-Pi/2,Pi/2]

                 'RA/DEC'                       'RANGE'
                                                'RIGHT ASCENSION'  [0,2Pi)
                                                'DECLINATION'      [-Pi/2,Pi/2]

                 'SPHERICAL'                    'RADIUS'
                                                'COLATITUDE'       [0,Pi]
                                                'LONGITUDE'        (-Pi,Pi]

                 'CYLINDRICAL'                  'RADIUS'
                                                'LONGITUDE'        [0,2Pi)
                                                'Z'

                 'GEODETIC'                     'LONGITUDE'        (-Pi,Pi]
                                                'LATITUDE'         [-Pi/2,Pi/2]
                                                'ALTITUDE'

                 'PLANETOGRAPHIC'               'LONGITUDE'        [0,2Pi)
                                                'LATITUDE'         [-Pi/2,Pi/2]
                                                'ALTITUDE'

                  Limit searches for coordinate events in the GEODETIC and
                  PLANETOGRAPHIC coordinate systems to TARGET bodies with
                  axial symmetry in the equatorial plane, i.e. equality
                  of the body X and Y radii (oblate or prolate spheroids).

     relate    the string or character describing the relational operator
               used to define a constraint on the selected coordinate of the
               observer-target vector. The result window found by this routine
               indicates the time intervals where the constraint is satisfied.
               Supported values of relate and corresponding meanings are
               shown below:

                  '>'      Separation is greater than the reference
                           value refval.

                  '='      Separation is equal to the reference
                           value refval.

                  '<'      Separation is less than the reference
                           value refval.

                 'ABSMAX'  Separation is at an absolute maximum.

                 'ABSMIN'  Separation is at an absolute  minimum.

                 'LOCMAX'  Separation is at a local maximum.

                 'LOCMIN'  Separation is at a local minimum.

              The caller may indicate that the region of interest
              is the set of time intervals where the quantity is
              within a specified measure of an absolute extremum.
              The argument ADJUST (described below) is used to
              specify this measure.

              Local extrema are considered to exist only in the
              interiors of the intervals comprising the confinement
              window:  a local extremum cannot exist at a boundary
              point of the confinement window.

              The relate string lacks sensitivity to case, leading
              and trailing blanks.

   refval     the double precision reference value used together with
              relate argument to define an equality or inequality to
              satisfy by the selected coordinate of the observer-target
              vector. See the discussion of relate above for
              further information.

              The units of refval correspond to the type as defined
              by coord, radians for angular measures, kilometers for
              distance measures.

   adjust     a double precision value used to modify searches for
              absolute extrema: when relate is set to ABSMAX or ABSMIN and
              adjust is set to a positive value, gfposc_c finds times when the
              observer-target vector coordinate is within adjust
              radians/kilometers of the specified extreme value.

              For relate set to ABSMAX, the result window contains
              time intervals when the observer-target vector coordinate has
              values between ABSMAX - adjust and ABSMAX.

              For relate set to ABSMIN, the result window contains
              time intervals when the observer-target vector coordinate has
              values between ABSMIN and ABSMIN + adjust.

              adjust is not used for searches for local extrema,
              equality or inequality conditions.

   step       the double precision time step size to use in the search.
              step must be short enough for a search using this step
              size to locate the time intervals where coordinate function
              of the observer-target vector is monotone increasing or
              decreasing. However, step must not be *too* short, or
              the search will take an unreasonable amount of time.

              The choice of step affects the completeness but not
              the precision of solutions found by this routine; the
              precision is controlled by the convergence tolerance.

              step has units of seconds.

   nintvls    an integer value specifying the number of intervals in the
              the internal workspace array used by this routine. 'nintvls'
              should be at least as large as the number of intervals
              within the search region on which the specified observer-target
              vector coordinate function is monotone increasing or decreasing.
              It does no harm to pick a value of 'nintvls' larger than the
              minimum required to execute the specified search, but if chosen
              too small, the search will fail.

   cnfine     a double precision SPICE window that confines the time
              period over which the specified search is conducted.
              cnfine may consist of a single interval or a collection
              of intervals.

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

   result     the SPICE window of intervals, contained within the
              confinement window cnfine, on which the specified
              constraint is satisfied.

              If result is non-empty on input, its contents
              will be discarded before gfposc_c conducts its
              search.

              result must be declared and initialized with sufficient
              size to capture the full set of time intervals
              within the search region on which the specified constraint
              is satisfied.

              If the search is for local extrema, or for absolute
              extrema with adjust set to zero, then normally each
              interval of result will be a singleton: the left and
              right endpoints of each interval will be identical.

              If no times within the confinement window satisfy the
              constraint, result will be returned with a
              cardinality of zero.

-Parameters

   SPICE_GF_CNVTOL

              is the convergence tolerance used for finding endpoints
              of the intervals comprising the result window.
              SPICE_GF_CNVTOL is used to determine when binary searches
              for roots should terminate: when a root is bracketed
              within an interval of length SPICE_GF_CNVTOL; the root is
              considered to have been found.

              The accuracy, as opposed to precision, of roots found by
              this routine depends on the accuracy of the input data.
              In most cases, the accuracy of solutions will be inferior
              to their precision.

              SPICE_GF_CNVTOL has the value 1.0e-6. Units are TDB
              seconds.

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

   5)   If the aberration correction specifier contains an
        unrecognized value, an error is signaled by a routine in the
        call tree of this routine.

   6)  If `adjust' is negative, an error is signaled by a routine in
       the call tree of this routine.

   7)  If either of the input body names do not map to NAIF ID
       codes, an error is signaled by a routine in the call tree of
       this routine.

   8)  If required ephemerides or other kernel data are not
       available, an error is signaled by a routine in the call tree
       of this routine.

   9)  If any input string argument pointer is null, the error
       SPICE(NULLPOINTER) will be signaled.

   10) If any input string argument is empty, the error
       SPICE(EMPTYSTRING) will be signaled.

   11) If the workspace interval count 'nintvls' is less than 1, the
       error SPICE(VALUEOUTOFRANGE) will be signaled.

   12) If the required amount of workspace memory cannot be
       allocated, the error SPICE(MALLOCFAILURE) will be
       signaled.

-Files

   Appropriate SPK and PCK kernels must be loaded by the
   calling program before this routine is called.

   The following data are required:

      - SPK data: the calling application must load ephemeris data
        for the targets, observer, and any intermediate objects in
        a chain connecting the targets and observer that cover the time
        period specified by the window CNFINE. If aberration
        corrections are used, the states of target and observer
        relative to the solar system barycenter must be calculable
        from the available ephemeris data. Typically ephemeris data
        are made available by loading one or more SPK files using
        FURNSH.

      - PCK data: bodies modeled as triaxial ellipsoids must have
        semi-axis lengths provided by variables in the kernel pool.
        Typically these data are made available by loading a text
        PCK file using FURNSH.

      - If non-inertial reference frames are used, then PCK
        files, frame kernels, C-kernels, and SCLK kernels may be
        needed.

   Such kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   This routine provides a simpler, but less flexible interface
   than does the routine gfevnt_c for conducting searches for
   observer-target vector coordinate value events. Applications
   that require support for progress reporting, interrupt
   handling, non-default step or refinement functions, or non-default
   convergence tolerance should call gfevnt_c rather than this routine.

   This routine determines a set of one or more time intervals
   within the confinement window when the selected coordinate of
   the observer-target vector satisfies a caller-specified
   constraint. The resulting set of intervals is returned as a SPICE
   window.

   Below we discuss in greater detail aspects of this routine's
   solution process that are relevant to correct and efficient
   use of this routine in user applications.


   The Search Process
   ==================

   Regardless of the type of constraint selected by the caller, this
   routine starts the search for solutions by determining the time
   periods, within the confinement window, over which the specified
   coordinate function is monotone increasing and monotone
   decreasing. Each of these time periods is represented by a SPICE
   window. Having found these windows, all of the coordinate
   function's local extrema within the confinement window are known.
   Absolute extrema then can be found very easily.

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

   The monotone windows (described above) are found using a two-step
   search process. Each interval of the confinement window is
   searched as follows: first, the input step size is used to
   determine the time separation at which the sign of the rate of
   change of coordinate will be sampled. Starting at
   the left endpoint of an interval, samples will be taken at each
   step. If a change of sign is found, a root has been bracketed; at
   that point, the time at which the time derivative of the coordinate
   is zero can be found by a refinement process, for example,
   using a binary search.

   Note that the optimal choice of step size depends on the lengths
   of the intervals over which the coordinate function is monotone:
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

   Practical use of the coordinate search capability would likely
   consist of searches over multiple coordinate constraints to find
   time intervals that satisfies the constraints. An effective
   technique to accomplish such a search is to use the result
   window from one search as the confinement window of the next.


   Longitude and Right Ascension
   =============================

   The cyclic nature of the longitude and right ascension coordinates
   produces branch cuts at +/- 180 degrees longitude and 0-360
   longitude. Round-off error may cause solutions near these branches
   to cross the branch. Use of the SPICE routine wncond_c will contract
   solution windows by some epsilon, reducing the measure of the
   windows and eliminating the branch crossing. A one millisecond
   contraction will in most cases eliminate numerical round-off caused
   branch crossings.

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

   Example(1):

      Find the time during 2007 for which the latitude of the
      Earth-Sun vector in IAU_EARTH frame has the maximum value,
      i.e. the latitude of the Tropic of Cancer.

      #include <stdio.h>
      #include <stdlib.h>
      #include <string.h>

      #include "SpiceUsr.h"

      #define       MAXWIN   750
      #define       TIMFMT   "YYYY-MON-DD HR:MN:SC.###### (TDB) ::TDB ::RND"
      #define       TIMLEN   41

      int main( int argc, char **argv )
         {

         /.
         Create the needed windows. Note, one window
         consists of two values, so the total number
         of cell values to allocate is twice
         the number of intervals.
         ./
         SPICEDOUBLE_CELL ( result, 2*MAXWIN );
         SPICEDOUBLE_CELL ( cnfine, 2       );

         SpiceDouble       begtim;
         SpiceDouble       endtim;
         SpiceDouble       step;
         SpiceDouble       adjust;
         SpiceDouble       refval;
         SpiceDouble       beg;
         SpiceDouble       end;

         SpiceChar         begstr [ TIMLEN ];
         SpiceChar         endstr [ TIMLEN ];
         SpiceChar       * relate = "ABSMAX";
         SpiceChar       * crdsys = "LATITUDINAL";
         SpiceChar       * coord  = "LATITUDE";
         SpiceChar       * targ   = "SUN";
         SpiceChar       * obsrvr = "EARTH";
         SpiceChar       * frame  = "IAU_EARTH";
         SpiceChar       * abcorr = "NONE";

         SpiceInt          count;
         SpiceInt          i;

         /.
         Load kernels.
         ./
         furnsh_c( "standard.tm" );

         /.
         Store the time bounds of our search interval in
         the cnfine confinement window.
         ./
         str2et_c( "2007 JAN 01", &begtim );
         str2et_c( "2008 JAN 01", &endtim );

         wninsd_c ( begtim, endtim, &cnfine );

         /.
         The latitude varies relatively slowly, ~46 degrees during the
         year. The extrema occur approximately every six months.
         Search using a step size less than half that value (180 days).
         For this example use ninety days (in units of seconds).
         ./
         step   = (90.)*spd_c();
         adjust = 0.;
         refval = 0;

         /.
         List the beginning and ending points in each interval
         if result contains data.
         ./
         gfposc_c (  targ,
                     frame,
                     abcorr,
                     obsrvr,
                     crdsys,
                     coord,
                     relate,
                     refval,
                     adjust,
                     step,
                     MAXWIN,
                     &cnfine,
                     &result  );

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

               if ( beg == end )
                  {
                  timout_c ( beg, TIMFMT, TIMLEN, begstr );
                  printf ( "Event time: %s\n", begstr );
                  }
               else
                  {

                  timout_c ( beg, TIMFMT, TIMLEN, begstr );
                  timout_c ( end, TIMFMT, TIMLEN, endstr );

                  printf ( "Interval %d\n", i + 1);
                  printf ( "From : %s \n", begstr );
                  printf ( "To   : %s \n", endstr );
                  printf( " \n" );
                  }

               }
            }

         kclear_c();
         return( 0 );
         }

      The program outputs:

         Event time: 2007-JUN-21 17:54:13.166910 (TDB)

   Example(2):

      A minor modification of the program listed in Example 1; find the
      time during 2007 for which the latitude of the Earth-Sun vector
      in IAU_EARTH frame has the minimum value, i.e. the latitude of
      the Tropic of Capricorn.

      Edit the example program, assign:

         SpiceChar       * relate = "ABSMIN";

      The program outputs:

         Event time: 2007-DEC-22 06:04:32.630160 (TDB)

   Example(3):

      Find the time during 2007 for which the Z component of the
      Earth-Sun vector in IAU_EARTH frame has value 0, i.e. crosses
      the equatorial plane (this also defines a zero latitude).
      The search should return two times, one for an ascending
      passage and one for descending.

      Edit the example program, assign:

         SpiceChar       * relate = "=";
         SpiceChar       * crdsys = "RECTANGULAR";
         SpiceChar       * coord  = "Z";

         Note, this RELATE operator refers to the REFVAL value,
         assigned to 0.D0 for this example.

      The program outputs:

         Event time: 2007-MAR-21 00:01:25.495120 (TDB)
         Event time: 2007-SEP-23 09:46:39.574124 (TDB)

   Example(4):

      Find the times between Jan 1, 2007 and Jan 1, 2008 corresponding
      to the apoapsis on the Moon's orbit around the Earth (note, the
      GFDIST routine can also perform this search).

      Edit the example program, assign:

         This search requires a change in the step size since the Moon's
         orbit about the earth (earth-moon barycenter) has a twenty-eight
         day period. Use a step size something less than half that value.
         In this case, we use twelve days.

            SpiceChar       * relate = "LOCMAX";
            SpiceChar       * crdsys = "SPHERICAL";
            SpiceChar       * coord  = "RADIUS";
            SpiceChar       * targ   = "MOON";
            SpiceChar       * frame  = "J2000";

            step   = 12.*spd_c();

      The program outputs:

         Event time: 2007-JAN-10 16:26:18.805837 (TDB)
         Event time: 2007-FEB-07 12:39:35.078525 (TDB)
         Event time: 2007-MAR-07 03:38:07.334769 (TDB)
         Event time: 2007-APR-03 08:38:55.222606 (TDB)
         Event time: 2007-APR-30 10:56:49.847027 (TDB)
         Event time: 2007-MAY-27 22:03:28.857783 (TDB)
         Event time: 2007-JUN-24 14:26:23.639351 (TDB)
         Event time: 2007-JUL-22 08:43:50.135565 (TDB)
         Event time: 2007-AUG-19 03:28:33.538169 (TDB)
         Event time: 2007-SEP-15 21:07:13.964698 (TDB)
         Event time: 2007-OCT-13 09:52:30.819372 (TDB)
         Event time: 2007-NOV-09 12:32:50.070555 (TDB)
         Event time: 2007-DEC-06 16:54:31.225504 (TDB)

   Example(5):

      Find times between Jan 1, 2007 and Jan 1, 2008 when the latitude
      (elevation) of the observer-target vector between DSS 17 and the
      Moon, as observed in the DSS 17 topocentric (station) frame,
      exceeds 83 degrees.

      This search uses a step size of four hours since the time
      for all declination zero-to-max-to-zero passes within
      the search window exceeds eight hours.

      This search requires kernels not included in the standard.tm
      meta kernel.

          Kernel name                      Contents
          -----------                      --------
          earthstns_itrf93_050714.bsp      SPK for DSN Station
                                           Locations
          earth_topo_050714.tf             Topocentric DSN stations
                                           frame definitions
          earth_000101_080120_071029.bpc   High precision earth PCK

      Edit the example program, assign:

         SpiceChar       * relate = ">";
         SpiceChar       * crdsys = "LATITUDINAL";
         SpiceChar       * coord  = "LATITUDE";
         SpiceChar       * targ   = "MOON";
         SpiceChar       * obsrvr = "DSS-17";
         SpiceChar       * frame  = "DSS-17_TOPO";


         furnsh_c ("earthstns_itrf93_050714.bsp");
         furnsh_c ("earth_topo_050714.tf");
         furnsh_c ("earth_000101_080120_071029.bpc");

         step   = (4./24.)*spd_c();
         refval = 83. * rpd_c();

      The program outputs:

         Interval 1
         From : 2007-FEB-26 03:18:48.229806 (TDB)
         To   : 2007-FEB-26 03:31:29.734169 (TDB)

         Interval 2
         From : 2007-MAR-25 01:12:38.551183 (TDB)
         To   : 2007-MAR-25 01:23:53.908601 (TDB)

-Restrictions

   1) The kernel files to be used by this routine must be loaded
      (normally via the CSPICE routine furnsh_c) before this routine
      is called.

   2) This routine has the side effect of re-initializing the
      coordinate quantity utility package.  Callers may
      need to re-initialize the package after calling this routine.


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

   -CSPICE Version 1.0.1, 26-AUG-2009 (EDW)

      Correction of several typos.

   -CSPICE Version 1.0.0, 10-FEB-2009 (NJB) (EDW)

-Index_Entries

   GF position coordinate search

-&
*/

   { /* Begin gfposc_c */

   /*
   Local variables
   */
   doublereal            * work;

   SpiceInt                nBytes;

   static SpiceInt         nw = SPICE_GF_NWMAX;

   /*
   Participate in error tracing.
   */
   if ( return_c() )
      {
      return;
      }
   chkin_c ( "gfposc_c" );


   /*
   Make sure cell data types are d.p.
   */
   CELLTYPECHK2 ( CHK_STANDARD, "gfposc_c", SPICE_DP, cnfine, result );

   /*
   Initialize the input cells if necessary.
   */
   CELLINIT2 ( cnfine, result );

   /*
   Check the input strings to make sure each pointer is non-null
   and each string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gfposc_c", target );
   CHKFSTR ( CHK_STANDARD, "gfposc_c", frame  );
   CHKFSTR ( CHK_STANDARD, "gfposc_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "gfposc_c", obsrvr );
   CHKFSTR ( CHK_STANDARD, "gfposc_c", crdsys );
   CHKFSTR ( CHK_STANDARD, "gfposc_c", coord  );
   CHKFSTR ( CHK_STANDARD, "gfposc_c", relate );

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
      chkout_c ( "gfposc_c"                                 );
      return;
      }

   /*
   Allocate the workspace. 'nintvls' indicates the maximum number of
   intervals returned in 'result'. An interval consists of
   two values.
   */

   nintvls = 2 * nintvls;

   nBytes = ( nintvls + SPICE_CELL_CTRLSZ ) * nw * sizeof(SpiceDouble);

   work   = (doublereal *) alloc_SpiceMemory( nBytes );

   if ( !work )
      {
      setmsg_c ( "Workspace allocation of # bytes failed due to "
                 "malloc failure"                               );
      errint_c ( "#",  nBytes                                   );
      sigerr_c ( "SPICE(MALLOCFAILED)"                          );
      chkout_c ( "gfposc_c"                                     );
      return;
      }


   /*
   Let the f2'd routine do the work.
   */

   gfposc_( ( char          * ) target,
            ( char          * ) frame,
            ( char          * ) abcorr,
            ( char          * ) obsrvr,
            ( char          * ) crdsys,
            ( char          * ) coord,
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
            ( ftnlen          ) strlen(frame),
            ( ftnlen          ) strlen(abcorr),
            ( ftnlen          ) strlen(obsrvr),
            ( ftnlen          ) strlen(crdsys),
            ( ftnlen          ) strlen(coord),
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

   chkout_c ( "gfposc_c" );

   } /* End gfposc_c */
