/*

-Procedure gfoclt_c ( GF, find occultation )

-Abstract
 
   Determine time intervals when an observer sees one target 
   occulted by, or in transit across, another. 
 
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
 
   FRAMES 
   GF 
   KERNEL 
   NAIF_IDS 
   SPK 
   TIME 
   WINDOWS   
 
-Keywords
 
   EVENT 
   GEOMETRY 
   SEARCH 
   WINDOW 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void gfoclt_c ( ConstSpiceChar   * occtyp,
                   ConstSpiceChar   * front,
                   ConstSpiceChar   * fshape,
                   ConstSpiceChar   * fframe,
                   ConstSpiceChar   * back,
                   ConstSpiceChar   * bshape,
                   ConstSpiceChar   * bframe,
                   ConstSpiceChar   * abcorr,
                   ConstSpiceChar   * obsrvr,
                   SpiceDouble        step,
                   SpiceCell        * cnfine,
                   SpiceCell        * result )
/*

-Brief_I/O
 
   VARIABLE        I/O  DESCRIPTION 
   --------------- ---  -------------------------------------------------
   SPICE_GF_CNVTOL  P   Convergence tolerance. 
   occtyp           I   Type of occultation. 
   front            I   Name of body occulting the other. 
   fshape           I   Type of shape model used for front body. 
   fframe           I   Body-fixed, body-centered frame for front body. 
   back             I   Name of body occulted by the other. 
   bshape           I   Type of shape model used for back body. 
   bframe           I   Body-fixed, body-centered frame for back body. 
   abcorr           I   Aberration correction flag. 
   obsrvr           I   Name of the observing body. 
   step             I   Step size in seconds for finding occultation  
                        events. 
   cnfine          I-O  SPICE window to which the search is restricted. 
   result           O   SPICE window containing results. 
    
-Detailed_Input
 
 
   occtyp     indicates the type of occultation that is to be found. 
              Note that transits are considered to be a type of
              occultation.

              Supported values and corresponding definitions are: 
 
                 "FULL"               denotes the full occultation 
                                      of the body designated by  
                                      `back' by the body designated 
                                      by `front', as seen from 
                                      the location of the observer. 
                                      In other words, the occulted 
                                      body is completely invisible 
                                      as seen from the observer's 
                                      location. 
 
                 "ANNULAR"            denotes an annular 
                                      occultation: the body 
                                      designated by `front' blocks 
                                      part of, but not the limb of, 
                                      the body designated by `back', 
                                      as seen from the location of 
                                      the observer. 
 
                 "PARTIAL"            denotes a partial, non-annular
                                      occultation: the body designated
                                      by `front' blocks part, but not
                                      all, of the limb of the body
                                      designated by `back', as seen
                                      from the location of the
                                      observer.
 
                 "ANY"                denotes any of the above three 
                                      types of occultations: 
                                      "PARTIAL", "ANNULAR", or 
                                      "FULL". 
 
                                      "ANY" should be used to search 
                                      for times when the body  
                                      designated by `front' blocks 
                                      any part of the body designated 
                                      by `back'. 
 
                                      The option "ANY" must be used 
                                      if either the front or back 
                                      target body is modeled as 
                                      a point. 
 
              Case and leading or trailing blanks are not 
              significant in the string `occtyp'. 
 
 
   front      is the name of the target body that occults---that is, 
              passes in front of---the other. Optionally, you may 
              supply the integer NAIF ID code for the body as a 
              string. For example both "MOON" and "301" are 
              legitimate strings that designate the Moon. 
 
              Case and leading or trailing blanks are not 
              significant in the string `front'. 
 
 
   fshape     is a string indicating the geometric model used to
              represent the shape of the front target body. The
              supported options are:
 
                 "ELLIPSOID"     Use a triaxial ellipsoid model
                                 with radius values provided via the 
                                 kernel pool. A kernel variable  
                                 having a name of the form 
 
                                    "BODYnnn_RADII"  
 
                                 where nnn represents the NAIF 
                                 integer code associated with the 
                                 body, must be present in the kernel 
                                 pool. This variable must be 
                                 associated with three numeric 
                                 values giving the lengths of the 
                                 ellipsoid's X, Y, and Z semi-axes. 
 
                 "POINT"         Treat the body as a single point. 
                                 When a point target is specified, 
                                 the occultation type must be 
                                 set to "ANY". 
                                  
              At least one of the target bodies `front' and `back' must 
              be modeled as an ellipsoid. 
 
              Case and leading or trailing blanks are not 
              significant in the string `fshape'. 
 
 
   fframe     is the name of the body-fixed, body-centered reference 
              frame associated with the front target body. Examples 
              of such names are "IAU_SATURN" (for Saturn) and 
              "ITRF93" (for the Earth). 
 
              If the front target body is modeled as a point, `fframe' 
              should be left empty or blank. 
 
              Case and leading or trailing blanks bracketing a
              non-blank frame name are not significant in the string
              `fframe'.

 
   back       is the name of the target body that is occulted 
              by---that is, passes in back of---the other. 
              Optionally, you may supply the integer NAIF ID code 
              for the body as a string. For example both "MOON" and 
              "301" are legitimate strings that designate the Moon. 
 
              Case and leading or trailing blanks are not 
              significant in the string `back'. 
 
 
   bshape     is the shape specification for the body designated by
              `back'. The supported options are those for `fshape'. See
              the description of `fshape' above for details.
               
 
   bframe     is the name of the body-fixed, body-centered reference 
              frame associated with the ``back'' target body. 
              Examples of such names are "IAU_SATURN" (for Saturn) 
              and "ITRF93" (for the Earth). 
 
              If the back target body is modeled as a point, `bframe' 
              should be left empty or blank. 
 
              Case and leading or trailing blanks bracketing a 
              non-blank frame name are not significant in the string 
              `bframe'. 
 
 
   abcorr     indicates the aberration corrections to be applied to 
              the state of each target body to account for one-way 
              light time.  Stellar aberration corrections are 
              ignored if specified, since these corrections don't 
              improve the accuracy of the occultation determination. 
 
              See the header of the SPICE routine spkezr_c for a 
              detailed description of the aberration correction 
              options. For convenience, the options supported by 
              this routine are listed below: 
 
                 "NONE"     Apply no correction.    
 
                 "LT"       "Reception" case:  correct for 
                            one-way light time using a Newtonian 
                            formulation. 
 
                 "CN"       "Reception" case:  converged 
                            Newtonian light time correction. 
 
                 "XLT"      "Transmission" case:  correct for 
                            one-way light time using a Newtonian 
                            formulation. 
 
                 "XCN"      "Transmission" case:  converged 
                            Newtonian light time correction. 
 
              Case and blanks are not significant in the string 
              `abcorr'. 
 
 
   obsrvr     is the name of the body from which the occultation is 
              observed. Optionally, you may supply the integer NAIF 
              ID code for the body as a string. 
 
              Case and leading or trailing blanks are not 
              significant in the string `obsrvr'. 


   step       is the step size to be used in the search. `step' must 
              be shorter than any interval, within the confinement 
              window, over which the specified condition is met. In
              other words, `step' must be shorter than the shortest
              occultation event that the user wishes to detect; `step'
              must also be shorter than the shortest time interval
              between two occultation events that occur within the
              confinement window (see below). However, `step' must not
              be *too* short, or the search will take an unreasonable
              amount of time.
 
              The choice of `step' affects the completeness but not the
              precision of solutions found by this routine; the
              precision is controlled by the convergence tolerance. See
              the discussion of the parameter SPICE_GF_CNVTOL for
              details.
 
              `step' has units of TDB seconds.  
 
  
   cnfine     is a SPICE window that confines the time period over 
              which the specified search is conducted. `cnfine' may 
              consist of a single interval or a collection of  
              intervals.  
 
              The endpoints of the time intervals comprising `cnfine'
              are interpreted as seconds past J2000 TDB.
                
              See the Examples section below for a code example  
              that shows how to create a confinement window. 
 
 
-Detailed_Output
 
   cnfine     is the input confinement window, updated if necessary
              so the control area of its data array indicates the
              window's size and cardinality. The window data are
              unchanged.


   result     is a SPICE window representing the set of time 
              intervals, within the confinement period, when the 
              specified occultation occurs. 
 
              The endpoints of the time intervals comprising `result'
              are interpreted as seconds past J2000 TDB.

              If `result' is non-empty on input, its contents 
              will be discarded before gfoclt_c conducts its 
              search. 
 
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
 
              SPICE_GF_CNVTOL is declared in the header file
             
                 SpiceGF.h
 
-Exceptions
 
   1)  In order for this routine to produce correct results, 
       the step size must be appropriate for the problem at hand. 
       Step sizes that are too large may cause this routine to miss 
       roots; step sizes that are too small may cause this routine 
       to run unacceptably slowly and in some cases, find spurious 
       roots. 
 
       This routine does not diagnose invalid step sizes, except 
       that if the step size is non-positive, the error  
       SPICE(INVALIDSTEPSIZE) will be signaled. 
 
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
 
   3)  If name of either target or the observer cannot be translated 
       to a NAIF ID code, the error will be diagnosed by a routine 
       in the call tree of this routine. 
        
   4)  If the radii of a target body modeled as an ellipsoid cannot 
       be determined by searching the kernel pool for a kernel 
       variable having a name of the form 
 
          "BODYnnn_RADII"  
 
       where nnn represents the NAIF integer code associated with 
       the body, the error will be diagnosed by a routine in the 
       call tree of this routine. 
 
   5)  If either of the target bodies `front' or `back' coincides with 
       the observer body `obsrvr', the error will be diagnosed by a 
       routine in the call tree of this routine. 
 
   6)  If the body designated by `front' coincides with that 
       designated by `back', the error will be diagnosed by a routine 
       in the call tree of this routine. 
        
   7)  If either of the body model specifiers `fshape' or `bshape' 
       is not recognized, the error will be diagnosed by a routine 
       in the call tree of this routine. 
 
   8)  If both of the body model specifiers `fshape' and `bshape' 
       specify point targets, the error will be diagnosed by a 
       routine in the call tree of this routine. 
 
   9)  If a target body-fixed reference frame associated with a  
       non-point target is not recognized, the error will be 
       diagnosed by a routine in the call tree of this routine. 
 
   10) If a target body-fixed reference frame is not centered at 
       the corresponding target body,  the error will be 
       diagnosed by a routine in the call tree of this routine. 
 
   11) If the loaded kernels provide insufficient data to  
       compute any required state vector, the deficiency will 
       be diagnosed by a routine in the call tree of this routine. 
 
   12) If an error occurs while reading an SPK or other kernel file, 
       the error will be diagnosed by a routine in the call tree  
       of this routine. 
 
   13) If the output SPICE window `result' has insufficient capacity 
       to contain the number of intervals on which the specified 
       occultation condition is met, the error will be diagnosed 
       by a routine in the call tree of this routine. 
 
   14) If a point target is specified and the occultation 
       type is set to a valid value other than "ANY", the 
       error will be diagnosed by a routine in the call tree  
       of this routine. 
 
   15) Invalid occultation types will be diagnosed by a routine in
       the call tree of this routine.

   16) Invalid aberration correction specifications will be
       diagnosed by a routine in the call tree of this routine.

   17) If any input string argument pointer is null, the error
       SPICE(NULLPOINTER) will be signaled.

   18) If any input string argument, other than `fframe' or `bframe',
       is empty, the error SPICE(EMPTYSTRING) will be signaled.

-Files
 
   Appropriate SPICE kernels must be loaded by the calling program
   before this routine is called.
 
   The following data are required: 
 
      - SPK data: the calling application must load ephemeris data 
        for the target, source and observer that cover the time 
        period specified by the window `cnfine'. If aberration 
        corrections are used, the states of target and observer 
        relative to the solar system barycenter must be calculable 
        from the available ephemeris data. Typically ephemeris data 
        are made available by loading one or more SPK files via 
        furnsh_c. 
 
      - PCK data: bodies modeled as triaxial ellipsoids must have 
        semi-axis lengths provided by variables in the kernel pool. 
        Typically these data are made available by loading a text 
        PCK file via furnsh_c. 
 
      - FK data: if either of the reference frames designated by
        `bframe' or `fframe' are not built in to the SPICE system,
        one or more FKs specifying these frames must be loaded. 

   Kernel data are normally loaded once per program run, NOT every time
   this routine is called.
 
-Particulars
 
   This routine provides a simpler, but less flexible, interface 
   than does the CSPICE routine gfocce_c for conducting searches for 
   occultation events. Applications that require support for 
   progress reporting, interrupt handling, non-default step or 
   refinement functions, or non-default convergence tolerance should 
   call gfocce_c rather than this routine. 
 
   This routine determines a set of one or more time intervals 
   within the confinement window when a specified type of 
   occultation occurs. The resulting set of intervals is returned as 
   a SPICE window. 
 
   Below we discuss in greater detail aspects of this routine's 
   solution process that are relevant to correct and efficient 
   use of this routine in user applications. 
    
 
   The Search Process 
   ================== 
 
   The search for occultations is treated as a search for state 
   transitions: times are sought when the state of the `back' body 
   changes from "not occulted" to "occulted" or vice versa. 
 
   Step Size 
   ========= 
 
   Each interval of the confinement window is searched as follows:
   first, the input step size is used to determine the time separation
   at which the occultation state will be sampled. Starting at the left
   endpoint of the interval, samples of the occultation state will be
   taken at each step. If a state change is detected, a root has been
   bracketed; at that point, the "root"--the time at which the state
   change occurs---is found by a refinement process, for example, via
   binary search.
 
   Note that the optimal choice of step size depends on the lengths 
   of the intervals over which the occultation state is constant: 
   the step size should be shorter than the shortest occultation 
   duration and the shortest period between occultations, within 
   the confinement window. 
 
   Having some knowledge of the relative geometry of the targets and 
   observer can be a valuable aid in picking a reasonable step size. 
   In general, the user can compensate for lack of such knowledge by 
   picking a very short step size; the cost is increased computation 
   time. 
 
   Note that the step size is not related to the precision with which 
   the endpoints of the intervals of the result window are computed. 
   That precision level is controlled by the convergence tolerance. 
 
 
   Convergence Tolerance 
   ===================== 
 
   Once a root has been bracketed, a refinement process is used to
   narrow down the time interval within which the root must lie. This
   refinement process terminates when the location of the root has been
   determined to within an error margin called the "convergence
   tolerance." The convergence tolerance used by this routine is set
   via the parameter SPICE_GF_CNVTOL.
 
   The value of SPICE_GF_CNVTOL is set to a "tight" value so that the
   tolerance doesn't limit the accuracy of solutions found by this
   routine. In general the accuracy of input data will be the limiting
   factor.
 
   To use a different tolerance value, a lower-level GF routine such as
   gfocce_c must be called. Making the tolerance tighter than
   SPICE_GF_CNVTOL is unlikely to be useful, since the results are
   unlikely to be more accurate. Making the tolerance looser will speed
   up searches somewhat, since a few convergence steps will be omitted.
   However, in most cases, the step size is likely to have a much
   greater effect on processing time than would the convergence
   tolerance.
 
 
   The Confinement Window 
   ====================== 
 
   The simplest use of the confinement window is to specify a time 
   interval within which a solution is sought. 

   The confinement window also can be used to restrict a search to
   a time window over which required data (typically ephemeris
   data, in the case of occultation searches) are known to be
   available.

   In some cases, the confinement window be used to make searches
   more efficient. Sometimes it's possible to do an efficient search
   to reduce the size of the time period over which a relatively
   slow search of interest must be performed. See the "CASCADE"
   example program in gf.req for a demonstration.
 
-Examples
 
 
   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Find occultations of the Sun by the Moon (that is, solar
      eclipses) as seen from the center of the Earth over the month
      December, 2001.
 
      Use light time corrections to model apparent positions of Sun 
      and Moon. Stellar aberration corrections are not specified 
      because they don't affect occultation computations. 
 
      We select a step size of 3 minutes, which means we 
      ignore occultation events lasting less than 3 minutes, 
      if any exist. 
 
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


         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'pck00008.tpc',
                                'naif0009.tls'  )

         \begintext
 
 

      Example code begins here.


         #include <stdio.h>
         #include "SpiceUsr.h"

         int main()
         {
            /.
            Local constants 
            ./

            #define TIMFMT          "YYYY MON DD HR:MN:SC.###### (TDB)::TDB"
            #define MAXWIN          200
            #define TIMLEN          41

            /.
            Local variables 
            ./
            SPICEDOUBLE_CELL      ( cnfine, MAXWIN );
            SPICEDOUBLE_CELL      ( result, MAXWIN );

            SpiceChar             * win0;
            SpiceChar             * win1;
            SpiceChar               begstr [ TIMLEN ];
            SpiceChar               endstr [ TIMLEN ];

            SpiceDouble             et0;
            SpiceDouble             et1;
            SpiceDouble             left;
            SpiceDouble             right;
            SpiceDouble             step;

            SpiceInt                i;

            /.
            Load kernels. 
            ./
            furnsh_c ( "standard.tm" );

            /.
            Obtain the TDB time bounds of the confinement
            window, which is a single interval in this case.
            ./
            win0 = "2001 DEC 01 00:00:00 TDB";
            win1 = "2002 JAN 01 00:00:00 TDB";

            str2et_c ( win0, &et0 );
            str2et_c ( win1, &et1 );

            /.
            Insert the time bounds into the confinement
            window.
            ./
            wninsd_c ( et0, et1, &cnfine );

            /.
            Select a 3-minute step. We'll ignore any occultations
            lasting less than 3 minutes.  Units are TDB seconds.
            ./
            step = 180.0;

            /.
            Perform the search.
            ./
            gfoclt_c ( "any",                            
                       "moon",    "ellipsoid",  "iau_moon", 
                       "sun",     "ellipsoid",  "iau_sun", 
                       "lt",      "earth",      step, 
                       &cnfine,   &result                 );

            if ( wncard_c(&result) == 0 )
            {
               printf ( "No occultation was found.\n" ); 
            }
            else
            {
               for ( i = 0;  i < wncard_c(&result); i++ )
               { 
                  /.
                  Fetch and display each occultation interval.
                  ./
                  wnfetd_c ( &result, i, &left, &right );

                  timout_c ( left,  TIMFMT, TIMLEN, begstr );
                  timout_c ( right, TIMFMT, TIMLEN, endstr );

                  printf ( "Interval %ld\n"
                           "   Start time: %s\n" 
                           "   Stop time:  %s\n",
                           i, begstr, endstr      );
               }
            }

            return ( 0 );
         }

 
      When this program was executed on a PC/Linux/gcc platform, the
      output was:
 
         Interval 0
            Start time: 2001 DEC 14 20:10:14.195952 (TDB)
            Stop time:  2001 DEC 14 21:35:50.317994 (TDB)

 
   2) Find occultations of Titan by Saturn or of Saturn by
      Titan as seen from the center of the Earth over the
      last four months of 2008. Model both target bodies as
      ellipsoids. Search for every type of occultation.

      Use light time corrections to model apparent positions of
      Saturn and Titan. Stellar aberration corrections are not
      specified because they don't affect occultation computations.

      We select a step size of 15 minutes, which means we
      ignore occultation events lasting less than 15 minutes,
      if any exist.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: gfoclt_ex2.tm

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
            sat288.bsp                    Satellite ephemeris for
                                          Saturn
            pck00008.tpc                  Planet orientation and
                                          radii
            naif0009.tls                  Leapseconds

         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'sat288.bsp',
                                'pck00008.tpc',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


     Example code begins here.
      
        #include <stdio.h>
        #include <string.h>
        #include "SpiceUsr.h"

        int main()
        {
           /.
           Local constants 
           ./
           #define TIMFMT          "YYYY MON DD HR:MN:SC.###### (TDB)::TDB"
           #define MAXWIN          200
           #define TIMLEN          41
           #define LNSIZE          81
           #define NTYPES          4

           /.
           Local variables 
           ./
           SPICEDOUBLE_CELL      ( cnfine, MAXWIN );
           SPICEDOUBLE_CELL      ( result, MAXWIN );

           SpiceChar             * back;
           SpiceChar             * bframe;
           SpiceChar             * front;
           SpiceChar             * fframe;
           SpiceChar               line   [ LNSIZE ];
           SpiceChar             * obsrvr;   

           SpiceChar             * occtyp [ NTYPES ] =
                                   {
                                      "FULL",
                                      "ANNULAR",
                                      "PARTIAL",
                                      "ANY"
                                   };

           SpiceChar             * templt [ NTYPES ] =
                                   {
                                      "Condition: # occultation of # by #",
                                      "Condition: # occultation of # by #",
                                      "Condition: # occultation of # by #",
                                      "Condition: # occultation of # by #"
                                   };

           SpiceChar               timstr [ TIMLEN ];
           SpiceChar               title  [ LNSIZE ];
           SpiceChar             * win0;
           SpiceChar             * win1;

           SpiceDouble             et0;
           SpiceDouble             et1;
           SpiceDouble             finish;
           SpiceDouble             start;
           SpiceDouble             step;

           SpiceInt                i;
           SpiceInt                j;
           SpiceInt                k;

           /.
           Load kernels. 
           ./
           furnsh_c ( "gfoclt_ex2.tm" );

           /.
           Obtain the TDB time bounds of the confinement
           window, which is a single interval in this case.
           ./
           win0 = "2008 SEP 01 00:00:00 TDB";
           win1 = "2009 JAN 01 00:00:00 TDB";

           str2et_c ( win0, &et0 );
           str2et_c ( win1, &et1 );

           /.
           Insert the time bounds into the confinement
           window.
           ./
           wninsd_c ( et0, et1, &cnfine );

           /.
           Select a 15-minute step. We'll ignore any occultations
           lasting less than 15 minutes. Units are TDB seconds.
           ./
           step = 900.0;

           /.
           The observation location is the Earth.
           ./
           obsrvr = "Earth";

           /.
           Loop over the occultation types.
           ./
           for ( i = 0;  i < NTYPES;  i++ )
           {
              /.
              For each type, do a search for both transits of
              Titan across Saturn and occultations of Titan by
              Saturn.
              ./
              for ( j = 0;  j < 2;  j++ )
              {
                 if ( j == 0 )
                 {
                    front  = "TITAN";
                    fframe = "IAU_TITAN";
                    back   = "SATURN";
                    bframe = "IAU_SATURN";
                 }
                 else
                 {
                    front  = "SATURN";
                    fframe = "IAU_SATURN";
                    back   = "TITAN";
                    bframe = "IAU_TITAN";
                 }

                 /.
                 Perform the search. The target body shapes
                 are modeled as ellipsoids.
                 ./
                 gfoclt_c ( occtyp[i],                            
                            front,    "ellipsoid",  fframe, 
                            back,     "ellipsoid",  bframe,  
                            "lt",     obsrvr,       step,   
                            &cnfine,  &result               );

                 /.
                 Display the results. 
                 ./
                 printf ( "\n" );

                 /.
                 Substitute the occultation type and target
                 body names into the title string:
                 ./
                 repmc_c ( templt[i], "#", occtyp[i], LNSIZE, title );
                 repmc_c ( title,     "#", back,      LNSIZE, title );
                 repmc_c ( title,     "#", front,     LNSIZE, title );

                 printf ( "%s\n", title );

                 if ( wncard_c(&result) == 0 )
                 {
                    printf ( " Result window is empty: "
                             "no occultation was found.\n" );
                 }
                 else
                 {
                    printf ( " Result window start, stop times:\n" );

                    for ( k = 0;  k < wncard_c(&result);  k++ )
                    { 
                       /.
                       Fetch the endpoints of the kth interval
                       of the result window.
                       ./
                       wnfetd_c ( &result, k, &start, &finish );

                       /.
                       Call strncpy with a length of 7 to include
                       a terminating null. 
                       ./
                       strncpy ( line, "  #  #", 7 );

                       timout_c ( start,  TIMFMT, TIMLEN, timstr );

                       repmc_c  ( line, "#", timstr, LNSIZE, line );

                       timout_c ( finish, TIMFMT, TIMLEN, timstr );

                       repmc_c  ( line, "#", timstr, LNSIZE, line );

                       printf ( "%s\n", line );
                    }
                 }
                 /.
                 We've finished displaying the results of the
                 current search.
                 ./
              }
              /.
              We've finished displaying the results of the
              searches using the current occultation type.
              ./
           }
           printf ( "\n" );

           return ( 0 );
        }

 
      When this program was executed on a PC/Linux/gcc platform, the
      output was:


         Condition: FULL occultation of SATURN by TITAN
          Result window is empty: no occultation was found.

         Condition: FULL occultation of TITAN by SATURN
          Result window start, stop times:
           2008 OCT 27 22:08:01.627053 (TDB)  2008 OCT 28 01:05:03.375236 (TDB)
           2008 NOV 12 21:21:59.252262 (TDB)  2008 NOV 13 02:06:05.053051 (TDB)
           2008 NOV 28 20:49:02.402832 (TDB)  2008 NOV 29 02:13:58.986344 (TDB)
           2008 DEC 14 20:05:09.246177 (TDB)  2008 DEC 15 01:44:53.523002 (TDB)
           2008 DEC 30 19:00:56.577073 (TDB)  2008 DEC 31 00:42:43.222909 (TDB)

         Condition: ANNULAR occultation of SATURN by TITAN
          Result window start, stop times:
           2008 OCT 19 21:29:20.599087 (TDB)  2008 OCT 19 22:53:34.518737 (TDB)
           2008 NOV 04 20:15:38.620368 (TDB)  2008 NOV 05 00:18:59.139978 (TDB)
           2008 NOV 20 19:38:59.647712 (TDB)  2008 NOV 21 00:35:26.725908 (TDB)
           2008 DEC 06 18:58:34.073268 (TDB)  2008 DEC 07 00:16:17.647040 (TDB)
           2008 DEC 22 18:02:46.288289 (TDB)  2008 DEC 22 23:26:52.712459 (TDB)

         Condition: ANNULAR occultation of TITAN by SATURN
          Result window is empty: no occultation was found.

         Condition: PARTIAL occultation of SATURN by TITAN
          Result window start, stop times:
           2008 OCT 19 20:44:30.326771 (TDB)  2008 OCT 19 21:29:20.599087 (TDB)
           2008 OCT 19 22:53:34.518737 (TDB)  2008 OCT 19 23:38:26.250580 (TDB)
           2008 NOV 04 19:54:40.339331 (TDB)  2008 NOV 04 20:15:38.620368 (TDB)
           2008 NOV 05 00:18:59.139978 (TDB)  2008 NOV 05 00:39:58.612935 (TDB)
           2008 NOV 20 19:21:46.689523 (TDB)  2008 NOV 20 19:38:59.647712 (TDB)
           2008 NOV 21 00:35:26.725908 (TDB)  2008 NOV 21 00:52:40.604703 (TDB)
           2008 DEC 06 18:42:36.100544 (TDB)  2008 DEC 06 18:58:34.073268 (TDB)
           2008 DEC 07 00:16:17.647040 (TDB)  2008 DEC 07 00:32:16.324244 (TDB)
           2008 DEC 22 17:47:10.776722 (TDB)  2008 DEC 22 18:02:46.288289 (TDB)
           2008 DEC 22 23:26:52.712459 (TDB)  2008 DEC 22 23:42:28.850542 (TDB)

         Condition: PARTIAL occultation of TITAN by SATURN
          Result window start, stop times:
           2008 OCT 27 21:37:16.970175 (TDB)  2008 OCT 27 22:08:01.627053 (TDB)
           2008 OCT 28 01:05:03.375236 (TDB)  2008 OCT 28 01:35:49.266506 (TDB)
           2008 NOV 12 21:01:47.105498 (TDB)  2008 NOV 12 21:21:59.252262 (TDB)
           2008 NOV 13 02:06:05.053051 (TDB)  2008 NOV 13 02:26:18.227357 (TDB)
           2008 NOV 28 20:31:28.522707 (TDB)  2008 NOV 28 20:49:02.402832 (TDB)
           2008 NOV 29 02:13:58.986344 (TDB)  2008 NOV 29 02:31:33.691598 (TDB)
           2008 DEC 14 19:48:27.094229 (TDB)  2008 DEC 14 20:05:09.246177 (TDB)
           2008 DEC 15 01:44:53.523002 (TDB)  2008 DEC 15 02:01:36.360243 (TDB)
           2008 DEC 30 18:44:23.485898 (TDB)  2008 DEC 30 19:00:56.577073 (TDB)
           2008 DEC 31 00:42:43.222909 (TDB)  2008 DEC 31 00:59:17.030568 (TDB)

         Condition: ANY occultation of SATURN by TITAN
          Result window start, stop times:
           2008 OCT 19 20:44:30.326771 (TDB)  2008 OCT 19 23:38:26.250580 (TDB)
           2008 NOV 04 19:54:40.339331 (TDB)  2008 NOV 05 00:39:58.612935 (TDB)
           2008 NOV 20 19:21:46.689523 (TDB)  2008 NOV 21 00:52:40.604703 (TDB)
           2008 DEC 06 18:42:36.100544 (TDB)  2008 DEC 07 00:32:16.324244 (TDB)
           2008 DEC 22 17:47:10.776722 (TDB)  2008 DEC 22 23:42:28.850542 (TDB)

         Condition: ANY occultation of TITAN by SATURN
          Result window start, stop times:
           2008 OCT 27 21:37:16.970175 (TDB)  2008 OCT 28 01:35:49.266506 (TDB)
           2008 NOV 12 21:01:47.105498 (TDB)  2008 NOV 13 02:26:18.227357 (TDB)
           2008 NOV 28 20:31:28.522707 (TDB)  2008 NOV 29 02:31:33.691598 (TDB)
           2008 DEC 14 19:48:27.094229 (TDB)  2008 DEC 15 02:01:36.360243 (TDB)
           2008 DEC 30 18:44:23.485898 (TDB)  2008 DEC 31 00:59:17.030568 (TDB)


-Restrictions
 
   The kernel files to be used by gfoclt_c must be loaded (normally via 
   the CSPICE routine furnsh_c) before gfoclt_c is called. 
 
-Literature_References
 
  None. 
 
-Author_and_Institution
 
  N. J. Bachman  (JPL) 
  L. S. Elson    (JPL) 
  E. D. Wright   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 07-APR-2009 (NJB) (LSE) (EDW)

-Index_Entries
 
   GF occultation search

-&
*/

{ /* Begin gfoclt_c */


   /*
   Local variables 
   */
   static const SpiceChar  * blankStr = " ";

   SpiceChar               * bFrameStr;
   SpiceChar               * fFrameStr;


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "gfoclt_c" );


   /*
   Make sure cell data types are d.p. 
   */
   CELLTYPECHK2 ( CHK_STANDARD, "gfoclt_c", SPICE_DP, cnfine, result );

   /*
   Initialize the input cells if necessary. 
   */
   CELLINIT2 ( cnfine, result );

   /*
   The input frame names are special cases because we allow the caller
   to pass in empty strings. If either of these strings are empty,
   we pass a null-terminated string containing one blank character to
   the underlying f2c'd routine. 

   First make sure the frame name pointers are non-null.
   */
   CHKPTR ( CHK_STANDARD, "gfoclt_c", bframe );
   CHKPTR ( CHK_STANDARD, "gfoclt_c", fframe );

   /*
   Use the input frame strings if they're non-empty; otherwise
   use blank strings for the frame names.
   */
  
   if ( bframe[0] )
   {
      bFrameStr = (SpiceChar *) bframe;
   }
   else
   {
      bFrameStr = (SpiceChar *) blankStr;
   }

   if ( fframe[0] )
   {
      fFrameStr = (SpiceChar *) fframe;
   }
   else
   {
      fFrameStr = (SpiceChar *) blankStr;
   }


   /*
   Check the other input strings to make sure each pointer is non-null 
   and each string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gfoclt_c", occtyp );
   CHKFSTR ( CHK_STANDARD, "gfoclt_c", front  );
   CHKFSTR ( CHK_STANDARD, "gfoclt_c", fshape );
   CHKFSTR ( CHK_STANDARD, "gfoclt_c", back   );
   CHKFSTR ( CHK_STANDARD, "gfoclt_c", bshape );
   CHKFSTR ( CHK_STANDARD, "gfoclt_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "gfoclt_c", obsrvr );


   /*
   Let the f2c'd routine do the work. 
   */
   gfoclt_ ( (char         *) occtyp,
             (char         *) front,
             (char         *) fshape,
             (char         *) fFrameStr,
             (char         *) back,
             (char         *) bshape,
             (char         *) bFrameStr,
             (char         *) abcorr,
             (char         *) obsrvr,
             (doublereal   *) &step,
             (doublereal   *) cnfine->base,
             (doublereal   *) result->base,
             (ftnlen        ) strlen(occtyp),
             (ftnlen        ) strlen(front),
             (ftnlen        ) strlen(fshape),
             (ftnlen        ) strlen(fframe),
             (ftnlen        ) strlen(back),
             (ftnlen        ) strlen(bshape),
             (ftnlen        ) strlen(bframe),
             (ftnlen        ) strlen(abcorr),
             (ftnlen        ) strlen(obsrvr)  );

   /*
   Sync the output result cell. 
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, result );
   }


   chkout_c ( "gfoclt_c" );

} /* End gfoclt_c */
