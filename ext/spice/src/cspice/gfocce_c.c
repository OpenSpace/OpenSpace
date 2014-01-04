/*

-Procedure gfocce_c ( GF, occultation event )

-Abstract
 
   Determine time intervals when an observer sees one target 
   occulted by another. Report progress and handle interrupts 
   if so commanded. 
 
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
   #include <signal.h>
   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #include "SpiceZfc.h"
   #include "SpiceZad.h"


   void gfocce_c ( ConstSpiceChar     * occtyp,
                   ConstSpiceChar     * front,
                   ConstSpiceChar     * fshape,
                   ConstSpiceChar     * fframe,
                   ConstSpiceChar     * back,
                   ConstSpiceChar     * bshape,
                   ConstSpiceChar     * bframe,
                   ConstSpiceChar     * abcorr,
                   ConstSpiceChar     * obsrvr,
                   SpiceDouble          tol,

                   void             ( * udstep ) ( SpiceDouble       et,
                                                   SpiceDouble     * step ),

                   void             ( * udrefn ) ( SpiceDouble       t1,
                                                   SpiceDouble       t2,
                                                   SpiceBoolean      s1,
                                                   SpiceBoolean      s2,
                                                   SpiceDouble     * t    ),
                   SpiceBoolean         rpt,  

                   void             ( * udrepi ) ( SpiceCell       * cnfine,
                                                   ConstSpiceChar  * srcpre,
                                                   ConstSpiceChar  * srcsuf ),

                   void             ( * udrepu ) ( SpiceDouble       ivbeg,
                                                   SpiceDouble       ivend,
                                                   SpiceDouble       et      ),

                   void             ( * udrepf ) ( void ),
                   SpiceBoolean         bail,      
                   SpiceBoolean     ( * udbail ) ( void ),
                   SpiceCell          * cnfine,
                   SpiceCell          * result                                )

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   occtyp     I   Type of occultation. 
   front      I   Name of body occulting the other. 
   fshape     I   Type of shape model used for front body. 
   fframe     I   Body-fixed, body-centered frame for front body. 
   back       I   Name of body occulted by the other. 
   bshape     I   Type of shape model used for back body. 
   bframe     I   Body-fixed, body-centered frame for back body. 
   abcorr     I   Aberration correction flag. 
   obsrvr     I   Name of the observing body. 
   tol        I   Convergence tolerance in seconds. 
   udstep     I   Name of the routine that returns a time step. 
   udrefn     I   Name of the routine that computes a refined time. 
   rpt        I   Progress report flag. 
   udrepi     I   Function that initializes progress reporting. 
   udrepu     I   Function that updates the progress report. 
   udrepf     I   Function that finalizes progress reporting. 
   bail       I   Logical indicating program interrupt monitoring. 
   udbail     I   Name of a routine that signals a program interrupt. 
   cnfine    I-O  SPICE window to which the search is restricted. 
   result     O   SPICE window containing results. 
 
-Detailed_Input
 
 
   occtyp     indicates the type of occultation that is to be found. 
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
 
                 "PARTIAL"            denotes an partial, 
                                      non-annular occultation: the 
                                      body designated by `front' 
                                      blocks part, but not all, of 
                                      the limb of the body 
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
 
 
   fshape     is a string indicating the geometric model used 
              to represent the shape of the front body. The 
              supported options are: 
 
                 "ELLIPSOID"     Use a triaxial ellipsoid model, 
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
 
 
   bshape     is the shape specification for the body designated 
              by `back'. See the description of `fshape' above for 
              details. 
               
 
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
              the state of the target body to account for one-way 
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

  
   tol        is a tolerance value used to determine convergence of 
              root-finding operations. `tol' is measured in TDB seconds 
              and must be greater than zero. 
 
 
   udstep     is an externally specified routine that computes a 
              time step in an attempt to find a transition of the 
              state being considered. In the context of this 
              routine's algorithm, a "state transition" occurs where 
              the state changes from being "in occultation" to being 
              "not in occultation" or vice versa. 
          
              This routine relies on `udstep' returning step sizes 
              small enough so that state transitions within the 
              confinement window are not overlooked.  There must 
              never be two roots A and B separated by less than 
              `step', where `step' is the minimum step size returned by 
              `udstep' for any value of `et'; in the interval [A, B]. 
  
              The prototype for `udstep' is
 
                 void   ( * udstep ) ( SpiceDouble       et,
                                       SpiceDouble     * step )

              where: 
 
                 et      is the input start time from which the 
                         algorithm is to search forward for a state 
                         transition. `et' is expressed as seconds past 
                         J2000 TDB.  
 
                 step    is the output step size.  `step' indicates 
                         how far to advance `et' so that `et' and 
                         et+step may bracket a state transition and 
                         definitely do not bracket more than one 
                         state transition. Units are TDB seconds. 
 
              If a constant step size is desired, the CSPICE routine 

                 gfstep_c 
 
              may be used as the step size function. If gfstep_c is
              used, the step size must be set by calling gfsstp_c prior
              to calling this routine.
 
 
   udrefn     is the name of the externally specified routine that 
              computes a refinement in the times that bracket a 
              transition point. In other words, once a pair of 
              times have been detected such that the system is in 
              different states at each of the two times, `udrefn' 
              selects an intermediate time which should be closer to 
              the transition state than one of the two known times. 
              The prototype for `udrefn' is: 
 
                 void   ( * udrefn ) ( SpiceDouble       t1,
                                       SpiceDouble       t2,
                                       SpiceBoolean      s1,
                                       SpiceBoolean      s2,
                                       SpiceDouble     * t   )
 
              where the inputs are: 
 
                 t1    is a time when the system is in state `s1'. `t1'
                       is expressed as seconds past J2000 TDB.
 
                 t2    is a time when the system is in state `s2'. `t2'
                       is expressed as seconds past J2000 TDB. `t2' is
                       assumed to be larger than `t1'.
 
                 s1    is the state of the system at time t1.

                 s2    is the state of the system at time t2.
  
              The output is: 
 
                 t     is next time to check for a state transition.
                       `t' is a number between `t1' and `t2'. `t' is
                       expressed as seconds past J2000 TDB.
 
              If a simple bisection method is desired, the CSPICE routine 
              gfrefn_c may be used as the refinement function.
 
 
   rpt        is a logical variable which controls whether 
              progress reporting is enabled. When `rpt' is SPICETRUE, 
              progress reporting is enabled and the routines 
              udrepi, udrepu, and udpref (see descriptions below) 
              are used to report progress.  
  
 
   udrepi     is a user-defined subroutine that initializes a 
              progress report.  When progress reporting is  
              enabled, `udrepi' is called at the start 
              of a search. The prototype for `udrefi' is

                 void   ( * udrepi ) ( SpiceCell       * cnfine,
                                       ConstSpiceChar  * srcpre,
                                       ConstSpiceChar  * srcsuf )
 
              where 
 
                 cnfine  
 
              is a confinement window specifying the time period
              over which a search is conducted, and
  
                 srcpre 
                 srcsuf 
 
              are prefix and suffix strings used in the progress 
              report: these strings are intended to bracket a  
              representation of the fraction of work done. For
              example, when the CSPICE progress reporting functions
              are used, if srcpre and srcsuf are, respectively,

                 "Occultation/transit search"
                 "done."
 
              the progress report display at the end of
              the search will be:

                 Occultation/transit search 100.00% done.

              The CSPICE routine gfrepi_c may be used as the
              actual argument corresponding to `udrepi'. If so,
              the CSPICE routines gfrepu_c and gfrepf_c must be
              the actual arguments corresponding to `udrepu' and
              `udrepf'.
             
 
   udrepu     is a user-defined subroutine that updates the  
              progress report for a search.  The prototype
              of `udrepu' is 
  
                 void   ( * udrepu ) ( SpiceDouble       ivbeg,
                                       SpiceDouble       ivend,
                                       SpiceDouble       et      )
 
              In order for a meaningful progress report to be displayed,
              `ivbeg' and `ivend' must satisfy the following constraints:

                 - `ivbeg' must be less than or equal to `ivend'. 

                 - Over a search, the sum of the differences 

                      ivend - ivbeg

                   for all calls to this routine made during the search
                   must equal the measure (that is, the sum of the 
                   lengths of the intervals) of the confinement window
                   `cnfine'.

              `et' is the current time reached in the search for an event. 
              `et' must lie in the interval  

                 ivbeg : ivend 

              inclusive. The input values of `et' for a given interval 
              need not form an increasing sequence.  

              The CSPICE routine gfrepu_c may be used as the actual
              argument corresponding to `udrepu'. If so, the CSPICE
              routines gfrepi_c and gfrepf_c must be the actual
              arguments corresponding to `udrepi' and `udrepf'.
 
 
   udrepf     is a user-defined subroutine that finalizes a progress
              report. `udrepf' has no arguments.
 
              The CSPICE routine gfrepf_c may be used as the actual
              argument corresponding to `udrepf'. If so, the CSPICE
              routines gfrepi_c and gfrepu_c must be the actual
              arguments corresponding to `udrepi' and `udrepu'.
 
 
   bail       is a logical variable indicating whether or not 
              interrupt handling is enabled. When `bail' is 
              set to SPICETRUE, the input function `udbail' (see 
              description below) is used to determine whether 
              an interrupt has been issued. 
 
 
   udbail     is the name of a user defined logical function that 
              indicates whether an interrupt signal has been  
              issued (for example, from the keyboard). udbail
              has the prototype

                 SpiceBoolean   ( * udbail ) ( void )

              The return value is SPICETRUE if an interrupt has 
              been issued; otherwise the value is SPICEFALSE.

              gfocce_c uses `udbail' only when `bail' (see above) is set 
              to SPICETRUE, indicating that interrupt handling is 
              enabled. When interrupt handling is enabled, gfocce_c 
              and routines in its call tree will call `udbail' to 
              determine whether to terminate processing and return 
              immediately.                

              If the user doesn't wish to provide a custom interrupt
              handling function, the CSPICE routine

                 gfbail_c

              may be used.   

              The function `udbail' will be usually be tested
              multiple times by the GF system between the time
              an interrupt is issued and the time when 
              control is returned to the calling program, so
              `udbail' nmust continue to return SPICETRUE
              until explicitly reset by the calling application.
              So `udbail' must provide a "reset" mechanism."
              In the case of gfbail_c, the reset function is
              
                 gfclrh_c              

              If interrupt handing is not enabled, a logical 
              function must still be passed to gfocce_c as 
              an input argument. The CSPICE function  
 
                 gfbail_c 
       
              may be used for this purpose. 
 
              See the Examples header section below for a complete code
              example demonstrating use of the CSPICE interrupt
              handling capability.


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
              will be discarded before gfocce_c conducts its 
              search. 
 
-Parameters
 
   None.
 
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
       compute the requested state vector, the deficiency will 
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
 
   15) If any input string argument pointer is null, the error
       SPICE(NULLPOINTER) will be signaled.

   16) If any input string argument, other than `fframe' or `bframe',
       is empty, the error SPICE(EMPTYSTRING) will be signaled.

   17) If the convergence tolerance size is non-positive, the error 
       SPICE(INVALIDTOLERANCE) will be signaled. 
  
   18) If the occultation type is not recognized, the error 
       SPICE(INVALIDOCCTYPE) is signaled.

   19) If any attempt to change the handler for the interrupt 
       signal SIGINT fails, the error SPICE(SIGNALFAILURE) is
       signaled.

   20) If operation of this routine is interrupted, the output result
       window will be invalid.
 

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
 
   In all cases, kernel data are normally loaded once per program 
   run, NOT every time this routine is called.   
 
-Particulars
 
   This routine provides the SPICE GF system's most flexible 
   interface for searching for occultation events. 
  
   Applications that require do not require support for progress 
   reporting, interrupt handling, non-default step or refinement 
   functions, or non-default convergence tolerance normally should 
   call gfoclt_c rather than this routine. 
 
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
   transitions: times are sought when the state of the BACK body 
   changes from "not occulted" to "occulted" or vice versa. 
 
   Step Size 
   ========= 
 
   Each interval of the confinement window is searched as follows: 
   first, the input step size is used to determine the time 
   separation at which the occultation state will be sampled. 
   Starting at the left endpoint of an interval, samples will be 
   taken at each step. If a state change is detected, a root has 
   been bracketed; at that point, the "root"--the time at which the 
   state change occurs---is found by a refinement process, for 
   example, via binary search. 
 
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
   tolerance." The convergence tolerance used by high-level GF routines
   that call this routine is set via the parameter SPICE_GF_CNVTOL,
   which is declared in the header file SpiceGF.h.
  
   The value of SPICE_GF_CNVTOL is set to a "tight" value so that the 
   tolerance doesn't become the limiting factor in the accuracy of 
   solutions found by this routine. In general the accuracy of input 
   data will be the limiting factor. 
 
   Making the tolerance tighter than SPICE_GF_CNVTOL is unlikely to be
   useful, since the results are unlikely to be more accurate. Making
   the tolerance looser will speed up searches somewhat, since a few
   convergence steps will be omitted. However, in most cases, the step
   size is likely to have a much greater affect on processing time than
   would the convergence tolerance.
 
 
   The Confinement Window 
   ====================== 
 
   The simplest use of the confinement window is to specify a time
   interval within which a solution is sought. However, the confinement
   window can, in some cases, be used to make searches more efficient.
   Sometimes it's possible to do an efficient search to reduce the size
   of the time period over which a relatively slow search of interest
   must be performed. For an example, see the program CASCADE in the GF
   Example Programs chapter of the GF Required Reading, gf.req.
 
 
-Examples
 
 
   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Conduct a search using default GF progress reporting 
      and interrupt handling capabilities. 

      The program will use console I/O to display a simple
      ASCII-based progress report. 

      The program will trap keyboard interrupts (on most systems,
      generated by typing the "control C" key combination). This
      feature can be used in non-trivial applications to allow
      the application to continue after a search as been interrupted.
 
      The program will find occultations of the Sun by the Moon as seen
      from the center of the Earth over the month December, 2001.

      Use light time corrections to model apparent positions of Sun
      and Moon. Stellar aberration corrections are not specified
      because they don't affect occultation computations.
 
      We select a step size of 20 seconds, which implies we ignore
      occultation events lasting less than 20 seconds, if any exist.
      Given this step size and the length of the search interval, the
      user has time to interrupt the computation. In an interactive
      setting, the user might speed up the search by lengthening the
      step size or shortening the search interval, as long as these
      adjustments don't prevent the search from finding the correct
      solution.
 
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


         #include "SpiceUsr.h"
         #include <stdio.h>

         int main()
         {
            /.
            Constants 
            ./
            #define  TIMFMT  "YYYY MON DD HR:MN:SC.###### ::TDB (TDB)"
            #define  CNVTOL  1.e-6
            #define  MAXWIN  200
            #define  TIMLEN  41

            /.
            Local variables 
            ./
            SpiceBoolean            bail;
            SpiceBoolean            rpt;

            SpiceChar             * win0;
            SpiceChar             * win1;
            SpiceChar               begstr [ TIMLEN ];
            SpiceChar               endstr [ TIMLEN ];

            SPICEDOUBLE_CELL      ( cnfine, MAXWIN );
            SPICEDOUBLE_CELL      ( result, MAXWIN );

            SpiceDouble             et0;
            SpiceDouble             et1;
            SpiceDouble             left;
            SpiceDouble             right;

            SpiceInt                i;

            /.
            Load kernels.
            ./
            furnsh_c ( "standard.tm" );

            /.
            Obtain the TDB time bounds of the confinement
            window, which is a single interval in this case.
            ./
            win0 = "2001 DEC 10 00:00:00 TDB";
            win1 = "2002 JAN 01 00:00:00 TDB";

            str2et_c ( win0, &et0 );
            str2et_c ( win1, &et1 );

            /.
            Insert the time bounds into the confinement
            window.
            ./
            wninsd_c ( et0, et1, &cnfine );

            /.
            Select a twenty-second step. We'll ignore any occultations
            lasting less than 20 seconds.
            ./
            gfsstp_c ( 20.0 );

            /.
            Turn on interrupt handling and progress reporting.
            ./
            bail = SPICETRUE;
            rpt  = SPICETRUE;

            /.
            Perform the search.
            ./
            gfocce_c ( "ANY",                            
                       "MOON",     "ellipsoid",  "IAU_MOON", 
                       "SUN",      "ellipsoid",  "IAU_SUN",  
                       "LT",       "EARTH",      CNVTOL,    
                       gfstep_c,   gfrefn_c,     rpt,       
                       gfrepi_c,   gfrepu_c,     gfrepf_c, 
                       bail,       gfbail_c,     &cnfine,   
                       &result                              );


            if ( gfbail_c() ) 
            {
               /.
               Clear the CSPICE interrupt indication. This is
               an essential step for programs that continue
               running after an interrupt; gfbail_c will
               continue to return SPICETRUE until this step
               has been performed.
               ./
               gfclrh_c();


               /.
               We've trapped an interrupt signal. In a realistic
               application, the program would continue operation
               from this point. In this simple example, we simply
               display a message and quit.
               ./
               printf ( "\nSearch was interrupted.\n\nThis message "
                        "was written after an interrupt signal\n"
                        "was trapped. By default, the program "
                        "would have terminated \nbefore this message "
                        "could be written.\n\n"                       );
            }
            else
            {

               if ( wncard_c(&result) == 0 ) 
               {
                  printf ( "No occultation was found.\n" );
               }
               else
               {
                  for ( i = 0;  i < wncard_c(&result);  i++ )
                  {
                     /.
                     fetch and display each occultation interval.
                     ./
                     wnfetd_c ( &result, i, &left, &right );

                     timout_c ( left,  TIMFMT, TIMLEN, begstr );
                     timout_c ( right, TIMFMT, TIMLEN, endstr );

                     printf ( "Interval %ld\n", i );
                     printf ( "   Start time: %s\n", begstr );
                     printf ( "   Stop time:  %s\n", endstr );
                  }
               }

            }

            return ( 0 );
         }
 
      When this program was executed on a PC/Linux/gcc platform, the
      progress report had the format shown below:

         Occultation/transit search   6.02% done.

      The completion percentage was updated approximately once per
      second.

      When this program completed execution, the output was:
 

         Occultation/transit search 100.00% done.

         interval 0
            start time: 2001 DEC 14 20:10:14.195952  (TDB)
            stop time:  2001 DEC 14 21:35:50.317994  (TDB)


      When the program was interrupted at an arbitrary time, 
      the output was:

         Occultation/transit search  13.63% done.
         Search was interrupted.

         This message was written after an interrupt signal
         was trapped. By default, the program would have terminated
         before this message could be written.


-Restrictions
 
   1) If the caller passes in the default, constant step  
      size routine, gfstep_c, the caller must set the step 
      size by calling the entry point gfsstp_c before 
      calling gfocce_c. The call syntax for gfsstp_c is 
 
         gfsstp_c ( step );
 

-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
   L.S. Elson     (JPL) 
   W.L. Taber     (JPL) 
   I.M. Underwood (JPL) 
   E.D. Wright    (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 15-APR-2009 (NJB) (LSE) (WLT) (IMU) (EDW )

-Index_Entries
 
   GF mid-level occultation search

-&
*/

{ /* Begin gfocce_c */

   /*
   Prototypes 
   */
   void                  ( * defSigHandler ) (int);
   void                  ( * sigPtr        ) (int);

   /*
   Local variables
   */
   logical                   interrupt;
   logical                   rep; 

   SpiceBoolean              newHandler;

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
   chkin_c ( "gfocce_c" );
 
   /*
   Make sure cell data types are d.p. 
   */
   CELLTYPECHK2 ( CHK_STANDARD, "gfocce_c", SPICE_DP, cnfine, result );

   
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
   CHKPTR ( CHK_STANDARD, "gfocce_c", bframe );
   CHKPTR ( CHK_STANDARD, "gfocce_c", fframe );

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
   CHKFSTR ( CHK_STANDARD, "gfocce_c", occtyp );
   CHKFSTR ( CHK_STANDARD, "gfocce_c", front  );
   CHKFSTR ( CHK_STANDARD, "gfocce_c", fshape );
   CHKFSTR ( CHK_STANDARD, "gfocce_c", back   );
   CHKFSTR ( CHK_STANDARD, "gfocce_c", bshape );
   CHKFSTR ( CHK_STANDARD, "gfocce_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "gfocce_c", obsrvr );

      
   /*
   Assign the SpiceBoolean report and interrupt flags.
   */
   rep       = rpt ; 
   interrupt = bail;


   /*
   Store the input function pointers so these functions can be
   called by the GF adapters. 
   */
   zzadsave_c ( UDSTEP,  (void *)(udstep)  );
   zzadsave_c ( UDREFN,  (void *)(udrefn)  );
   zzadsave_c ( UDREPF,  (void *)(udrepf)  );
   zzadsave_c ( UDREPI,  (void *)(udrepi)  );
   zzadsave_c ( UDREPU,  (void *)(udrepu)  );
   zzadsave_c ( UDBAIL,  (void *)(udbail)  );


   /*
   If interrupt handling is enabled, and if the default bail-out
   routine gfbail_c is being used, set the SPICE interrupt 
   handler.
   */

   newHandler = SPICEFALSE;

   if ( bail ) 
   {
      newHandler = (  (void *)udbail == (void *)gfbail_c );

      if ( newHandler )
      {
         defSigHandler = signal ( SIGINT, gfinth_c );

         if ( defSigHandler == SIG_ERR )
         {
            setmsg_c ( "Attempt to establish the CSPICE routine "
                       "gfinth_c as the handler for the interrupt "
                       "signal SIGINT failed."                     );
            sigerr_c ( "SPICE(SIGNALFAILED)"                       );
            chkout_c ( "gfocce_c"                                  );
            return;
         }
      }
   }


   /*
   Let the f2c'd routine do the work. 

   We pass the adapter functions, not those provided as inputs,
   to the f2c'd routine.
   */

   gfocce_ ( ( char         * )  occtyp,
             ( char         * )  front,
             ( char         * )  fshape,
             ( char         * )  fframe,
             ( char         * )  back,
             ( char         * )  bshape,
             ( char         * )  bframe,
             ( char         * )  abcorr,
             ( char         * )  obsrvr,
             ( doublereal   * )  &tol,
             ( U_fp           )  zzadstep_c,
             ( U_fp           )  zzadrefn_c,
             ( logical      * )  &rep,
             ( S_fp           )  zzadrepi_c,
             ( U_fp           )  zzadrepu_c,
             ( S_fp           )  zzadrepf_c,
             ( logical      * )  &interrupt, 
             ( L_fp           )  zzadbail_c,
             ( doublereal   * )  (cnfine->base),
             ( doublereal   * )  (result->base),
             ( ftnlen         )  strlen(occtyp),
             ( ftnlen         )  strlen(front),
             ( ftnlen         )  strlen(fshape),
             ( ftnlen         )  strlen(fframe),
             ( ftnlen         )  strlen(back),
             ( ftnlen         )  strlen(bshape),
             ( ftnlen         )  strlen(bframe),
             ( ftnlen         )  strlen(abcorr),
             ( ftnlen         )  strlen(obsrvr)  );

   /*
   If we've changed the signal handler, restore the previous one.
   */
   if ( newHandler )
   {
      sigPtr = signal ( SIGINT, defSigHandler );

      if ( sigPtr == SIG_ERR )
      {
         setmsg_c ( "Attempt to restore the previous handler "
                    "for the interrupt signal SIGINT failed."  );
         sigerr_c ( "SPICE(SIGNALFAILED)"                      );
         chkout_c ( "gfocce_c"                                 );
         return;
      }
   }

   /*
   Sync the output cell. 
   */
   if ( !failed_c() )
   {
     zzsynccl_c ( F2C, result ) ;
   }


   chkout_c ( "gfocce_c" );

} /* End gfocce_c */
