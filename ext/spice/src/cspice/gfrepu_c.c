/*

-Procedure gfrepu_c ( GF, progress report update )

-Abstract
 
   This function tells the progress reporting system  
   how far a search has progressed. 
   
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
 
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"

   void gfrepu_c ( SpiceDouble ivbeg,
                   SpiceDouble ivend,
                   SpiceDouble time  ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  --------------------------------------------------
   ivbeg      I   Start time of work interval. 
   ivend      I   End time of work interval.  
   time       I   Current time being examined in the search process.
 
-Detailed_Input  
 
   ivbeg, 
   ivend    are the bounds of a time interval. Normally this interval
            is contained within the confinement window `cnfine' passed to
            gfrepi_c on the latest call to that function, but this is
            not a requirement.
 
            In order for a meaningful progress report to be displayed,
            `ivbeg' and `ivend' must satisfy the following constraints:
  
               - `ivbeg' must be less than or equal to `ivend'. 
 
               - Over a search pass, the sum of the differences 
 
                    ivend - ivbeg
 
                 for all calls to this routine made during the pass 
                 must equal the measure (that is, the sum of the 
                 lengths of the intervals) of the confinement window
                 `cnfine'.
 

   time     is the current time reached in the search for an event. 
            `time' must lie in the interval  
 
               ivbeg : ivend 
 
            inclusive. The input values of `time' for a given interval 
            need not form an increasing sequence.  


-Detailed_Output
 
   None. This routine does perform console I/O when progress 
   reporting is enabled. 
 
-Parameters
 
   None.
 
-Exceptions
 
   1)  If `ivbeg' and `ivend' are in decreasing order, the error
       SPICE(BADENDPOINTS) is signaled.
 
   2)  If `time' is not in the closed interval [ivbeg, ivend], the
       error SPICE(VALUEOUTOFRANGE) is signaled.
 
   3)  Any I/O errors resulting from writing to standard output will be
       diagnosed by routines in the call tree of this routine.
 
-Files
 
   None. 
 
-Particulars
   
   This is one of three GF progress reporting routines that cooperate
   in order to display a report via console I/O. These routines may 
   be used by SPICE-based applications as inputs to mid-level GF 
   search routines.
 
   Developers wishing to use their own GF progress reporting routines
   must design them with the same interfaces and should assign them the
   same progress reporting roles as those of these routines.

   The GF progress reporting API routines are written to simplify
   reporting of work (such as searching for a geometric event) over a
   particular window. This is an important feature for interactive
   programs that may "go away" from the user's control for a
   considerable length of time. It allows the user to see that
   something is still going on (although maybe not too quickly).
 
   The three routines constituting the GF progress reporting API
   are: 
 
      gfrepi_c  is used to prepare the reporting mechanism for a search
                pass. It is used to store the confinement window and
                progress report message prefix and suffix, and to
                initialize parameters associated with the reporting of
                the job in progress.
 
      gfrepu_c  is used to notify the progress reporting system that
                a specified increment of work has been completed
                since the last call to gfrepu_c or gfrepi_c, whichever
                occurred most recently.
 
      gfrepf_c  is used to "finish" the reporting of work (set the
                completion value to 100%. 
 
-Examples


   1)  This example shows how to call a mid-level GF search API that 
       requires as input progress reporting routines. 
 
       If custom progress reporting routines are available, they 
       can replace gfrepi_c, gfrepu_c, and gfrepf_c in any GF API calls. 
 
       The code fragment below is from the first code example in the 
       header of 
 
          gfocce_c.c
 
       Only the portions of that program relevant to use of the 
       progress reporting routines are copied here.
       
          /.
          Select a twenty-second step. We'll ignore any occultations
          lasting less than 20 seconds.
          ./
          gfsstp_c ( 20.0 );

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

-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL)
   W.L. Taber     (JPL) 
   I.M. Underwood (JPL) 
   L.S. Elson     (JPL)
   E.D. Wright    (JPL)  
 
 
-Version
 
   -CSPICE Version 1.0.0, 28-FEB-2009 (NJB) (LSE) (WLT) (IMU) (EDW)

-Index_Entries
 
   GF update progress report
 
-&
 
*/{ /* Begin gfrepu_c */

   /*
   Participate in error tracing.
   */

   if ( return_c() )
   {
      return;
   }
   chkin_c ( "gfrepu_c" );

   /*
   Let the f2c'd routine do the work.
   */
   gfrepu_  ( ( doublereal * ) &ivbeg,
              ( doublereal * ) &ivend,
              ( doublereal * ) &time   );

   chkout_c ( "gfrepu_c" );

} /* End gfrepu_c */
