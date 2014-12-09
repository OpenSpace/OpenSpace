/*

-Procedure gfrepi_c ( GF, progress report initialization )

-Abstract
 
   This entry point initializes a search progress report. 
  
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
   #undef   gfrepi_c


   void gfrepi_c ( SpiceCell        * window,
                   ConstSpiceChar   * begmss,
                   ConstSpiceChar   * endmss  ) 
 
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   MXBEGM     P   Maximum progress report message prefix length. 
   MXENDM     P   Maximum progress report message suffix length. 
   window     I   A window over which a job is to be performed. 
   begmss     I   Beginning of the text portion of the output message. 
   endmss     I   End of the text portion of the output message. 
 
-Detailed_Input
 
   window   is the name of a constraint window. This is the window 
            associated with some root finding activity. It is 
            used to determine how much total time is being searched 
            in order to find the events of interest. 

 
   begmss   is the beginning of the progress report message 
            written to standard output by the GF subsystem.
            This output message has the form 
 
               begmss xxx.xx% endmss 

            For example, the progress report message created
            by the CSPICE routine gfocce_c at the completion
            of a search is

               Occultation/transit search 100.00% done.

            In this message, begmss is

               "Occultation/transit search"

            The total length of `begmss' must be less than 
            MXBEGM characters.

            All characters of `begmss' must be printable.

 
   endmss   is the last portion of the output message 
            written to standard output by the GF subsystem.

            The total length of `endmss' must be less than 
            MXENDM characters.

            All characters of `endmss' must be printable.

 
-Detailed_Output
 
  None.
 
-Parameters

   MXBEGM, 
   MXENDM    are, respectively, the maximum lengths of the progress 
             report message prefix and suffix. 

             Normally CSPICE developers will not need to reference
             these parameters; these are discussed only to help
             explain the functionality of this routine.

             The values of these parameters are defined in the
             SPICELIB Fortran INCLUDE file

                zzgf.inc
 
-Exceptions
 
   1) If `begmss' has length greater than MXBEGM characters, or if 
      `endmss' has length greater than MXENDM characters, the error 
      SPICE(MESSAGETOOLONG) is signaled. 
 
   2) If either `begmss' or `endmss' contains non-printing characters, 
      the error SPICE(NOTPRINTABLECHARS) is signaled. 

   3) The error SPICE(EMPTYSTRING) is signaled if the either input
      string does not contain at least one character, since the
      input string cannot be converted to a Fortran-style string
      in this case.
      
   4) The error SPICE(NULLPOINTER) is signaled if either input string
      pointer is null.

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
   L.S. Elson     (JPL) 
   W.L. Taber     (JPL) 
   I.M. Underwood (JPL) 
   E.D. Wright    (JPL)  
 
-Version
 
   -CSPICE Version 1.0.0, 28-FEB-2009 (NJB) (LSE) (WLT) (IMU) (EDW)

-Index_Entries
 
   GF initialize progress report
 
-&
*/

{ /* Begin gfrepi_c */

   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "gfrepi_c" );


   /*
   Check the input strings to make sure the pointers are non-null 
   and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gfrepi_c", begmss );
   CHKFSTR ( CHK_STANDARD, "gfrepi_c", endmss );

   /*
   Make sure cell data type is d.p. 
   */
   CELLTYPECHK ( CHK_STANDARD, "gfrepi_c", SPICE_DP, window );

   /*
   Initialize the window if necessary.
   */
   CELLINIT ( window );

  
   /*
   Let the f2c'd routine do the work.
   */
   gfrepi_  ( ( doublereal  * ) (window->base), 
              ( char        * ) begmss,
              ( char        * ) endmss,
              ( ftnlen        ) strlen(begmss),
              ( ftnlen        ) strlen(endmss)  );

   /*
   The cell is an input argument so no sync is necessary. 
   */

   chkout_c ( "gfrepi_c" );

} /* End gfrepi_c */
