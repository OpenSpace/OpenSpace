/*

-Procedure cnmfrm_c ( Center name to associated frame )

-Abstract
 
   Retrieve frame ID code and name to associate with an object. 
 
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
 
   FRAMES 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"
   

   void cnmfrm_c ( ConstSpiceChar   * cname,
                   SpiceInt           lenout,
                   SpiceInt         * frcode,
                   SpiceChar        * frname,
                   SpiceBoolean     * found   ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   cname      I   Name of the object to find a frame for.
   lenout     I   Maximum length available for frame name.
   frcode     O   The ID code of the frame associated with cname.
   frname     O   The name of the frame with ID frcode.
   found      O   SPICETRUE if the requested information is available. 
 
-Detailed_Input
 
   cname          is the name for object for which there is a 
                  preferred reference frame.
 
   lenout         is the amount of space available, counting the
                  space required for the terminating null character,
                  in the output string frname.  Normally lenout is
                  the declared length of frname.
                  
-Detailed_Output
 
   frcode         is the frame id-code to associate with a the object 
                  specified by cname. 
 
   frname         is the name of the frame that should be associated 
                  with the object specified by cname. 
 
   found          is SPICETRUE if the appropriate frame id-code and 
                  frame name can be determined.  Otherwise found is
                  returned with the value SPICEFALSE. 
 
-Parameters
 
   None. 
 
-Files
 
   None. 
 
-Exceptions
 
    1) If either cname or frname is a null pointer, the error 
       SPICE(NULLPOINTER) will be signaled.
       
    2) If cname has length zero, the error SPICE(EMPTYSTRING) will
       be signaled.
       
    3) If lenout is less than 1, the error SPICE(STRINGTOOSHORT) will
       be signaled. 
 
    4) If the output string is too short to contain the frame name, 
       the result is truncated on the right.  The output string is still 
       null-terminated.
       
-Particulars
 
   This routine allows the caller to determine the frame that should 
   be associated with a particular object.  For example, if you 
   need the frame to associate with the Io, you can call cnmfrm_c 
   to determine the frame name and id-code for the bodyfixed frame 
   of Io. 
 
   The preferred frame to use with an object is specified via one 
   of the kernel pool variables: 
 
       OBJECT_<cname>_FRAME 
 
   where <cname> is the non-blank portion of the string CNAME. 
 
   For those PCK objects that have "built-in" frame names this 
   routine returns the corresponding "IAU" frame and frame ID code. 
 
-Examples
 
   Suppose that you want to determine the state of a target 
   in the preferred reference frame of some observer.  This 
   routine can be used in conjunction with spkezr_c to compute 
   the state. 
 
      #include <stdio.h>
      #include <stdlib.h>
      #include "SpiceUsr.h"
          .
          .
          .
      #define LENOUT     80
      
      cnmfrm_c ( obsnam, LENOUT, &frcode, frname, &found );
 
      if ( !found ) 
      {
         printf ( "The bodyfixed frame for object %s "
                  "could not be identified.\n",        
                  obsnam                               );
         exit(1);
      }
 
      spkezr_c ( target, et, frname, abcorr, obsnam, state, &lt );
       
 
-Restrictions
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.0, 25-JUN-1999 (NJB) (WLT)

-Index_Entries
 
   Fetch reference frame attributes 
 
-&
*/

{ /* Begin cnmfrm_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "cnmfrm_c" );

   /*
   Check the input object's name string to make sure the pointer
   is non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "cnmfrm_c", cname );

   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "cnmfrm_c", frname, lenout );

   
   /*
   Invoke the f2c'd routine.
   */
   cnmfrm_ (  ( char      * ) cname,
              ( integer   * ) frcode,
              ( char      * ) frname,
              ( logical   * ) found,
              ( ftnlen      ) strlen(cname), 
              ( ftnlen      ) lenout-1       ); 
              

   /*
   Convert the output string to C-style.
   */
   F2C_ConvertStr ( lenout, frname );


   chkout_c ( "cnmfrm_c" );

} /* End cnmfrm_c */
