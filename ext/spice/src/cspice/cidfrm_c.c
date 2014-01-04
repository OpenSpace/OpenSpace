/*

-Procedure cidfrm_c ( center SPK ID frame )

-Abstract
 
   Retrieve frame ID code and name to associate with a frame center. 
 
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
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   

   void cidfrm_c ( SpiceInt        cent,
                   SpiceInt        lenout,
                   SpiceInt      * frcode,
                   SpiceChar     * frname,
                   SpiceBoolean  * found  ) 
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   cent       I   An object to associate a frame with. 
   lenout     I   Available space in output string frname.
   frcode     O   The ID code of the frame associated with cent.
   frname     O   The name of the frame with ID frcode.
   found      O   SPICETRUE if the requested information is available. 
 
-Detailed_Input
 
   cent        is the ID code for object for which there is a 
               preferred reference frame. 
               
   lenout      is the available space in the output string frname,
               including room for the terminating null character.
 
-Detailed_Output
 
   frcode      is the frame ID code to associate with the object 
               specified by cent. 
 
   frname      is the name of the frame that should be associated 
               with the object specified by cent. 
 
   found       is SPICETRUE if the appropriate frame ID code and frame 
               name can be determined.  Otherwise found is returned 
               with the value SPICEFALSE. 
 
-Parameters
 
   None. 
 
-Files
 
   None. 
 
-Exceptions
 
   None. 
 
-Particulars
 
   This routine allows the user to determine the frame that should 
   be associated with a particular object.   For example, if you 
   need the frame to associate with the Io, you can call cidfrm_c 
   to determine the frame name and ID code for the bodyfixed frame 
   of Io. 
 
   The preferred frame to use with an object is specified via one 
   of the kernel pool variables: 
 
       OBJECT_<cent>_FRAME 
 
   where <cent> is the decimal representation of the integer cent. 
 
   For those PCK objects that have "built-in" frame names this 
   routine returns the corresponding "IAU" frame and frame ID code. 
 
-Examples
 
   Suppose that you want to determine the state of a target in the
   preferred reference frame of some observer.  This routine can be
   used in conjunction with spkezr_c to compute the state.
 
      #include <stdlib.h>
      #include <stdio.h>
      #include "SpiceUsr.h"
      
      #define LENOUT   32
           .
           .
           .
           
      cidfrm_c ( obs, LENOUT, &frcode, frname, &found ); 
      
      if ( !found )
      {
         printf ( "The bodyfixed frame for object %d\n" 
                  "could not be identified.\n",
                  obs                                   );
         exit(1);
      }
 
      spkezr_c ( targ, et, frname, abcorr, obs, state, &lt );
       
 
-Restrictions
 
   None. 
 
-Author_and_Institution
 
    N.J. Bachman    (JPL)
    W.L. Taber      (JPL) 
 
-Literature_References
 
    None. 
 
-Version
 
   -CSPICE Version 1.0.0, 22-JUL-1999 (NJB) (WLT)

-Index_Entries
 
   Fetch reference frame attributes 
 
-&
*/

{ /* Begin cidfrm_c */

   /*
   Local variables
   */
   logical                 fnd;
   


   /*
   Participate in error tracing.
   */
   chkin_c ( "cidfrm_c" );


   /*
   Check the output string to make sure the pointer is non-null and that
   there is room for at least one character plus a null terminator.
   */
   CHKOSTR ( CHK_STANDARD, "cidfrm_c", frname, lenout );
   

   /*
   Call the f2c'd routine.  
   */
   
   cidfrm_ (  ( integer   * ) &cent,
              ( integer   * ) frcode,
              ( char      * ) frname,
              ( logical   * ) &fnd,
              ( ftnlen      ) lenout-1 );
   
   /*
   Convert the output string from Fortran to C style.
   */
   F2C_ConvertStr ( lenout, frname );
   

   /*
   Set the SpiceBoolean found flag.
   */
   
   *found = fnd;
   

   chkout_c ( "cidfrm_c" );

} /* End cidfrm_c */
