/*

-Procedure namfrm_c (Name to frame)

-Abstract
 
   Look up the frame ID code associated with a string. 
 
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

   void namfrm_c ( ConstSpiceChar   * frname,
                   SpiceInt         * frcode  ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   frname     I   The name of some reference frame.
   frcode     O   The SPICE ID code of the frame. 
 
-Detailed_Input
 
   frname      is a character string that stands for some 
               reference frame (either inertial or non-inertial). 
 
               Leading blanks in frname are ignored.  The 
               case of the letters in frname are insignificant. 
 
               Note that all legitimate frame names contain 
               32 or fewer characters. 
 
-Detailed_Output
 
   frcode      is the SPICE integer code used for internal 
               representation of the named reference frame. 
 
               If the name input through frname is not recognized,
               frcode will be returned with a value of zero. 
 
-Parameters
 
   None. 
 
-Files
 
   None. 
 
-Exceptions
 
   1) If the input name is not recognized, frcode will be 
      returned with a value of 0. 
 
   2) If the input string pointer is null, the error
      SPICE(NULLPOINTER) will be signaled.

   3) If the input string has length zero, the error
      SPICE(EMPTYSTRING) will be signaled.

-Particulars
 
   This is a low level interface routine intended primarily for 
   use within the SPK and CK systems to assist in the transformation 
   to user specified reference frames. 
 
   The routine first consults a stored list of reference frame 
   names in an attempt to determine the appropriate reference 
   frame code. 
 
   If this search is unsuccessful, the routine then examines the 
   kernel pool to determine whether or not a variable of the 
   form 
 
      "FRAME_"<frname> 
 
      (where leading blanks of frname are ignored) 
 
   is present.  If it is and the number of values associated with the 
   name is 1, this value is taken to be the frame ID code. 
 
   Note:  It is NOT possible to override the default names and 
   ID codes stored locally in this routine by placing an 
   appropriately named variable in the kernel pool with a different 
   ID code.  The predefined values always take precedence. 
 
   Consult the FRAMES required reading document for more details 
   about constructing your own frame definitions. 
 
-Examples
 
   Suppose that you needed to find the SPICE ID code for the 
   bodyfixed reference frame for Mars as modeled by the 
   IAU cartographic working group.  Use the following code 
   to perform this task. 
 
      #include "SpiceUsr.h"
          .
          .
          .
          
      namfrm_c ( "IAU_MARS", &frcode );
 
      printf ( "The SPICE code for the Mars bodyfixed frame is: %d\n",
                frcode                                                );
 
-Restrictions
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.0, 13-AUG-2001 (NJB) (WLT)

-Index_Entries

   Frame name to frame idcode translation

-&
*/

{ /* Begin namfrm_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "namfrm_c" );

   /*
   Check the input string to make sure the pointer
   is non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "namfrm_c", frname );
   
   namfrm_ ( ( char    * ) frname,
             ( integer * ) frcode,
             ( ftnlen    ) strlen(frname) );


   chkout_c ( "namfrm_c" );

} /* End namfrm_c */
