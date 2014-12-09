/*

-Procedure frmnam_c (Frame to Name)

-Abstract
 
   Retrieve the name of a reference frame associated with 
   a SPICE ID code. 
 
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


   void frmnam_c ( SpiceInt      frcode,
                   SpiceInt      lenout,
                   SpiceChar *   frname  )
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   frcode     I   an integer code for a reference frame 
   lenout     I   Maximum length of output string.
   frname     O   the name associated with the reference frame. 
 
-Detailed_Input
 
   frcode         is an integer code for a reference frame. 
 
   lenout         is the maximum number of characters that can be 
                  accommodated in the output string.  This count 
                  includes room for the terminating null character.
                  For example, if the maximum allowed length of the 
                  output string, including the terminating null, is 33
                  characters, then lenout should be set to 33.
 
-Detailed_Output
 
   frname      is the name associated with the reference frame. 
               It will be returned left-justified. 
 
               If frcode is not recognized as the name of a 
               known reference frame, frname will be returned 
               as an empty string. 
 
               If frname is not sufficiently long to hold the 
               name, it will be truncated on the right. 
 
               All reference frame names are 32 or fewer characters 
               in length.  Thus declaring frname to be SpiceChar[33] 
               will ensure that the returned name will not be 
               truncated. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If frcode is not recognized as the name of a known reference
      frame, frname will be returned as a blank. 
 
   2) If the output string pointer is null, the error SPICE(NULLPOINTER)
      is signaled.
      
   3) If the output string has length less than two characters, it 
      is too short to contain one character of output data plus a null  
      terminator, so it cannot be passed to the underlying Fortran 
      routine.  In this event, the error SPICE(STRINGTOOSHORT) is
      signaled.
      
   4) If the length of frname (indicated by lenout) is at least two
      characters but not large enough to contain the output string, 
      the output string will be truncated on the right. 
 
-Files
 
   None. 
 
-Particulars
 
   This routine retrieves the name of a reference frame associated 
   with a SPICE frame ID code. 
 
   The ID codes stored locally are scanned for a match with frcode. 
   If a match is found, the name stored locally will be returned 
   as the name for the frame. 
 
   If frcode is not a member of the list of internally stored 
   ID codes, the kernel pool will be examined to see if the 
   variable 
 
      FRAME_idcode_NAME 
 
   is present (where idcode is the decimal character equivalent 
   of frcode).  If the variable is located and it has both 
   character type and dimension 1, the string value of the 
   kernel pool variable is returned as the name of the reference 
   frame. 
 
   Note that because the local information is always examined 
   first and searches of the kernel pool are performed only 
   after exhausting local information, it is not possible to 
   override the local name for any reference frame that is 
   known by this routine. 
 
-Examples
 
   Suppose you needed to create a message concerning a reference 
   frame and wish to use the name of the frame in the message. 
   Suppose further that you have only the frame ID code at your 
   disposal.  You can capture the frame name using this routine 
   as shown here. 
 
      #include "SpiceUsr.h"   
           .
           .
           .
      #define NAMELEN         33
      
      SpiceChar               frname [NAMELEN];
      SpiceInt                frcode;

 
      frmnam_c ( frcode, NAMELEN, frname );
 
      if ( iswhsp_c(frname) )  
      { 
         sprintf ( frname, "%ld", frcode );
      }

      printf ( "Concerning reference frame: %s\n", frname );
 
        [Print the rest of your message.]
        
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   W.L. Taber      (JPL) 
   B.V. Semenov    (JPL) 
   N.J. Bachman    (JPL)
   
-Version
 
   -CSPICE Version 1.0.2, 08-JAN-2014 (BVS) 

       Fixed typo in Examples (frname_c -> frmnam_c). Reordered
       header sections.

   -CSPICE Version 1.0.1, 26-MAR-2003 (NJB) 

       Fixed description of exception (4):  replaced "lenout-1"
       with "lenout."  Removed spurious word "clock" from string
       description.

   -CSPICE Version 1.0.0, 13-AUG-2001 (NJB) (WLT)

-Index_Entries
 
   Frame idcode to frame name translation   

-&
*/

{ /* Begin frmnam_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "frmnam_c" );

   /*
   Make sure the output frmnam has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "frmnam_c", frname, lenout );


   /*
   Do the conversion.
   */
   frmnam_ ( ( integer * ) &frcode, 
             ( char    * ) frname, 
             ( ftnlen    ) lenout-1 );
      
   /*
   Convert the Fortran string to a C string by placing a null
   after the last non-blank character.  This operation is valid
   whether or not the CSPICE routine signaled an error.
   */
   F2C_ConvertStr ( lenout, frname );


   chkout_c ( "frmnam_c" );

} /* End frmnam_c */

