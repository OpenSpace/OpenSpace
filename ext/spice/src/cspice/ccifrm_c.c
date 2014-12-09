/*

-Procedure ccifrm_c ( Class and class ID to associated frame )

-Abstract
 
   Return the frame name, frame ID, and center associated with 
   a given frame class and class ID. 
 
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
 
-Keywords
 
   FRAMES 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void ccifrm_c ( SpiceInt          frclss,
                   SpiceInt          clssid,
                   SpiceInt          lenout,
                   SpiceInt        * frcode,
                   SpiceChar       * frname,
                   SpiceInt        * center,
                   SpiceBoolean    * found   )

 
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   frclss     I   Class of frame. 
   clssid     I   Class ID of frame. 
   lenout     I   Maximum length of output string.
   frcode     O   ID code of the frame.
   frname     O   Name of the frame.
   center     O   ID code of the center of the frame.
   found      O   SPICETRUE if the requested information is available. 
 
-Detailed_Input
 
   frclss     is the class or type of the frame. This identifies which
              subsystem will be used to perform frame transformations.
 
   clssid     is the ID code used for the frame within its class. This
              may be different from the frame ID code.

   lenout     The allowed length of the output frame name. This length
              must large enough to hold the output string plus the
              null terminator.  If the output string is expected to have 
              n characters, `lenout' should be n + 1.  

-Detailed_Output
 
   frcode      is the frame ID code for the reference frame  
               identified by `frclss' and `clssid'.
 
   frname      is the name of the frame identified by  
               `frclss' and `clssid'.

               `frname' should be declared 

                   SpiceChar frname [33]
 
               to ensure that it can contain the full name of the
               frame.  If `frname' does not have enough room to hold
               the full name of the frame, the name will be truncated
               on the right.
 
   center      is the body ID code for the center of the reference 
               frame identified  by `frclss' and `clssid'.

   found       is SPICETRUE if a valid frame specification
               corresponding to the input frame class and frame class
               ID is available, in which case the other outputs are
               valid. Otherwise, `found' is returned with the value
               SPICEFALSE.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) This routine assumes that the first frame found with matching 
      class and class ID is the correct one. SPICE's frame system 
      does not diagnose the situation where there are multiple, 
      distinct frames with matching classes and class ID codes, but 
      this situation could occur if such conflicting frame 
      specifications are loaded via one or more frame kernels. The 
      user is responsible for avoiding such frame specification 
      conflicts. 
 
   2) If `frname' does not have room to contain the frame name, the 
      name will be truncated on the right. (Declaring `frname' to have 
      a length of 33 characters will ensure that the name will not be 
      truncated. 
 
   3) If a frame class assignment is found that associates a  
      string (as opposed to numeric) value with a frame class 
      keyword, the error SPICE(INVALIDFRAMEDEF) will be signaled. 
 
   4) If a frame class assignment is found that matches the input 
      class, but a corresponding class ID assignment is not 
      found in the kernel pool, the error SPICE(INVALIDFRAMEDEF)  
      will be signaled. 
 
   5) If a frame specification is found in the kernel pool with 
      matching frame class and class ID, but either the frame name 
      or frame ID code are not found, the error 
      SPICE(INVALIDFRAMEDEF) will be signaled. 
 
   6) If a frame specification is found in the kernel pool with 
      matching frame class and class ID, but the frame center 
      is not found, the error will be diagnosed by routines 
      in the call tree of this routine. 
 
   7) The error SPICE(NULLPOINTER) is signaled if the output string
      pointer is null.

   8) The caller must pass a value indicating the length of the output
      string. If this value is not at least 2, the error
      SPICE(STRINGTOOSHORT) is signaled.


-Files
 
   The frame specifications sought by this routine may be provided
   by loaded frames kernels. Such kernels will always be required if 
   the frame class is CK, TK, or dynamic, and will be required if
   the frame class is PCK but the frame of interest is not built-in.

-Particulars
 
   This routine allows the user to determine the frame associated 
   with a given frame class and class ID code. The kernel pool is 
   searched first for a matching frame; if no match is found, then 
   the set of built-in frames is searched. 
 
   Since the neither the frame class nor the class ID are primary 
   keys, searching for matching frames is a linear (and therefore 
   typically slow) process. 
 
-Examples
 
   Suppose that you want to find the frame information of a named frame,
   "ITRF93" for this example. One could use the following code fragment: 
 
      #include <stdlib.h>
      #include <stdio.h>
      #include "SpiceUsr.h"
      #include "SpiceZfc.h"
   
      #define FRNAMLEN       33

      SpiceChar              frname[FRNAMLEN];
      SpiceInt               clss;
      SpiceInt               clss_ID;

      SpiceInt               frcode1;
      SpiceInt               frcode2;

      SpiceInt               center1;
      SpiceInt               center2;

      SpiceBoolean           found;

      int main()
         {

         namfrm_ ( "ITRF93", &frcode1, 6 );

         frinfo_c ( frcode1, 
                    &center1, &clss, &clss_ID, &found );

         ccifrm_c ( clss, clss_ID, FRNAMLEN, 
                    &frcode2,  frname, &center2, &found );

         if ( !found )
            { 
            puts( "No joy");
            exit(1);
            }

         printf(  "Class     : %d \n"
                  "Class ID  : %d \n"
                  "Frame name: %s \n"
                  "Frame Code: %d \n"
                  "Center ID : %d \n", 
                  clss, clss_ID, frname, frcode2, center2 );

         exit(0);
         }

   The program outputs:

      Class     : 2 
      Class ID  : 3000 
      Frame name: ITRF93 
      Frame Code: 13000 
      Center ID : 399

-Restrictions
 
   See item (1) in the Exceptions section above. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 14-JUL-2014 (NJB)

       Added to the Brief_I/O header section a description 
       of input argument `lenout'.

    Last update was  10-JAN-2011 (NJB)(EDW)

-Index_Entries
 
   Find info associated with a frame class and class id 
   Map frame class and class id to frame info 
   Map frame class and class id to frame name, id, and center 
 
-&
*/

{ /* Begin ccifrm_c */

  

   /*
   Local variables
   */
   logical                 fnd;

   /*
   Participate in error tracing.
   */
   chkin_c ( "ccifrm_c" );

   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "ccifrm_c", frname, lenout );

   /*
   Map the inputs to frame attributes, if possible.
   */
   ccifrm_( ( integer * ) &frclss,
            ( integer * ) &clssid,
            ( integer * ) frcode,
            ( char    * ) frname,
            ( integer * ) center,
            ( logical * ) &fnd,
            ( ftnlen    ) lenout-1 );

   /*
   The string returned, output, is a Fortranish type string.
   Convert the string to C style. 
   */
   F2C_ConvertStr ( lenout, frname );

   /*
   Return the FOUND flag as a SpiceBoolean value.
   */

   *found = (SpiceBoolean)fnd;


   chkout_c ( "ccifrm_c" );

} /* End ccifrm_c */
