/*

-Procedure repmf_c  ( Replace marker with formatted d.p. value )

-Abstract
 
   Replace a marker in a string with a formatted double precision 
   value. 
 
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
 
   CHARACTER 
   CONVERSION 
   STRING 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void repmf_c ( ConstSpiceChar     * in,
                  ConstSpiceChar     * marker,
                  SpiceDouble          value,
                  SpiceInt             sigdig,
                  SpiceChar            format,
                  SpiceInt             lenout,
                  SpiceChar          * out ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   in         I   Input string. 
   marker     I   Marker to be replaced. 
   value      I   Replacement value.
   sigdig     I   Significant digits in replacement text.
   format     I   Format: 'E' or 'F'. 
   lenout     I   Available space in output string.
   out        O   Output string. 
   MAXLFD     P   Maximum length of a formatted DP number. 
 
-Detailed_Input
 
   in             is an arbitrary character string. 
 
   marker         is an arbitrary character string. The first occurrence 
                  of marker in the input string is to be replaced by value. 
 
                  Leading and trailing blanks in marker are NOT significant. 
                  In particular, no substitution is performed if marker 
                  is blank. 
 
   value          is an arbitrary double precision number.
 
   sigdig         is the number of significant digits with which value
                  is to be represented. sigdig must be greater than
                  zero and less than 15.
 
   format         is the format in which value is to be represented. 
                  format may be any of the following: 
 
                     format  Meaning      Example 
                     ------  -----------  ---------------- 
                     E, e    Scientific   3.14159E+03 
                             (exponent) 
                             notation 
 
                     F, f    Fixed-point  3141.59 
                             notation 

   lenout         is the allowed length of the output string.  This length
                  must large enough to hold the output string plus the
                  terminator.  If the output string is expected to have x
                  characters, lenout should be at least x + 1.
 
-Detailed_Output
 
   out            is the string obtained by substituting the text 
                  representation of value for the first occurrence 
                  of marker in the input string. 
 
                  The text representation of value is in scientific 
                  (exponent) or fixed-point notation, depending on 
                  having the value of format, and having the number 
                  of significant digits specified by sigdig. 
                  The representation of value is produced by the 
                  routine dpstrf_; see that routine for details 
                  concerning the representation of double precision 
                  numbers. 
 
                  out and in must be identical or disjoint. 
 
-Parameters
 
   MAXLFD         is the maximum expected length of the text 
                  representation of a formatted double precision 
                  number. 56 characters are sufficient to hold any 
                  result returned by dpstrf_. (See $Restrictions.) 
 
-Exceptions
  
   1) The error SPICE(NULLPOINTER) is signaled if any of 
      the input or output string pointers is null.

   2) If the marker string is blank or empty, this routine leaves 
      the input string unchanged, except that trailing blanks
      will be trimmed.  This case is not considered an error.

   3) If the output string is too short to accommodate a terminating
      null character, the error SPICE(STRINGTOOSHORT) is signaled.

   4) If out does not have sufficient length to accommodate the 
      result of the substitution, the result will be truncated on 
      the right. 
 
   5) If the requested format is not supported, the error MAY be
      diagnosed by routines in the call tree of this routine.
      The current Fortran implementation defaults to F format
      if the format is anything other than 'E'.

-Files
 
   None. 

-Particulars
 
   This is one of a family of related routines for inserting values 
   into strings. They are typically to construct messages that 
   are partly fixed, and partly determined at run time. For example, 
   a message like 
 
      "Fifty-one pictures were found in directory [USER.DATA]." 
 
   might be constructed from the fixed string 
 
      "#1 pictures were found in directory #2." 
 
   by the calls 
 
      repmct_c ( string, "#1",  51,  'c',      LENOUT, string );
      repmc_c  ( string, "#2", "[USER.DATA]",  LENOUT, string );
 
   which substitute the cardinal text "Fifty-one" and the character 
   string "[USER.DATA]" for the markers "#1" and "#2" respectively. 
 
   The complete list of routines is shown below. 
 
      repmc_c  ( Replace marker with character string value ) 
      repmd_c  ( Replace marker with double precision value ) 
      repmf_c  ( Replace marker with formatted d.p. value   ) 
      repmi_c  ( Replace marker with integer value          ) 
      repmct_c ( Replace marker with cardinal text          ) 
      repmot_c ( Replace marker with ordinal text           ) 
 
-Examples
 
 
   1. Let 
 
         in == "Invalid duration value.  The value was #." 
 
      Then following the call, 
 
         #include "SpiceUsr.h"
              .
              .
              .
         #define   LENOUT                  201
              .
              .
              .
         repmf_c ( in, "#", 5.0e3, 5, 'f', LENOUT, in ); 
 
      in is 
 
         "Invalid duration value.  The value was 5000.0." 
 
 
   2. Let 
 
         in == "Left endpoint exceeded right endpoint.  The left "
               "endpoint was: XX.  The right endpoint was: XX." 
 
      Then following the call, 
 
         #include "SpiceUsr.h"
              .
              .
              .
         #define   LENOUT                  201
              .
              .
              .
         repmf_c ( in, "  XX  ",  -5.2d-9, 3, 'e', lenout, out );
 
      out is 
 
         "Left endpoint exceeded right endpoint.  The left "
         "endpoint was: -5.20E-09.  The right endpoint was: XX." 
 
 
   3. Let 
 
         in == "Invalid quantity.  The value was # units." 
 
      Then following the call, 
 
         #include "SpiceUsr.h"
              .
              .
              .
         #define   LENOUT                  201
              .
              .
              .
         repmf_c ( in, "#", 5.0e1, 3, 'f', LENOUT, in );
 
      in is 
 
         "Invalid quantity.  The value was 50.0 units." 
 
 
   4. In the above example, if sigdig is 1 instead of 3, in becomes 
 
         "Invalid quantity.  The value was 50. units." 
 
 
   5. Let 
 
         in == "Invalid duration value.  The value was #." 
 
      Then following the call, 
 
         #include "SpiceUsr.h"
              .
              .
              .
         #define   LENOUT                  201
              .
              .
              .
         repmf_c ( in, "#", 5.0e1, 100, 'e', LENOUT, in );
 
      in is 
 
         "Invalid duration value.  The value was "
         "5.0000000000000E+01." 
 
      Note that even though 100 digits of precision were requested, 
      only 14 were returned. 
 
 
   6. Let 
 
         marker == "&" 
         num    == 23 
         chance == "fair" 
         score  == 4.665 
 
      Then following the sequence of calls, 
 
         #include "SpiceUsr.h"
              .
              .
              .
         #define   LENOUT                  201
              .
              .
              .
         repmi_c ( "There are & routines that have a "  
                   "& chance of meeting your needs.  "    
                   "The maximum score was &.", 
                   marker, 
                   num, 
                   LENOUT,
                   msg                                  );
 
         repmc_c ( msg, marker, chance,        LENOUT, msg );
 
         repmf_c ( msg, marker, score, 4, 'f', LENOUT, msg );
 
      msg is 
 
         "There are 23 routines that have a fair chance of "
         "meeting your needs.  The maximum score was 4.665." 
 
-Restrictions
 
   1) The maximum number of significant digits returned is 14. 
 
   2) This routine makes explicit use of the format of the string 
      returned by dpstrf_; should that routine change, substantial 
      work may be required to bring this routine back up to snuff. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
   I.M. Underwood (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 14-AUG-2002 (NJB) (IMU)

-Index_Entries
 
   replace marker with formatted d.p. value 
 
-&
*/

{ /* Begin repmf_c */


   /*
   Local variables 
   */
   ConstSpiceChar        * markPtr;


   /*
   Use discovery check-in. 

   Make sure no string argument pointers are null.
   */
   CHKPTR( CHK_DISCOVER, "repmf_c", in     );
   CHKPTR( CHK_DISCOVER, "repmf_c", marker );
   CHKPTR( CHK_DISCOVER, "repmf_c", out    );



   /*
   If the output string can't hold a terminating null character,
   we can't proceed. 
   */
   if ( lenout < 1 )
   {
      chkin_c  ( "repmf_c"                                    );
      setmsg_c ( "String length lenout must be >= 1; actual "
                 "value = #."                                 );
      errint_c ( "#", lenout                                  );
      sigerr_c ( "SPICE(STRINGTOOSHORT)"                      );
      chkout_c ( "repmf_c"                                    );
      return;
   }


   /*
   If the output string has no room for data characters, we simply
   terminate the string.
   */
   if ( lenout == 1 )
   {
      out[0] = NULLCHAR;
      return;
   }


   /*
   If the input string has zero length, the output is empty as well. 
   */
   if ( in[0] == NULLCHAR )
   {
      out[0] = NULLCHAR;

      return;
   }


   /*
   If the marker is empty, pass a blank marker to the f2c'd routine.
   Otherwise, pass in the marker.
   */
   if ( marker[0] == NULLCHAR )
   {
      markPtr = " ";
   }
   else
   {
      markPtr = marker;
   }


   /*
   Simply call the f2c'd routine. 
   */
   repmf_ ( ( char        * ) in,
            ( char        * ) marker,
            ( doublereal  * ) &value,
            ( integer     * ) &sigdig,
            ( char        * ) &format,
            ( char        * ) out,
            ( ftnlen        ) strlen(in),
            ( ftnlen        ) strlen(markPtr),
            ( ftnlen        ) 1,
            ( ftnlen        ) lenout-1        );

   /*
   Convert the output string from Fortran to C style. 
   */
   F2C_ConvertStr ( lenout, out );
   

} /* End repmf_c */
