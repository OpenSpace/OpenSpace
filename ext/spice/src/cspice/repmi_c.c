/*

-Procedure repmi_c  ( Replace marker with integer )

-Abstract
 
   Replace a marker with an integer. 
 
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


   void repmi_c ( ConstSpiceChar     * in,
                  ConstSpiceChar     * marker,
                  SpiceInt             value,
                  SpiceInt             lenout,
                  SpiceChar          * out     ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   in         I   Input string. 
   marker     I   Marker to be replaced. 
   value      I   Replacement value.
   lenout     I   Available space in output string.
   out        O   Output string. 
   MAXLI      P   Maximum length of an integer. 
 
-Detailed_Input
 
   in             is an arbitrary character string. 
 
   marker         is an arbitrary character string. The first occurrence 
                  of marker in the input string is to be replaced by value. 
 
                  Leading and trailing blanks in marker are NOT significant. 
                  In particular, no substitution is performed if marker 
                  is blank. 
 
   value          is an arbitrary integer. 

 
   lenout         is the allowed length of the output string.  This length
                  must large enough to hold the output string plus the
                  terminator.  If the output string is expected to have x
                  characters, lenout should be at least x + 1.

-Detailed_Output
 
   out            is the string obtained by substituting the text 
                  representation of value for the first occurrence 
                  of marker in the input string. 
 
                  out and in must be identical or disjoint. 
 
-Parameters
 
   MAXLI          is the maximum expected length of the text 
                  representation of an integer. 11 characters are 
                  sufficient to hold any integer whose absolute 
                  value is less than 10 billion. 
 
                  This routine assumes that the input integer
                  is such that its string representation contains
                  no more than MAXLI characters.
 
-Files
 
   None. 
 
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
  
-Particulars
 
   This is one of a family of related routines for inserting values 
   into strings. They are typically to construct messages that 
   are partly fixed, and partly determined at run time. For example, 
   a message like 
 
      "Fifty-one pictures were found in directory [USER.DATA]." 
 
   might be constructed from the fixed string 
 
      "#1 pictures were found in directory #2." 
 
   by the calls 
 
      #include "SpiceUsr.h"
           .
           .
           .
      #define   LENOUT                  81
           .
           .
           .
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
 
         in == "Invalid operation value.  The value was <opcode>." 
 
      Then following the call, 
 
         #include "SpiceUsr.h"
              .
              .
              .
         #define   LENOUT                  201
              .
              .
              .
         repmi_c ( in, "<opcode>", 5, LENOUT, outstr );
 

      outstr contains the string: 
 
         "Invalid operation value.  The value was 5." 
 
 

   2. Let 
 
         in ==  "Left endpoint exceeded right endpoint.  "
                "The left endpoint was:  XX.  The right "
                "endpoint was:  XX." 
 
      Then following the call, 
 
 
         #include "SpiceUsr.h"
              .
              .
              .
         #define   LENOUT                  201
              .
              .
              .
         repmi_c ( in, "  XX  ", 5, LENOUT, out );
 
      out is 
 
         "Left endpoint exceeded right endpoint.  The left "
         "endpoint was:  5.  The right endpoint was:  XX."
 
 
   3. Let 
 
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
                   "& chance of meeting your needs."    
                   "The maximum score was &.", 
                   "&",
                    num, 
                    LENOUT,
                    msg                              ); 
 
         repmc_c ( msg, marker, chance, LENOUT, msg );
 
         repmf_c ( msg, marker, score,  4, 'f', LENOUT, msg ); 
 

      msg is 
 
         "There are 23 routines that have a fair chance of "
         "meeting your needs.  The maximum score was 4.665." 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
   I.M. Underwood (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 14-AUG-2002 (NJB) (IMU)

-Index_Entries
 
   replace marker with integer 
 
-&
*/

{ /* Begin repmi_c */


   /*
   Local variables 
   */
   ConstSpiceChar        * markPtr;


   /*
   Use discovery check-in. 

   Make sure no string argument pointers are null.
   */
   CHKPTR( CHK_DISCOVER, "repmi_c", in     );
   CHKPTR( CHK_DISCOVER, "repmi_c", marker );
   CHKPTR( CHK_DISCOVER, "repmi_c", out    );


   /*
   If the output string can't hold a terminating null character,
   we can't proceed. 
   */
   if ( lenout < 1 )
   {
      chkin_c  ( "repmi_c"                                    );
      setmsg_c ( "String length lenout must be >= 1; actual "
                 "value = #."                                 );
      errint_c ( "#", lenout                                  );
      sigerr_c ( "SPICE(STRINGTOOSHORT)"                      );
      chkout_c ( "repmi_c"                                    );
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
   repmi_ ( ( char     * ) in,
            ( char     * ) markPtr,
            ( integer  * ) &value,
            ( char     * ) out,
            ( ftnlen     ) strlen(in),
            ( ftnlen     ) strlen(markPtr),
            ( ftnlen     ) lenout-1         );

   /*
   Convert the output string from Fortran to C style. 
   */
   F2C_ConvertStr ( lenout, out );
   

} /* End repmi_c */
