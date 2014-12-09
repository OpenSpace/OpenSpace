/*

-Procedure      lcase_c ( Convert to lowercase )

-Abstract
 
    Convert the characters in a string to lowercase. 
 
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
 
    ASCII,  CHARACTER 
 
*/

   #include <string.h>
   #include "SpiceUsr.h"
   #include "SpiceZmc.h"


   void lcase_c ( SpiceChar       * in,
                  SpiceInt          lenout, 
                  SpiceChar       * out    ) 

/*

-Brief_I/O
 
    VARIABLE  I/O  DESCRIPTION 
    --------  ---  -------------------------------------------------- 
    in         I   Input string. 
    lenout     I   Maximum length of output string.
    out        O   Output string, all lowercase. 
 
-Detailed_Input
 
    in          is the input string. 
    
    lenout      is the maximum allowed length of the output string, 
                including the terminating null.
 
-Detailed_Output
 
    out         is the output string. This is the input string 
                with all lowercase letters converted to lowercase. 
                Non-letters are not affected. 
                
                If 
                  
                   lenout < strlen(in)+1
                   
                the output string will be truncated on the right.

                A terminating null will be placed in out at position
                
                   MinVal ( strlen(in),  lenout-1 )
                   
                unless lenout is less than or equal to zero.
                
 
                out may overwrite in. 
 
-Parameters
 
   None. 
 
-Exceptions

    1) If the input string pointer is null, the error 
       SPICE(NULLPOINTER) will be signaled.
       
    2) If the output string pointer is null, the error 
       SPICE(NULLPOINTER) will be signaled.
       
    3) If lenout is less than or equal to zero, the error 
       SPICE(STRINGTOOSHORT) will be signaled.      
 
    4) If the output string is shorter than the input string, the
       result will be truncated on the right.
       
-Files
 
    None. 
 
-Particulars
 
    Convert each lowercase character in IN to lowercase. 
 
-Examples
 
    "This is an example"   becomes   "THIS IS AN EXAMPLE" 
    "12345 +-=? > * $ &"             "12345 +-=? > * $ &" 
 
-Restrictions
 
    None. 
 
-Author_and_Institution
 
    N.J. Bachman    (JPL)
    K.R. Gehringer  (JPL) 
    I.M. Underwood  (JPL) 
 
-Literature_References
 
    None. 
 
-Version
 
   -CSPICE Version 1.1.0, 26-JAN-2005 (NJB)

       Cast to SpiceInt was applied to strlen output to suppress
       compiler warnings about comparison of signed and unsigned types.

   -CSPICE Version 1.0.0, 26-AUG-1999 (NJB)

      Based on SPICELIB Version 1.1.0, 13-MAR-1996 (KRG)
      
      
-Index_Entries
 
   convert to lowercase 
 
-&
*/

{ /* Begin lcase_c */


   /*
   Local macros
   */
   #define  UPPERA   (  (SpiceInt) ('A')           )
   #define  UPPERZ   (  (SpiceInt) ('Z')           )
   #define  SHIFT    (  UPPERA  - (SpiceInt) ('a') )


   /*
   Local variables
   */
   SpiceInt                i;
   SpiceInt                ich;
   SpiceInt                nmove;


   /*
   Check the input string pointer to make sure it's non-null.
   */
   CHKPTR( CHK_DISCOVER, "lcase_c", in );
   

   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_DISCOVER, "lcase_c", out, lenout );

   
   /*
   Move the string from in to out. Step through in one character
   at a time, translating letters between 'a' and 'z' to lowercase.
   
   First, determine how many characters to move.
   */
   nmove = MinVal ( (SpiceInt)strlen(in), lenout-1 );
   
   
   for ( i = 0;  i < nmove;  i++ )
   {
      ich = (SpiceInt) in[i];

      if ( ( ich >= UPPERA ) && ( ich <= UPPERZ ) ) 
      {
         out[i] = (char) ( ich - SHIFT );
      }
      else
      {
         out[i] = in[i];
      }
   }
 
 
   /*
   Terminate the output string with a null. We know it has room for at 
   least one character.
   */
   out[nmove] = NULLCHAR;
   
   
} /* End lcase_c */
