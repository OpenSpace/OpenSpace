/*

-Procedure lparse_c ( Parse items from a list )

-Abstract
 
    Parse a list of items delimited by a single character. 
 
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
 
    CHARACTER,  LIST,  PARSING,  STRING 
 
*/
   #include <stdio.h>

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void lparse_c ( ConstSpiceChar   * list,
                   ConstSpiceChar   * delim,
                   SpiceInt           nmax,
                   SpiceInt           lenout,
                   SpiceInt         * n,
                   void             * items   ) 

/*

-Brief_I/O
 
    VARIABLE  I/O  DESCRIPTION 
    --------  ---  -------------------------------------------------- 
    list       I    List of items delimited by delim. 
    delim      I    Single character used to delimit items. 
    nmax       I    Maximum number of items to return. 
    lenout     I    Length of strings in item array.
    n          O    Number of items in the list. 
    items      O    Items in the list, left justified. 
 
-Detailed_Input
 
    list        is a string containing a list of items delimited by the 
                single character delim. Consecutive delimiters, and 
                delimiters at the beginning and end of the list, are 
                considered to delimit empty items. A blank or empty
                list is considered to contain a single (empty) item. 
 
    delim       is the character delimiting the items in the list. 
                This may be any ASCII character, including a blank. 
                However, by definition, consecutive blanks are NOT 
                considered to be consecutive delimiters. In addition, 
                leading and trailing blanks are ignored. 
 
    nmax        is the maximum number of items to be returned from 
                the list. This allows the user to guard against 
                overflow from a list containing more items than 
                expected. 
                
    lenout      is the declared length of the strings in the string 
                array items.  This length must include room for the
                terminating null character in each string.
 
-Detailed_Output
 
    n           is the number of items in the list. n may be 
                any number between one and nmax. n is always the 
                number of delimiters plus one. 
 
    items       is an array of strings containing the items in the list,
                left justified. Any item in the list to long to fit into 
                an element of items is truncated on the right.  Empty
                (null) or blank items in the input string are mapped to
                empty strings on output.
                
                items should be declared by the caller as:
                
                   SpiceCharitem [nmax][lenout]
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If nmax is less than one, then n will be set to zero, and no
       items will be returned.  This case is not an error.  The other
       exceptional cases can occur only if nmax > 0.
       
   2)  The error SPICE(NULLPOINTER) is signaled if either the input or
       output string pointer is null.

   3)  If the output string length lenout is less than one, the error
       SPICE(STRINGTOOSHORT) will be signaled.
       
   4)  An empty input string will result in a single, empty output 
       token.  This case is not an error.
   
-Particulars
 
   None. 
 
-Examples
 
   The following examples illustrate the operation of lparse_c. 

   1) Let 
            LIST  = "  A number of words   separated   by spaces   " 
            DELIM = " " 
            nmax  = 20 

      Then 
            ITEMS[0] = "A" 
            ITEMS[1] = "number" 
            ITEMS[2] = "of" 
            ITEMS[3] = "words" 
            ITEMS[4] = "separated" 
            ITEMS[5] = "by" 
            ITEMS[6] = "spaces" 

   2) Let 
            LIST  = "//option1//option2/ //" 
            DELIM = "/" 
            nmax  = 20 

      Then 
            ITEMS[0] = "" 
            ITEMS[1] = "" 
            ITEMS[2] = "option1" 
            ITEMS[3] = "" 
            ITEMS[4] = "option2" 
            ITEMS[5] = "" 
            ITEMS[6] = "" 
            ITEMS[7] = "" 

   3) Let 
            LIST  = " ,bob,   carol,, ted,  alice" 
            DELIM = "," 
            nmax  = 4 

      Then 
            ITEMS[0] = "" 
            ITEMS[1] = "bob" 
            ITEMS[2] = "carol" 
            ITEMS[3] = "" 

-Restrictions
 
   None. 
 
-Files
 
   None. 
 
-Author_and_Institution

   N.J. Bachman    (JPL) 
   H.A. Neilan     (JPL) 
   I.M. Underwood  (JPL) 
 
-Literature_References
 
   None. 
 
-Version

   -CSPICE Version 2.2.0, 18-MAY-2001 (WLT)

      Added a cast to (char *) in the call to  F2C_ConvertTrStrArr

   -CSPICE Version 2.1.0, 20-APR-2000 (NJB)
   
      Bug fix:  set n to zero for nmax < 1.
      
   -CSPICE Version 2.0.0, 25-MAR-2000 (NJB)
   
      Updated header to accurately describe treatment of null tokens.
      Updated code to handle the case of an empty input string or 
      nmax < 1.
      
      Changed typedef SpiceVoid to void.

   -CSPICE Version 1.0.0, 09-FEB-1998 (NJB)

-Index_Entries
 
   parse items from a list 
 
-&
*/

{ /* Begin lparse_c */

   
   /*
   Participate in error handling.
   */
   chkin_c ( "lparse_c" );


   /*
   If there's no room for output tokens, just return.
   */
   if ( nmax < 1 )
   {
      *n = 0;
      chkout_c ( "lparse_c" );
      return;
   }  
   
   
   /*
   Make sure the output string array contains at least enough room
   for a null character in each string.  Unlike most CSPICE wrappers,
   lparse_c must check the output array before checking the inputs
   because there's a special case that results in returning before
   the input checks are performed.
   */
   CHKOSTR ( CHK_STANDARD, "lparse_c", items, lenout );
  
  
   /*
   Special case:  if the input string is empty, return a single blank
   string. 
   
   We must know that list is not a null pointer first.
   */
   CHKPTR ( CHK_STANDARD, "lparse_c", list  );
   
   if ( list[0] == NULLCHAR ) 
   {
      *n                   =  1;
      *(SpiceChar *)items  =  NULLCHAR;
      
      chkout_c ( "lparse_c" );
      return;
   }
   
   
   /*
   Check the input delimiter string to make sure the pointers are 
   non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "lparse_c", list  );
   CHKFSTR ( CHK_STANDARD, "lparse_c", delim );
   
   
   /*
   Call the f2c'd routine.
   */
   lparse_ ( ( char       * ) list,
             ( char       * ) delim,
             ( integer    * ) &nmax,
             ( integer    * ) n,
             ( char       * ) items,
             ( ftnlen       ) strlen(list),
             ( ftnlen       ) strlen(delim),
             ( ftnlen       ) lenout-1      );
   
   /*
   Reformat the output item array from Fortran to C style.  Trim
   trailing blanks from output tokens.
   */
   
   F2C_ConvertTrStrArr ( *n, lenout, (char *) items );


   chkout_c ( "lparse_c" );
   

} /* End lparse_c */
