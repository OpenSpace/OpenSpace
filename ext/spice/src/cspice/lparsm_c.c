/*

-Procedure lparsm_c (Parse a list of items having multiple delimiters)

-Abstract
 
   Parse a list of items separated by multiple delimiters. 
 
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

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void lparsm_c ( ConstSpiceChar   * list,
                   ConstSpiceChar   * delims,
                   SpiceInt           nmax,
                   SpiceInt           lenout,
                   SpiceInt         * n,
                   void             * items   ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   list       I    List of items delimited by delims. 
   delims     I    Single characters which delimit items. 
   nmax       I    Maximum number of items to return. 
   lenout     I    Length of strings in item array.
   n          O    Number of items in the list. 
   items      O    Items in the list, left justified. 
 
-Detailed_Input
 
   list        is a list of items delimited by any one of the
               characters in the string delims. Consecutive delimiters,
               and delimiters at the beginning and end of the list, are
               considered to delimit empty items. A blank or empty list
               is considered to contain a single, empty item.  Leading
               and trailing blanks in list are ignored.
  
   delims      contains the individual characters which delimit
               the items in the list. These may be any ASCII
               characters, including blanks.

               However, by definition, consecutive blanks are NOT
               considered to be consecutive delimiters. Nor are
               a blank and any other delimiter considered to be
               consecutive delimiters.  

   nmax        is the maximum number of items to be returned from the
               list. This allows the user to guard against overflow
               from a list containing more items than expected.

   lenout      is the declared length of the strings in the string 
               array items.  This length must include room for the
               terminating null character in each string.

-Detailed_Output
 
   n           is the number of items in the list. n may be any number
               between one and nmax.

   items       is an array of strings containing the items in the list,
               left justified. Any item in the list too long to fit into 
               an element of items is truncated on the right.  Empty
               (null) or blank items in the input string are mapped to
               empty strings on output.

               items should be declared by the caller as:

                  SpiceChar items [nmax][lenout]

-Parameters
 
   None. 
 
-Exceptions
 
   1)  If nmax is less than one, then n will be set to zero, and no
       items will be returned.  This case is not an error.  The other
       exceptional cases can occur only if nmax > 0.
       
   2)  The error SPICE(NULLPOINTER) is signaled if either of the input
       string pointers or the output void pointer is null.

   3)  If the output string length lenout is less than one, the error
       SPICE(STRINGTOOSHORT) will be signaled.
       
   4)  An empty input string will result in a single, empty output 
       token.  This case is not an error.
    
-Files
 
   None. 
 
-Particulars
 
   None. 
 
-Examples
 
   The following examples illustrate the operation of lparsm_c. 

  1) Let 

        list   == "  A number of words   separated   by spaces   " 
        delims == " " 
        nmax   == 20 

     Then 

        items[0] == "A" 
        items[1] == "number" 
        items[2] == "of" 
        items[3] == "words" 
        items[4] == "separated" 
        items[5] == "by" 
        items[6] == "spaces" 



   2) Let 

         list   == " ,bob,   carol,, ted,  alice" 
         delims == "," 
         nmax   == 4 

      Then 

         items[0] == "" 
         items[1] == "bob" 
         items[2] == "carol" 
         items[3] == "" 


   3) Let 

         list   == "  1986-187// 13:15:12.184 " 
         delims == " ,/-:" 
         nmax   == 20 

      Then 

         items[0] == "1986" 
         items[1] == "187" 
         items[2] == "" 
         items[3] == "13" 
         items[4] == "15" 
         items[5] == "12.184" 
 
-Restrictions
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   I.M. Underwood  (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.0, 18-AUG-2002 (NJB) (IMU)

-Index_Entries
 
   parse a list of items 
 
-&
*/

{ /* Begin lparsm_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "lparsm_c" );


   /*
   If there's no room for output tokens, just return.
   */
   if ( nmax < 1 )
   {
      *n = 0;
      chkout_c ( "lparsm_c" );
      return;
   }  
   
   
   /*
   Make sure the output string array contains at least enough room
   for a null character in each string.  Unlike most CSPICE wrappers,
   lparsm_c must check the output array before checking the inputs
   because there's a special case that results in returning before
   the input checks are performed.
   */
   CHKOSTR ( CHK_STANDARD, "lparsm_c", items, lenout );
  
  
   /*
   Special case:  if the input string is empty, return a single empty
   string. 
   
   We must know that list is not a null pointer first.
   */
   CHKPTR ( CHK_STANDARD, "lparsm_c", list  );
   
   if ( list[0] == NULLCHAR ) 
   {
      *n                   =  1;
      *(SpiceChar *)items  =  NULLCHAR;
      
      chkout_c ( "lparsm_c" );
      return;
   }
   
   
   /*
   Check the input delimiter string to make sure the pointers are 
   non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "lparsm_c", list   );
   CHKFSTR ( CHK_STANDARD, "lparsm_c", delims );
   
   
   /*
   Call the f2c'd routine.
   */
   lparsm_ ( ( char       * ) list,
             ( char       * ) delims,
             ( integer    * ) &nmax,
             ( integer    * ) n,
             ( char       * ) items,
             ( ftnlen       ) strlen(list),
             ( ftnlen       ) strlen(delims),
             ( ftnlen       ) lenout-1        );
   
   /*
   Reformat the output item array from Fortran to C style.  Trim
   trailing blanks from output tokens.
   */
   
   F2C_ConvertTrStrArr ( *n, lenout, (char *) items );


   chkout_c ( "lparsm_c" );

} /* End lparsm_c */
