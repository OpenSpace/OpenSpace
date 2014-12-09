/*

-Procedure kxtrct_c ( Extract a substring starting with a keyword )

-Abstract
 
   Locate a keyword in a string and extract the substring from 
   the beginning of the first word following the keyword to the 
   beginning of the first subsequent recognized terminator of a list. 
 
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
 
    SEARCH, PARSING, PARSING 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void kxtrct_c ( ConstSpiceChar       * keywd,
                   SpiceInt               termlen,
                   const void           * terms,
                   SpiceInt               nterms,
                   SpiceInt               stringlen,
                   SpiceInt               substrlen,
                   SpiceChar            * string,
                   SpiceBoolean         * found,
                   SpiceChar            * substr     )
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   keywd      I   Word that marks the beginning of text of interest. 
   termlen    I   Length of strings in string array term.
   terms      I   Set of words, any of which marks the end of text. 
   nterms     I   Number of terms. 
   stringlen  I   Available space in argument string.
   substrlen  I   Available space in output substring.
   string    I/O  String containing a sequence of words. 
   found      O   SPICETRUE if the keyword is found in the string. 
   substr     O   String from end of keywd to beginning of first 
                  terms item found. 

-Detailed_Input
 
   keywd          is a word used to mark the start of text of interest. 

   termlen        is the maximum number of characters that can be 
                  accommodated in the each element of the input argument 
                  terms.  This count includes room for the terminating null 
                  characters.  

   terms          is a set of words, any one of which may signal the 
                  end of text of interest.  

                  The array terms should be declared with dimensions

                     [nterms][termlen]

   nterms         is the number of elements in the array terms. 

   stringlen      is the maximum number of characters that can be 
                  accommodated in the in/out argument string.  This count 
                  includes room for the terminating null character.
                  For example, if the maximum allowed length of the 
                  output string, including the terminating null, is 25
                  characters, then stringlen should be set to 25.
 
   substrlen      is the maximum number of characters that can be 
                  accommodated in the output argument substr.  This count 
                  includes room for the terminating null character.
 
   string         is a character string made up of words, which may 
                  contain the keyword in keywd. 
 
-Detailed_Output
 
   string         is the input string stripped of all words from 
                  the beginning of the keyword keywd to the end of 
                  the last word preceding one of the words in terms 
                  (or the end of the string if none of the terms follows 
                  keywd in the string). 

   found          is a flag indicating whether keywd is present in the 
                  input string.  found is set to SPICETRUE if the keyword
                  is present and SPICEFALSE otherwise.

   substr         is the substring that begins with the first word 
                  following keywd up to the beginning of any of the 
                  words in term or the end of the string.  If no words
                  are found between the keyword and the next terminator,
                  substr is returned empty.

                  substr cannot overwrite string.

-Parameters
 
   None. 
 
-Exceptions
 
   1) If any string pointer argument is null, the error
      SPICE(NULLPOINTER) will be signaled.
 
   2) If keywd has string length zero, the error SPICE(EMPTYSTRING) 
      will be signaled.

   3) If any of the arguments terms, string, or substr has length 
      less than 2, as indicated by their associated length arguments 
      termlen, stringlen and substrlen, the error SPICE(STRINGTOOSHORT) 
      will be signaled.

-Files
 
   None. 
 
-Particulars
 
   Definitions: 

      A WORD     is a set of consecutive non-blank characters 
                 delimited by blanks or either end of the string 
                 that contains them. 

   Given a string and a keyword this routine locates the first 
   occurrence of the keyword in the string and returns the 
   substring between the end of the keyword and the first occurrence 
   of any of the words in a list of terminating words.  If none 
   of the terminating words follows the keyword in the string, 
   the routine returns all of the string following the keyword. 

   If the next word following the keyword is a terminating word, 
   the substring returned will be empty. 

   If the keyword can not be located in the string, the variable 
   found will be returned as SPICEFALSE and the input string will be 
   unchanged.  The substring will be returned empty. 

   In all other cases, the part of the input string from the 
   beginning of the keyword to the start of the first terminating 
   word will be removed.  If no terminating word follows the keyword 
   the portion of the string from the keyword to the last non-blank 
   character of the string will be removed. 
 
-Examples
 
   Example 1. 
   ---------- 
     Input:  string  "FROM 1 October 1984 12:00:00 TO 1 January 1987" 
             keywd   "TO" 
             terms   "FROM" 
                     "TO" 
                     "BEGINNING" 
                     "ENDING" 
 
     Output: string  "FROM 1 October 1984 12:00:00" 
             found   SPICETRUE 
             substr  "1 January 1987" 
 
 
 
   Example 2. 
   ---------- 
     Input:  string  "FROM 1 October 1984 12:00:00 TO 1 January 1987" 
             keywd   "FROM" 
             terms   "FROM" 
                     "TO" 
                     "BEGINNING" 
                     "ENDING" 
 
     Output: string  " TO 1 January 1987" 
             found   SPICETRUE 
             substr  "1 October 1984 12:00:00" 
 
 
 
   Example 3. 
   ---------- 
     Input:  string  "ADDRESS: 4800 OAK GROVE DRIVE PHONE: 354-4321 " 
             keywd   "ADDRESS:" 
             terms   "ADDRESS:" 
                     "PHONE:" 
                     "NAME:" 
 
     Output: string  " PHONE: 354-4321" 
             found   SPICETRUE 
             substr  "4800 OAK GROVE DRIVE" 
 
 
   Example 4. 
   ---------- 
     Input:  string  "ADDRESS: 4800 OAK GROVE DRIVE PHONE: 354-4321 " 
             keywd   "NAME:" 
             terms   "ADDRESS:" 
                     "PHONE:" 
                     "NAME:" 
 
     Output: string  "ADDRESS: 4800 OAK GROVE DRIVE PHONE: 354-4321" 
             found   SPICEFALSE 
             substr  "" 
 
-Restrictions

   None.

-Author_and_Institution
 
   N.J. Bachman    (JPL)
   H.A. Neilan     (JPL) 
   W.L. Taber      (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.0, 18-AUG-2002 (NJB) (HAN) (WLT)

-Index_Entries
 
   extract a substring starting with a keyword 
 
-&
*/

{ /* Begin kxtrct_c */


   /*
   Local variables
   */
   logical                 fnd;

   SpiceChar             * fTermsArr;
   SpiceChar            ** strptrs;
   
   SpiceInt                fTermsLen;
   SpiceInt                i;


   /*
   Participate in error tracing.
   */
   chkin_c ( "kxtrct_c" );

   /*
   Check the input keyword to make sure the pointer is
   non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "kxtrct_c", keywd );

   /*
   Make sure the input string pointer for the terms array is non-null 
   and that the length termlen is sufficient.  
   */
   CHKOSTR ( CHK_STANDARD, "kxtrct_c", terms, termlen );
   
   /*
   Make sure the string pointer for the argument "string" is non-null 
   and that the length stringlen is sufficient.  
   */
   CHKOSTR ( CHK_STANDARD, "kxtrct_c", string, stringlen );

   /*
   Make sure the string pointer for the argument "substr" is non-null 
   and that the length substrlen is sufficient.  
   */
   CHKOSTR ( CHK_STANDARD, "kxtrct_c", substr, substrlen );
   

   /*
   We're going to need a Fortran style array of strings to pass to 
   the f2c'd routine kxtrct_.  We can create such an array using
   dynamically allocated memory by calling C2F_CreateStrArr_Sig.  But 
   first, we'll need an array of character pointers, each one pointing 
   to a string in the input terms array.
   */

   strptrs = (SpiceChar **) malloc( (size_t) nterms * sizeof(SpiceChar *) );

   if ( strptrs == 0 )
   {
      setmsg_c ( "Failure on malloc call to create pointer array "
                 "for terms values."                               );
      sigerr_c ( "SPICE(MALLOCFAILED)"                             );
      chkout_c ( "kxtrct_c"                                        );
      return;
   }

   /*
   Getting this far means we succeeded in allocating our character
   pointer array.  Assign the pointers.
   */
   
   for ( i = 0;  i < nterms;  i++ )
   {
      strptrs[i] =  ( (SpiceChar *) terms )  +  i * termlen;
   }

   /*
   Create a Fortran-style string array.
   */
   C2F_CreateStrArr_Sig (   nterms, 
                          ( ConstSpiceChar ** ) strptrs, 
                           &fTermsLen, 
                           &fTermsArr                      );

   if ( failed_c() )
   {
      free ( strptrs );
      
      chkout_c ( "kxtrct_c" );
      return;
   }


   /*
   Call the f2c'd routine.
   */
   kxtrct_ (  ( char       * ) keywd,
              ( char       * ) fTermsArr,
              ( integer    * ) &nterms,
              ( char       * ) string,
              ( logical    * ) &fnd,
              ( char       * ) substr,
              ( ftnlen       ) strlen(keywd),
              ( ftnlen       ) fTermsLen,
              ( ftnlen       ) stringlen-1,
              ( ftnlen       ) substrlen-1  );

   /*
   Free the dynamically allocated arrays.
   */
   free ( fTermsArr );
   free ( strptrs   );

   /*
   Convert the output strings to C style.  Also set the output found flag.
   */
   F2C_ConvertStr ( stringlen, string );
   F2C_ConvertStr ( substrlen, substr );

   *found = fnd;


   chkout_c ( "kxtrct_c" );

} /* End kxtrct_c */


  
