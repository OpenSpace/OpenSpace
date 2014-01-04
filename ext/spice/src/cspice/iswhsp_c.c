/*

-Procedure iswhsp_c ( Determine whether a string is white space )

-Abstract
 
   Return a boolean value indicating whether a string contains
   only white space characters.
 
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
 
   FILES,  TEXT 
 
*/

   #include <ctype.h>
   #include "SpiceUsr.h"
   #include "SpiceZmc.h"


   SpiceBoolean iswhsp_c ( ConstSpiceChar * string ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   string     I   String to be tested.
    
   The function returns the boolean value SPICETRUE if the string is
   empty or contains only white space characters; otherwise it returns 
   the value SPICEFALSE.
 
-Detailed_Input

   string     is a character pointer designating a string to be 
              searched for non-white-space characters.
              
-Detailed_Output

   The function returns the boolean value SPICETRUE if the string 
   contains only white space characters; otherwise it returns the 
   value SPICEFALSE.
   
   White space characters are those in the set
              
      { ' ', '\f', '\n', '\r', '\t', '\v' }
                 
               
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the input string pointer is null, the error SPICE(NULLPOINTER)
       is signaled.
    
   2)  An empty string, that is a string with a null character
       at index 0, is considered to be blank.
   
-Files
 
   None. 
 
-Particulars
 
   This routine provides a short cut for testing lines for the presence
   of non-blank characters; this is a test which is performed frequently
   in CSPICE.
    
-Examples
 
   1) Read a text file; print the non-blank lines.   
   
         #include <stdio.h>
         #include "SpiceUsr.h"
         
         void main()
         {
            #define MAXLEN 82

            FILE          *fptr;
            SpiceBoolean   eof;
            SpiceChar      line [MAXLEN];
            
            
            txtopr_c ( "myfile", &fptr );
            
            readln_c ( fptr, MAXLEN, line, &eof );
            
            while ( !eof )
            {
               if ( !iswhsp_c(line) )
               {
                  printf ( "%s\n", line );
               }
            
               readln_c ( fptr, MAXLEN, line, &eof );
            }
         }
 
-Restrictions
 
   None. 
 
-Literature_References
 
   1)  "American National Standard for Programming Languages -- C,"
        Published by the American National Standards Institute, 1990. 
        Section 7.3.1.9., p. 104.
 
-Author_and_Institution
 
   N.J. Bachman (JPL) 
 
-Version
 
   -CSPICE Version 1.1.0, 27-AUG-1999 (NJB)

      Now checks for null input string.
      
   -CSPICE Version 1.0.0, 24-FEB-1999 (NJB)

      Arguments passed to isspace are now cast to unsigned char to 
      suppress compilation warnings on some systems.

   -CSPICE Version 1.0.0, 08-FEB-1998 (NJB)

-Index_Entries
 
   read a non-blank line from a text file 
 
-&
*/

{ /* Begin iswhsp_c */


   /*
   Local variables
   */
   SpiceBoolean            blank;
   ConstSpiceChar         * sptr;


   /*
   Check the input string pointer to make sure it's non-null.
   */
   CHKPTR_VAL ( CHK_DISCOVER, "iswhsp_c", string, SPICEFALSE );
   

   /*
   Start out assuming the string is blank.  If the string is empty,
   we've got the right return value already.
   */ 
   
   blank  =  SPICETRUE;
   sptr   =  string;
   
   while (   blank   && ( (SpiceBoolean) *sptr )  )
   {
      if (  !isspace( (unsigned char) *sptr )  )
      {
         blank = SPICEFALSE;
      }
      
      sptr++;
   }


   return ( blank );
   

} /* End iswhsp_c */
