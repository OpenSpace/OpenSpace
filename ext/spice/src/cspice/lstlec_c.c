/*

-Procedure lstlec_c ( Last character element less than or equal to. )

-Abstract
 
   Given a character string and an ordered array of character 
   strings, find the index of the largest array element less than 
   or equal to the given string. 
 
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
 
   SEARCH,  ARRAY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #include "f2cMang.h"
   #undef    lstlec_c

   SpiceInt lstlec_c ( ConstSpiceChar  * string,
                       SpiceInt          n,   
                       SpiceInt          lenvals,
                       const void      * array   ) 
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   string     I   Upper bound value to search against.
   n          I   Number elements in array.
   lenvals    I   String length.
   array      I   Array of possible lower bounds.

   The function returns the index of the last element of array that
   is lexically less than or equal to string. 
 
-Detailed_Input
 
   string      is a string acting as an upper bound:  the array element
               that is lexically the greatest element less than or
               equal to string is to be found.  Trailing blanks in this
               bound value are not significant.

   n           is the dimension of the array. 

   lenvals     is the declared length of the strings in the input
               string array, including null terminators.  The input   
               array should be declared with dimension 

                  [n][lenvals]

   array       is the array of character strings to be searched.
               Trailing blanks in the strings in this array are not
               significant. The strings must be sorted in
               non-decreasing order. The elements of array need not be
               distinct.

 
-Detailed_Output
 
   The function returns the index of the highest-indexed element in the 
   input array that is less than or equal to string.  The routine assumes
   the array elements are sorted in non-decreasing order.
 
   If all elements of the input array are greater than the specified 
   upper bound string, the function returns -1. 

-Parameters
 
   None. 
 
-Exceptions
  
   1) If ndim < 1 the function value is -1.  This is not considered
      an error.

   2) If input key value pointer is null, the error SPICE(NULLPOINTER) will 
      be signaled.  The function returns -1.
 
   3) The input key value may have length zero.  This case is not
      considered an error.

   4) If the input array pointer is null,  the error SPICE(NULLPOINTER) will 
      be signaled.  The function returns -1.

   5) If the input array string's length is less than 2, the error
      SPICE(STRINGTOOSHORT) will be signaled.  The function returns -1.
  
-Files
 
   None. 

-Particulars
 
   Note:  If you need to find the first element of the array that is
          greater than string, simply add 1 to the result returned by
          this function and check to see if the result is within the
          array bounds given by n.
 
-Examples

   Let array be a character array of dimension 

      [5][lenvals]

   which contains the following elements:

      "BOHR"
      "EINSTEIN"
      "FEYNMAN"
      "GALILEO"
      "NEWTON"

   Then

      lstlec_c ( "NEWTON",   5, lenvals, array )    ==   4
      lstlec_c ( "EINSTEIN", 5, lenvals, array )    ==   1
      lstlec_c ( "GALILEO",  5, lenvals, array )    ==   3
      lstlec_c ( "Galileo",  5, lenvals, array )    ==   3
      lstlec_c ( "BETHE",    5, lenvals, array )    ==  -1

-Restrictions
  
   1)  The input array is assumed to be sorted in increasing order. If 
       this condition is not met, the results of bsrchc_c are unpredictable.

   2)  String comparisons performed by this routine are Fortran-style:
       trailing blanks in the input array or key value are ignored.
       This gives consistent behavior with CSPICE code generated by
       the f2c translator, as well as with the Fortran SPICE Toolkit.
      
       Note that this behavior is not identical to that of the ANSI
       C library functions strcmp and strncmp.
  
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
   H.A. Neilan     (JPL) 
   W.L. Taber      (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 22-JUL-2002 (NJB) (HAN) (WLT)

-Index_Entries
 
   last character element less_than_or_equal_to 
 
-&
*/

{ /* Begin lstlec_c */


   /*
   f2c library utility prototypes 
   */
   logical          l_ge   (char *a, char *b, ftnlen la, ftnlen lb ); 
   logical          l_le   (char *a, char *b, ftnlen la, ftnlen lb ); 
   logical          l_lt   (char *a, char *b, ftnlen la, ftnlen lb ); 

   /*
   Local macros 
   */
   #define ARRAY( i )     (  ( (SpiceChar *)array ) + (i)*lenvals  )


   /*
   Local variables
   */
   SpiceInt                begin;
   SpiceInt                end;
   SpiceInt                items;
   SpiceInt                j;
   SpiceInt                keylen;
   SpiceInt                middle;



   /*
   Use discovery check-in.

   Return immediately if the array dimension is non-positive. 
   */
   if ( n < 1 ) 
   {
      return ( -1 );
   }

   /*
   Make sure the pointer for the key value is non-null 
   and that the length is adequate.  
   */
   CHKPTR_VAL ( CHK_DISCOVER, "lstlec_c", string, -1 );

   
   /*
   Make sure the pointer for the string array is non-null 
   and that the length lenvals is sufficient.  
   */
   CHKOSTR_VAL ( CHK_DISCOVER, "lstlec_c", array, lenvals, -1 );   


   /*
   Return if none of the array's elements are less than or equal to
   the key value. 
   */
   keylen = strlen(string);

   begin  = 0;
   end    = n - 1;

   if (  l_lt( ( char * )string, 
               ( char * )ARRAY(begin), 
               ( ftnlen )keylen, 
               ( ftnlen )strlen(ARRAY(begin)) )  )
   {
      return ( -1 );
   }


   /*
   Return if the key string is greater than or equal to
   all of the array's elements. 
   */
   if (  l_ge( ( char * )string, 
               ( char * )ARRAY(end), 
               ( ftnlen )keylen, 
               ( ftnlen )strlen(ARRAY(end)) )  )
   {
      return ( end );
   }


   /*
   Do a binary search for the specified key value. 

   At this point, string is greater than or equal to the first element
   of array and strictly less than the last element of array.
   */
   items  = n;

   while ( items > 2 )
   {
      /*
      Check the middle element. 
      */
      j      = items / 2;
      middle = begin + j;

 
      /*
      Narrow the search area.
      */
      if (  l_le ( (char    * ) ARRAY(middle),  
                   (char    * ) string,
                   (ftnlen    ) lenvals-1,
                   (ftnlen    ) keylen        )  )
      {
         /*
         The middle element is less than or equal to string.
         */
         begin = middle;
      }
      else
      {
         end   = middle;
      }

      items = end - begin + 1;

      /*
      At this point, string is greater than or equal to the array element 
      at index begin and strictly less than the element at index end.
      */
   }

   /*
   The element at index begin is the winner.
   */   
   return ( begin );


} /* End lstlec_c */
