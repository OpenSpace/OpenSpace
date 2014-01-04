/*

-Procedure bschoc_c  ( Binary search with order vector, character )

-Abstract
 
   Do a binary search for a given value within a character string array, 
   accompanied by an order vector.  Return the index of the matching array 
   entry, or -1 if the key value is not found. 

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
 
   ARRAY,  SEARCH 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h" 
   #include "f2cMang.h"
   #undef    bschoc_c


   SpiceInt bschoc_c ( ConstSpiceChar  * value,
                       SpiceInt          ndim,   
                       SpiceInt          lenvals,
                       const void      * array,
                       ConstSpiceInt   * order    ) 
/*

-Brief_I/O
 
   VARIABLE  I/O              DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   value      I   Key value to be found in array. 
   ndim       I   Dimension of array. 
   lenvals    I   String length.
   array      I   Character string array to search. 
   order      I   Order vector.

   The function returns the index of the first matching array 
   element or -1 if the value is not found. 

-Detailed_Input
 
   value       is the key value to be found in the array.  Trailing
               blanks space in this key are not significant:  string
               matches found by this routine do not require trailing
               blanks in value to match those in the corresponding
               element of array.

   ndim        is the dimension of the array. 

   lenvals     is the declared length of the strings in the input
               string array, including null terminators.  The input   
               array should be declared with dimension 

                  [ndim][lenvals]

   array       is the array of character srings to be searched.  Trailing
               blanks in the strings in this array are not significant.
 
   order       is an order vector which can be used to access the elements 
               of array in order.  The contents of order are a permutation
               of the sequence of integers ranging from zero to ndim-1.

-Detailed_Output
  
   The function returns the index of the specified value in the input array. 
   Indices range from zero to ndim-1.

   If the input array does not contain the specified value, the function 
   returns -1. 
 
   If the input array contains more than one occurrence of the specified
   value, the returned index may point to any of the occurrences.

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

   6) If memory cannot be allocated to create a Fortran-style version of
      the input order vector, the error SPICE(MALLOCFAILED) is signaled.
      The function returns -1 in this case.
 
-Files
 
   None.
 
-Particulars
 
   A binary search is performed on the input array, whose order is given
   by an associated order vector.  If an element of the array is found to 
   match the input value, the index of that element is returned. If no 
   matching element is found, -1 is returned. 
 
-Examples
 
   Let the input arguments array and order contain the following elements:

        array           order

      "FEYNMAN"           1
      "BOHR"              2
      "EINSTEIN"          0
      "NEWTON"            4
      "GALILEO"           3

   Then

      bschoc_c ( "NEWTON",   5, lenvals, array, order )    ==   3
      bschoc_c ( "EINSTEIN", 5, lenvals, array, order )    ==   2
      bschoc_c ( "GALILEO",  5, lenvals, array, order )    ==   4
      bschoc_c ( "Galileo",  5, lenvals, array, order )    ==  -1
      bschoc_c ( "BETHE",    5, lenvals, array, order )    ==  -1
 
-Restrictions

   1)  The input array is assumed to be sorted in increasing order. If 
       this condition is not met, the results of bschoc_c are unpredictable.

   2)  String comparisons performed by this routine are Fortran-style:
       trailing blanks in the input array or key value are ignored.
       This gives consistent behavior with CSPICE code generated by
       the f2c translator, as well as with the Fortran SPICE Toolkit.
      
       Note that this behavior is not identical to that of the ANSI
       C library functions strcmp and strncmp.
      
-Literature_References
 
   None 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL)
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.1.0, 07-MAR-2009 (NJB)

       This file now includes the header file f2cMang.h.
       This header supports name mangling of f2c library
       functions.

       Header sections were re-ordered.

   -CSPICE Version 1.0.0, 26-AUG-2002 (NJB) (WLT) (IMU)

-Index_Entries
 
   search in a character array 
 
-&
*/

{ /* Begin bschoc_c */


   /*
   f2c library utility prototypes 
   */
   logical          l_lt   (char *a, char *b, ftnlen la, ftnlen lb ); 
   extern integer   s_cmp  (char *a, char *b, ftnlen la, ftnlen lb ); 

   /*
   Local macros 
   */
   #define ARR_ORD( i )   (  ( (SpiceChar *)array ) + order[(i)]*lenvals  )


   /*
   Local variables
   */
   SpiceInt                i;
   SpiceInt                keylen;
   SpiceInt                left;
   SpiceInt                lexord;
   SpiceInt                right;


   /*
   Use discovery check-in.

   Return immediately if the array dimension is non-positive. 
   */
   if ( ndim < 1 ) 
   {
      return ( -1 );
   }


   /*
   Make sure the pointer for the key value is non-null 
   and that the length is adequate.  
   */
   CHKPTR_VAL ( CHK_DISCOVER, "bschoc_c", value, -1 );

   
   /*
   Make sure the pointer for the string array is non-null 
   and that the length lenvals is sufficient.  
   */
   CHKOSTR_VAL ( CHK_DISCOVER, "bschoc_c", array, lenvals, -1 );   

  
   /*
   Do a binary search for the specified key value. 
   */
   keylen = strlen(value);

   left   = 0;
   right  = ndim - 1;

   while ( left <= right )
   {
      /*
      Check the middle element. 
      */
      i  =  ( left + right ) / 2;

      /*
      The f2c library function s_cmp performs a Fortran-style 
      lexical order comparison.  A negative return value indicates
      the first argument is less than the second, a return value
      of zero indicates equality, and a positive value indicates
      the second argument is greater.
      */
      lexord =  (SpiceInt) s_cmp ( (char    * ) value, 
                                   (char    * ) ARR_ORD(i),
                                   (ftnlen    ) keylen,
                                   (ftnlen    ) strlen(ARR_ORD(i)) ); 

      /*
      If the middle element matches, return its location.
      */
      if ( lexord == 0 )
      {
         return ( order[i] );
      }
 
      /*
      Otherwise, narrow the search area.
      */
      else if ( lexord < 0 )
      {
         /*
         value is less than the middle element. 
         */
         right = i - 1;
      }

      else
      {
         left  = i + 1;
      }

   }

   /*
   If the search area is empty, indicate the value was not found.
   */   
   return ( -1 );



} /* End bschoc_c */
