/*

-Procedure lstlti_c ( Last integer element less than )

-Abstract
 
   Given a number x and an array of non-decreasing numbers, 
   find the index of the largest array element less than x. 
 
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
   #include "SpiceZim.h"
   #undef    lstlti_c


   SpiceInt lstlti_c ( SpiceInt          x,
                       SpiceInt          n,
                       ConstSpiceInt   * array )      

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   x          I   Value to search against. 
   n          I   Number of elements in array. 
   array      I   Array of possible lower bounds. 

   The function returns the index of the last element of array that is 
   less than x.
 
-Detailed_Input
 
   x              is an integer serving as a key value. 

   n              is the total number of elements in array. 

   array          is an array of integers that forms a  non-decreasing 
                  sequence. 
 
-Detailed_Output
 
   The function returns the index of the last element of the non-decreasing
   sequence

      {array[i] : 0 <=  i < n } 

   that is less than x.  Indices range from zero to n-1.

   If all elements of array are greater than or equal to x, this routine 
   returns the value -1.

-Parameters
 
   None. 
 
-Files
 
   None. 

-Exceptions
 
   Error free. 
 
   If n is less than or equal to zero, the function returns -1.  This case
   is not treated as an error. 
 
-Particulars  
 
   This routine uses a binary search algorithm and so requires 
   at most on the order of

      log (n)
         2

   steps to compute the value of lstlti_c. 

   Note:  If you need to find the first element of the array that is greater 
          than or equal to x, simply add 1 to the result returned by this 
          function and check to see if the result is within the array bounds 
          given by n. 
 
-Examples
 
 
   1)  Let array be assigned the following values:

          array[0] = -2;
          array[1] = -2;
          array[2] =  0;
          array[3] =  1;
          array[4] =  1;
          array[5] = 11;


       The table below demonstrates the behavior of lstlti_c:

                    Call                       Returned Value
          =========================            ==============
          lstlti_c ( -3, 6, array )                -1

          lstlti_c ( -2, 6, array )                -1

          lstlti_c (  0, 6, array )                 1

          lstlti_c (  1, 6, array )                 2

          lstlti_c ( 12, 6, array )                 5

-Restrictions
 
   If the sequence in the input argument array is not non-decreasing, 
   the results of this routine are undefined. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   H.A. Neilan     (JPL) 
   W.L. Taber      (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.0, 09-JUL-2002 (NJB) (HAN) (WLT)

-Index_Entries
 
   last integer element less_than_or_equal_to 
 
-&
*/

{ /* Begin lstlti_c */


   return  (   lstlti_ ( (integer *) &x, 
                         (integer *) &n, 
                         (integer *) array )  -  1   );

} /* End lstlti_c */
