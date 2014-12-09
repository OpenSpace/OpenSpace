/*

-Procedure  xpose6_c ( Transpose a matrix, 6x6 )

-Abstract

   Transpose a 6x6 matrix.

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

   None.

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #undef    xpose6_c


   void xpose6_c ( ConstSpiceDouble m1[6][6],  SpiceDouble mout[6][6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
    m1        I   6x6 matrix to be transposed.
    mout      O   Transpose of m1.  mout can overwrite m1.
 
-Detailed_Input

    m1            This variable may contain any double precision 6x6
                  matrix.
 
-Detailed_Output

    mout          This variable is a double precision, 6x6 matrix which
                  contains the transpose of m1.  mout may overwrite m1.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This is a utility routine intended to facilitate passing state
   transformation matrices between C and Fortan.

-Examples

   Given below is one example of a matrix m1 with the output matrix
   mout which is implied by m1.

           | 1  2  3  4  5  6  |                | 1  0  0  0  0  0 |
           | 0  7  8  9  10 11 |                | 2  7  0  0  0  0 |
           | 0  0  12 13 14 15 |                | 3  8  12 0  0  0 |
      m1=  | 0  0  0  16 17 18 |   then  mout = | 4  9  13 16 0  0 |
           | 0  0  0  0  19 20 |                | 5  10 14 17 19 0 |
           | 0  0  0  0  0  21 |                | 6  11 15 18 20 21|

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman       (JPL)
   W.L. Taber         (JPL)
   W.M. Owen          (JPL)

-Version

   -CSPICE Version 1.0.3, 08-JAN-2014 (BVS)

      Corrected a minor typo in the header.

   -CSPICE Version 1.0.2, 16-JAN-2008   (EDW)

      Corrected typos in header titles:
      
      Detailed Input to Detailed_Input
      Detailed Output to Detailed_Output

   -CSPICE Version 1.0.1, 10-NOV-2006   (EDW)

      Added Keywords and Parameters section headers. 
      Reordered section headers.

   -CSPICE Version 1.0.0, 16-APR-1999 (NJB)

-Index_Entries

      transpose a 6x6_matrix

-&
*/

{  /* Begin xpose6_c */


   /*
   Local constants
   */
   #define   SIZE          6                   
   #define   SIZESQ        36                   

   /*
   Local variables
   */
   SpiceInt                col;
   SpiceInt                row;

   SpiceDouble             temp[SIZE][SIZE];
   

   /*
   Capture a temporary copy of the input matrix.
   */
   MOVED ( m1, SIZESQ, temp );

   /*
   Move the temporary matrix to the output matrix, transposing as
   we go.
   */
   for ( row = 0;  row < SIZE;  row++ )
   {
      for ( col = 0;  col < SIZE;  col++ )
      {
         mout[row][col] = temp[col][row];
      }
   }

} /* End xpose6_c */
