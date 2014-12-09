/*

-Procedure det_c  ( Determinant of a double precision 3x3 matrix )

-Abstract
 
    Compute the determinant of a double precision 3x3 matrix. 
 
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
 
    MATRIX,  MATH 
 
*/

   #include "SpiceUsr.h"
   #undef    det_c
   

   SpiceDouble det_c ( ConstSpiceDouble m1[3][3] ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   m1         I     Matrix whose determinant is to be found. 
 
-Detailed_Input
 
   m1      This variable may be any double precision, 3x3 matrix. 
 
-Detailed_Output
 
   det_c   This is the value of the determinant found by direct 
           application of the definition of the determinant. 
 
-Parameters
 
   None. 
 
-Particulars
 
   det_c calculates the determinant of m1 in a single arithmetic 
   expression which is, effectively, the expansion of m1 about its 
   first row.  Since the calculation of the determinant involves 
   the multiplication of numbers whose magnitudes are unrestricted, 
   there is the possibility of floating point overflow or underflow. 
   NO error checking or recovery is implemented in this routine. 
 
-Examples
 
          | 1  2  3 | 
     M1 = | 4  5  6 |   ---->   det_c(m1) = 0 
          | 7  8  9 | 
  
          | 1  2  3 | 
     M1 = | 0  5  6 |   ---->   det_c(m1) = 45 
          | 0  0  9 | 
 
-Restrictions
 
   No checking is implemented to determine whether M1 will cause 
   overflow or underflow in the process of calculating the 
   determinant.  In most cases, this will not pose a problem. 
   The user is required to determine if M1 is suitable matrix 
   for det_c to operate on. 

-Exceptions
 
   Error free. 
 
-Files
 
   None.
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.M. Owen       (JPL) 
 
-Literature_References
 
   None 
 
-Version
 
   -CSPICE Version 1.0.0, 21-OCT-1998 (NJB)

-Index_Entries
 
   determinant of a d.p. 3x3_matrix 
 
-&
*/

{ /* Begin det_c */


   return (  ( m1[0][0] * ( m1[1][1]*m1[2][2] - m1[2][1]*m1[1][2] ) )
           - ( m1[0][1] * ( m1[1][0]*m1[2][2] - m1[2][0]*m1[1][2] ) )
           + ( m1[0][2] * ( m1[1][0]*m1[2][1] - m1[2][0]*m1[1][1] ) ) );


} /* End det_c */
