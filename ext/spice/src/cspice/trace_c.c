/*

-Procedure trace_c ( Trace of a 3x3 matrix )

-Abstract
 
    Return the trace of a 3x3 matrix. 
 
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
 
    MATRIX 
 
*/

   #include "SpiceUsr.h"
   #undef    trace_c

   SpiceDouble trace_c ( ConstSpiceDouble  matrix[3][3] ) 

/*

-Brief_I/O
 
    VARIABLE  I/O  DESCRIPTION 
    --------  ---  -------------------------------------------------- 
    matrix     I     3x3 matrix of double precision numbers. 
    trace      O     The trace of matrix. 
 
-Detailed_Input
 
    matrix  is a double precision 3x3 matrix. 
 
-Detailed_Output
 
    trace   is the trace of matrix, i.e. it is the sum of the 
            diagonal elements of matrix. 
 
-Parameters
 
   None. 
 
-Particulars
 
    trace_c simply executes in C code the following loop: 
 
    trace_c = Summation from i = 1 to 3 of matrix[i][i]
 
    This functions implements no error detection. 
 
-Examples
 
                          | 3   5   7 | 
    Suppose that matrix = | 0  -2   8 |  , then 
                          | 4   0  -1 | 
 
    trace_c (matrix) = 0.  (which is the sum of 3, -2 and -1). 
 
-Restrictions
 
    No checking is performed to guard against floating point overflow 
    or underflow.  This routine should probably not be used if the 
    input matrix is expected to have large double precision numbers 
    along the diagonal. 
 
-Exceptions
 
    Error free. 
 
-Files
 
    None 
 
-Author_and_Institution
 
    W.L. Taber      (JPL) 
    E.D. Wright     (JPL)
 
-Literature_References
 
    None 
 
-Version
 
   -CSPICE Version 1.0.0, 29-JUN-1999

-Index_Entries
 
   trace of a 3x3_matrix 
 
-&
*/

{ /* Begin trace_c */

   /*
   Local variables
   */
   SpiceInt          i;
   SpiceDouble       trace = 0.;


   /* Do it.  This isn't rocket science. */
   for ( i = 0; i < 3; i++ )
      {
      trace += matrix[i][i];
      }

   return trace;


} /* End trace_c */
