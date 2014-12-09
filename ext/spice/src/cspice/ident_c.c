/*

-Procedure ident_c (Return the 3x3 identity matrix)

-Abstract
 
  This routine returns the 3x3 identity matrix. 
 
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
   #include "SpiceZmc.h"

   void ident_c ( SpiceDouble    matrix[3][3] ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   matrix     O   is the 3x3 identity matrix. 
 
-Detailed_Input
 
   None. 
 
-Detailed_Output
 
   matrix     is the 3x3 Identity matrix.  That is MATRIX is 
              the following 
                _                 _ 
               |  1.0   0.0   0.0  |
               |  0.0   1.0   0.0  | 
               |  0.0   0.0   1.0  | 
                -                  - 
-Parameters
 
   None. 
 
-Files
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Particulars
 
   This is a utility routine for obtaining the 3x3 identity matrix 
   so that you may avoid having to write the loop or assignments 
   needed to get the matrix. 
 
-Examples
 
   Suppose that you need to construct the matrix sum 
 
      ident + omega 
 
   where omega is some matrix you have already computed. 
 
   The code fragment below shows how you could accomplish this 
   with this routine. 
 
      First get the Identity matrix 
      
      #include "SpiceUsr.h"
            .
            .
            .
         
      SpiceDouble             ident[3][3];
 
      ident_c ( ident ); 
      vaddg_c ( ident, omega, 9, sum ); 
 
 
-Restrictions
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.0, 1-JUN-1999 (NJB) (WLT)

-Index_Entries
 
   Get the 3x3 identity matrix 
 
-&
*/

{ /* Begin ident_c */

 
   matrix[0][0] =  1.0;
   matrix[0][1] =  0.0;
   matrix[0][2] =  0.0;
   matrix[1][0] =  0.0;
   matrix[1][1] =  1.0;
   matrix[1][2] =  0.0;
   matrix[2][0] =  0.0;
   matrix[2][1] =  0.0;
   matrix[2][2] =  1.0;
   
} /* End ident_c */
