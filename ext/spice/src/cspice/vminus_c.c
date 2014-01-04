/*

-Procedure vminus_c ( Minus V, "-V", 3 dimensions )

-Abstract
 
   Negate a double precision 3-dimensional vector. 
 
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
 
   VECTOR 
 
*/

   #include "SpiceUsr.h"
   #undef    vminus_c
   

   void vminus_c ( ConstSpiceDouble v1[3],  SpiceDouble vout[3] ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   v1         I     Vector to be negated. 
   vout       O     Negated vector -v1. vout can overwrite v1. 
 
 
-Detailed_Input
 
   v1      This may be any 3-dimensional, double precision vector. 
 
-Detailed_Output
 
   vout    This will be the negation (additive inverse) of v1.  It 
           is a 3-dimensional, double precision vector.  vout may 
           overwrite v1. 
 
-Parameters
 
   None. 
 
-Particulars
 
   vminus_c implements (by components) the expression vminus_c = -v1. 
   No error checking is performed since overflow can occur ONLY if 
   the dynamic range of positive floating point numbers is not the 
   same size as the dynamic range of negative floating point 
   numbers AND at least one component of v1 falls outside the 
   common range.  The likelihood of this occuring is so small as to 
   be of no concern. 
 
-Examples
 
   The following table shows the output vout as a function of the 
   the input v1 from the subroutine vminus_c. 
 
   v1                     vout 
   ------------------------------------------------------- 
   (1, -2, 0)       (-1, 2, 0) 
   (0, 0,  0)       (0, 0,  0) 
 
-Restrictions
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Author_and_Institution
 
   W.M. Owen       (JPL)
   E.D. Wright     (JPL)
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

      Made input vector const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries
 
   negate a 3-dimensional vector 
 
-&
*/

{ /* Begin vminus_c */


   vout[0] = -v1[0];
   vout[1] = -v1[1];
   vout[2] = -v1[2];


} /* End vminus_c */
