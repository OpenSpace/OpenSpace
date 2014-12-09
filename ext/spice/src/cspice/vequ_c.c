/*

-Procedure vequ_c ( Vector equality, 3 dimensions )

-Abstract
 
    Make one double precision 3-dimensional vector equal to 
    another. 
 
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
 
    ASSIGNMENT,  VECTOR 
 
*/

   #include "SpiceUsr.h"
   #undef    vequ_c


   void vequ_c ( ConstSpiceDouble   vin[3],
                 SpiceDouble        vout[3] ) 
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   vin       I   3-dimensional double precision vector. 
   vout      O   3-dimensional double precision vector set equal 
                 to vin. 
 
-Detailed_Input
 
   vin      This may be ANY 3-dimensional double precision vector. 
 
-Detailed_Output
 
   vout    This 3-dimensional double precision vector is set equal 
           to vin. 
 
-Parameters
 
   None. 
 
-Particulars
 
   vequ_c simply sets each component of vout in turn equal to vin.  No 
   error checking is performed because none is needed. 
 
-Examples
 
   Let state be a state vector. The angular momentum vector is 
   determined by the cross product of the position vector and the 
   velocity vector. 
 
    vequ_c ( state[0], R );
    vequ_c ( state[3], V ); 
 
    vcrss_c ( R, V, H );
 
 
-Restrictions
 
    None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
    None. 
 
-Author_and_Institution
 
    W.M. Owen       (JPL) 
 
-Literature_References
 
    None. 
 
-Version
 
   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

      Made input vector const.  Removed #include of SpiceZfc.h.

   -CSPICE Version 1.0.0, 08-FEB-1998   (EDW)

-Index_Entries
 
   assign a 3-dimensional vector to another 
 
-&
*/

{ /* Begin vequ_c */

   vout[0] = vin[0];
   vout[1] = vin[1];
   vout[2] = vin[2];


} /* End vequ_c */
