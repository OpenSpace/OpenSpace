/*
 
-Procedure vsclg_c ( Vector scaling, general dimension )
 
-Abstract
 
   Multiply a scalar and a double precision vector of arbitrary
   dimension.
 
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
   #undef    vsclg_c
   
 
   void vsclg_c ( SpiceDouble          s,
                  ConstSpiceDouble   * v1,
                  SpiceInt             ndim,
                  SpiceDouble        * vout )
/*
 
-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   s          I     Scalar to multiply a vector.
   v1         I     Vector to be multiplied.
   ndim       I     Dimension of v1 (and also vout).
   vout       O     Product vector, s*v1. vout can overwrite v1.
 
-Detailed_Input
 
   s      is a double precision scalar.
 
   v1     is a double precision vector of arbitrary dimension.
 
   ndim   is the dimension of v1 (and vout).
 
-Detailed_Output
 
   vout   is a double precision vector of arbitrary dimension
          containing the product of the scalar with the vector v1.
          vout may overwrite v1.
 
-Parameters
 
   None.
 
-Exceptions
 
   Error free.
 
-Particulars
 
   For each value of the index i from 0 to ndim-1, this subroutine
   performs the following multiplication
 
      vout[i] = s * v1[i];
 
   No error checking is performed to guard against numeric overflow
   or underflow.  vout may overwrite v1.
 
-Examples
 
   The following table shows the results of vsclg_c from various
   inputs.
 
   v1                 s           ndim        vout
   -----------------------------------------------------------------
   (1, 2, -3, 4)      3            4         ( 3,  6, -9, 12)
   (1, 2, -3, 4)      0            4         ( 0,  0,  0,  0)
   (1, 2, -3, 4)     -1            4         (-3, -6,  9,-12)
 
-Restrictions
 
   No error checking is performed to guard against numeric overflow.
   The programmer is thus required to insure that the values in v1
   and s are reasonable and will not cause overflow.
 
-Files
 
   None.
 
-Author_and_Institution
 
   W.M. Owen       (JPL)
 
-Literature_References
 
   None.
 
-Version
 
   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

      Made input vector const.  Removed #includes of SpiceZfc.h and
      SpiceZst.h.

  -CSPICE Version 1.0.0, 13-JUL-1998 (NJB) (WMO)
 
-Index_Entries
 
   n-dimensional vector scaling
 
-&
*/
 
{ /* Begin vsclg_c */
 
 
   /*
   Local variables
   */
 
   SpiceInt                i;
 
 
   for ( i = 0;  i < ndim;  i++ )
   {
      vout[i] = s * v1[i];
   }
 
 
} /* End vsclg_c */
