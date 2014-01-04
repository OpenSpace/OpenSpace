/*

-Procedure vnormg_c ( Vector norm, general dimension )

-Abstract
 
   Compute the magnitude of a double precision vector of arbitrary 
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
   #include "SpiceZmc.h"
   #undef    vnormg_c
   

   SpiceDouble vnormg_c ( ConstSpiceDouble   * v1,
                          SpiceInt             ndim ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
    v1        I     Vector whose magnitude is to be found. 
    ndim      I     Dimension of v1. 
 
-Detailed_Input
 
   v1      This may be any double precision vector or arbitrary 
           size. 
 
-Detailed_Output
 
   vnormg_c is the magnitude of v1 calculated in a numerically stable 
   way. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If ndim is not physically realistic, greater than zero, a
       BADDIMENSION error is signaled.  The value 0. is returned.
 
-Files
 
   None. 
 
-Particulars
 
   vnormg_c finds the component of v1 whose magnitude is the largest. 
   If the absolute magnitude of that component indicates that a 
   numeric overflow would occur when it is squared, or if it 
   indicates that an underflow would occur when squared (falsely 
   giving a magnitude of zero) then the following expression is 
   used: 
 
   vnormg_c = v1max * MAGNITUDE OF [ (1/v1max)*v1 ] 
 
   therwise a simpler expression is used: 
 
   vnormg_c = MAGNITUDE OF [ v1 ] 
 
   Beyond the logic described above, no further checking of the 
   validity of the input is performed. 
 
-Examples
 
   The following table show the correlation between various input 
   vectors v1 and vnormg_c: 
 
   ndim   v1                       ndim        vnormg_c 
   ----------------------------------------------------------------- 
   1      (-7.0D20)                 1            7.D20 
   3      (1., 2., 2.)              3            3. 
   4      (3., 3., 3., 3.)          4            6. 
   5      (5., 12., 0., 0., 0.)     5            13. 
   3      (-5.D-17, 0.0, 12.D-17)   3            13.D-17 
 
-Restrictions
 
   None. 
 
-Author_and_Institution
 
   W.M. Owen       (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

      Made input vector const.

   -CSPICE Version 1.0.0, 1-APR-1998   (EDW)

-Index_Entries
 
   norm of n-dimensional vector 
 
-&
*/

{ /* Begin vnormg_c */

   /*
   Local variables
   */

   SpiceInt                i;
   SpiceDouble             norm;
   SpiceDouble             scale;


   /*
   Use discovery check-in.
   */

   /* Initialize norm and scale to zero. */
   
   norm  = 0.;
   scale = 0.;


   /* Check ndim is cool.  Dimension is positive definite. */

   if ( ndim <= 0 )
      {
      
      chkin_c    ( "vnormg_c"                                     );
      SpiceError ( "Vector dimension less than or equal to zero",
                   "BADDIMENSION"                                 );
      chkout_c   ( "vnormg_c"                                     );
      return     ( 0.                                             );
      
      }


   /*
   Determine an appropriate scale factor to prevent numerical
   overflow.  Overflow would be bad!
   */

   for ( i = 0; i < ndim; i++ )
      {
      scale = MaxAbs( scale, v1[i] );
      }


   /* If the vector is zero, return zero. */

   if ( scale == 0. )
      {
      return 0.;
      }


   /* Do the calculation.  Not very involved. */

   for ( i = 0; i < ndim; i++ )
      {
      norm += pow( v1[i] / scale, 2 );
      }



    /* Return the value. */


   return ( scale * sqrt( norm ) );


} /* End vnormg_c */
