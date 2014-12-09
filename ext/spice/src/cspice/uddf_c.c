/*

-Procedure uddf_c ( First derivative of a function, df(x)/dx )

-Abstract

   Routine to calculate the first derivative of a caller-specified
   function using a three-point estimation.

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

   DERIVATIVE

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef   uddf_c

   void uddf_c (  void             ( * udfunc ) ( SpiceDouble    et,
                                                  SpiceDouble  * value ),
                  SpiceDouble          x,
                  SpiceDouble          dx,
                  SpiceDouble        * deriv )

/*
-Brief_I/O

     Variable  I/O  Description
     --------  ---  --------------------------------------------------
     udfunc     I   Name of the routine that computes the scalar value
                    of interest.
     x          I   Independent variable of 'udfunc'
     dx         I   Interval from 'x' for derivative calculation
     deriv      O   Approximate derivative of 'udfunc' at 'x'

-Detailed_Input

     udfunc     is an externally specified routine that returns the 
                value of the scalar quantity function of interest
                at x. 

                The prototype for 'udfunc' is

                    void   ( * udfunc ) ( SpiceDouble    et,
                                          SpiceDouble  * value )

                where: 
 
                    et      an input double precision value of the independent
                            variable the function at which to determine the
                            scalar value.  
 
                    value   the scalar double precision value of 'udfunc' 
                            at 'x'.

     x          a scalar double precision value representing the independent 
                variable at which to determine the derivative of 'udfunc'.

                For many SPICE uses, 'x' will represent the TDB ephemeris
                time.

     dx         a scalar double precision value representing half the 
                interval in units of X separating the evaluation
                epochs of UDFUNC; the evaluations occur at (x + dx)) 
                and (x - dx).

                'dx' may be negative but must be non-zero.
     
-Detailed_Output

     deriv      the scalar double precision approximate value of the 
                first derivative of udfunc with respect to 'x'.

                Functionally:

                            d  udfunc ( x )
                   deriv =  --
                            dx

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   This routine provides a simple interface to numerically calculate
   the first derivative of a scalar quantity function.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine 
   specific arithmetic implementation. 


      #include <stdio.h>
      #include "SpiceUsr.h"

      void udfunc ( SpiceDouble et, SpiceDouble * value );
                   
      int main()
         {

         SpiceDouble       et;
         SpiceDouble       dt;
         SpiceDouble       deriv;

         /.
         Load leapsecond and SPK kernels. The name of the 
         meta kernel file shown here is fictitious; you 
         must supply the name of a file available 
         on your own computer system.
         ./

         furnsh_c ( "standard.tm" );

         /.
         Use a shift of one second off the epoch of interest.
         ./
         dt = 1.;

         /.
         Convert the epoch date string to ephemeris seconds.
         ./
         str2et_c ( "JAN 1 2009", &et );

         /.
         Calculate the derivative of UDFUNC at ET.
         ./
         uddf_c( udfunc, et, dt, &deriv );

         /.
         Output the calculated derivative.
         ./

         printf( "%18.12f\n", deriv );

         return ( 0 );
         }


      /.
      A scalar quantity function that returns the light-time
      between the Moon and Mercury at 'et'.
      ./

      void udfunc ( SpiceDouble et, SpiceDouble * value )
         {

         SpiceDouble          lt;
         SpiceDouble          pos[3];

         /.
         Evaluate the apparent position of Mercury with respect 
         to the Moon at 'et'.
         ./
         spkpos_c ( "MERCURY", et, "J2000", "LT+S", "MOON", pos, &lt );
         
         /.
         Return the light-time value as the scalar quantity.
         ./
         *value = lt;

         return;
         }

   The program outputs: 

      -0.000135670940

-Restrictions

   'udfunc' must evaluate to real values at x + dx and x - dx.
   
-Literature_References

   See qderiv.c header.

-Author_and_Institution

   N.J. Bachman   (JPL)
   E.D. Wright    (JPL)

-Version

   CSPICE Version 1.0.0  31-MAR-2010 (EDW) 

-Index_Entries

   first derivative of a function

-&
*/

   {  /* Begin uddf_c */

   /*
   Local variables
   */

   SpiceInt                  n;
   SpiceDouble               dfdx  [1];
   SpiceDouble               udval [2];

   /*
   Participate in error tracing.
   */
   if ( return_c() )
      {
      return;
      }
   chkin_c ( "uddf_c" );

   /*
   Apply a three-point estimation of the derivative for 'udfunc' at
   'x' by evaluating udfunc at [x-dx, x+dx].
   
   The qderiv_ call returns a single value in the 'dfdx' array.
   */
   n = 1;

   udfunc ( x - dx, &(udval[0]) );
   udfunc ( x + dx, &(udval[1]) );

   (void) qderiv_( (integer    *) &n, 
                   (doublereal *) &(udval[0]), 
                   (doublereal *) &(udval[1]), 
                   (doublereal *) &dx, 
                   (doublereal *) dfdx );

   *deriv = dfdx[0];

   chkout_c (  "uddf_c" );
   }
   
