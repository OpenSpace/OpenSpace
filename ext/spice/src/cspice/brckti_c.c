/*

-Procedure brckti_c (Bracket an integer value within an interval)

-Abstract
 
   Bracket a number. That is, given a number and an acceptable 
   interval, make sure that the number is contained in the 
   interval. (If the number is already in the interval, leave it 
    alone. If not, set it to the nearest endpoint of the interval.) 
 
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


   SpiceInt brckti_c ( SpiceInt  number, 
                       SpiceInt  end1,
                       SpiceInt  end2   )
/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   number     I   Number to be bracketed. 
   end1       I   One of the bracketing endpoints for number. 
   end2       I   The other bracketing endpoint for number. 
  
   The function returns the bracketed number.    
 
-Detailed_Input

   number      is the number to be bracketed. That is, the 
               value of number is constrained to lie in the 
               interval bounded by end1 and end2. 

   end1, 
   end2        are the lower and upper bounds for number. The 
               order is not important. 
 
-Detailed_Output
 
   The function returnes the input number, if it was already in the 
   interval provided. Otherwise the returned value is the nearest 
   bound of the interval. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Particulars
 
   This routine provides a shorthand notation for code fragments 
   like the following 

      #include "SpiceUsr.h"
            .
            .
            .
      if ( number < end 1 )
      {
         number = end1;
      }
      else if ( number > end2 )
      {
         number = end2;
      }
      

   which occur frequently during the processing of program inputs. 
 
-Examples
 
    The following illustrates the operation of brckti_c. 
 
          brckti_c (  -1,   1,  10 )  =  1.0; 
          brckti_c (  29,   1,  10 )  = 10.0; 
          brckti_c (   3, -10,  10 )  =  3.0; 
          brckti_c (   3, -10,  -1 )  = -1.0; 
 
    The following code fragment illustrates a typical use for brckti_c. 
 
       #include "SpiceUsr.h"
            .
            .
            .
       /.
       Number of time steps must be in the range 1-10. 
       ./    
       
       prompt_c ( "Enter number of time steps > ", 80, nStr );
       
       prsint_c ( nStr, &n );
       
       nstep = brckti_c ( n, 1, 10 );
       

-Restrictions
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Literature_References
 
   None. 
 
-Version

   -CSPICE Version 1.0.1, 11-NOV-2006 (EDW)

      Added "None." text to Keywords section, required for
      API doc script (cspicehtml.pl) integrity checks.
 
   -CSPICE Version 1.0.0, 16-AUG-1999 (NJB) (WLT) (IMU)

-Index_Entries
 
   bracket an integer value within an interval 
 
-&
*/

{ /* Begin brckti_c */

   if ( number < end1 )
   {
      return ( end1 );
   }
   else if ( number > end2 )
   {
      return ( end2 );
   }
   
   return ( number );
      
} /* End brckti_c */
