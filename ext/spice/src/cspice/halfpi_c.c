/*

-Procedure halfpi_c ( Half the value of pi )

-Abstract
 
   Return half the value of pi (the ratio of the circumference of 
   a circle to its diameter). 
 
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
 
   CONSTANTS 
 
*/

   #include <math.h>
   #include "SpiceUsr.h"

   SpiceDouble halfpi_c ( void ) 

/*

-Brief_I/O
 
   The function returns half the value of pi. 
 
-Detailed_Input
 
   None. 
 
-Detailed_Output
 
   The function returns half the value of pi (the ratio of 
   a circle's circumference to its diameter), determined by 
   the ACOS function. That is, 
 
         halfpi_c = acos ( -1.0 ) * 0.50 
 
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Particulars
 
   The first time the function is referenced, the value is computed 
   as shown above. The value is saved, and returned directly upon 
   subsequent reference. 
 
-Examples
 
   The subroutine shown below illustrates the use of halfpi_c. 
 
            void bftran ( ra, dec, w, tipm )
               {
 
                /. 
                Compute the transformation from inertial to body 
                fixed coordinates, given the directions of the north 
                pole and prime meridian of the body. 
                ./

                SpiceDouble   ra;
                SpiceDouble   dec; 
                SpiceDouble   w;
                SpiceDouble   tipm [3][3];
 
 
                /.
                The transformation is defined by the compund 
                rotation 
           
                  [W] [pi/2 - Dec] [RA + pi/2] 
                     3            1           3 
                ./


                rotate_c (        ra + halfpi_c(), 3, tipm );
                rotmat_c ( tipm, halfpi_c() - dec, 1, tipm );
                rotmat_c ( tipm, w,                3, tipm );
 
               } 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution

   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
   E.D. Wright     (JPL)
 
-Version

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries
 
   half the value of pi 
 
-&
*/

{ /* Begin halfpi_c */

   /*
   Local Variables
   */

   static SpiceDouble  value = 0.;


   if ( value == 0.)
      {
      value = 0.5 * acos( -1. );
      }


   return value;


} /* End halfpi_c */
