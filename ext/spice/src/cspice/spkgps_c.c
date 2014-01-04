/*

-Procedure spkgps_c ( S/P Kernel, geometric position )

-Abstract
 
   Compute the geometric position of a target body relative to an 
   observing body. 
 
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
 
   SPK 
 
-Keywords
 
   EPHEMERIS 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void spkgps_c ( SpiceInt           targ,
                   SpiceDouble        et,
                   ConstSpiceChar   * ref,
                   SpiceInt           obs,
                   SpiceDouble        pos[3],
                   SpiceDouble      * lt     ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   targ       I   Target body. 
   et         I   Target epoch. 
   ref        I   Target reference frame. 
   obs        I   Observing body. 
   pos        O   Position of target. 
   lt         O   Light time. 
 
-Detailed_Input
 
   targ        is the standard NAIF ID code for a target body. 
 
   et          is the epoch (ephemeris time) at which the position 
               of the target body is to be computed. 
 
   ref         is the name of the reference frame to 
               which the vectors returned by the routine should 
               be rotated. This may be any frame supported by 
               the CSPICE subroutine sxform_c. 
 
   obs         is the standard NAIF ID code for an observing body. 
 
-Detailed_Output
 
   pos         contains the position of the target 
               body, relative to the observing body. This vector is 
               rotated into the specified reference frame. Units 
               are always km. 
 
   lt          is the one-way light time from the observing body 
               to the geometric position of the target body at the 
               specified epoch. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If insufficient ephemeris data has been loaded to compute 
      the necessary positions, the error SPICE(SPKINSUFFDATA) is 
      signalled. 
 
-Files
 
   See: $Restrictions. 
 
-Particulars
 
   spkgps_c computes the geometric position, T(t), of the target 
   body and the geometric position, O(t), of the observing body 
   relative to the first common center of motion.  Subtracting 
   O(t) from T(t) gives the geometric position of the target 
   body relative to the observer. 
 
 
      CENTER ----- O(t) 
          |      / 
          |     / 
          |    / 
          |   /  T(t) - O(t) 
          |  / 
         T(t) 
 
 
   The one-way light time, tau, is given by 
 
 
             | T(t) - O(t) | 
      tau = ----------------- 
                    c 
 
 
   For example, if the observing body is -94, the Mars Observer 
   spacecraft, and the target body is 401, Phobos, then the 
   first common center is probably 4, the Mars Barycenter. 
   O(t) is the position of -94 relative to 4 and T(t) is the 
   position of 401 relative to 4. 
 
   The center could also be the Solar System Barycenter, body 0. 
   For example, if the observer is 399, Earth, and the target 
   is 299, Venus, then O(t) would be the position of 399 relative 
   to 0 and T(t) would be the position of 299 relative to 0. 
 
   Ephemeris data from more than one segment may be required 
   to determine the positions of the target body and observer 
   relative to a common center.  spkgps_c reads as many segments 
   as necessary, from as many files as necessary, using files 
   that have been loaded by previous calls to spklef_c (load 
   ephemeris file). 
 
   spkgps_c is similar to spkgeo_c but returns geometric positions 
   only. 
 
-Examples
 
   The following code example computes the geometric 
   position of the moon with respect to the earth and 
   then prints the distance of the moon from the 
   the earth at a number of epochs. 
 
   Assume the SPK file SAMPLE.BSP contains ephemeris data 
   for the moon relative to earth over the time interval 
   whose endpoints are represented by the variables begin and
   end. 
 
 
      #include <stdio.h>
      #include "SpiceUsr.h"
          .
          .
          .
      
      int main()
      {
 
         #define EARTH           399
         #define MOON            301
         #define N               100
         #define TIMLEN          30
         
         SpiceChar               utc    [TIMLEN];

         SpiceDouble             begin;
         SpiceDouble             delta;
         SpiceDouble             end;
         SpiceDouble             et;
         SpiceDouble             pos    [3];

         SpiceInt                handle;

         /.
         Load the binary SPK ephemeris file. 
         ./
         
         spklef_c ( "SAMPLE.BSP", &handle );
 
             . 
             . 
             . 
 
         /.
         Divide the interval of coverage [begin,end] into 
         n steps.  At each step, compute the position, and 
         print out the epoch in UTC time and position norm. 
         ./
         
         delta = ( end - begin ) / n 
 
         for ( i = 0;  i < N;  i++ )
         {
            et  =  begin + i * delta;
             
            spkgps_c ( MOON, et, "J2000", EARTH, pos, &lt );
 
            et2utc_c ( et, "C", 0, utc );
 
            printf   ( "%s  %25.15e\n", utc, vnorm_c(pos) );
         } 
     
         return ( 0 );
      }   
      

-Restrictions
 
   1) The ephemeris files to be used by spkgps_c must be loaded 
      by spklef_c before spkgps_c is called. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman (JPL)
   J.E. McLean  (JPL) 
   W.L. Taber   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 30-MAY-1999 (NJB) (JEM) (WLT)

-Index_Entries
 
   geometric position of one body relative to another 
 
-&
*/

{ /* Begin spkgps_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "spkgps_c" );


   /*
   Check the input string ref to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkgps_c", ref );

   /*
   Call the f2c'd routine.
   */
   
   spkgps_ (  ( integer     * ) &targ,
              ( doublereal  * ) &et,
              ( char        * ) ref,
              ( integer     * ) &obs,
              ( doublereal  * ) pos,
              ( doublereal  * ) lt,
              ( ftnlen        ) strlen(ref)  );
              
   chkout_c ( "spkgps_c" );

} /* End spkgps_c */
