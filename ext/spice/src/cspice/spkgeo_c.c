/*

-Procedure spkgeo_c ( S/P Kernel, geometric state )

-Abstract
 
   Compute the geometric state (position and velocity) of a target 
   body relative to an observing body.

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
   #include "SpiceZmc.h"
   #include "SpiceZfc.h"

   void spkgeo_c ( SpiceInt          targ, 
                   SpiceDouble       et, 
                   ConstSpiceChar  * ref, 
                   SpiceInt          obs, 
                   SpiceDouble       state[6], 
                   SpiceDouble     * lt       ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   targ       I   Target body. 
   et         I   Target epoch. 
   ref        I   Target reference frame. 
   obs        I   Observing body. 
   state      O   State of target. 
   lt         O   Light time. 
 
-Detailed_Input
 
   targ        is the standard NAIF ID code for a target body. 
 
   et          is the epoch (ephemeris time) at which the state 
               of the target body is to be computed. 
 
   ref         is the name of the reference frame to which the state 
               vector returned by the routine should be rotated. This 
               may be any frame supported by the SPICELIB subroutine 
               FRMCHG.  See also the Frames Required Reading for a list
               of supported frames.
 
   obs         is the standard NAIF ID code for an observing body. 
 
-Detailed_Output
 
   state       contains the geometric position and velocity of the
               target body, relative to the observing body, at epoch
               'et'. 'state' has six elements: the first three contain
               the target's position; the last three contain the
               target's velocity. These vectors are transformed into
               the specified reference frame.

               Units are always km and km/sec. 
 
   lt          is the one-way light time from the observing body 
               to the geometric position of the target body
               in seconds at the specified epoch. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If insufficient ephemeris data have been loaded to compute 
      the requested state, the error SPICE(SPKINSUFFDATA) is 
      signalled. 
 
-Files
 
   See Restrictions. 
 
-Particulars
 
   spkgeo_c computes the geometric state, T(t), of the target 
   body and the geometric state, O(t), of the observing body 
   relative to the first common center of motion.  Subtracting 
   O(t) from T(t) gives the geometric state of the target 
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
   O(t) is the state of -94 relative to 4 and T(t) is the 
   state of 401 relative to 4. 
 
   The center could also be the Solar System Barycenter, body 0. 
   For example, if the observer is 399, Earth, and the target 
   is 299, Venus, then O(t) would be the state of 399 relative 
   to 0 and T(t) would be the state of 299 relative to 0. 
 
   Ephemeris data from more than one segment may be required 
   to determine the states of the target body and observer 
   relative to a common center.  spkgeo_c reads as many segments 
   as necessary, from as many files as necessary, using files 
   that have been loaded by previous calls to spklef_c (load 
   ephemeris file). 
 
   spkgeo_c is similar to spkez_c but returns geometric states 
   only, with no option to make planetary (light-time) nor 
   stellar aberration corrections.  The geometric states 
   returned by spkez_c and spkgeo_c are the same. 
 
-Examples
 
   The following code example computes the geometric 
   state of the moon with respect to the earth and 
   then prints the distance of the moon from the 
   the earth at a number of epochs. 
 
   Assume the SPK file sample.bsp contains ephemeris data 
   for the moon relative to earth over the time interval 
   from BEGIN to END. 
 
      #include <stdio.h>
      #include "SpiceUsr.h"
      
      #define   EARTH         399
      #define   MOON          301
      #define   N             100
      #define   UTCLEN        35


      SpiceChar               utc [ 20 ];
 
      SpiceInt                handle;
      SpiceInt                i;

      SpiceDouble             begin;
      SpiceDouble             delta;
      SpiceDouble             end;
      SpiceDouble             et;
      SpiceDouble             lt;
      SpiceDouble             state [6];

 
      /.
      Load the binary SPK ephemeris file. 
      ./
      furnsh_c ( "sample.bsp" ); 
 
          . 
          . 
          . 
 
      /.
      Divide the interval of coverage [BEGIN,END] into 
      N steps.  At each step, compute the state, and 
      print out the epoch in UTC time and position norm. 
      ./
      
      delta = ( end - begin ) / N;

      for ( i = 0;  i < N;  i++ ) 
      {
         et = begin + i*delta;

         spkgeo_c ( MOON, et, "J2000", EARTH, state, &lt ); 

         et2utc_c ( et, "c", 0, UTCLEN, utc ); 

         printf ( "UTC = %s; ||pos|| = %f\n", utc, vnorm_c(state) ); 
      }
 
 
 
-Restrictions
 
   1) The ephemeris files to be used by spkgeo_c must be loaded 
      by SPKLEF before spkgeo_c is called. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman  (JPL)
   J.E. McLean   (JPL) 
   W.L. Taber    (JPL) 
 
-Version
 
   -CSPICE Version 1.1.2, 08-JAN-2014 (BVS)

       Fixed description of 'state' in Detailed Output. Replaced 
       spklef_c with furnsh_c and fixed errors in Examples.

   -CSPICE Version 1.1.1, 13-OCT-2003 (EDW)

       Various minor header changes were made to improve clarity.
       Added mention that 'lt' returns a value in seconds.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)  
   
       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly.  String checks are now done using
       the macro CHKFSTR.
       
   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

      Based on SPICELIB Version 2.2.0, 11-APR-1997 (WLT)
      
-Index_Entries
 
   geometric state of one body relative to another 
 
-&
*/

{ /* Begin spkgeo_c */

     
   /*
   Participate in error tracing.
   */
   chkin_c ( "spkgeo_c" );


   /*
   Check the input string 'ref' to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkgeo_c", ref );
   

   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.
   */
   spkgeo_ (  ( integer    * )  &targ, 
              ( doublereal * )  &et, 
              ( char       * )  ref, 
              ( integer    * )  &obs,
              ( doublereal * )  state,
              ( doublereal * )  lt,
              ( ftnlen       )  strlen(ref)   );


   chkout_c ( "spkgeo_c" );

} /* End spkgeo_c */
