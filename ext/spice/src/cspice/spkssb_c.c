/*

-Procedure spkssb_c ( S/P Kernel, solar system barycenter )

-Abstract
 
   Return the state (position and velocity) of a target body 
   relative to the solar system barycenter. 
 
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
   #include "SpiceZmc.h"


   void spkssb_c ( SpiceInt           targ,
                   SpiceDouble        et,
                   ConstSpiceChar   * ref,
                   SpiceDouble        starg[6] ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   targ       I   Target body. 
   et         I   Target epoch. 
   ref        I   Target reference frame. 
   starg      O   State of target. 
 
-Detailed_Input
 
   targ        is the standard NAIF ID code for a target body. 
 
   et          is the epoch (ephemeris time) at which the state of the
               target body is to be computed.
 
   ref         is the name of the reference frame to which the vectors
               returned by the routine should be rotated. This may be
               any frame supported by the CSPICE frame system,
               including dynamic and other non-inertial frames.

-Detailed_Output
 
   starg       contains the position and velocity of the target body,
               relative to the solar system barycenter, at epoch 'et'.
               These vectors are rotated into the specified reference
               frame. Units are always km and km/sec.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If sufficient information has not been "loaded" via the 
      routine spklef_c or the PCK kernel loaders, the problem will 
      be diagnosed by a routine in the call tree of this routine. 
 
   2) The error SPICE(EMPTYSTRING) is signaled if the input
      string 'ref' does not contain at least one character, since the
      input string cannot be converted to a Fortran-style string
      in this case.
      
   3) The error SPICE(NULLPOINTER) is signaled if the input string
      pointer 'ref' is null.

-Files
 
   See:  Restrictions. 
 
-Particulars
 
   In order to compute the state of one body relative to another, 
   the states of the two bodies must be known relative to a third 
   body. One simple solution is to use the solar system barycenter 
   as the third body. 
 
   Ephemeris data from more than one segment may be required 
   to determine the state of a body relative to the barycenter. 
   spkssb_c reads as many segments as necessary, from as many 
   files as necessary, using files that have been loaded by 
   previous calls to spklef_c (load ephemeris file). 
 
-Examples
 
   In the following code fragment, spkssb_c is used to display 
   the distance from Earth (Body 399) to Mars (body 499) at 
   a series of epochs. 
 
      #include <stdio.h>
      #include "SpiceUsr.h"


      #define EARTH   399 
      #define MARS    499 
          . 
          . 
          . 
      spklef_c ( "DE125.SPK", &handle ); 
          . 
          . 
          . 
 
      while ( epoch <= end )
      {
         spkssb_c ( EARTH, epoch, "J2000", searth ); 
         spkssb_c ( MARS,  epoch, "J2000", smars  );
 
         printf ( "%f   %22.15e\n", epoch, vdist_c( searth, smars )  ); 
 
         epoch += delta;
      }
      
 
-Restrictions
 
   1) The ephemeris files to be used by spkssb_c must be loaded 
      by spklef_c before spkssb_c is called. 
 
-Literature_References
 
   NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and 
   User's Guide" 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.0.2, 20-NOV-2004 (NJB)

      Updated description of input argument `ref' to indicate all
      frames supported by CSPICE are allowed.

   -CSPICE Version 1.0.1, 14-OCT-2003 (EDW)

      Various minor corrections to the header.

   -CSPICE Version 1.0.0, 23-JUN-1999 (NJB) (WLT) (IMU)

-Index_Entries
 
   state relative to solar system barycenter 
 
-&
*/

{ /* Begin spkssb_c */

 

   /*
   Participate in error tracing.
   */
   chkin_c ( "spkssb_c" );


   /*
   Check the input string 'ref' to make sure the pointer
   is non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkssb_c", ref );


   spkssb_ (  ( integer    * ) &targ,
              ( doublereal * ) &et,
              ( char       * ) ref,
              ( doublereal * ) starg,
              ( ftnlen       ) strlen(ref)  );


   chkout_c ( "spkssb_c" );

} /* End spkssb_c */
