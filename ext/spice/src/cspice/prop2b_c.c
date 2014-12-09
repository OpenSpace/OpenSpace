/*

-Procedure prop2b_c ( Propagate a two-body solution )

-Abstract

   Given a central mass and the state of massless body at time t_0,
   this routine determines the state as predicted by a two-body
   force model at time t_0 + dt.

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

   CONIC
   EPHEMERIS
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZim.h"
   #undef    prop2b_c

   void prop2b_c ( SpiceDouble         gm,
                   ConstSpiceDouble    pvinit[6],
                   SpiceDouble         dt,
                   SpiceDouble         pvprop[6] )

/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   gm         I   Gravity of the central mass.
   pvinit     I   Initial state from which to propagate a state.
   dt         I   Time offset from initial state to propagate to.
   pvprop     O   The propagated state.

-Detailed_Input

   gm         is the gravitational constant G times the mass M of the
              central body.

   pvinit     is the state at some specified time relative to the
              central mass.  The mass of the object is assumed to
              be negligible when compared to the central mass.

   dt         is a offset in time from the time of the initial
              state to which the two-body state should be
              propagated. (The units of time and distance must be
              the same in gm, pvinit, and dt).

-Detailed_Output

   pvprop     is the two-body propagation of the initial state
              dt units of time past the epoch of the initial state.

-Parameters

   None.

-Exceptions

   1) If gm is not positive, the error SPICE(NONPOSITIVEMASS) will
      be signalled.

   2) If the position of the initial state is the zero vector, the
      error SPICE(ZEROPOSITION) will be signalled.

   3) If the velocity of the initial state is the zero vector, the
      error SPICE(ZEROVELOCITY) will be signalled.

   4) If the cross product of the position and velocity of pvinit
      has squared length of zero, the error SPICE(NONCONICMOTION)
      will be signalled.

   5) The value of dt must be "reasonable".  In other words, dt
      should be less than 10**20 seconds for realistic solar system
      orbits specified in the MKS system.  (The actual bounds
      on dt are much greater but require substantial computation.)
      The "reasonableness" of dt is checked at run-time.  If dt is
      so large that there is a danger of floating point overflow
      during computation, the error SPICE(DTOUTOFRANGE) is
      signalled and a message is generated describing the problem.

-Files

   None.

-Particulars

   This routine uses a universal variables formulation for the
   two-body motion of an object in orbit about a central mass. It
   propagates an initial state to an epoch offset from the
   epoch of the initial state by time dt.

   This routine does not suffer from the finite precision
   problems of the machine that are inherent to classical
   formulations based on the solutions to Kepler's equation:

         n( t - T ) = E - e Sin(E)         elliptic case
         n( t - T ) = e sinh(F) - F        hyperbolic case

   The derivation used to determine the propagated state is a
   slight variation of the derivation in Danby's book
   `Fundamentals of Celestial Mechanics' [1] .

-Examples

   When the eccentricity of an orbit is near 1, and the epoch
   of classical elements is near the epoch of periapse, classical
   formulations that propagate a state from elements tend to
   lack robustness due to the finite precision of floating point
   machines. In those situations it is better to use a universal
   variables formulation to propagate the state.

   By using this routine, you need not go from a state to elements
   and back to a state. Instead, you can get the state from an
   initial state.

   If pv is your initial state and you want the state 3600
   seconds later, the following call will suffice.

        Look up gm somewhere

        dt = 3600.0;

        prop2b_c ( gm, pv, dt, pvdt );

   After the call, pvdt will contain the state of the
   object 3600 seconds after the time it had state pv.

-Restrictions

   Users should be sure that gm, pvinit and dt are all in the
   same system of units ( for example MKS ).

-Literature_References

   [1] `Fundamentals of Celestial Mechanics', Second Edition
       by J.M.A. Danby;  Willman-Bell, Inc., P.O. Box 35025
       Richmond Virginia;  pp 168-180

-Author_and_Institution

   W.L. Taber     (JPL)
   N.J. Bachman   (JPL)
   E.D. Wright    (JPL)

-Version

   -CSPICE Version 1.1.0, 24-JUL-2001   (NJB)

       Changed protoype:  input pvinit is now type 
       (ConstSpiceDouble [6]). Implemented interface macro for 
       casting input pvinit to const.

   -CSPICE Version 1.0.1, 20-MAR-1998 (EDW)

      Minor correction to header.

   -CSPICE Version 1.0.0, 08-FEB-1998   (EDW)

-Index_Entries

    Propagate state vector using two-body force model

-&
*/

{ /* Begin prop2b_c */


   /*
   Participate in error handling.
   */

   chkin_c ( "prop2b_c");


   prop2b_ ( ( doublereal * ) &gm,
             ( doublereal * ) pvinit,
             ( doublereal * ) &dt,
             ( doublereal * ) pvprop );


   chkout_c ( "prop2b_c");


} /* End prop2b_c */
