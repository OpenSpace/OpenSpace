/*

-Procedure spkw17_c ( SPK, write a type 17 segment )

-Abstract
 
   Write an SPK segment of type 17 given a type 17 data record. 
 
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
   #undef    spkw17_c
   
   
   void spkw17_c ( SpiceInt           handle,
                   SpiceInt           body, 
                   SpiceInt           center, 
                   ConstSpiceChar   * frame, 
                   SpiceDouble        first,
                   SpiceDouble        last,
                   ConstSpiceChar   * segid, 
                   SpiceDouble        epoch,
                   ConstSpiceDouble   eqel   [9],
                   SpiceDouble        rapol,
                   SpiceDouble        decpol      )

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     I   Handle of an SPK file open for writing. 
   body       I   Body code for ephemeris object. 
   center     I   Body code for the center of motion of the body. 
   frame      I   The reference frame of the states. 
   first      I   First valid time for which states can be computed. 
   last       I   Last valid time for which states can be computed. 
   segid      I   Segment identifier. 
   epoch      I   Epoch of elements in seconds past J2000. 
   eqel       I   Array of equinoctial elements. 
   rapol      I   Right Ascension of the pole of the reference plane. 
   decpol     I   Declination of the pole of the reference plane. 
 
-Detailed_Input
 
   handle         is the file handle of an SPK file that has been 
                  opened for writing. 
 
   body           is the NAIF ID for the body whose states are 
                  to be recorded in an SPK file. 
 
   center         is the NAIF ID for the center of motion associated 
                  with body. 
 
   frame          is the reference frame that states are referenced to, 
                  for example "J2000". 
 
   first          are the bounds on the ephemeris times, expressed as 
   last           seconds past J2000. 
 
   segid          is the segment identifier. An SPK segment identifier 
                  may contain up to 40 characters. 
 
   epoch          is the epoch of equinoctial elements in seconds 
                  past the J2000 epoch. 
 
   eqel           is an array of 9 double precision numbers that 
                  are the equinoctial elements for some orbit relative 
                  to the equatorial frame of a central body. 
 
                  The z-axis of the equatorial frame is the direction 
                  of the pole of the central body relative to frame. 
                  The x-axis is given by the cross product of the 
                  Z-axis of frame with the direction of the pole of 
                  the central body.  The Y-axis completes a right 
                  handed frame. 
 
                  The specific arrangement of the elements is spelled 
                  out below.  The following terms are used in the 
                  discussion of elements of eqel:
     
                      inc  --- inclination of the orbit 
                      argp --- argument of periapse 
                      node --- longitude of the ascending node 
                      e    --- eccentricity of the orbit 
 
                  eqel[0] is the semi-major axis (A) of the orbit in km. 
     
                  eqel[1] is the value of H at the specified epoch. 
                          ( e*sin(argp+node) ). 
     
                  eqel[2] is the value of K at the specified epoch 
                          ( e*cos(argp+node) ). 
     
                  eqel[3] is the mean longitude (mean0+argp+node) at 
                          the epoch of the elements measured in radians. 
     
                  eqel[4] is the value of p (tan(inc/2)*sin(node)) at 
                          the specified epoch. 
     
                  eqel[5] is the value of q (tan(inc/2)*cos(node)) at 
                          the specified epoch. 
     
                  eqel[6] is the rate of the longitude of periapse 
                          (dargp/dt + dnode/dt ) at the epoch of 
                          the elements.  This rate is assumed to hold 
                          for all time. The rate is measured in 
                          radians per second. 
     
                  eqel[7] is the derivative of the mean longitude 
                          ( dm/dt + dargp/dt + dnode/dt ).  This 
                          rate is assumed to be constant and is 
                          measured in radians/second. 
     
                  eqel[8] is the rate of the longitude of the ascending 
                          node ( dnode/dt).  This rate is measured 
                          in radians per second. 
 
   rapol          Right Ascension of the pole of the reference plane 
                  relative to frame measured in radians. 
 
   DECPOL         Declination of the pole of the reference plane 
                  relative to frame measured in radians. 
 
-Detailed_Output
 
   None.  A type 17 segment is written to the file attached 
   to handle. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the semi-major axis is less than or equal to zero, the error 
      SPICE(BADSEMIAXIS) is signaled. 
 
   2) If the eccentricity of the orbit corresponding to the values 
      of H and K ( eqel[1] and eqel[2] ) is greater than 0.9 the 
      error SPICE(ECCOUTOFRANGE) is signaled. 
 
   3) If the segment identifier has more than 40 non-blank characters 
      the error SPICE(SEGIDTOOLONG) is signaled. 
 
   4) If the segment identifier contains non-printing characters 
      the error SPICE(NONPRINTABLECHARS) is signaled. 
 
   5) If there are inconsistencies in the body, center, frame or 
      first and last times, the problem will be diagnosed by 
      a routine in the call tree of this routine. 
 
   6) If either the input frame or segment ID string pointer is null,
      the error SPICE(NULLPOINTER) is signaled.
   
   7) If either the input frame or segment ID string is empty,
      the error SPICE(EMPTYSTRING) is signaled.
      
      
-Files
 
   A new type 17 SPK segment is written to the SPK file attached 
   to HANDLE. 
 
-Particulars
 
   This routine writes an SPK type 17 data segment to the open SPK 
   file according to the format described in the type 17 section of 
   the SPK Required Reading. The SPK file must have been opened with 
   write access. 
 
-Examples
 
   Suppose that at time epoch you have the classical elements 
   of some body relative to the equatorial frame of some central 
   body CENTER. These can be converted to equinoctial elements 
   and stored in an SPK file as a type 17 segment so that this 
   body can be used within the SPK subsystem of the SPICE system. 
 
   Below is a list of the variables used to represent the 
   classical elements 
 
         Variable     Meaning 
         --------     ---------------------------------- 
         a            Semi-major axis in km 
         ecc          Eccentricity of orbit 
         inc          Inclination of orbit 
         node         Longitude of the ascending node at epoch 
         omega        Argument of periapse at epoch 
         m            Mean anomaly at epoch 
         dmdt         Mean anomaly rate in radians/second 
         dnode        Rate of change of longitude of ascending node 
                      in radians/second 
         domega       Rate of change of argument of periapse in 
                      radians/second 
         epoch        is the epoch of the elements in seconds past 
                      the J2000 epoch. 
 
 
      These elements are converted to equinoctial elements (in 
      the order compatible with type 17) as shown below. 
 
         #include "SpiceUsr.h"
               .
               .
               .
               
         eqel[0] = a; 
         eqel[1] = ecc * sin ( omega + node ); 
         eqel[2] = ecc * cos ( omega + node ); 
 
         eqel[3] = m + omega + node; 
 
         eqel[4] = tan(inc/2.0) * sin(node); 
         eqel[5] = tan(inc/2.0) * cos(node); 
 
         eqel[6] = domega; 
         eqel[7] = domega + dmdt + dnode;
         eqel[8] = dnode;
 
 
         /.
         Now add the segment. 
         ./
 
         spkw17_c ( handle, body,  center, frame,  first, last, 
                    segid,  epoch, eqel,   rapol,  decpol      ); 
 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 21-JUN-1999 (NJB) (WLT)

-Index_Entries
 
   Write a type 17 spk segment 
 
-&
*/

{ /* Begin spkw17_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "spkw17_c" );


   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkw17_c", frame );
   CHKFSTR ( CHK_STANDARD, "spkw17_c", segid );


   /*
   Write the segment. 
   */
   spkw17_ ( ( integer    * ) &handle,
             ( integer    * ) &body,
             ( integer    * ) &center,
             ( char       * ) frame,
             ( doublereal * ) &first,
             ( doublereal * ) &last,
             ( char       * ) segid,
             ( doublereal * ) &epoch,
             ( doublereal * ) eqel,
             ( doublereal * ) &rapol,
             ( doublereal * ) &decpol,
             ( ftnlen       ) strlen(frame),
             ( ftnlen       ) strlen(segid)  );


   chkout_c ( "spkw17_c" );

} /* End spkw17_c */
