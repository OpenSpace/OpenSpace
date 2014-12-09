/*

-Procedure tipbod_c ( Transformation, inertial position to bodyfixed )

-Abstract
 
   Return a 3x3 matrix that transforms positions in inertial 
   coordinates to positions in body-equator-and-prime-meridian 
   coordinates. 
 
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
 
   PCK 
   NAIF_IDS 
   ROTATION
   TIME 
 
-Keywords
 
   TRANSFORMATION 
   ROTATION 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void tipbod_c ( ConstSpiceChar  * ref,
                   SpiceInt          body,
                   SpiceDouble       et,
                   SpiceDouble       tipm[3][3] ) 

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   ref        I   ID of inertial reference frame to transform from. 
   body       I   ID code of body. 
   et         I   Epoch of transformation. 
   tipm       O   Transformation (position), inertial to prime 
                  meridian. 
 
-Detailed_Input
 
   ref         is the NAIF name for an inertial reference frame. 
               Acceptable names include: 

                 Name       Description 
                 --------   -------------------------------- 
                 "J2000"    Earth mean equator, dynamical 
                            equinox of J2000 

                 "B1950"    Earth mean equator, dynamical 
                            equinox of B1950 

                 "FK4"      Fundamental Catalog (4) 

                 "DE-118"   JPL Developmental Ephemeris (118) 

                 "DE-96"    JPL Developmental Ephemeris ( 96) 

                 "DE-102"   JPL Developmental Ephemeris (102) 

                 "DE-108"   JPL Developmental Ephemeris (108) 

                 "DE-111"   JPL Developmental Ephemeris (111) 

                 "DE-114"   JPL Developmental Ephemeris (114) 

                 "DE-122"   JPL Developmental Ephemeris (122) 

                 "DE-125"   JPL Developmental Ephemeris (125) 

                 "DE-130"   JPL Developmental Ephemeris (130) 

                 "GALACTIC" Galactic System II 

                 "DE-200"   JPL Developmental Ephemeris (200) 

                 "DE-202"   JPL Developmental Ephemeris (202) 

               (See the routine CHGIRF for a full list of names.) 

               The output tipm will give the transformation 
               from this frame to the bodyfixed frame specified by 
               body at the epoch specified by et. 


   body        is the integer ID code of the body for which the 
               position transformation matrix is requested. Bodies 
               are numbered according to the standard NAIF 
               numbering scheme.  The numbering scheme is 
               explained in the NAIF_IDS required reading file. 

   et          is the epoch at which the position transformation 
               matrix is requested. (This is typically the 
               epoch of observation minus the one-way light time 
               from the observer to the body at the epoch of 
               observation.) 
 
-Detailed_Output
 
   tipm        is a 3x3 coordinate transformation matrix.  It is 
               used to transform positions from inertial coordinates to 
               body fixed (also called equator and prime meridian)
               coordinates. 

               Given a position P in the inertial reference frame 
               specified by ref, the corresponding bodyfixed 
               position is given by the matrix vector product

                  tipm * s 

               The X axis of the PM system is directed to the 
               intersection of the equator and prime meridian. 
               The Z axis points along  the spin axis and points 
               towards the same side of the invariable plane of 
               the solar system as does earth's north pole. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the kernel pool does not contain all of the data required 
      for computing the transformation matrix, tipm, the error 
      SPICE(INSUFFICIENTANGLES) is signalled. 

   2) If the reference frame ref is not recognized, a routine 
      called by tipbod_c will diagnose the condition and signal an
      error.
       
   3) If the code body is not recognized, the error is diagnosed by a 
      routine called by tipbod_c. 
 
   4) If the input string pointer is null, the error 
      SPICE(NULLPOINTER) will be signaled.
      
   5) If the input string has length zero, the error 
      SPICE(EMPTYSTRING) will be signaled.
      
-Files
 
    None. 
 
-Particulars
 
   tipbod_c takes PCK information as input, either in the form of a 
   binary or text PCK file.  If the data required to compute tipm are
   available in a binary PCK, these data will take precedence over data
   from a text PCK.  If there are multiple binary PCKs containing data
   from which tipm can be computed, the last loaded PCK takes 
   precedence.  If binary PCK data are available for the requested body 
   and time, the Euler angles giving the body's orientation are 
   evaluated, and the transformation matrix tipm is calculated from
   them.  Using the Euler angles PHI, DELTA and W we compute 
 
      TIPM = [W] [DELTA] [PHI] 
                3       1     3 
 
   If no appropriate binary PCK files have been loaded, text PCK data
   are used.  Here information is found as RA, DEC and W (with the 
   possible addition of nutation and libration terms for satellites).  
   Again, the Euler angles are found, and the transformation matrix is 
   calculated from them.  The transformation from inertial to 
   bodyfixed coordinates is represented as: 
 
      TIPM = [W] [HALFPI-DEC] [RA+HALFPI] 
                3            1           3 
 
   These Euler angles RA, DEC and W are related to PHI, DELTA and W
   by the equations
 
      RA  = PHI  - pi/2 
      DEC = pi/2 - DELTA 
      W   = W 
 
   In the text file, RA, DEC, and W are defined as follows: 
 
                                   2      ____ 
                              RA2*t       \ 
      RA  = RA0  + RA1*t/T  + ------   +  /     a  sin theta 
                                 2        ----   i          i 
                                T           i 

                                    2     ____ 
                              DEC2*t      \ 
      DEC = DEC0 + DEC1*t/T + -------  +  /    d  cos theta 
                                  2       ----  i          i 
                                 T          i 


                                  2      ____ 
                              W2*t       \ 
      W   = W0   + W1*t/d   + -----   +  /     w  sin theta 
                                 2       ----   i          i 
                                d          i 
 
   where: 

      d = seconds/day 

      T = seconds/Julian century 

      a , d , and w  arrays apply to satellites only. 
       i   i       i 

      theta  = THETA0(i) + THETA1(i)*t/T are specific to each 
           i 

      planet. 


   These angles---typically nodal rates---vary in number and 
   definition from one planetary system to the next. 
 
-Examples
 
   Note that the items necessary to compute the Euler angles 
   must have been loaded into the kernel pool (by one or more 
   previous calls to furnsh_c).  The Euler angles are typically 
   stored in the P_constants kernel file that comes with 
   CSPICE. 

   1)  In the following code fragment, tipbod_c is used to transform 
       a position in J2000 inertial coordinates to a position in 
       bodyfixed coordinates. 

       The 3-vector postn represents the inertial position 
       of an object with respect to the center of the 
       body at time et. 

          #include "SpiceUsr.h"
                .
                .
                .
          /. 
          First load the kernel pool. 
          ./ 
          furnsh_c ( "PLANETARY_CONSTANTS.KER" ); 

          /. 
          Next get the transformation. 
          ./ 
          tipbod_c ( "J2000", body, et, tipm ); 

          /. 
          Convert position to bodyfixed coordinates. 
          ./ 
          mxv_c ( tipm, postn, bfxpos ); 
 
-Restrictions
 
   The kernel pool must be loaded with the appropriate 
   coefficients (from the P_constants kernel or binary PCK file) 
   prior to calling this routine. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
   W.L. Taber     (JPL) 
   K.S. Zukor     (JPL) 
 
-Version

   -CSPICE Version 1.0.2, 14-AUG-2006   (EDW)

      Replace mention of ldpool_c with furnsh_c.

   -CSPICE Version 1.0.1, 13-APR-2000 (NJB) 
   
      Made some minor updates and corrections in the code example.
      
   -CSPICE Version 1.0.0, 08-FEB-1998 (NJB)
   
      Based on SPICELIB Version 1.0.3, 10-MAR-1994 (KSZ).

-Index_Entries
 
   transformation from inertial position to bodyfixed 
 
-&
*/

{ /* Begin tipbod_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "tipbod_c" );

   /*
   Check the input string ref to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "tipbod_c", ref );

   /*
   Call the f2c'd Fortran routine.
   */
   tipbod_ ( ( char        * )  ref, 
             ( integer     * )  &body, 
             ( doublereal  * )  &et, 
             ( doublereal  * )  tipm,
             ( ftnlen        )  strlen(ref) );

   /*
   Transpose the output matrix to put it in row-major order.
   */
   xpose_c  ( tipm, tipm );
   
   chkout_c ( "tipbod_c" );
 
 
} /* End tipbod_c */
