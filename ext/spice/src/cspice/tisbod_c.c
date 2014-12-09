/*

-Procedure  tisbod_c ( Transformation, inertial state to bodyfixed )

-Abstract

   Return a 6x6 matrix that transforms states in inertial coordinates to 
   states in body-equator-and-prime-meridian coordinates.

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

   None.

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"   
   #include "SpiceZmc.h"   


   void tisbod_c ( ConstSpiceChar   * ref,    
                   SpiceInt           body,
                   SpiceDouble        et,     
                   SpiceDouble        tsipm[6][6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   ref        I   ID of inertial reference frame to transform from
   body       I   ID code of body
   et         I   Epoch of transformation
   tsipm      O   Transformation (state), inertial to prime meridian
    
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

               (See the routine chgirf_c for a full list of names.)

               The output tipm will give the transformation
               from this frame to the bodyfixed frame specified by
               body at the epoch specified by et.

   body        is the integer ID code of the body for which the
               state transformation matrix is requested. Bodies
               are numbered according to the standard NAIF
               numbering scheme.  The numbering scheme is
               explained in the NAIF_IDS required reading file.

   et          is the epoch at which the state transformation
               matrix is requested. (This is typically the
               epoch of observation minus the one-way light time
               from the observer to the body at the epoch of
               observation.)
            
-Detailed_Output
 
   tsipm       is a 6x6 transformation matrix.  It is used to
               transform states from inertial coordinates to body
               fixed (also called equator and prime meridian --- PM)
                

               Given a state s in the inertial reference frame
               specified by ref, the corresponding bodyfixed state
               is given by the matrix vector product:

                  tsipm * s

               The X axis of the PM system is directed  to the
               intersection of the equator and prime meridian.
               The Z axis points along  the spin axis and points
               towards the same side of the invariable plane of
               the solar system as does earth's north pole.

               NOTE: The inverse of tsipm is NOT its transpose.
               The matrix tsipm has the structure shown below:
 
                    -            -
                   |       :      |
                   |   r   :  0   |
                   | ......:......|
                   |       :      |
                   | dr_dt :  r   |
                   |       :      |
                    -            -
 
               where r is a time varying rotation matrix and
               dr_dt is its derivative.  The inverse of this
               matrix is:
 
                    -              -
                   |     T  :       |
                   |    r   :  0    |
                   | .......:.......|
                   |        :       |
                   |      T :   T   |
                   | dr_dt  :  r    |
                   |        :       |
                    -              -
 
               The CSPICE routine invstm_c is available for
               producing this inverse.

-Parameters

   None.

-Exceptions

   1) If the kernel pool does not contain all of the data required
      for computing the transformation matrix, tsipm, the error
      SPICE(INSUFFICIENTANGLES) is signalled.

   2) If the reference frame ref is not recognized, a routine
      called by tisbod_c will diagnose the condition and invoke the
      SPICE error handling system.

   3) If the specified ID code body is not recognized, the
      error is diagnosed by a routine called by tisbod_c.

-Files

   None.

-Particulars
 
   The matrix for transforming inertial states to bodyfixed
   states is the 6x6 matrix shown below as a block structured
   matrix.

              -            -
             |       :      |
             | tipm  :  0   |
             | ......:......|
             |       :      |
             | dtipm : tipm |
             |       :      |
              -            -

  This can also be expressed in terms of Euler angles
  phi, delta and w.  The transformation from inertial to
  bodyfixed coordinates is represented in the SPICE kernel
  pool as:

         tipm =   [w] [delta] [phi]
                     3       1     3
   Thus

        dtipm =   D[w] /Dt   [delta]       [phi]
                      3             1           3

                +  [w]      D[delta] /Dt   [phi]
                      3             1           3

                +  [w]       [delta]      D[phi] /Dt
                      3             1           3

   If a binary PCK file record can be used for the time and
   body requested, it will be used.  The most recently loaded
   binary PCK file has first priority, followed by previously
   loaded binary PCK files in backward time order.  If no
   binary PCK file has been loaded, the text P_constants
   kernel file is used.

   If there is only text PCK kernel information, it is
   expressed in terms of ra, dec and w (same w as above), where

     ra    = phi  - pi/2
     dec   = pi/2 - delta

   The angles ra, dec, and w are defined as follows in the
   text PCK file:

                                      2      ____
                                 ra2*t       \
         ra  = ra0  + ra1*t/T  + ------   +  /     a  sin theta
                                    2        ----   i          i
                                   T           i

                                       2     ____
                                 dec2*t      \
         dec = dec0 + dec1*t/T + -------  +  /    d  cos theta
                                     2       ----  i          i
                                    T          i


                                     2      ____
                                 w2*t       \
         w   = w0   + w1*t/d   + -----   +  /     w  sin theta
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


     These angles -- typically nodal rates -- vary in number and
     definition from one planetary system to the next.

     Thus
                                      ____
                          2*ra2*t     \
       dra/dt  = ra1/T  + -------   + /   a THETA1(i)/T cos theta
                              2       ---- i                     i
                             T          i

                                      ____
                          2*dec2*t    \
       ddec/dt = dec1/T + --------  - /    d  THETA1(i)/T sin theta
                              2       ----  i                      i
                             T          i

                                      ____
                          2*w2*t      \
       dw/dt   = w1/d   + ------    + /    w  THETA1(i)/T cos theta
                              2       ----  i                      i
                             d          i

-Examples
 
   Note that the data needed to compute the output state transition
   matrix must have been made available to your program by having
   loaded an appropriate binary or text PCK file via furnsh_c.

   Example 1.

      In the following code fragment, tisbod_c is used to transform
      a state in J2000 inertial coordinates to a state in bodyfixed
      coordinates.
   
      The 6-vector eulang represents the inertial state (position and
      velocity) of an object with respect to the center of the body
      at time et.
         
         #include "SpiceUsr.h"
               .
               .
               .
       
         /.
         First load the kernel pool.
         ./
         furnsh_c ( "planetary_constants.ker" );
   
         /.
         Next get the transformation and its derivative.
         ./
         tisbod_c ( "J2000", body, et, tsipm );
   
         /.
         Convert position to bodyfixed coordinates.
         ./
         mxvg_c   ( tsipm, eulang, 6, 6, bdstat );


   Example 2.

      In the example below, tisbod_c is used to compute the angular 
      velocity vector (with respect to an inertial frame) of the 
      specified body at time et.

         #include "SpiceUsr.h"
               .
               .
               .
         /.
         First get the state transformation matrix.
         ./
         tisbod_c ( body,  et,  tsipm );
 
 
         /.
         This matrix has the form:
    
             -            -
            |       :      |
            | tipm  :  0   |
            | ......:......|
            |       :      |
            | dtipm : tipm |
            |       :      |
             -            -
    
         We extract tipm and dtipm:
         ./
         
         
         for ( i = 0;  i<3;  i++ )
         {
            for ( j = 0;  j<3;  j++ )
            {
               tipm  [i][j] = tsipm[i  ][j];
               dtipm [i][j] = tsipm[i+3][j];
            }
         }
         
         
         /.

         The transposes of tipm and dtipm, (tpmi and dtpmi), give
         the transformation from bodyfixed coordinates to inertial
         coordinates and its time derivative.
   
         Here is a fact about the relationship between angular
         velocity associated with a time varying rotation matrix
         that gives the orientation of a body with respect to
         an inertial frame:
    
         The angular velocity vector can be read from the off
         diagonal components of the matrix product:
 
                                 t
         omega =     dtpmi * tpmi
 
                          t
               =     dtipm * tipm
 
         the components of the angular velocity v will appear
         in this matrix as:
 
              _                   _
             |                     |
             |   0    -v(3)  v(2)  |
             |                     |
             |  v(3)    0   -v(1)  |
             |                     |
             | -v(2)   v(1)   0    |
             |_                   _|
             
 
         Pick off the angular velocity components from omega.
         
        ./
 
         mtxm_c ( dtipm, tipm, omega );

         v[0] = omega [2][1];
         v[1] = omega [0][2];
         v[2] = omega [1][0];


-Restrictions

   The kernel pool must be loaded with the appropriate coefficients
   (from the P_constants kernel or binary PCK file) prior to calling
   this routine.

-Literature_References

   None.

-Author_and_Institution

   N. J. Bachman   (JPL)
   W. L. Taber     (JPL)
   K. S. Zukor     (JPL)

-Version

   -CSPICE Version 1.0.3, 16-JAN-2008   (EDW)

      Corrected typos in header titles:
      
      Detailed Input to Detailed_Input
      Detailed Output to Detailed_Output
      
   -CSPICE Version 1.0.2, 10-NOV-2006   (EDW)

      Replace mention of ldpool_c and pcklof_c with furnsh_c.
      Added Keywords and Parameters section headers. 
      Reordered section headers.

   -CSPICE Version 1.0.1, 02-JUL-2003 (EDW)

       Corrected trivial typo in the Version 1.0.0 line.
       The typo caused an integrity check script to fail.

   -CSPICE Version 1.0.0, 20-JUN-1999 (NJB) (WLT) (KSZ)
   
       Initial release, based on SPICELIB Version 3.3.0, 29-MAR-1995

-Index_Entries

   transformation from inertial state to bodyfixed

-&
*/

{  /* Begin tisbod_c */


   /*
   Participate in tracing.
   */
   chkin_c ( "tisbod_c" );
   
   
   /*
   Check the input string ref to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "tisbod_c", ref );


   /*
   Call the f2c'd Fortran routine.
   */
   tisbod_ (  ( char       * )  ref, 
              ( integer    * )  &body, 
              ( doublereal * )  &et, 
              ( doublereal * )  tsipm,
              ( ftnlen       )  strlen(ref)  );

   /*
   Transpose the output from tisbod_ to put the matrix in row-major
   order, which is what C uses.
   */
   xpose6_c ( tsipm, tsipm );
   
   
   chkout_c ( "tisbod_c" );

} /* End tisbod_c */
