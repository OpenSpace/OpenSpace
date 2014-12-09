/*

-Procedure ilumin_c ( Illumination angles )

-Abstract
 
   Find the illumination angles (phase, solar incidence, and
   emission) at a specified surface point of a target body.
 
   This routine supersedes illum_c.
 
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
 
   FRAMES
   NAIF_IDS 
   PCK
   SPK 
   TIME 
 
-Keywords
 
   GEOMETRY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    ilumin_c

   void ilumin_c ( ConstSpiceChar          * method,
                   ConstSpiceChar          * target,
                   SpiceDouble               et,
                   ConstSpiceChar          * fixref, 
                   ConstSpiceChar          * abcorr, 
                   ConstSpiceChar          * obsrvr, 
                   ConstSpiceDouble          spoint [3],
                   SpiceDouble             * trgepc,
                   SpiceDouble               srfvec [3],
                   SpiceDouble             * phase,
                   SpiceDouble             * solar,
                   SpiceDouble             * emissn     )
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   method     I   Computation method. 
   target     I   Name of target body. 
   et         I   Epoch in ephemeris seconds past J2000 TDB.
   fixref     I   Body-fixed, body-centered target body frame. 
   abcorr     I   Desired aberration correction. 
   obsrvr     I   Name of observing body. 
   spoint     I   Body-fixed coordinates of a target surface point. 
   trgepc     O   Target surface point epoch. 
   srfvec     O   Vector from observer to target surface point. 
   phase      O   Phase angle at the surface point. 
   solar      O   Solar incidence angle at the surface point. 
   emissn     O   Emission angle at the surface point. 
 
-Detailed_Input
 

   method      is a short string providing parameters defining
               the computation method to be used. Parameters
               include, but are not limited to, the shape model
               used to represent the surface of the target body.

               The only choice currently supported is

                  "Ellipsoid"        The illumination angle computation
                                     uses a triaxial ellipsoid to model
                                     the surface of the target body.
                                     The ellipsoid's radii must be
                                     available in the kernel pool.

               Neither case nor white space are significant in
               `method'. For example, the string ' eLLipsoid ' is
               valid.
   

   target      is the name of the target body. `target' is
               case-insensitive, and leading and trailing blanks in
               `target' are not significant. Optionally, you may supply
               a string containing the integer ID code for the object.
               For example both "MOON" and "301" are legitimate strings
               that indicate the Moon is the target body.

 
   et          is the epoch, specified as ephemeris seconds past J2000
               TDB, at which the apparent illumination angles at the
               specified surface point on the target body, as seen from
               the observing body, are to be computed.

 
   fixref      is the name of the body-fixed, body-centered reference
               frame associated with the target body. The input surface
               point `spoint' and the output vector `srfvec' are
               expressed relative to this reference frame. The string
               `fixref' is case-insensitive, and leading and trailing
               blanks in `fixref' are not significant.
 

   abcorr      is the aberration correction to be used in computing the
               position and orientation of the target body and the
               location of the Sun.

               For remote sensing applications, where the apparent
               illumination angles seen by the observer are desired,
               normally either of the corrections

                  "LT+S"
                  "CN+S"

               should be used. These and the other supported options
               are described below. `abcorr' may be any of the
               following:

                  "NONE"     No aberration correction.

 
               Let `lt' represent the one-way light time between the
               observer and `spoint' (note: NOT between the observer
               and the target body's center). The following values of
               `abcorr' apply to the "reception" case in which photons
               depart from `spoint' at the light-time corrected epoch
               et-lt and *arrive* at the observer's location at `et':
 
                  "LT"       Correct both the position of `spoint' as
                             seen by the observer, and the position of
                             the Sun as seen by the target, for light
                             time.

                  "LT+S"     Correct both the position of `spoint' as
                             seen by the observer, and the position of
                             the Sun as seen by the target, for light
                             time and stellar aberration.

                  "CN"       Converged Newtonian light time correction.
                             In solving the light time equations for
                             target and the Sun, the "CN" correction
                             iterates until the solution converges.

                  "CN+S"     Converged Newtonian light time and
                             stellar aberration corrections. This
                             option produces a solution that is at
                             least as accurate at that obtainable
                             with the "LT+S" option. Whether the
                             "CN+S" solution is substantially more
                             accurate depends on the geometry of the
                             participating objects and on the
                             accuracy of the input data. In all
                             cases this routine will execute more
                             slowly when a converged solution is
                             computed.

               Neither case nor white space are significant in
               `abcorr'. For example, the string

                 "Lt + s"

               is valid.


   obsrvr      is the name of the observing body.  This is typically a
               spacecraft, the earth, or a surface point on the earth.
               `obsrvr' is case-insensitive, and leading and trailing
               blanks in `obsrvr' are not significant. Optionally, you
               may supply a string containing the integer ID code for
               the object. For example both "MOON" and "301" are
               legitimate strings that indicate the Moon is the
               observer.
 
               `obsrvr' may be not be identical to `target'.


   spoint      is a surface point on the target body, expressed in
               Cartesian coordinates, relative to the body-fixed
               target frame designated by `fixref'.

               `spoint' need not be visible from the observer's
               location at the epoch `et'.

               The components of `spoint' have units of km.


-Detailed_Output
 
   trgepc      is the "surface point point epoch." `trgepc' is defined
               as follows: letting `lt' be the one-way light time
               between the observer and the input surface point
               `spoint', `trgepc' is either the epoch et-lt or `et'
               depending on whether the requested aberration correction
               is, respectively, for received radiation or omitted.
               `lt' is computed using the method indicated by `abcorr'.
 
               `trgepc' is expressed as seconds past J2000 TDB.
 
 
   srfvec      is the vector from the observer's position at `et' to
               the aberration-corrected (or optionally, geometric)
               position of `spoint', where the aberration corrections
               are specified by `abcorr'. `srfvec' is expressed in the
               target body-fixed reference frame designated by
               `fixref', evaluated at `trgepc'.
 
               The components of `srfvec' are given in units of km. 
 
               One can use the CSPICE function vnorm_c to obtain the 
               distance between the observer and `spoint': 
 
                  dist = vnorm_c ( srfvec );
 
               The observer's position `obspos', relative to the 
               target body's center, where the center's position is 
               corrected for aberration effects as indicated by 
               `abcorr', can be computed via the call: 
 
                  vsub_c ( spoint, srfvec, obspos );

               To transform the vector `srfvec' from a reference frame
               `fixref' at time `trgepc' to a time-dependant reference
               frame `ref' at time `et', the routine pxfrm2_c should be
               called. Let `xform' be the 3x3 matrix representing the
               rotation from the reference frame `fixref' at time
               `trgepc' to the reference frame `ref' at time `et'. Then
               `srfvec' can be transformed to the result `refvec' as
               follows:
 
                  pxfrm2_c ( fixref, ref,    trgepc, et, xform );
                  mxv_c    ( xform,  srfvec, refvec            );

 
   phase       is the phase angle at `spoint', as seen from `obsrvr' at
               time `et'. This is the angle between the spoint-obsrvr
               vector and the spoint-sun vector. Units are radians. The
               range of `phase' is [0, pi]. See Particulars below for a
               detailed discussion of the definition.
 

   solar       is the solar incidence angle at `spoint', as seen from
               `obsrvr' at time `et'. This is the angle between the
               surface normal vector at `spoint' and the spoint-sun
               vector. Units are radians. The range of `solar' is [0,
               pi]. See Particulars below for a detailed discussion of
               the definition.
 
 
   emissn      is the emission angle at `spoint', as seen from `obsrvr'
               at time `et'. This is the angle between the surface
               normal vector at `spoint' and the spoint-observer
               vector. Units are radians. The range of `emissn' is [0,
               pi]. See Particulars below for a detailed discussion of
               the definition.

-Parameters
 
   None. 
 
-Exceptions

 
   1)  If the specified aberration correction is relativistic or
       calls for stellar aberration but not light time correction,
       the error SPICE(NOTSUPPORTED) is signaled. If the specified 
       aberration correction is any other unrecognized value, the 
       error will be diagnosed and signaled by a routine in the call 
       tree of this routine.  

   2)  If either the target or observer input strings cannot be
       converted to an integer ID code, the error SPICE(IDCODENOTFOUND)
       is signaled.
 
   3)  If `obsrvr' and `target' map to the same NAIF integer ID code, 
       the error SPICE(BODIESNOTDISTINCT) is signaled. 
 
   4)  If the input target body-fixed frame `fixref' is not recognized,
       the error SPICE(NOFRAME) is signaled. A frame name may fail
       to be recognized because a required frame specification kernel
       has not been loaded; another cause is a misspelling of the 
       frame name.
 
   5)  If the input frame `fixref' is not centered at the target body,
       the error SPICE(INVALIDFRAME) is signaled.

   6)  If the input argument `method' is not recognized, the error 
       SPICE(INVALIDMETHOD) is signaled. 

   7)  If the target and observer have distinct identities but are
       at the same location (for example, the target is Mars and
       the observer is the Mars barycenter), the error 
       SPICE(NOSEPARATION) is signaled.

   8)  If insufficient ephemeris data have been loaded prior to 
       calling ilumin_c, the error will be diagnosed and signaled by a 
       routine in the call tree of this routine. Note that when 
       light time correction is used, sufficient ephemeris data 
       must be available to propagate the states of observer,
       target, and the Sun to the solar system barycenter. 
 
   9)  If the computation method specifies an ellipsoidal target shape
       and triaxial radii of the target body have not been loaded 
       into the kernel pool prior to calling ilumin_c, the error will 
       be diagnosed and signaled by a routine in the call tree of 
       this routine. 
 
   10) The target must be an extended body: if any of the radii of  
       the target body are non-positive, the error will be diagnosed 
       and signaled by routines in the call tree of this routine. 
  
   11) If PCK data specifying the target body-fixed frame orientation
       have not been loaded prior to calling ilumin_c, the error will 
       be diagnosed and signaled by a routine in the call tree of 
       this routine. 

   12) The error SPICE(EMPTYSTRING) is signaled if any input string
       argument does not contain at least one character, since the
       input string cannot be converted to a Fortran-style string in
       this case.
      
   13) The error SPICE(NULLPOINTER) is signaled if any input
       string argument pointer is null.


-Files
 
 
   Appropriate kernels must be loaded by the calling program before
   this routine is called.

   The following data are required:

      - SPK data: ephemeris data for target, observer, and the
        Sun must be loaded. If aberration corrections are used, the
        states of target, observer, and the Sun relative to the
        solar system barycenter must be calculable from the
        available ephemeris data. Typically ephemeris data are made
        available by loading one or more SPK files via furnsh_c.

      - PCK data: if the target body shape is modeled as an
        ellipsoid, triaxial radii for the target body must be loaded
        into the kernel pool. Typically this is done by loading a
        text PCK file via furnsh_c.

      - Further PCK data: rotation data for the target body must be
        loaded. These may be provided in a text or binary PCK file.

      - Frame data: if a frame definition is required to convert the
        observer and target states to the body-fixed frame of the
        target, that definition must be available in the kernel
        pool. Typically the definition is supplied by loading a
        frame kernel via furnsh_c.

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.


-Particulars
 

   The term "illumination angles" refers to following set of
   angles:


      phase angle              Angle between the vectors from the
                               surface point to the observer and from
                               the surface point to the Sun.

      solar incidence angle    Angle between the surface normal at
                               the specified surface point and the
                               vector from the surface point to the
                               Sun.

      emission angle           Angle between the surface normal at
                               the specified surface point and the
                               vector from the surface point to the
                               observer.


   The diagram below illustrates the geometric relationships
   defining these angles. The labels for the solar incidence,
   emission, and phase angles are "s.i.", "e.", and "phase".


                                                    *
                                                   Sun

                  surface normal vector
                            ._                 _.
                            |\                 /|  Sun vector
                              \    phase      /
                               \   .    .    /
                               .            .
                                 \   ___   /
                            .     \/     \/
                                  _\ s.i./
                           .    /   \   /
                           .   |  e. \ /
       *             <--------------- *  surface point on
    viewing            vector            target body
    location           to viewing
    (observer)         location


   Note that if the target-observer vector, the target normal vector
   at the surface point, and the target-sun vector are coplanar,
   then phase is the sum of incidence and emission. This is rarely
   true; usually

      phase angle  <  solar incidence angle + emission angle

   All of the above angles can be computed using light time
   corrections, light time and stellar aberration corrections, or
   no aberration corrections. In order to describe apparent
   geometry as observed by a remote sensing instrument, both
   light time and stellar aberration corrections should be used.

   The way aberration corrections are applied by this routine
   is described below.

      Light time corrections
      ======================

         Observer-target surface point body vector
         -----------------------------------------

         Let `et' be the epoch at which an observation or remote
         sensing measurement is made, and let et - lt ("lt" stands
         for "light time") be the epoch at which the photons
         received at `et' were emitted from the surface point `spoint'.
         Note that the light time between the surface point and
         observer will generally differ from the light time between
         the target body's center and the observer.


         Target body's orientation
         -------------------------

         Using the definitions of `et' and `lt' above, the target body's
         orientation at et-lt is used. The surface normal is
         dependent on the target body's orientation, so the body's
         orientation model must be evaluated for the correct epoch.


         Target body -- Sun vector
         -------------------------

         The surface features on the target body near `spoint' will
         appear in a measurement made at `et' as they were at et-lt. In
         particular, lighting on the target body is dependent on the
         apparent location of the Sun as seen from the target body at
         et-lt. So, a second light time correction is used to compute
         the position of the Sun relative to the surface point.


      Stellar aberration corrections
      ==============================

      Stellar aberration corrections are applied only if
      light time corrections are applied as well.

         Observer-target surface point vector
         ------------------------------------

         When stellar aberration correction is performed, the direction
         vector `srfvec' is adjusted so as to point to the apparent
         position of `spoint': considering `spoint' to be an ephemeris
         object, `srfvec' points from the observer's position at `et'
         to the light time and stellar aberration corrected position of
         `spoint'.

         Target body-Sun vector
         ----------------------

         The target body-Sun vector is the apparent position of the
         Sun, corrected for light time and stellar aberration, as
         seen from the target body at time et-lt. 


-Examples
 
   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   
   1) Find the phase, solar incidence, and emission angles at the
      sub-solar and sub-spacecraft points on Mars as seen from the Mars
      Global Surveyor spacecraft at a user-specified UTC time. Use
      light time and stellar aberration corrections. 

      Use the meta-kernel shown below to load the required SPICE
      kernels.

         KPL/MK

         File: mgs_example.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                     Contents
            ---------                     --------
            de418.bsp                     Planetary ephemeris
            pck00008.tpc                  Planet orientation and
                                          radii
            naif0008.tls                  Leapseconds
            mgs_ext13_ipng_mgs95j.bsp     MGS ephemeris

         \begindata

            KERNELS_TO_LOAD = ( 'de418.bsp',
                                'pck00008.tpc',
                                'naif0008.tls',
                                'mgs_ext13_ipng_mgs95j.bsp'  )
         \begintext


      Example code begins here.


      #include <string.h>
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      { 
         /.
         Local constants 
         ./
         #define  META           "mgs_example.tm" 

         /.
         Local variables 
         ./
         SpiceChar             * abcorr;
         SpiceChar             * obsrvr;
         SpiceChar             * target;
         SpiceChar             * utc;

         SpiceDouble             et;
         SpiceDouble             srfvec  [3];   
         SpiceDouble             sscemi;
         SpiceDouble             sscphs;
         SpiceDouble             sscpt   [3];   
         SpiceDouble             sscsol;
         SpiceDouble             sslemi;
         SpiceDouble             sslphs;
         SpiceDouble             sslsol;
         SpiceDouble             ssolpt  [3];
         SpiceDouble             trgepc;

         /.
         Load kernel files.
         ./
         furnsh_c ( META );
 
         /.
         Convert the UTC request time string to seconds past J2000 TDB.
         ./
         utc = "2004 JAN 1 12:00:00";

         str2et_c ( utc, &et );
                
         /.
         Assign observer and target names. The acronym MGS
         indicates Mars Global Surveyor. See NAIF_IDS for a 
         list of names recognized by SPICE. Also set the
         aberration correction flag.
         ./
         target = "Mars";
         obsrvr = "MGS";
         abcorr = "CN+S";
 
         /.
         Find the sub-solar point on the Earth as seen from 
         the MGS spacecraft at et. Use the "near point: ellipsoid" 
         style of sub-point definition. 
         ./
         subslr_c ( "near point: ellipsoid",  
                    target, et,     "iau_mars", abcorr, 
                    obsrvr, ssolpt, &trgepc,    srfvec  );
 
         /.
         Now find the sub-spacecraft point. 
         ./                
         subpnt_c ( "near point: ellipsoid",  
                    target, et,    "iau_mars", abcorr, 
                    obsrvr, sscpt, &trgepc,    srfvec  );

         /.
         Find the phase, solar incidence, and emission 
         angles at the sub-solar point on the Earth as seen 
         from MGS at time et. 
         ./
         ilumin_c ( "Ellipsoid",
                    target,  et,      "IAU_MARS", abcorr,  
                    obsrvr,  ssolpt,  &trgepc,    srfvec,
                    &sslphs, &sslsol, &sslemi            ); 

         /.
         Do the same for the sub-spacecraft point. 
         ./
         ilumin_c ( "Ellipsoid",
                    target,  et,      "IAU_MARS", abcorr,
                    obsrvr,  sscpt,   &trgepc,    srfvec,
                    &sscphs, &sscsol, &sscemi            ); 

         /.
         Convert the angles to degrees and write them out. 
         ./
         sslphs *= dpr_c(); 
         sslsol *= dpr_c(); 
         sslemi *= dpr_c(); 

         sscphs *= dpr_c(); 
         sscsol *= dpr_c(); 
         sscemi *= dpr_c(); 

         printf ( "\n"
                  "UTC epoch is %s\n"
                  "\n"
                  "Illumination angles at the sub-solar point:\n"
                  "\n"
                  "Phase angle             (deg):  %f\n"
                  "Solar incidence angle   (deg):  %f\n"
                  "Emission angle          (deg):  %f\n"
                  "\n"
                  "The solar incidence angle should be 0.\n"
                  "The emission and phase angles should be "
                  "equal.\n"
                  "\n"
                  "\n"
                  "Illumination angles at the sub-s/c point:\n"
                  "\n"
                  "Phase angle             (deg):  %f\n"
                  "Solar incidence angle   (deg):  %f\n"
                  "Emission angle          (deg):  %f\n"
                  "\n"
                  "The emission angle should be 0.\n"
                  "The solar incidence and phase angles "
                  "should be equal.\n"
                  "\n",
                  utc,
                  sslphs,
                  sslsol,
                  sslemi,                 
                  sscphs,
                  sscsol,
                  sscemi                                    );

         return ( 0 );
      }
          
          
   When this program was executed on a PC/Linux/gcc platform,
   the output was:   

      UTC epoch is 2004 JAN 1 12:00:00

      Illumination angles at the sub-solar point:

      Phase angle             (deg):  115.542001
      Solar incidence angle   (deg):  0.000000
      Emission angle          (deg):  115.542001

      The solar incidence angle should be 0.
      The emission and phase angles should be equal.


      Illumination angles at the sub-s/c point:

      Phase angle             (deg):  62.084003
      Solar incidence angle   (deg):  62.084003
      Emission angle          (deg):  0.000000

      The emission angle should be 0.
      The solar incidence and phase angles should be equal.

          
-Restrictions

   None.
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version

   -CSPICE Version 1.0.2, 17-OCT-20011 (SCK)
 
       References to the new pxfrm2_c routine were added
       to the Detailed Output section.

   -CSPICE Version 1.0.1, 06-FEB-2009 (NJB)
 
       Incorrect frame name fixfrm was changed to fixref in
       documentation.
 
       In the header examples, meta-kernel names were updated to use
       the suffix

          ".tm"

   -CSPICE Version 1.0.0, 02-MAR-2008 (NJB)

-Index_Entries
 
   illumination angles 
   lighting angles
   phase angle
   emission angle
   solar incidence angle
 
-&
*/

{ /* Begin ilumin_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "ilumin_c" );

   /*
   Check the input strings: target, fixref, abcorr, and obsrvr. Make
   sure none of the pointers are null and that each string contains at
   least one non-null character.
   */
   CHKFSTR ( CHK_STANDARD, "ilumin_c", method );
   CHKFSTR ( CHK_STANDARD, "ilumin_c", target );
   CHKFSTR ( CHK_STANDARD, "ilumin_c", fixref );
   CHKFSTR ( CHK_STANDARD, "ilumin_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "ilumin_c", obsrvr );

   /*
   Call the f2c'd routine.
   */
   ilumin_ ( ( char         * ) method,
             ( char         * ) target,
             ( doublereal   * ) &et, 
             ( char         * ) fixref, 
             ( char         * ) abcorr, 
             ( char         * ) obsrvr, 
             ( doublereal   * ) spoint, 
             ( doublereal   * ) trgepc, 
             ( doublereal   * ) srfvec, 
             ( doublereal   * ) phase, 
             ( doublereal   * ) solar, 
             ( doublereal   * ) emissn, 
             ( ftnlen         ) strlen(method), 
             ( ftnlen         ) strlen(target), 
             ( ftnlen         ) strlen(fixref), 
             ( ftnlen         ) strlen(abcorr), 
             ( ftnlen         ) strlen(obsrvr)  );
                         
   chkout_c ( "ilumin_c" );

} /* End ilumin_c */

