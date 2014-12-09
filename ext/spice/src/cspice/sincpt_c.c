/*

-Procedure sincpt_c ( Surface intercept )

-Abstract
 
   Given an observer and a direction vector defining a ray, compute 
   the surface intercept of the ray on a target body at a specified 
   epoch, optionally corrected for light time and stellar 
   aberration. 
 
   This routine supersedes srfxpt_c. 
 
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
   #undef   sincpt_c

   void sincpt_c ( ConstSpiceChar      * method,
                   ConstSpiceChar      * target,
                   SpiceDouble           et,
                   ConstSpiceChar      * fixref,
                   ConstSpiceChar      * abcorr,
                   ConstSpiceChar      * obsrvr,
                   ConstSpiceChar      * dref,
                   ConstSpiceDouble      dvec   [3],
                   SpiceDouble           spoint [3],
                   SpiceDouble         * trgepc,
                   SpiceDouble           srfvec [3],
                   SpiceBoolean        * found       )
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   method     I   Computation method. 
   target     I   Name of target body. 
   et         I   Epoch in ephemeris seconds past J2000 TDB. 
   fixref     I   Body-fixed, body-centered target body frame. 
   abcorr     I   Aberration correction. 
   obsrvr     I   Name of observing body. 
   dref       I   Reference frame of ray's direction vector. 
   dvec       I   Ray's direction vector. 
   spoint     O   Surface intercept point on the target body. 
   trgepc     O   Intercept epoch. 
   srfvec     O   Vector from observer to intercept point. 
   found      O   Flag indicating whether intercept was found. 
 
-Detailed_Input
 
   method      is a short string providing parameters defining 
               the computation method to be used.
 
               The only choice currently supported is 
 
                  "Ellipsoid"        The intercept computation uses 
                                     a triaxial ellipsoid to model 
                                     the surface of the target body. 
                                     The ellipsoid's radii must be 
                                     available in the kernel pool. 
 
               Neither case nor white space are significant in  
               `method'. For example, the string ' eLLipsoid ' is  
               valid.                  

                
   target      is the name of the target body. `target' is 
               case-insensitive, and leading and trailing blanks in 
               `target' are not significant. Optionally, you may 
               supply a string containing the integer ID code  
               for the object. For example both "MOON" and "301" 
               are legitimate strings that indicate the Moon is the 
               target body. 
 
               When the target body's surface is represented by a 
               tri-axial ellipsoid, this routine assumes that a 
               kernel variable representing the ellipsoid's radii is 
               present in the kernel pool. Normally the kernel 
               variable would be defined by loading a PCK file. 
 
 
   et          is the epoch of participation of the observer, 
               expressed as ephemeris seconds past J2000 TDB: `et' is 
               the epoch at which the observer's state is computed. 
 
               When aberration corrections are not used, `et' is also 
               the epoch at which the position and orientation of the 
               target body are computed. 
 
               When aberration corrections are used, the position and
               orientation of the target body are computed at et-lt or
               et+lt, where `lt' is the one-way light time between the
               intercept point and the observer, and the sign applied
               to `lt' depends on the selected correction. See the
               description of `abcorr' below for details.
 
                                 
   fixref      is the name of the body-fixed, body-centered 
               reference frame associated with the target body. The 
               output intercept point `spoint' and the observer to 
               intercept vector `srfvec' will be expressed relative to 
               this reference frame. 
 
 
   abcorr      indicates the aberration corrections to be applied when
               computing the target's position and orientation.
 
               For remote sensing applications, where the apparent
               target surface intercept point seen by the observer is
               desired, normally the correction
             
                  "CN+S" 
    
               should be used. This and the other supported options 
               are described below. `abcorr' may be any of the  
               following: 
 
                  "NONE"     Apply no correction. Return the  
                             geometric surface intercept point on the 
                             target body. 
 
               Let `lt' represent the one-way light time between the 
               observer and the surface intercept point (note: NOT 
               between the observer and the target body's center). 
               The following values of `abcorr' apply to the 
               "reception" case in which photons depart from the 
               intercept point's location at the light-time 
               corrected epoch et-lt and *arrive* at the observer's 
               location at `et': 
 
 
                  "LT"       Correct for one-way light time (also 
                             called "planetary aberration") using a 
                             Newtonian formulation. This correction 
                             yields the location of the surface 
                             intercept point at the moment it 
                             emitted photons arriving at the 
                             observer at `et'. 
  
                             The light time correction uses an 
                             iterative solution of the light time 
                             equation. The solution invoked by the 
                             "LT" option uses one iteration. 
 
                             Both the target position as seen by the 
                             observer, and rotation of the target 
                             body, are corrected for light time. 
 
                  "LT+S"     Correct for one-way light time and stellar
                             aberration using a Newtonian formulation.
                             This option modifies the surface intercept
                             obtained with the "LT" option to account
                             for the observer's velocity relative to
                             the solar system barycenter. These
                             computations yield the apparent surface
                             intercept point.

                  "CN"       Converged Newtonian light time correction.
                             In solving the light time equation, the
                             "CN" correction iterates until the
                             solution converges. Both the position and
                             rotation of the target body are corrected
                             for light time.
 
                  "CN+S"     Converged Newtonian light time and stellar
                             aberration corrections. This option
                             produces a solution that is at least as
                             accurate at that obtainable with the "LT+S"
                             option. Whether the "CN+S" solution is
                             substantially more accurate depends on the
                             geometry of the participating objects and
                             on the accuracy of the input data. In all
                             cases this routine will execute more
                             slowly when a converged solution is
                             computed.
 
                             For reception-case applications involving
                             intercepts near the target body limb, this
                             option should be used
 
               The following values of `abcorr' apply to the 
               "transmission" case in which photons *depart* from 
               the observer's location at `et' and arrive at the 
               intercept point at the light-time corrected epoch 
               et+lt: 
 
 
                  "XLT"      "Transmission" case: correct for 
                             one-way light time using a Newtonian 
                             formulation. This correction yields the 
                             intercept location at the moment it 
                             receives photons emitted from the 
                             observer's location at `et'.  
 
                             The light time correction uses an 
                             iterative solution of the light time 
                             equation. The solution invoked by the 
                             "XLT" option uses one iteration. 

                             Both the target position as seen by the 
                             observer, and rotation of the target 
                             body, are corrected for light time. 
 
                  "XLT+S"    "Transmission" case: correct for 
                             one-way light time and stellar 
                             aberration using a Newtonian 
                             formulation  This option modifies the 
                             intercept obtained with the "XLT" 
                             option to account for the observer's 
                             velocity relative to the solar system 
                             barycenter. 
 
                  "XCN"      Converged Newtonian light time 
                             correction. This is the same as XLT 
                             correction but with further iterations 
                             to a converged Newtonian light time 
                             solution.  
 
                  "XCN+S"    "Transmission" case: converged Newtonian
                             light time and stellar aberration
                             corrections. This option produces a
                             solution that is at least as accurate at
                             that obtainable with the "XLT+S" option.
                             Whether the "XCN+S" solution is
                             substantially more accurate depends on the
                             geometry of the participating objects and
                             on the accuracy of the input data. In all
                             cases this routine will execute more
                             slowly when a converged solution is
                             computed.
 
                             For transmission-case applications
                             involving intercepts near the target body
                             limb, this option should be used.
 
               Case and embedded blanks are not significant in `abcorr'.
               For example, the string 

                  "Cn + s"

               is valid.

 
   obsrvr      is the name of the observing body. This is typically 
               a spacecraft, the earth, or a surface point on the 
               earth. `obsrvr' is case-insensitive, and leading and 
               trailing blanks in `obsrvr' are not significant. 
               Optionally, you may supply a string containing the 
               integer ID code for the object. For example both 
               "MOON" and "301" are legitimate strings that indicate 
               the Moon is the observer. 
 
 
   dref        is the name of the reference frame relative to which the
               ray's direction vector is expressed. This may be any
               frame supported by the SPICE system, including built-in
               frames (documented in the Frames Required Reading) and
               frames defined by a loaded frame kernel (FK). The string
               `dref' is case-insensitive, and leading and trailing
               blanks in `dref' are not significant.
 
               When `dref' designates a non-inertial frame, the 
               orientation of the frame is evaluated at an epoch 
               dependent on the frame's center and, if the center is 
               not the observer, on the selected aberration 
               correction. See the description of the direction 
               vector `dvec' for details. 
 
 
   dvec        Ray direction vector emanating from the observer. The
               intercept with the target body's surface of the ray
               defined by the observer and `dvec' is sought.
 
               `dvec' is specified relative to the reference frame 
               designated by `dref'. 
 
               Non-inertial reference frames are treated as follows: 
               if the center of the frame is at the observer's 
               location, the frame is evaluated at `et'. If the 
               frame's center is located elsewhere, then letting 
               `ltcent' be the one-way light time between the observer 
               and the central body associated with the frame, the 
               orientation of the frame is evaluated at et-ltcent, 
               et+ltcent, or `et' depending on whether the requested 
               aberration correction is, respectively, for received 
               radiation, transmitted radiation, or is omitted. 
               `ltcent' is computed using the method indicated by 
               `abcorr'. 
                
 
-Detailed_Output
 
 
   spoint      is the surface intercept point on the target body of 
               the ray defined by the observer and the direction 
               vector. If the ray intersects the target body in 
               multiple points, the selected intersection point is 
               the one closest to the observer. The output argument 
               `found' (see below) indicates whether an intercept was 
               found. 
 
               `spoint' is expressed in Cartesian coordinates, 
               relative to the target body-fixed frame designated by 
               `fixref'. The body-fixed target frame is evaluated at 
               the intercept epoch `trgepc' (see description below). 
 
               When light time correction is used, the duration of
               light travel between `spoint' to the observer is
               considered to be the one way light time. When both light
               time and stellar aberration corrections are used,
               `spoint' is selected such that, when `spoint' is
               corrected for light time an stellar aberration, `spoint'
               lies on the ray defined by the observer's location and
               `dvec'.
 
               The components of `spoint' are given in units of km. 
 
 
   trgepc      is the "intercept epoch." `trgepc' is defined as
               follows: letting `lt' be the one-way light time between
               the observer and the intercept point, `trgepc' is the
               epoch et-lt, et+lt, or `et' depending on whether the
               requested aberration correction is, respectively, for
               received radiation, transmitted radiation, or omitted.
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
               `fixref' at time `trgepc' to a time-dependent reference
               frame `ref' at time `et', the routine pxfrm2_c should be
               called. Let `xform' be the 3x3 matrix representing the
               rotation from the reference frame `fixref' at time
               `trgepc' to the reference frame `ref' at time `et'. Then
               `srfvec' can be transformed to the result `refvec' as
               follows:
 
                  pxfrm2_c ( fixref, ref,    trgepc, et, xform );
                  mxv_c    ( xform,  srfvec, refvec            );

               The second example in the Examples header section 
               below presents a complete program that demonstrates 
               this procedure. 
    
 
   found       A logical flag indicating whether or not the ray 
               intersects the target. If an intersection exists 
               `found' will be returned as SPICETRUE If the ray misses 
               the target, `found' will be returned as SPICEFALSE.
 
-Parameters
 
   None. 
 
-Exceptions
 
 
   1)  If the specified aberration correction is unrecognized, the
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
       calling sincpt_c, the error will be diagnosed and signaled by a 
       routine in the call tree of this routine. Note that when 
       light time correction is used, sufficient ephemeris data 
       must be available to propagate the states of both observer 
       and target to the solar system barycenter. 
 
   9)  If the computation method specifies an ellipsoidal target shape
       and triaxial radii of the target body have not been loaded 
       into the kernel pool prior to calling sincpt_c, the error will 
       be diagnosed and signaled by a routine in the call tree of 
       this routine. 
 
   10) The target must be an extended body: if any of the radii of  
       the target body are non-positive, the error will be diagnosed 
       and signaled by routines in the call tree of this routine. 
  
   11) If PCK data specifying the target body-fixed frame orientation
       have not been loaded prior to calling sincpt_c, the error will 
       be diagnosed and signaled by a routine in the call tree of 
       this routine. 

   12) If the reference frame designated by `dref' is not recognized
       by the SPICE frame subsystem, the error SPICE(NOFRAME)
       will be signaled.

   13) If the direction vector `dvec' is the zero vector, the error
       SPICE(ZEROVECTOR) will be signaled.

   14) The error SPICE(EMPTYSTRING) is signaled if any input string
       argument does not contain at least one character, since the
       input string cannot be converted to a Fortran-style string in
       this case.
      
   15) The error SPICE(NULLPOINTER) is signaled if any input
       string argument pointer is null.
 

-Files
 
   Appropriate kernels must be loaded by the calling program before
   this routine is called. 
 
   The following data are required: 
 
      - SPK data: ephemeris data for target and observer must be 
        loaded. If aberration corrections are used, the states of 
        target and observer relative to the solar system barycenter 
        must be calculable from the available ephemeris data. 
        Typically ephemeris data are made available by loading one 
        or more SPK files via furnsh_c. 
 
      - PCK data: if the computation method is specified as 
        "Ellipsoid," triaxial radii for the target body must be  
        loaded into the kernel pool. Typically this is done by 
        loading a text PCK file via furnsh_c. 
 
      - Further PCK data: rotation data for the target body must 
        be loaded. These may be provided in a text or binary PCK 
        file.  
  
   The following data may be required: 
 
      - Frame data: if a frame definition is required to convert 
        the observer and target states to the body-fixed frame of 
        the target, that definition must be available in the kernel 
        pool. Similarly, the frame definition required to map 
        between the frame designated by `dref' and the target 
        body-fixed frame must be available. Typically the 
        definitions of frames not already built-in to SPICE are 
        supplied by loading a frame kernel. 

      - CK data: if the frame to which `dref' refers is fixed to a
        spacecraft instrument or structure, at least one CK file will
        be needed to permit transformation of vectors between that
        frame and both the J2000 and the target body-fixed frames.
 
      - SCLK data: if a CK file is needed, an associated SCLK 
        kernel is required to enable conversion between encoded SCLK 
        (used to time-tag CK data) and barycentric dynamical time 
        (TDB). 
 
   In all cases, kernel data are normally loaded once per program 
   run, NOT every time this routine is called. 
 
-Particulars
 
   Given a ray defined by a direction vector and the location of an 
   observer, sincpt_c computes the surface intercept point of the ray 
   on a specified target body. sincpt_c also determines the vector 
   from the observer to the surface intercept point. 
 
   When aberration corrections are used, this routine finds the 
   value of `spoint' such that, if `spoint' is regarded as an ephemeris 
   object, after the selected aberration corrections are applied to 
   the vector from the observer to `spoint', the resulting vector is 
   parallel to the direction vector `dvec'. 
 
   This routine computes light time corrections using light time 
   between the observer and the surface intercept point, as opposed 
   to the center of the target. Similarly, stellar aberration 
   corrections done by this routine are based on the direction of 
   the vector from the observer to the light-time corrected 
   intercept point, not to the target center. This technique avoids 
   errors due to the differential between aberration corrections 
   across the target body. Therefore it's valid to use aberration 
   corrections with this routine even when the observer is very 
   close to the intercept point, in particular when the 
   observer-intercept point distance is much less than the 
   observer-target center distance. It's also valid to use stellar 
   aberration corrections even when the intercept point is near or 
   on the limb (as may occur in occultation computations using a 
   point target). 
 
   When comparing surface intercept point computations with results 
   from sources other than SPICE, it's essential to make sure the 
   same geometric definitions are used. 
    
-Examples
  
   The numerical results shown for this example may differ across 
   platforms. The results depend on the SPICE kernels used as 
   input, the compiler and supporting libraries, and the machine  
   specific arithmetic implementation.  
 

   1) The following program computes surface intercept points on 
      Mars for the boresight and FOV boundary vectors of the MGS MOC
      narrow angle camera. The intercepts are computed for a single
      observation epoch. Light time and stellar aberration corrections
      are used. For simplicity, camera distortion is ignored.
     
      Use the meta-kernel shown below to load the required SPICE 
      kernels. 

           KPL/MK
 
           File: mgs_example2.tm

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
              mgs_moc_v20.ti                MGS MOC instrument
                                            parameters
              mgs_sclkscet_00061.tsc        MGS SCLK coefficients
              mgs_sc_ext12.bc               MGS s/c bus attitude
              mgs_ext12_ipng_mgs95j.bsp     MGS ephemeris
 
           \begindata
 
              KERNELS_TO_LOAD = ( 'de418.bsp',
                                  'pck00008.tpc',
                                  'naif0008.tls',
                                  'mgs_moc_v20.ti',
                                  'mgs_sclkscet_00061.tsc',
                                  'mgs_sc_ext12.bc',
                                  'mgs_ext12_ipng_mgs95j.bsp' )
           \begintext


     Example code begins here. 

      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"
      #include "SpiceZmc.h"

      int main()
      {
         /.
         Local parameters 
         ./
         #define META                "mgs_example2.tm" 
         #define ABCLEN              20
         #define LNSIZE              81
         #define NAMLEN              33
         #define TIMLEN              51
         #define SHPLEN              81
         #define NCORNR               4

         /. 
         Local variables 
         ./
         SpiceBoolean            found;

         SpiceChar             * abcorr  = "CN+S";
         SpiceChar             * camera  = "MGS_MOC_NA";
         SpiceChar               dref    [NAMLEN];
         SpiceChar             * fixref  = "IAU_MARS";
         SpiceChar             * method  = "Ellipsoid";
         SpiceChar             * obsrvr  = "MGS";
         SpiceChar               shape   [ SHPLEN ];
         SpiceChar             * target  = "Mars";
         SpiceChar               title   [ LNSIZE ];
         SpiceChar             * utc     = "2003 OCT 13 06:00:00 UTC";

         SpiceDouble             bounds  [NCORNR][3];
         SpiceDouble             bsight  [3];
         SpiceDouble             dist;
         SpiceDouble             dvec    [3];
         SpiceDouble             et;
         SpiceDouble             lat;
         SpiceDouble             lon;
         SpiceDouble             radius;
         SpiceDouble             spoint [3];
         SpiceDouble             srfvec [3];
         SpiceDouble             trgepc;

         SpiceInt                camid;
         SpiceInt                i;
         SpiceInt                n;


         /.
         Load kernel files:
         ./
         furnsh_c ( META );

         /.
         Convert the UTC request time to ET (seconds past
         J2000, TDB). 
         ./ 
         str2et_c ( utc, &et );

         /.
         Get the MGS MOC Narrow angle camera (MGS_MOC_NA) 
         ID code. Then look up the field of view (FOV) 
         parameters. 
         ./ 
         bodn2c_c ( camera, &camid, &found );

         if ( !found )
         {
             setmsg_c ( "Could not find ID code for "
                        "instrument #."               );
             errch_c  ( "#", camera                   );
             sigerr_c ( "SPICE(NOTRANSLATION)"        );
         }

         /.
         getfov_c will return the name of the camera-fixed frame
         in the string `dref', the camera boresight vector in
         the array `bsight', and the FOV corner vectors in the
         array `bounds'.
         ./
         getfov_c ( camid, NCORNR, SHPLEN, NAMLEN,
                    shape, dref,   bsight, &n,     bounds );

         printf ( "\n"
                  "Surface Intercept Locations for Camera\n" 
                  "FOV Boundary and Boresight Vectors\n" 
                  "\n" 
                  "   Instrument:             %s\n" 
                  "   Epoch:                  %s\n" 
                  "   Aberration correction:  %s\n" 
                  "\n",
                  camera, utc, abcorr                             ); 

         /. 
         Now compute and display the surface intercepts for the 
         boresight and all of the FOV boundary vectors. 
         ./ 

         for ( i = 0;  i <= NCORNR;  i++ )
         {
            if ( i < NCORNR )
            {
               sprintf ( title, "Corner vector %ld", i );

               vequ_c ( bounds[i], dvec );
            }
            else
            {
               strcpy ( title,  "Boresight vector" );

               vequ_c ( bsight, dvec );
            }

            /.  
            Compute the surface intercept point using 
            the specified aberration corrections. 
            ./
            sincpt_c ( method, 
                       target,  et,     fixref, abcorr, 
                       obsrvr,  dref,   dvec,   spoint,  
                       &trgepc, srfvec, &found         );

            if ( found )
            {
               /.
               Compute range from observer to apparent intercept. 
               ./
               dist = vnorm_c( srfvec );

               /.
               Convert rectangular coordinates to planetocentric 
               latitude and longitude. Convert radians to degrees. 
               ./ 
               reclat_c ( spoint, &radius, &lon, &lat );

               lon *= dpr_c ();
               lat *= dpr_c ();

               /.
               Display the results. 
               ./

               printf ( "\n"
                        "%s\n", title );

               sprintf ( title, "  Vector in %s frame = ", dref );

               printf ( "\n"
                        "%s\n", title );

               if ( i < NCORNR )
               {
                   printf ( "   %18.10e %18.10e %18.10e\n", 
                            bounds[i][0], bounds[i][1], bounds[i][2] );
               }
               else
               {
                  printf ( "   %18.10e %18.10e %18.10e\n", 
                           bsight[0],    bsight[1],    bsight[2]    );
               }

               printf ( "\n"
                        "  Intercept:\n"
                        "\n"                     
                        "     Radius                   (km)  = %18.10e\n"
                        "     Planetocentric Latitude  (deg) = %18.10e\n"
                        "     Planetocentric Longitude (deg) = %18.10e\n"
                        "     Range                    (km)  = %18.10e\n"
                        "\n",
                        radius,  lat,  lon,  dist                          );
            } 
            else 
            { 
                printf ( "\n"
                         "Intercept not found.\n"
                         "\n"                     );
            }

         }
         return ( 0 );
      }


   When this program was executed on a PC/Linux/gcc platform, the 
   output was:
 
      Surface Intercept Locations for Camera
      FOV Boundary and Boresight Vectors

         Instrument:             MGS_MOC_NA
         Epoch:                  2003 OCT 13 06:00:00 UTC
         Aberration correction:  CN+S


      Corner vector 0

        Vector in MGS_MOC_NA frame =
           1.8571383810e-06  -3.8015622659e-03   9.9999277403e-01

        Intercept:

           Radius                   (km)  =   3.3849411359e+03
           Planetocentric Latitude  (deg) =  -4.8477481924e+01
           Planetocentric Longitude (deg) =  -1.2347407905e+02
           Range                    (km)  =   3.8898310366e+02


      Corner vector 1

        Vector in MGS_MOC_NA frame =
           1.8571383810e-06   3.8015622659e-03   9.9999277403e-01

        Intercept:

           Radius                   (km)  =   3.3849396987e+03
           Planetocentric Latitude  (deg) =  -4.8481636340e+01
           Planetocentric Longitude (deg) =  -1.2339882297e+02
           Range                    (km)  =   3.8897512130e+02


      Corner vector 2

        Vector in MGS_MOC_NA frame =
          -1.8571383810e-06   3.8015622659e-03   9.9999277403e-01

        Intercept:

           Radius                   (km)  =   3.3849396899e+03
           Planetocentric Latitude  (deg) =  -4.8481661910e+01
           Planetocentric Longitude (deg) =  -1.2339882618e+02
           Range                    (km)  =   3.8897466238e+02


      Corner vector 3

        Vector in MGS_MOC_NA frame =
          -1.8571383810e-06  -3.8015622659e-03   9.9999277403e-01

        Intercept:

           Radius                   (km)  =   3.3849411271e+03
           Planetocentric Latitude  (deg) =  -4.8477507498e+01
           Planetocentric Longitude (deg) =  -1.2347408220e+02
           Range                    (km)  =   3.8898264472e+02


      Boresight vector

        Vector in MGS_MOC_NA frame =
           0.0000000000e+00   0.0000000000e+00   1.0000000000e+00

        Intercept:

           Radius                   (km)  =   3.3849404102e+03
           Planetocentric Latitude  (deg) =  -4.8479579822e+01
           Planetocentric Longitude (deg) =  -1.2343645396e+02
           Range                    (km)  =   3.8897573572e+02



   2) Use subpnt_c to find the sub-spacecraft point on Mars for the 
      Mars Reconnaissance Orbiter spacecraft (MRO) at a specified 
      time, using the "near point: ellipsoid" computation method. 
      Use both LT+S and CN+S aberration corrections to illustrate 
      the differences. 
 
      Convert the spacecraft to sub-observer point vector obtained 
      from subpnt_c into the MRO_HIRISE_LOOK_DIRECTION reference frame 
      at the observation time. Perform a consistency check with this 
      vector: compare the Mars surface intercept of the ray 
      emanating from the spacecraft and pointed along this vector 
      with the sub-observer point. 
 
      Use the meta-kernel shown below to load the required SPICE 
      kernels. 
 
 
         KPL/MK 
 
         File: mro_example.tm 
 
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
            mro_psp4_ssd_mro95a.bsp       MRO ephemeris 
            mro_v11.tf                    MRO frame specifications 
            mro_sclkscet_00022_65536.tsc  MRO SCLK coefficients and 
                                               parameters 
            mro_sc_psp_070925_071001.bc   MRO attitude 
 
 
         \begindata 
 
            KERNELS_TO_LOAD = ( 'de418.bsp', 
                                'pck00008.tpc', 
                                'naif0008.tls', 
                                'mro_psp4_ssd_mro95a.bsp', 
                                'mro_v11.tf', 
                                'mro_sclkscet_00022_65536.tsc', 
                                'mro_sc_psp_070925_071001.bc'  ) 
         \begintext 
 
 
     Example code begins here. 
 

     /.
     Program EX2
     ./
     #include <stdio.h>
     #include "SpiceUsr.h"

     int main()
     {
        /.
        Local constants
        ./
        #define META            "mro_example.tm"
        #define NCORR           2

        /.    
        Local variables
        ./
        SpiceBoolean            found;

        static SpiceChar      * abcorr[NCORR] =
                                {
                                   "LT+S", "CN+S"
                                };

        static SpiceChar      * hiref;
        static SpiceChar      * method;

        SpiceDouble             alt;
        SpiceDouble             et;
        SpiceDouble             lat;
        SpiceDouble             lon;
        SpiceDouble             mrovec [3];
        SpiceDouble             r1     [3][3];
        SpiceDouble             r2     [3][3];
        SpiceDouble             radius;
        SpiceDouble             spoint [3];
        SpiceDouble             srfvec [3];
        SpiceDouble             trgepc;
        SpiceDouble             xepoch;
        SpiceDouble             xform  [3][3];
        SpiceDouble             xpoint [3];
        SpiceDouble             xvec   [3];

        SpiceInt                i;

        /.
        Load kernel files via the meta-kernel.
        ./
        furnsh_c ( META );

        /.
        Convert the TDB request time string to seconds past
        J2000, TDB.
        ./
        str2et_c ( "2007 SEP 30 00:00:00 TDB", &et );

        /.
        Compute the sub-spacecraft point using the
        "NEAR POINT: ELLIPSOID" definition.
        Compute the results using both LT+S and CN+S
        aberration corrections.
        ./
        method = "Near point: ellipsoid";

        printf ( "\nComputation method = %s\n", method );

        for ( i = 0;  i < 2;  i++ )
        {
           subpnt_c ( method,                                      
                      "mars", et,     "iau_mars", abcorr[i], 
                      "mro",  spoint, &trgepc,    srfvec    );

           /.
           Compute the observer's altitude above `spoint'.
           ./
           alt = vnorm_c ( srfvec );

           /.
           Express `srfvec' in the MRO_HIRISE_LOOK_DIRECTION
           reference frame at epoch `et'. Since `srfvec' is expressed
           relative to the IAU_MARS frame at `trgepc', we must
           call pxfrm2_c to compute the position transformation matrix
           from IAU_MARS at `trgepc' to the MRO_HIRISE_LOOK_DIRECTION
           frame at time `et'.

           To make code formatting a little easier, we'll store
           the long MRO reference frame name in a variable:
           ./
           hiref = "MRO_HIRISE_LOOK_DIRECTION";

           pxfrm2_c ( "iau_mars", hiref,  trgepc, et, xform );
           mxv_c    (  xform,     srfvec, mrovec            );

           /.
           Convert rectangular coordinates to planetocentric
           latitude and longitude. Convert radians to degrees.
           ./
           reclat_c ( spoint, &radius, &lon, &lat );

           lon *= dpr_c();
           lat *= dpr_c();

           /.
           Write the results.
           ./
           printf ( "\n"
                    "Aberration correction = %s\n\n"
                    "  MRO-to-sub-observer vector in\n"
                    "  MRO HIRISE look direction frame\n"
                    "     X-component             (km) = %21.9f\n"
                    "     Y-component             (km) = %21.9f\n"
                    "     Z-component             (km) = %21.9f\n"
                    "  Sub-observer point radius  (km) = %21.9f\n"
                    "  Planetocentric latitude   (deg) = %21.9f\n"
                    "  Planetocentric longitude  (deg) = %21.9f\n"
                    "  Observer altitude          (km) = %21.9f\n",
                    abcorr[i],
                    mrovec[0],
                    mrovec[1],
                    mrovec[2],
                    radius,
                    lat,
                    lon,
                    alt         );

           /.
           Consistency check: find the surface intercept on
           Mars of the ray emanating from the spacecraft and having
           direction vector MROVEC in the MRO HIRISE look direction
           reference frame at ET. Call the intercept point
           XPOINT. XPOINT should coincide with SPOINT, up to a
           small round-off error.
           ./
           sincpt_c ( "ellipsoid", "mars",  et,    "iau_mars",  
                      abcorr[i],   "mro",   hiref, mrovec, 
                      xpoint,      &xepoch, xvec,  &found     );

           if ( !found )
           {
              printf ( "Bug: no intercept\n" );        
           }
           else
           {
              /.
              Report the distance between XPOINT and SPOINT.
              ./
              printf ( "  Intercept comparison error (km) = %21.9f\n\n",
                       vdist_c( xpoint, spoint )                        );
           }
        }

        return ( 0 );
     } 
 
 
   When this program was executed on a PC/Linux/gcc platform, the 
   output was: 


      Computation method = Near point: ellipsoid

      Aberration correction = LT+S

        MRO-to-sub-observer vector in
        MRO HIRISE look direction frame
           X-component             (km) =           0.286931987
           Y-component             (km) =          -0.260417167
           Z-component             (km) =         253.816284981
        Sub-observer point radius  (km) =        3388.299078207
        Planetocentric latitude   (deg) =         -38.799836879
        Planetocentric longitude  (deg) =        -114.995294746
        Observer altitude          (km) =         253.816580760
        Intercept comparison error (km) =           0.000002144


      Aberration correction = CN+S

        MRO-to-sub-observer vector in
        MRO HIRISE look direction frame
           X-component             (km) =           0.286931866
           Y-component             (km) =          -0.260417914
           Z-component             (km) =         253.816274506
        Sub-observer point radius  (km) =        3388.299078205
        Planetocentric latitude   (deg) =         -38.799836883
        Planetocentric longitude  (deg) =        -114.995294968
        Observer altitude          (km) =         253.816570285
        Intercept comparison error (km) =           0.000000001  

 
-Restrictions
 
   A cautionary note: if aberration corrections are used, and  
   if `dref' is the target body-fixed frame, the epoch at which that 
   frame is evaluated is offset from `et' by the light time between 
   the observer and the *center* of the target body. This light time 
   normally will differ from the light time between the observer and 
   intercept point. Consequently the orientation of the target 
   body-fixed frame at `trgepc' will not match that of the target 
   body-fixed frame at the epoch associated with `dref'. As a result, 
   various derived quantities may not be as expected: for example, 
   `srfvec' would not be parallel to `dvec'.
 
   In many applications the errors arising from this frame 
   discrepancy may be insignificant; however a safe approach is to 
   always use as `dref' a frame other than the target body-fixed 
   frame. 
    
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
   S.C. Krening   (JPL)
 
-Version

   -CSPICE Version 1.0.2, 02-APR-2011 (NJB) (SCK)

       References to the new pxfrm2_c routine were added, which
       changed the Detailed Output section and the second example. 

       Miscellaneous, minor header comment corrections were made.

   -CSPICE Version 1.0.1, 06-FEB-2009 (NJB)
 
       Typos in the Detailed Input section's description of `dref'
       were corrected. Incorrect frame name fixfrm was changed to
       fixref in documentation.

       In the header examples, meta-kernel names were updated to use
       the suffix

          ".tm"

   -CSPICE Version 1.0.0, 02-MAR-2008 (NJB)

-Index_Entries
 
   find surface intercept point 
   find intersection of ray and target body surface 
   find intercept of ray on target body surface 
 
-&
*/

{ /* Begin sincpt_c */


   /*
   Local variables
   */
   logical                 fnd;

   /*
   Participate in error tracing.
   */
   chkin_c ( "sincpt_c" );

   /*
   Check the input string arguments:

      method
      target
      fixref
      abcorr
      obsrvr 
      dref

   Make sure each pointer is non-null and each string contains
   at least one data character: that is, one character 
   preceding the null terminator.
   */
   CHKFSTR ( CHK_STANDARD, "sincpt_c", method );
   CHKFSTR ( CHK_STANDARD, "sincpt_c", target );
   CHKFSTR ( CHK_STANDARD, "sincpt_c", fixref );
   CHKFSTR ( CHK_STANDARD, "sincpt_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "sincpt_c", obsrvr );
   CHKFSTR ( CHK_STANDARD, "sincpt_c", dref   );
   
   /*
   Call the f2c'd SPICELIB function.
   */
   sincpt_ ( (char       *) method,
             (char       *) target,
             (doublereal *) &et,
             (char       *) fixref,
             (char       *) abcorr,
             (char       *) obsrvr,
             (char       *) dref,
             (doublereal *) dvec,
             (doublereal *) spoint,
             (doublereal *) trgepc,
             (doublereal *) srfvec,
             (logical    *) &fnd,
             (ftnlen      ) strlen(method),
             (ftnlen      ) strlen(target),
             (ftnlen      ) strlen(fixref),
             (ftnlen      ) strlen(abcorr),
             (ftnlen      ) strlen(obsrvr),
             (ftnlen      ) strlen(dref)    );

   /*
   Move the found flag into a variable of type SpiceBoolean.
   The SpiceBoolean type may have a different size than
   the logical type.
   */
   
   *found = fnd;

   chkout_c ( "sincpt_c" );

} /* End sincpt_c */
