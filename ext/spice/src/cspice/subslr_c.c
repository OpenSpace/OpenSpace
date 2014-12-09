/*

-Procedure subslr_c ( Sub-solar point )

-Abstract
 
   Compute the rectangular coordinates of the sub-solar point on 
   a target body at a specified epoch, optionally corrected for 
   light time and stellar aberration. 
 
   This routine supersedes subsol_c. 
 
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
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
 
   void subslr_c ( ConstSpiceChar       * method,
                   ConstSpiceChar       * target,
                   SpiceDouble            et,
                   ConstSpiceChar       * fixref,
                   ConstSpiceChar       * abcorr,
                   ConstSpiceChar       * obsrvr,
                   SpiceDouble            spoint [3],
                   SpiceDouble          * trgepc,
                   SpiceDouble            srfvec [3] )
 
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
   spoint     O   Sub-solar point on the target body. 
   trgepc     O   Sub-solar point epoch. 
   srfvec     O   Vector from observer to sub-solar point. 
 
-Detailed_Input
 
   method      is a short string providing parameters defining 
               the computation method to be used.  
 
               The supported values of `method' are listed below. 
               Please note that the colon is a required delimiter; 
               using a blank will not work. 
 
                  "Near point: ellipsoid"   The sub-solar point 
                                            computation uses a 
                                            triaxial ellipsoid to 
                                            model the surface of the 
                                            target body. The 
                                            sub-solar point is 
                                            defined as the nearest 
                                            point on the target 
                                            relative to the Sun.  
 
                  "Intercept: ellipsoid"    The sub-solar point 
                                            computation uses a 
                                            triaxial ellipsoid to 
                                            model the surface of the 
                                            target body. The 
                                            sub-solar point is 
                                            defined as the target 
                                            surface intercept of the 
                                            line containing the 
                                            Sun and the 
                                            target's center. 
 
               Neither case nor white space are significant in 
               `method'. For example, the string  
 
                 " nearpoint:ELLIPSOID " 
 
               is valid. 
 
                
   target      is the name of the target body. The target body is  
               an ephemeris object (its trajectory is given by 
               SPK data), and is an extended object. 
 
               The string `target' is case-insensitive, and leading 
               and trailing blanks in `target' are not significant. 
               Optionally, you may supply a string containing the 
               integer ID code for the object. For example both 
               "MOON" and "301" are legitimate strings that indicate 
               the Moon is the target body. 
 
               When the target body's surface is represented by a 
               tri-axial ellipsoid, this routine assumes that a 
               kernel variable representing the ellipsoid's radii is 
               present in the kernel pool. Normally the kernel 
               variable would be defined by loading a PCK file. 
 
 
   et          is the epoch of participation of the observer, 
               expressed as ephemeris seconds past J2000 TDB: `et' is 
               the epoch at which the observer's state is computed. 
 
               When aberration corrections are not used, `et' is also 
               the epoch at which the position and orientation of 
               the target body and the position of the Sun are computed. 
 
               When aberration corrections are used, `et' is the epoch
               at which the observer's state relative to the solar
               system barycenter is computed; in this case the position
               and orientation of the target body are computed at
               et-lt, where `lt' is the one-way light time between the
               sub-solar point and the observer. See the description of
               `abcorr' below for details.
 
 
   fixref      is the name of the body-fixed, body-centered reference
               frame associated with the target body. The output
               sub-solar point `spoint' will be expressed relative to
               this reference frame. The string `fixref' is
               case-insensitive, and leading and trailing blanks in
               `fixref' are not significant.
 
                
   abcorr      indicates the aberration correction to be applied when
               computing the target position and orientation, and the
               position of the Sun.
        
               For remote sensing applications, where the apparent 
               sub-solar point seen by the observer is desired, 
               normally either of the corrections  
             
                  "LT+S"  
                  "CN+S" 
    
               should be used. These and the other supported options 
               are described below. `abcorr' may be any of the  
               following: 
 
                  "NONE"     Apply no correction. Return the  
                             geometric sub-solar point on the 
                             target body. 
 
               Let `lt' represent the one-way light time between the 
               observer and the sub-solar point (note: NOT 
               between the observer and the target body's center). 
               The following values of `abcorr' apply to the 
               "reception" case in which photons depart from the 
               sub-solar point's location at the light-time 
               corrected epoch et-lt and *arrive* at the observer's 
               location at `et': 
 
 
                  "LT"       Correct for one-way light time (also 
                             called "planetary aberration") using a 
                             Newtonian formulation. This correction 
                             yields the location of sub-solar 
                             point at the moment it emitted photons 
                             arriving at the observer at `et'. 
  
                             The light time correction uses an 
                             iterative solution of the light time 
                             equation. The solution invoked by the 
                             "LT" option uses one iteration. 

                             The target position and orientation as
                             seen by the observer are corrected for
                             light time. The position of the Sun
                             relative to the target is corrected for
                             one-way light time between the Sun and
                             target.
 
 
                  "LT+S"     Correct for one-way light time and stellar
                             aberration using a Newtonian formulation.
                             This option modifies the sub-solar point
                             obtained with the "LT" option to account
                             for the observer's velocity relative to
                             the solar system barycenter. These
                             corrections yield the apparent
                             sub-solar point.

 
                  "CN"       Converged Newtonian light time 
                             correction. In solving the light time 
                             equation, the "CN" correction iterates 
                             until the solution converges. Both the 
                             position and rotation of the target 
                             body, and the position of the Sun,
                             are corrected for light time. 
 
                  "CN+S"     Converged Newtonian light time and 
                             stellar aberration corrections. This 
                             option produces a solution that is at 
                             least as accurate at that obtainable 
                             with the "LT+S" option. Whether the "CN+S" 
                             solution is substantially more accurate 
                             depends on the geometry of the 
                             participating objects and on the 
                             accuracy of the input data. In all 
                             cases this routine will execute more 
                             slowly when a converged solution is 
                             computed. 

                Neither case nor white space are significant in
                `abcorr'. For example, the string

                  'Lt + s'

                is valid.

 
   obsrvr      is the name of the observing body. The observing body 
               is an ephemeris object: it typically is a spacecraft, 
               the earth, or a surface point on the earth. `obsrvr' is 
               case-insensitive, and leading and trailing blanks in 
               `obsrvr' are not significant. Optionally, you may 
               supply a string containing the integer ID code for 
               the object. For example both "MOON" and "301" are 
               legitimate strings that indicate the Moon is the 
               observer. 
 
               The observer may coincide with the target.

-Detailed_Output
 
 
   spoint      is the sub-solar point on the target body. 
 
               The sub-solar point is defined either as the point 
               on the target body that is closest to the Sun, 
               or the target surface intercept of the line from the 
               Sun to the target's center; the input argument 
               `method' selects the definition to be used.  
 
               `spoint' is expressed in Cartesian coordinates, 
               relative to the body-fixed target frame designated by 
               `fixref'. The body-fixed target frame is evaluated at 
               the sub-solar epoch `trgepc' (see description below). 
 
               When light time correction is used, the duration of 
               light travel between `spoint' to the observer is 
               considered to be the one way light time. 
 
               When aberration corrections are used, `spoint' is 
               computed using target body position and orientation 
               that have been adjusted for the corrections 
               applicable to `spoint' itself rather than to the target 
               body's center. In particular, if the stellar 
               aberration correction applicable to `spoint' is 
               represented by a shift vector `s', then the light-time 
               corrected position of the target is shifted by `s'
               before the sub-solar point is computed. 
                
               The components of `spoint' have units of km. 
 
 
   trgepc      is the "sub-solar point epoch." `trgepc' is defined 
               as follows: letting `lt' be the one-way light time 
               between the observer and the sub-solar point, 
               `trgepc' is either the epoch et-lt or `et' depending on 
               whether the requested aberration correction is, 
               respectively, for received radiation or omitted. `lt' is 
               computed using the method indicated by `abcorr'. 
 
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
 
 
-Parameters
 
   None. 
 
-Exceptions

 
   1)  If the specified aberration correction is unrecognized, the 
       error will be diagnosed and signaled by a routine in the call 
       tree of this routine.  

   2)  If either the target or observer input strings cannot be
       converted to an integer ID code, the error SPICE(IDCODENOTFOUND)
       is signaled.
  
   3)  If the input target body-fixed frame `fixref' is not recognized,
       the error SPICE(NOFRAME) is signaled. A frame name may fail
       to be recognized because a required frame specification kernel
       has not been loaded; another cause is a misspelling of the 
       frame name.
 
   4)  If the input frame `fixref' is not centered at the target body,
       the error SPICE(INVALIDFRAME) is signaled.

   5)  If the input argument `method' is not recognized, the error 
       SPICE(INVALIDMETHOD) is signaled. 

   6)  If insufficient ephemeris data have been loaded prior to 
       calling subslr_c, the error will be diagnosed and signaled by a 
       routine in the call tree of this routine. Note that when 
       light time correction is used, sufficient ephemeris data 
       must be available to propagate the states of observer,
       target, and the Sun to the solar system barycenter. 
 
   7)  If the computation method specifies an ellipsoidal target shape
       and triaxial radii of the target body have not been loaded 
       into the kernel pool prior to calling subslr_c, the error will 
       be diagnosed and signaled by a routine in the call tree of 
       this routine. 
 
   8)  The target must be an extended body: if any of the radii of  
       the target body are non-positive, the error will be diagnosed 
       and signaled by routines in the call tree of this routine. 
  
   9)  If PCK data specifying the target body-fixed frame orientation
       have not been loaded prior to calling subslr_c, the error will 
       be diagnosed and signaled by a routine in the call tree of 
       this routine. 

   10) The error SPICE(EMPTYSTRING) is signaled if any input string
       argument does not contain at least one character, since the
       input string cannot be converted to a Fortran-style string in
       this case.
      
   11) The error SPICE(NULLPOINTER) is signaled if any input
       string argument pointer is null.

-Files
 
   Appropriate kernels must be loaded by the calling program before
   this routine is called.
 
   The following data are required: 
 
      - SPK data: ephemeris data for target, observer, and the Sun must
        be loaded. If aberration corrections are used, the states of
        target and observer relative to the solar system barycenter
        must be calculable from the available ephemeris data. Typically
        ephemeris data are made available by loading one or more SPK
        files via furnsh_c.

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
 
   There are two different popular ways to define the sub-solar point:
   "nearest point on the target to the Sun" or "target surface
   intercept of the line containing the Sun and the target." These
   coincide when the target is spherical and generally are distinct
   otherwise.
 
   This routine computes light time corrections using light time
   between the observer and the sub-solar point, as opposed to the
   center of the target. Similarly, stellar aberration corrections done
   by this routine are based on the direction of the vector from the
   observer to the light-time corrected sub-solar point, not to the
   target center. This technique avoids errors due to the differential
   between aberration corrections across the target body. Therefore
   it's valid to use aberration corrections with this routine even when
   the observer is very close to the sub-solar point, in particular
   when the observer to sub-solar point distance is much less than the
   observer to target center distance.
 
   The definition of the aberration-corrected sub-solar point is
   implicit: `spoint' is defined by an equation of the general form
 
      spoint = f ( spoint ) 
 
   Because of the contraction properties of both light time and 
   stellar aberration corrections---that is, the difference in the 
   corrections for two vectors is much smaller than the difference 
   between the vectors themselves---it's easy to solve this equation 
   accurately and fairly quickly. 
    
   When comparing sub-solar point computations with results from 
   sources other than SPICE, it's essential to make sure the same 
   geometric definitions are used. 
 
-Examples
 
   The numerical results shown for these examples may differ across 
   platforms. The results depend on the SPICE kernels used as 
   input, the compiler and supporting libraries, and the machine  
   specific arithmetic implementation.  


   1) Find the sub-solar point on Mars as seen from the Earth for a
      specified time. Perform the computation twice, using both the
      "intercept" and "near point" options. Display the locations of
      the Sun and the sub-solar point using both planetocentric
      and planetographic coordinates.
 
      Use the meta-kernel shown below to load the required SPICE 
      kernels. 
 
         KPL/MK

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.


         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'pck00010.tpc',
                                'naif0010.tls'  )

         \begintext

 
      Example code begins here. 
 
         /.
         Program EX1 
         ./

         #include <stdio.h>
         #include "SpiceUsr.h"

         int main()
         {
            /.
            Local parameters
            ./
            #define META            "subslr.tm"

            /.
            Local variables
            ./
            static SpiceChar      * method[2] =
                                    {
                                       "Intercept:  ellipsoid",
                                       "Near point: ellipsoid"
                                    };

            SpiceDouble             et;
            SpiceDouble             f;
            SpiceDouble             radii  [3];
            SpiceDouble             re;
            SpiceDouble             rp;
            SpiceDouble             spclat;
            SpiceDouble             spclon;
            SpiceDouble             spcrad;
            SpiceDouble             spgalt;
            SpiceDouble             spglat;
            SpiceDouble             spglon;
            SpiceDouble             spoint [3];
            SpiceDouble             srfvec [3];
            SpiceDouble             sunlt;
            SpiceDouble             sunpos [3];
            SpiceDouble             sunst  [6];
            SpiceDouble             supcln;
            SpiceDouble             supclt;
            SpiceDouble             supcrd;
            SpiceDouble             supgal;
            SpiceDouble             supgln;
            SpiceDouble             supglt;
            SpiceDouble             trgepc;

            SpiceInt                i;
            SpiceInt                n;

            /.
            Load kernel files via the meta-kernel.
            ./
            furnsh_c ( META );

            /.
            Convert the UTC request time string to seconds past
            J2000, TDB.
            ./
            str2et_c ( "2008 aug 11 00:00:00", &et );

            /.
            Look up the target body's radii. We'll use these to
            convert Cartesian to planetographic coordinates. Use
            the radii to compute the flattening coefficient of
            the reference ellipsoid.
            ./
            bodvrd_c ( "MARS", "RADII", 3, &n, radii );

            /.
            Let `re' and `rp' be, respectively, the equatorial and
            polar radii of the target.
            ./
            re = radii[0];
            rp = radii[2];

            f  = ( re - rp ) / re;

            /.
            Compute the sub-solar point using light time and stellar
            aberration corrections. Use the "target surface intercept"
            definition of the sub-solar point on the first loop
            iteration, and use the "near point" definition on the
            second.
            ./

            for ( i = 0;  i < 2;  i++ )
            {
               subslr_c ( method[i],
                          "mars",  et,     "iau_mars", "lt+s",
                          "earth", spoint, &trgepc,    srfvec );

               /.
               Convert the sub-observer point's rectangular coordinates
               to planetographic longitude, latitude and altitude.
               Convert radians to degrees.
               ./
               recpgr_c ( "mars",  spoint,  re,     f,
                          &spglon, &spglat, &spgalt   );

               spglon *= dpr_c();
               spglat *= dpr_c();

               /.
               Convert the sub-solar point's rectangular coordinates to
               planetocentric radius, longitude, and latitude. Convert
               radians to degrees.
               ./
               reclat_c ( spoint, &spcrad, &spclon, &spclat );

               spclon *= dpr_c();
               spclat *= dpr_c();

               /.
               Compute the Sun's apparent position relative to the 
               sub-solar point at `trgepc'. Add the position of the
               sub-solar point relative to the target's center to
               obtain the position of the sun relative to the target's
               center. Express the latter position in planetographic 
               coordinates.
               ./
               spkcpo_c ( "sun",   trgepc,  "iau_mars", "OBSERVER",
                          "lt+s",  spoint,  "mars",     "iau_mars",
                          sunst,   &sunlt                          );

               vadd_c ( sunst, spoint, sunpos );

               recpgr_c ( "mars",  sunpos,  re,     f,
                          &supgln, &supglt, &supgal    );

               supgln *= dpr_c ();
               supglt *= dpr_c ();

               /.
               Convert the Sun's rectangular coordinates to
               planetocentric radius, longitude, and latitude.
               Convert radians to degrees.
               ./
               reclat_c ( sunpos, &supcrd, &supcln, &supclt );

               supcln *= dpr_c();
               supclt *= dpr_c();

               /.
               Write the results.
               ./
               printf ( "\n"
                        " Computation method = %s\n\n"
                        "  Sub-solar point altitude            (km) = %21.9f\n"
                        "  Sub-solar planetographic longitude (deg) = %21.9f\n"
                        "  Sun's planetographic longitude     (deg) = %21.9f\n"
                        "  Sub-solar planetographic latitude  (deg) = %21.9f\n"
                        "  Sun's planetographic latitude      (deg) = %21.9f\n"
                        "  Sub-solar planetocentric longitude (deg) = %21.9f\n"
                        "  Sun's planetocentric longitude     (deg) = %21.9f\n"
                        "  Sub-solar planetocentric latitude  (deg) = %21.9f\n"
                        "  Sun's planetocentric latitude      (deg) = %21.9f\n"
                        "\n",
                        method[i], 
                        spgalt, 
                        spglon,
                        supgln, 
                        spglat, 
                        supglt, 
                        spclon, 
                        supcln,
                        spclat,
                        supclt      );
            }

            return ( 0 );
         }

 
   When this program was executed on a PC/Linux/gcc platform, the 
   output was: 


      Computation method = Intercept:  ellipsoid

       Sub-solar point altitude            (km) =          -0.000000000
       Sub-solar planetographic longitude (deg) =         175.810675510
       Sun's planetographic longitude     (deg) =         175.810675508
       Sub-solar planetographic latitude  (deg) =          23.668550281
       Sun's planetographic latitude      (deg) =          23.420823362
       Sub-solar planetocentric longitude (deg) =        -175.810675510
       Sun's planetocentric longitude     (deg) =        -175.810675508
       Sub-solar planetocentric latitude  (deg) =          23.420819936
       Sun's planetocentric latitude      (deg) =          23.420819936


      Computation method = Near point: ellipsoid

       Sub-solar point altitude            (km) =          -0.000000000
       Sub-solar planetographic longitude (deg) =         175.810675410
       Sun's planetographic longitude     (deg) =         175.810675408
       Sub-solar planetographic latitude  (deg) =          23.420823362
       Sun's planetographic latitude      (deg) =          23.420823362
       Sub-solar planetocentric longitude (deg) =        -175.810675410
       Sun's planetocentric longitude     (deg) =        -175.810675408
       Sub-solar planetocentric latitude  (deg) =          23.175085578
       Sun's planetocentric latitude      (deg) =          23.420819936

 
 
-Restrictions
 
  None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
   S.C. Krening   (JPL)
 
-Version

   -CSPICE Version 1.1.0, 08-MAY-2012 (NJB) (SCK)
 
       Some changes to computation of the surface point-to-sun vector
       were made in the underlying SPICELIB routine SUBSLR. These
       changes have a small effect on the outputs of this routine.
  
       Exceptions removed: the observer and target are now
       permitted to coincide.

       The header example program was updated to reflect the new
       method of computing the apparent sun location, and the set
       of kernels referenced by the example meta-kernel were updated.
       The display of the program's output was updated accordingly.

       References to the new pxfrm2_c routine were added to the
       Detailed Output section.

   -CSPICE Version 1.0.1, 06-FEB-2009 (NJB)
 
       Incorrect frame name fixfrm was changed to fixref in
       documentation.
 
       In the header examples, meta-kernel names were updated to use
       the suffix

          ".tm"
 
   -CSPICE Version 1.0.0, 02-MAR-2008 (NJB) 

-Index_Entries
 
   find sub-solar point on target body 
   find nearest point to Sun on target body 
 
-&
*/

{ /* Begin subslr_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "subslr_c" );

   /*
   Check the input strings: method, target, fixref, abcorr, and obsrvr.
   Make sure none of the pointers are null and that each string
   contains at least one non-null character.
   */
   CHKFSTR ( CHK_STANDARD, "subslr_c", method );
   CHKFSTR ( CHK_STANDARD, "subslr_c", target );
   CHKFSTR ( CHK_STANDARD, "subslr_c", fixref );
   CHKFSTR ( CHK_STANDARD, "subslr_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "subslr_c", obsrvr );
 
   /*
   Call the f2c'd routine.
   */
   subslr_ ( ( char         * ) method,
             ( char         * ) target,
             ( doublereal   * ) &et,
             ( char         * ) fixref,
             ( char         * ) abcorr,
             ( char         * ) obsrvr,
             ( doublereal   * ) spoint,
             ( doublereal   * ) trgepc,
             ( doublereal   * ) srfvec,
             ( ftnlen         ) strlen(method),
             ( ftnlen         ) strlen(target),
             ( ftnlen         ) strlen(fixref),
             ( ftnlen         ) strlen(abcorr),
             ( ftnlen         ) strlen(obsrvr)  );

   chkout_c ( "subslr_c" );

} /* End subslr_c */
