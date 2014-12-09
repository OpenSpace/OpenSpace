/*

-Procedure subpnt_c ( Sub-observer point )

-Abstract
 
   Compute the rectangular coordinates of the sub-observer point on 
   a target body at a specified epoch, optionally corrected for 
   light time and stellar aberration. 
 
   This routine supersedes subpt_c.
 
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
 
   frame_c 
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
 
   void subpnt_c ( ConstSpiceChar       * method,
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
   spoint     O   Sub-observer point on the target body. 
   trgepc     O   Sub-observer point epoch. 
   srfvec     O   Vector from observer to sub-observer point. 
 
-Detailed_Input
 
   method      is a short string providing parameters defining 
               the computation method to be used.  
 
               The supported values of `method' are listed below. 
               Please note that the colon is a required delimiter; 
               using a blank will not work. 
 
                  "Near point: ellipsoid"   The sub-observer point 
                                            computation uses a 
                                            triaxial ellipsoid to 
                                            model the surface of the 
                                            target body. The 
                                            sub-observer point is 
                                            defined as the nearest 
                                            point on the target 
                                            relative to the 
                                            observer.  
 
                  "Intercept: ellipsoid"    The sub-observer point 
                                            computation uses a 
                                            triaxial ellipsoid to 
                                            model the surface of the 
                                            target body. The 
                                            sub-observer point is 
                                            defined as the target 
                                            surface intercept of the 
                                            line containing the 
                                            observer and the 
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
               the target body are computed. 
 
               When aberration corrections are used, the position and
               orientation of the target body are computed at et-lt or
               et+lt, where `lt' is the one-way light time between the
               sub-observer point and the observer, and the sign
               applied to `lt' depends on the selected correction. See
               the description of `abcorr' below for details.
 
 
   fixref      is the name of the body-fixed, body-centered reference
               frame associated with the target body. The output
               sub-observer point `spoint' will be expressed relative
               to this reference frame. The string `fixref' is
               case-insensitive, and leading and trailing blanks in
               `fixref' are not significant.
 
                
   abcorr      indicates the aberration corrections to be applied when
               computing the target's position and orientation.
 
               For remote sensing applications, where the apparent 
               sub-observer point seen by the observer is desired, 
               normally either of the corrections  
             
                  "LT+S"  
                  "CN+S" 
    
               should be used. These and the other supported options 
               are described below. `abcorr' may be any of the  
               following: 
 
                  "NONE"     Apply no correction. Return the  
                             geometric sub-observer point on the 
                             target body. 
 
               Let `lt' represent the one-way light time between the 
               observer and the sub-observer point (note: NOT 
               between the observer and the target body's center). 
               The following values of `abcorr' apply to the 
               "reception" case in which photons depart from the 
               sub-observer point's location at the light-time 
               corrected epoch et-lt and *arrive* at the observer's 
               location at `et': 
 
 
                  "LT"       Correct for one-way light time (also 
                             called "planetary aberration") using a 
                             Newtonian formulation. This correction 
                             yields the location of sub-observer 
                             point at the moment it emitted photons 
                             arriving at the observer at `et'. 
  
                             The light time correction uses an 
                             iterative solution of the light time 
                             equation. The solution invoked by the 
                             "LT" option uses one iteration. 
 
                             Both the target position as seen by the 
                             observer, and rotation of the target 
                             body, are corrected for light time. 
 
                  "LT+S"     Correct for one-way light time and stellar
                             aberration using a Newtonian formulation.
                             This option modifies the sub-observer
                             point obtained with the "LT" option to
                             account for the observer's velocity
                             relative to the solar system barycenter.
                             These corrections yield the apparent
                             sub-observer point.

                  "CN"       Converged Newtonian light time 
                             correction. In solving the light time 
                             equation, the "CN" correction iterates 
                             until the solution converges. Both the 
                             position and rotation of the target 
                             body are corrected for light time. 
 
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
                              
 
               The following values of `abcorr' apply to the 
               "transmission" case in which photons *depart* from 
               the observer's location at `et' and arrive at the 
               sub-observer point at the light-time corrected epoch 
               et+lt: 
 
                  "XLT"      "Transmission" case: correct for 
                             one-way light time using a Newtonian 
                             formulation. This correction yields the 
                             sub-observer location at the moment it 
                             receives photons emitted from the 
                             observer's location at `et'.  
 
                             The light time correction uses an 
                             iterative solution of the light time 
                             equation. The solution invoked by the 
                             "LT" option uses one iteration. 
 
                             Both the target position as seen by the 
                             observer, and rotation of the target 
                             body, are corrected for light time. 
 
                  "XLT+S"    "Transmission" case: correct for 
                             one-way light time and stellar 
                             aberration using a Newtonian 
                             formulation  This option modifies the 
                             sub-observer point obtained with the 
                             "XLT" option to account for the 
                             observer's velocity relative to the 
                             solar system barycenter. 
 
                  "XCN"      Converged Newtonian light time 
                             correction. This is the same as "XLT"
                             correction but with further iterations 
                             to a converged Newtonian light time 
                             solution.  
 
                  "XCN+S"    "Transmission" case: converged  
                             Newtonian light time and stellar  
                             aberration corrections. 
 
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
 
-Detailed_Output
 
 
   spoint      is the sub-observer point on the target body. 
 
               The sub-observer point is defined either as the point 
               on the target body that is closest to the observer, 
               or the target surface intercept of the line from the 
               observer to the target's center; the input argument 
               `method' selects the definition to be used.  
 
               `spoint' is expressed in Cartesian coordinates, 
               relative to the body-fixed target frame designated by 
               `fixref'. The body-fixed target frame is evaluated at 
               the sub-observer epoch `trgepc' (see description below). 
 
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
               before the sub-observer point is computed. 
                
               The components of `spoint' have units of km. 
 
 
   trgepc      is the "sub-observer point epoch." `trgepc' is defined 
               as follows: letting `lt' be the one-way light time 
               between the observer and the sub-observer point, 
               `trgepc' is the epoch et-lt, et+lt, or `et' depending on 
               whether the requested aberration correction is, 
               respectively, for received radiation, transmitted 
               radiation, or omitted. `lt' is computed using the 
               method indicated by `abcorr'. 
 
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
       calling subpnt_c, the error will be diagnosed and signaled by a 
       routine in the call tree of this routine. Note that when 
       light time correction is used, sufficient ephemeris data 
       must be available to propagate the states of both observer 
       and target to the solar system barycenter. 
 
   9)  If the computation method specifies an ellipsoidal target shape
       and triaxial radii of the target body have not been loaded 
       into the kernel pool prior to calling subpnt_c, the error will 
       be diagnosed and signaled by a routine in the call tree of 
       this routine. 
 
   10) The target must be an extended body: if any of the radii of  
       the target body are non-positive, the error will be diagnosed 
       and signaled by routines in the call tree of this routine. 
  
   11) If PCK data specifying the target body-fixed frame orientation
       have not been loaded prior to calling subpnt_c, the error will 
       be diagnosed and signaled by a routine in the call tree of 
       this routine. 

   12)  The error SPICE(EMPTYSTRING) is signaled if any input string
       argument does not contain at least one character, since the
       input string cannot be converted to a Fortran-style string in
       this case.
      
   13) The error SPICE(NULLPOINTER) is signaled if any input
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
 
   There are two different popular ways to define the sub-observer 
   point: "nearest point on the target to the observer" or "target 
   surface intercept of the line containing observer and target." 
   These coincide when the target is spherical and generally are 
   distinct otherwise. 
 
   This routine computes light time corrections using light time 
   between the observer and the sub-observer point, as opposed to 
   the center of the target. Similarly, stellar aberration 
   corrections done by this routine are based on the direction of 
   the vector from the observer to the light-time corrected 
   sub-observer point, not to the target center. This technique 
   avoids errors due to the differential between aberration 
   corrections across the target body. Therefore it's valid to use 
   aberration corrections with this routine even when the observer 
   is very close to the sub-observer point, in particular when the 
   observer to sub-observer point distance is much less than the 
   observer to target center distance. 
 
   The definition of the aberration-corrected sub-observer point is
   implicit: `spoint' is defined by an equation of the form
 
      spoint = f ( spoint ) 
 
   Because of the contraction properties of both light time and 
   stellar aberration corrections---that is, the difference in the 
   corrections for two vectors is much smaller than the difference 
   between the vectors themselves---it's easy to solve this equation 
   accurately and fairly quickly. 
    
   When comparing sub-observer point computations with results from 
   sources other than SPICE, it's essential to make sure the same 
   geometric definitions are used. 
 
-Examples
 
 
   The numerical results shown for these examples may differ across 
   platforms. The results depend on the SPICE kernels used as 
   input, the compiler and supporting libraries, and the machine  
   specific arithmetic implementation.  
 
  
   1) Find the sub-Earth point on Mars for a specified time. Perform 
      the computation twice, using both the "intercept" and "near 
      point" options. Display the location of both the Earth and the 
      sub-Earth point using both planetocentric and planetographic 
      coordinates. 
 
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
 
            KERNELS_TO_LOAD = ( 'de418.bsp', 
                                'pck00008.tpc', 
                                'naif0008.tls'  ) 
 
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
        #define META            "example.tm"

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
        SpiceDouble             obspos [3];
        SpiceDouble             odist;
        SpiceDouble             opclat;
        SpiceDouble             opclon;
        SpiceDouble             opcrad;
        SpiceDouble             opgalt;
        SpiceDouble             opglat;
        SpiceDouble             opglon;
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
        Let `re and `rp' be, respectively, the equatorial and
        polar radii of the target.
        ./
        re = radii[0];
        rp = radii[2];

        f  = ( re - rp ) / re;

        /.
        Compute sub-observer point using light time and stellar
        aberration corrections. Use the "target surface intercept"
        definition of the sub-observer point on the first loop
        iteration, and use the "near point" definition on the
        second.
        ./

        for ( i = 0;  i < 2;  i++ )
        {
           subpnt_c ( method[i],
                      "mars",  et,     "iau_mars", "lt+s",
                      "earth", spoint, &trgepc,    srfvec );
           /.
           Compute the observer's distance from SPOINT.
           ./
           odist = vnorm_c ( srfvec );

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
           Convert sub-observer point's rectangular coordinates to
           planetocentric radius, longitude, and latitude. Convert
           radians to degrees.
           ./
           reclat_c ( spoint, &spcrad, &spclon, &spclat );

           spclon *= dpr_c();
           spclat *= dpr_c();

           /.
           Compute the observer's position relative to the center
           of the target, where the center's location has been
           adjusted using the aberration corrections applicable
           to the sub-point. Express the observer's location in
           planetographic coordinates.
           ./
           vsub_c ( spoint, srfvec, obspos );

           recpgr_c ( "mars",  obspos,  re,    f,
                      &opglon, &opglat, &opgalt   );

           opglon *= dpr_c ();
           opglat *= dpr_c ();

           /.
           Convert the observer's rectangular coordinates to
           planetocentric radius, longitude, and latitude.
           Convert radians to degrees.
           ./
           reclat_c ( obspos, &opcrad, &opclon, &opclat );

           opclon *= dpr_c();
           opclat *= dpr_c();

           /.
           Write the results.
           ./
           printf ( "\n"
                    " Computation method = %s\n\n"
                    "  Observer altitude                      (km) = %21.9f\n"
                    "  Length of SRFVEC                       (km) = %21.9f\n"
                    "  Sub-observer point altitude            (km) = %21.9f\n"
                    "  Sub-observer planetographic longitude (deg) = %21.9f\n"
                    "  Observer planetographic longitude     (deg) = %21.9f\n"
                    "  Sub-observer planetographic latitude  (deg) = %21.9f\n"
                    "  Observer planetographic latitude      (deg) = %21.9f\n"
                    "  Sub-observer planetocentric longitude (deg) = %21.9f\n"
                    "  Observer planetocentric longitude     (deg) = %21.9f\n"
                    "  Sub-observer planetocentric latitude  (deg) = %21.9f\n"
                    "  Observer planetocentric latitude      (deg) = %21.9f\n"
                    "\n",
                    method[i], 
                    opgalt,
                    odist,
                    spgalt, 
                    spglon,
                    opglon, 
                    spglat, 
                    opglat, 
                    spclon, 
                    opclon,
                    spclat,
                    opclat      );
        }

        return ( 0 );
     }

 
   When this program was executed on a PC/Linux/gcc platform, the 
   output was: 
 
      Computation method = Intercept:  ellipsoid

       Observer altitude                      (km) =   349199089.542324722
       Length of SRFVEC                       (km) =   349199089.579020321
       Sub-observer point altitude            (km) =           0.000000000
       Sub-observer planetographic longitude (deg) =         199.302305055
       Observer planetographic longitude     (deg) =         199.302305055
       Sub-observer planetographic latitude  (deg) =          26.262401212
       Observer planetographic latitude      (deg) =          25.994936725
       Sub-observer planetocentric longitude (deg) =         160.697694945
       Observer planetocentric longitude     (deg) =         160.697694945
       Sub-observer planetocentric latitude  (deg) =          25.994934146
       Observer planetocentric latitude      (deg) =          25.994934146


      Computation method = Near point: ellipsoid

       Observer altitude                      (km) =   349199089.542316437
       Length of SRFVEC                       (km) =   349199089.542316437
       Sub-observer point altitude            (km) =          -0.000000000
       Sub-observer planetographic longitude (deg) =         199.302305055
       Observer planetographic longitude     (deg) =         199.302305055
       Sub-observer planetographic latitude  (deg) =          25.994936725
       Observer planetographic latitude      (deg) =          25.994936725
       Sub-observer planetocentric longitude (deg) =         160.697694945
       Observer planetocentric longitude     (deg) =         160.697694945
       Sub-observer planetocentric latitude  (deg) =          25.729407202
       Observer planetocentric latitude      (deg) =          25.994934146

 
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
 
  None. 
 
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
 
       Incorrect frame name fixfrm was changed to fixref in
       documentation.
 
       In the header examples, meta-kernel names were updated to use
       the suffix

          ".tm"

   -CSPICE Version 1.0.0, 02-MAR-2008 (NJB) 

-Index_Entries
 
   find sub-observer point on target body 
   find sub-spacecraft point on target body 
   find nearest point to observer on target body 
 
-&
*/

{ /* Begin subpnt_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "subpnt_c" );

   /*
   Check the input strings: method, target, fixref, abcorr, and obsrvr.
   Make sure none of the pointers are null and that each string
   contains at least one non-null character.
   */
   CHKFSTR ( CHK_STANDARD, "subpnt_c", method );
   CHKFSTR ( CHK_STANDARD, "subpnt_c", target );
   CHKFSTR ( CHK_STANDARD, "subpnt_c", fixref );
   CHKFSTR ( CHK_STANDARD, "subpnt_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "subpnt_c", obsrvr );
 
   /*
   Call the f2c'd routine.
   */
   subpnt_ ( ( char         * ) method,
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

   chkout_c ( "subpnt_c" );

} /* End subpnt_c */
