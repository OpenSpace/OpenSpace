/*

-Procedure edlimb_c   ( Ellipsoid Limb )

-Abstract
 
   Find the limb of a triaxial ellipsoid, viewed from a specified 
   point. 
 
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
 
   ELLIPSES 
 
-Keywords
 
   ELLIPSE 
   ELLIPSOID 
   GEOMETRY 
   MATH 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    edlimb_c
   
   
   void edlimb_c ( SpiceDouble           a,
                   SpiceDouble           b,
                   SpiceDouble           c,
                   ConstSpiceDouble      viewpt[3],
                   SpiceEllipse        * limb      ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   a          I   Length of ellipsoid semi-axis lying on the x-axis. 
   b          I   Length of ellipsoid semi-axis lying on the y-axis. 
   c          I   Length of ellipsoid semi-axis lying on the z-axis. 
   viewpt     I   Location of viewing point. 
   limb       O   Limb of ellipsoid as seen from viewing point. 
 
-Detailed_Input
 
   a, 
   b, 
   c              are the lengths of the semi-axes of a triaxial 
                  ellipsoid.  The ellipsoid is centered at the 
                  origin and oriented so that its axes lie on the 
                  x, y and z axes.  a, b, and c are the lengths of 
                  the semi-axes that point in the x, y, and z 
                  directions respectively. 
 
   viewpt         is a point from which the ellipsoid is viewed. 
                  viewpt must be outside of the ellipsoid. 
 
-Detailed_Output
 
   limb           is a CSPICE ellipse that represents the limb of 
                  the ellipsoid. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the length of any semi-axis of the ellipsoid is 
       non-positive, the error DEGENERATECASE is signaled. 
       limb is not modified. 
 
   2)  If the length of any semi-axis of the ellipsoid is zero after 
       the semi-axis lengths are scaled by the reciprocal of the 
       magnitude of the longest semi-axis and then squared, the error 
       SPICE(DEGENERATECASE) is signaled.  limb is not modified. 
 
   3)  If the viewing point viewpt is inside the ellipse, the error 
       SPICE(INVALIDPOINT) is signaled.  limb is not modified. 
 
   4)  If the geometry defined by the input ellipsoid and viewing 
       point is so extreme that the limb cannot be found, the error 
       SPICE(DEGENERATECASE) is signaled. 
 
   5)  If the shape of the ellipsoid and the viewing geometry are 
       such that the limb is an excessively flat ellipsoid, the 
       limb may be a degenerate ellipse.  You must determine whether 
       this possibility poses a problem for your application. 
 
-Files
 
   None. 
 
-Particulars
 
   The limb of a body, as seen from a viewing point, is the boundary 
   of the portion of the body's surface that is visible from that 
   viewing point.  In this definition, we consider a surface point 
   to be `visible' if it can be connected to the viewing point by a 
   line segment that doen't pass through the body.  This is a purely 
   geometrical definition that ignores the matter of which portions 
   of the surface are illuminated, or whether the view is obscured by 
   any additional objects. 
 
   If a body is modelled as a triaxial ellipsoid, the limb is always 
   an ellipse.  The limb is determined by its center, a semi-major 
   axis vector, and a semi-minor axis vector. 
 
   We note that the problem of finding the limb of a triaxial 
   ellipsoid is mathematically identical to that of finding its 
   terminator, if one makes the simplifying assumption that the 
   terminator is the limb of the body as seen from the vertex of the 
   umbra.  So, this routine can be used to solve this simplified 
   version of the problem of finding the terminator. 
 
-Examples
 
   1)  We'd like to find the apparent limb of Jupiter, corrected for 
       light time and stellar aberration, as seen from a spacecraft's 
       position at time ET. 
 
     
          /.
          Find the viewing point in Jupiter-fixed coordinates.  To do 
          this, find the apparent position of Jupiter as seen from the
          spacecraft in Jupiter-fixed coordinates and negate this 
          vector.  In this case we'll use light time and stellar 
          aberration corrections to arrive at the apparent limb. jstat 
          is the Jupiter's state (position and velocity) as seen
          from the spacecraft.  scpos is the spacecraft's
          position relative to Jupiter.
          ./
          spkez_c( jupid, et, "IAU_JUPITER", "LT+S", scid, scstat, &lt);
 
          vminus_c ( scstat, scpos );
 
          /.
          Get Jupiter's semi-axis lengths.
          ./
          bodvcd_c ( jupid, "RADII", 3, &n, rad );
 
          /.
          Find the apparent limb.  limb is a CSPICE ellipse
          representing the limb.
          ./
          edlimb_c ( rad[0], rad[1], rad[2], scpos, &limb );
 
          /.
          lcentr, smajor, and sminor are the limb's center,
          semi-major axis, and semi-minor axis. 
          ./
          el2cgv_c ( &limb, center, smajor, sminor );
 
 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version

   -CSPICE Version 1.1.0, 24-JUN-2014 (NJB)
 
       Edit to correct chkout_c call passing the wrong routine name.
 
   -CSPICE Version 1.0.1, 24-OCT-2005 (NJB)

       Header update: reference to bodvar_c was replaced with
       reference to bodvcd_c.

   -CSPICE Version 1.0.0, 13-JUN-1999 (NJB)

-Index_Entries
 
   ellipsoid limb 
 
-&
*/

{ /* Begin edlimb_c */


   /*
   Local variables
   */

   SpiceBoolean            found;

   SpiceDouble             level;
   SpiceDouble             normal  [3];
   SpiceDouble             scale;
   SpiceDouble             scla;
   SpiceDouble             scla2;
   SpiceDouble             sclb;
   SpiceDouble             sclb2;
   SpiceDouble             sclc;
   SpiceDouble             sclc2;
   SpiceDouble             v       [3];

   SpicePlane              lplane;



   /*
   Participate in error tracing.
   */

   chkin_c ( "edlimb_c" );

   if (         ( a <= 0. )            
          ||    ( b <= 0. )            
          ||    ( c <= 0. )   )    
   {
      setmsg_c  ( "Semi-axis lengths: a = #,  b = #,  c = #."  );
      errdp_c   ( "#", a                                       );
      errdp_c   ( "#", b                                       );
      errdp_c   ( "#", c                                       );
      sigerr_c  ( "SPICE(DEGENERATECASE)"                      );
      chkout_c  ( "edlimb_c"                                   );
      return;
   }


   /*
   Scale the semi-axes lengths for better numerical behavior.
   If squaring any one of the scaled lengths causes it to
   underflow to zero, we cannot continue the computation. Otherwise,
   scale the viewing point too.
   */

   scale  =  MaxAbs ( a, b     );
   scale  =  MaxAbs ( c, scale ); 

   scla   =  a / scale;
   sclb   =  b / scale;
   sclc   =  c / scale;

   scla2  =  scla*scla;
   sclb2  =  sclb*sclb;
   sclc2  =  sclc*sclc;

   if (       ( scla2   ==   0. )
         ||   ( sclb2   ==   0. )
         ||   ( sclc2   ==   0. )   )    
   {
      setmsg_c ( "Semi-axis too small:  a = #, b = #, c = #. " );
      errdp_c  ( "#", a                                        );
      errdp_c  ( "#", b                                        );
      errdp_c  ( "#", c                                        );
      sigerr_c ( "SPICE(DEGENERATECASE)"                       );
      chkout_c ( "edlimb_c"                                    );
      return;
   }

   vscl_c ( 1. / scale,  viewpt,  v );


   /*
   The viewing point must be outside of the ellipsoid.  level is the
   constant of the level surface that v lies on.  The ellipsoid
   itself is the level surface corresponding to level = 1.
   */
   
   level   =     ( v[0]*v[0] / scla2 ) 
              +  ( v[1]*v[1] / sclb2 ) 
              +  ( v[2]*v[2] / sclc2 );

   if ( level < 1. ) 
   {
      setmsg_c ( "Viewing point is inside the ellipsoid." );
      sigerr_c ( "SPICE(DEGENERATECASE)"                  );
      chkout_c ( "edlimb_c"                               );
      return;
   }


   /*
   Find a normal vector for the limb plane.

   To compute this vector, we use the fact that the surface normal at
   each limb point is orthogonal to the line segment connecting the
   viewing point and the limb point.   Let the notation

      < a, b >

   indicate the dot product of the vectors a and b.  If we call the
   viewing point v and the limb point x, then



                          x[0]         x[1]         x[2]
      0  =   < v - x,  ( -------- ,   -------- ,   --------  )  >
                              2           2             2
                          scla        sclb          sclc


                          x[0]         x[1]         x[2]
         =   <   v,    ( -------- ,   -------- ,   --------  )  >
                              2           2             2
                          scla        sclb          sclc


                          x[0]         x[1]         x[2]
          - <   x,    ( -------- ,   -------- ,   --------  )  >
                              2           2             2
                          scla        sclb          sclc

                              2           2             2
                          x[0]        x[1]          x[2]
         =             --------  +   --------  +  --------
                             2            2             2
                         scla         sclb          sclc


         =   1


   This last equation is just the equation of the scaled ellipsoid.
   We can combine the last two equalities and interchange the
   positions of x and v to obtain


                    v[0]         v[1]         v[2]
      <   x,    ( -------- ,   -------- ,   --------  )  >   =   1
                        2           2             2
                    scla        sclb          sclc


   This is the equation of the limb plane.
   */

   /*
   Put together a SPICELIB plane, lplane, that represents the limb
   plane.
   */
   normal[0]  =  v[0] / scla2;
   normal[1]  =  v[1] / sclb2;
   normal[2]  =  v[2] / sclc2;

   nvc2pl_c ( normal, 1.0, &lplane );
   

   /*
   Find the limb by intersecting the limb plane with the ellipsoid.
   */
   inedpl_c ( scla,  sclb,  sclc,  &lplane,  limb,  &found );
   

   /*
   found should be true unless we've encountered numerical problems.
   */
   
   if ( !found ) 
   {
      setmsg_c ( "Ellipsoid shape and viewing geometry are too " 
                 "extreme; the limb was not found. "             );
      sigerr_c ( "SPICE(DEGENERATECASE)"                         );
      chkout_c ( "edlimb_c"                                      );
      return;
   }


   /*
   Undo the scaling before returning the limb.
   */

   vscl_c ( scale,  limb->center,     limb->center    );
   vscl_c ( scale,  limb->semiMajor,  limb->semiMajor );
   vscl_c ( scale,  limb->semiMinor,  limb->semiMinor );
 

   chkout_c ( "edlimb_c" );

} /* End edlimb_c */

