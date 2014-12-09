/*

-Procedure rav2xf_c ( Rotation and angular velocity to transform )

-Abstract
 
   This routine determines a state transformation matrix 
   from a rotation matrix and the angular velocity of the 
   rotation. 
 
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
 
   FRAMES 
 
*/

   #include "SpiceUsr.h"
   #undef rav2xf_c
   

   void rav2xf_c ( ConstSpiceDouble    rot   [3][3],
                   ConstSpiceDouble    av    [3],
                   SpiceDouble         xform [6][6]  ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   rot        I   Rotation matrix.
   av         I   Angular velocity vector. 
   xform      O   State transformation associated with rot and av.
 
-Detailed_Input
 
   rot         is a rotation that gives the transformation from 
               some frame frame1 to another frame frame2. 
 
   av          is the angular velocity of the transformation. 
               In other words, if p is the position of a fixed 
               point in frame2, then from the point of view of 
               frame1,  p rotates (in a right handed sense) about 
               an axis parallel to av.  Moreover the rate of rotation 
               in radians per unit time is given by the length of 
               av. 
 
               More formally, the velocity v of p in frame1 is 
               given by 
                                  t 
                   v  = av x ( rot * p ) 
 
-Detailed_Output
 
   xform       is a state transformation matrix associated 
               with rot and av.  If s1 is the state of an object 
               with respect to frame1, then the state s2 of the 
               object with respect to frame2 is given by 
 
                   s2  =  xform * s1 
 
               where "*" denotes matrix-vector multiplication. 
 
 
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
   1) No checks are performed on ROT to ensure that it is indeed 
      a rotation matrix. 
 
-Files
 
   None. 
 
-Particulars
 
   This routine is essentially a macro routine for converting 
   a rotation and angular velocity of the rotation to the 
   equivalent state transformation matrix. 
 
   This routine is an inverse of xf2rav_c.
 
-Examples
 
   Suppose that you wanted to determine state transformation 
   matrix from a platform frame to the J2000 frame. 
 
      /.
      The following call obtains the J2000-to-platform transformation
      matrix and platform angular velocity at the time of interest.
      The time value is expressed as encoded SCLK.
      ./
      
      ckgpav_c ( ckid, time, tol, "J2000", rot, av, &clkout, &fnd );
 
      /.
      Recall that rot and av are the rotation and angular velocity 
      of the transformation from J2000 to the platform frame. 
      ./
      
      if ( fnd )
      { 
         /.
         First get the state transformation from J2000 to the platform 
         frame. 
         ./
         
         rav2xf_c ( rot, av, j2plt );

         /. 
         Invert the state transformation matrix (using invstm_c) to  
         the desired state transformation matrix. 
         ./
         
         invstm_c ( j2plt, xform );
      } 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
 
-Version

   -CSPICE Version 1.0.1, 12-APR-2007 (EDW) 

      Edit to abstract.
 
   -CSPICE Version 1.0.0, 18-JUN-1999 (WLT) (NJB) 

-Index_Entries
 
  State transformation to rotation and angular velocity 
 
-&
*/

   { /* Begin rav2xf_c */


   /*
   Local variables
   */
   
   SpiceDouble             drdt   [3][3];
   SpiceDouble             omegat [3][3];

   SpiceInt                i;
   SpiceInt                j;

   

   /*
   Error free:  no tracing required.
   
   
   A state transformation matrix xform has the following form


       [      |     ]
       |  r   |  0  |
       |      |     |
       | -----+-----|
       |  dr  |     |
       |  --  |  r  |
       [  dt  |     ]


   where r is a rotation and dr/dt is the time derivative of that
   rotation.  From this we can immediately fill in most of the
   state transformation matrix.
   */
   
   
   
   for ( i = 0;  i < 3;  i++ )
      {
      for ( j = 0;  j < 3;  j++ )
         {
         xform[i  ][j  ]  =  rot [i][j];
         xform[i+3][j+3]  =  rot [i][j];
         xform[i  ][j+3]  =  0.;
         }
      }
   


   /*
   Now for the rest.
 
   Recall that rot is a transformation that converts positions
   in some frame frame1 to positions in a second frame frame2.
 
   The angular velocity matrix omega (the cross product matrix
   corresponding to av) has the following property.
 
   If p is the position of an object that is stationary with
   respect to frame2 then the velocity v of that object in frame1
   is given by:
                        t
       v  =  omega * rot  *  p
 
   But v is also given by
 
                  t
             d rot
       v =   -----  * p
               dt
 
   So that
                                t
                  t        d rot
       omega * rot    =   -------
                             dt
 
   Hence
 
        d rot                 t
        -----   =  rot * omega
          dt
 
 
   From this discussion we can see that we need omega transpose.
   Here it is.
   */
   
   omegat[0][0] =  0.0;
   omegat[1][0] = -av[2];
   omegat[2][0] =  av[1];

   omegat[0][1] =  av[2];
   omegat[1][1] =  0.0;
   omegat[2][1] = -av[0];

   omegat[0][2] = -av[1];
   omegat[1][2] =  av[0];
   omegat[2][2] =  0.0;
 
      
   mxm_c ( rot, omegat, drdt );
   
   
   for ( i = 0;  i < 3;  i++ )
      {
      for ( j = 0;  j < 3;  j++ )
         {
         xform[i+3][j]  =  drdt [i][j];
         }
      }


   } /* End rav2xf_c */

