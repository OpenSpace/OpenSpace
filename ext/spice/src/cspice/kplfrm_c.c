/*

-Procedure kplfrm_c ( Kernel pool frame IDs )

-Abstract
 
   Return a SPICE set containing the frame IDs of all reference 
   frames of a given class having specifications in the kernel pool. 
 
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
 
   CELLS 
   FRAMES 
   KERNEL 
   NAIF_IDS 
   SETS 
 
-Keywords
 
   FRAME 
   SET 
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void kplfrm_c ( SpiceInt      frmcls,
                   SpiceCell   * idset   ) 
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   frmcls     I   Frame class. 
   idset      O   Set of ID codes of frames of the specified class. 
 
-Detailed_Input
 
   frmcls         is an integer code specifying the frame class or
                  classes for which frame ID codes are requested. The
                  applicable reference frames are those having
                  specifications present in the kernel pool.
 
                  `frmcls' may designate a single class or "all 
                  classes." 
 
                  The headerfile SpiceFrm.h declares parameters 
                  identifying frame classes. The supported values 
                  and corresponding meanings of `frmcls' are 
 
                     Parameter          Value    Meaning 
                     ================   =====    ================= 
                     SPICE_FRMTYP_ALL     -1     All frame classes 
                                                 specified in the  
                                                 kernel pool. Class 1 
                                                 is not included. 
 
                     SPICE_FRMTYP_INERTL   1     Built-in inertial. 
                                                 No frames will be  
                                                 returned in the  
                                                 output set. 
                                              
                     SPICE_FRMTYP_PCK      2     PCK-based frame.
 
                     SPICE_FRMTYP_CK       3     CK-based frame.
 
                     SPICE_FRMTYP_TK       4     Fixed rotational 
                                                 offset ("text 
                                                 kernel") frame.
 
                     SPICE_FRMTYP_DYN      5     Dynamic frame.

 
-Detailed_Output
 
   idset          is a SPICE set containing the ID codes of all 
                  reference frames having specifications present in 
                  the kernel pool and belonging to the specified 
                  class or classes. 
 
-Parameters
 
   See the header file SpiceFrm.h. 
 
-Exceptions
 
   1)  If the input frame class argument is not defined in SpiceFrm.h,
       the error is diagnosed by a routine in the call tree of this
       routine.
 
   2)  If the size of `idset' is too small to hold the requested frame
       ID set, the error is diagnosed by a routine in the call tree of
       this routine.
 
   3)  Frames of class 1 may not be specified in the kernel pool.
       However, for the convenience of users, this routine does not
       signal an error if the input class is set to
       SPICE_FRMTYP_INERTL. In this case the output set will be empty.
 
   4)  This routine relies on the presence of just three kernel 
       variable assignments for a reference frame in order to 
       determine that that reference frame has been specified: 
 
         FRAME_<frame name>       = <ID code> 
         FRAME_<ID code>_NAME     = <frame name> 
 
      and either 
 
         FRAME_<ID code>_CLASS    = <class> 
 
      or 
 
         FRAME_<frame name>_CLASS = <class> 
 
      It is possible for the presence of an incomplete frame  
      specification to trick this routine into incorrectly  
      deciding that a frame has been specified. This routine 
      does not attempt to diagnose this problem. 
 
-Files
 
   1) Reference frame specifications for frames that are not 
      built in are typically established by loading frame kernels. 
 
-Particulars
 
   This routine enables SPICE-based applications to conveniently 
   find the frame ID codes of reference frames having specifications 
   present in the kernel pool. Such frame specifications are  
   introduced into the kernel pool either by loading frame kernels 
   or by means of calls to the kernel pool "put" API routines 
  
      pcpool_c 
      pdpool_c 
      pipool_c 
    
   Given a reference frame's ID code, other attributes of the 
   frame can be obtained via calls to the CSPICE APIs
 
      frmnam_c {Return a frame's name} 
      frinfo_c {Return a frame's center, class, and class ID} 
 
   This routine has a counterpart  
 
      bltfrm_c 
 
   which fetches the frame IDs of all built-in reference frames. 
    
-Examples
 
   1)  Display the IDs and names of all reference frames having 
       specifications present in the kernel pool. Group the outputs 
       by frame class. Also fetch and display the entire set of IDs 
       and names using the parameter SPICE_FRMTYP_ALL. 
 
       The meta-kernel used for this example is shown below. The 
       Rosetta kernels referenced by the meta-kernel are available 
       in the path 
 
          pub/naif/ROSETTA/kernels/fk 
 
       on the NAIF server. Older, but officially archived versions 
       of these kernels are available in the path 
 
          pub/naif/pds/data/ros-e_m_a_c-spice-6-v1.0/ 
          rossp_1000/DATA/FK 
 
       The referenced PCK is available from the pck path under the 
       generic_kernels path on the same server. 
 
 
          KPL/MK 
 
          \begindata 
 
             KERNELS_TO_LOAD = ( 'pck00010.tpc' 
                                 'EARTHFIXEDITRF93.TF' 
                                 'ROS_LUTETIA_RSOC_V03.TF' 
                                 'ROS_V18.TF' 
                                 'RSSD0002.TF'            ) 
          \begintext 
 
 
       Program source code: 


          #include <stdio.h>
          #include "SpiceUsr.h"

          int main()
          {
             /.
             Local parameters 
             ./    
             #define META            "kplfrm.tm"
             #define FRNMLN          33
             #define NFRAME          1000
             #define LNSIZE          81

             /.
             Local variables 
             ./
             SPICEINT_CELL           ( idset, NFRAME );

             SpiceChar               frname  [ FRNMLN ];
             SpiceChar               outlin  [ LNSIZE ];

             SpiceInt                i;
             SpiceInt                j;

             /.
             Load kernels that contain frame specifications.
             ./
             furnsh_c ( META );

             /.
             Fetch and display the frames of each class. 
             ./
             for ( i = 1;  i <= 6;  i++ )
             {
                if ( i < 6 )
                {
                   /.
                   Fetch the frames of class i.
                   ./
                   kplfrm_c ( i, &idset );

                   sprintf ( outlin, 
                             "Number of frames of class %ld: %ld",
                             (long) i,
                             (long) card_c(&idset)                );
                }
                else
                {
                   /.
                   Fetch IDs of all frames specified in the kernel pool.
                   ./
                   kplfrm_c ( SPICE_FRMTYP_ALL, &idset );

                   sprintf ( outlin, 
                             "Number of frames in the kernel pool: %ld",
                             (long) card_c(&idset)                     );
                }

                /.
                Display the fetched frame IDs and corresponding names. 
                ./
                printf ( "\n"
                         "%s\n"
                         "   Frame IDs and names\n",
                         outlin                     );

                for ( j = 0;  j < card_c(&idset);  j++ )
                {
                   frmnam_c ( ((SpiceInt *)idset.data)[j], FRNMLN, frname );

                   printf ( "%12.0ld   %s\n", 
                            ( (long) ((SpiceInt *)idset.data)[j] ),  frname );
                }
             }

             return ( 0 );
          }

 
       The output from the program, when the program was linked 
       against the N0064 CSPICE Toolkit, is shown below. The output 
       shown here has been abbreviated. 
 
 
          Number of frames of class 1: 0 
             Frame IDs and names 
 
          Number of frames of class 2: 3 
             Frame IDs and names 
               1000012   67P/C-G_FIXED 
               2000021   LUTETIA_FIXED 
               2002867   STEINS_FIXED 
 
          Number of frames of class 3: 7 
             Frame IDs and names 
               -226570   ROS_RPC_BOOM2 
               -226215   ROS_VIRTIS-M_SCAN 
               -226072   ROS_HGA_AZ 
               -226071   ROS_HGA_EL 
               -226025   ROS_SA-Y 
               -226015   ROS_SA+Y 
               -226000   ROS_SPACECRAFT 
 
          Number of frames of class 4: 64 
             Frame IDs and names 
              -2260021   ROS_LUTETIA 
               -226999   ROSLND_LOCAL_LEVEL 
               -226900   ROSLND_LANDER 
               -226560   ROS_RPC_BOOM1 
 
                  ... 
 
               -226030   ROS_MGA-S 
               -226020   ROS_SA-Y_ZERO 
               -226010   ROS_SA+Y_ZERO 
               1502010   HCI 
               1502301   LME2000 
               1503299   VME2000 
               1503499   MME2000 
 
          Number of frames of class 5: 19 
             Frame IDs and names 
               -226967   2867/STEINS_CSO 
               -226945   45P/H-M-P_CSO 
               -226921   21/LUTETIA_CSO 
               -226920   21/LUTETIA_CSEQ 
               -226912   67P/C-G_CSO 
               -226910   67P/C-G_CSEQ 
               1500010   HEE 
               1500299   VSO 
               1500301   LSE 
               1500399   GSE 
               1500499   MME 
               1501010   HEEQ 
               1501299   VME 
               1501301   LME 
               1501399   EME 
               1501499   MME_IAU2000 
               1502399   GSEQ 
               1502499   MSO 
               1503399   ECLIPDATE 
 
          Number of frames in the kernel pool: 93 
             Frame IDs and names 
              -2260021   ROS_LUTETIA 
               -226999   ROSLND_LOCAL_LEVEL 
               -226967   2867/STEINS_CSO 
               -226945   45P/H-M-P_CSO 
               -226921   21/LUTETIA_CSO 
 
                  ... 
 
               1503299   VME2000 
               1503399   ECLIPDATE 
               1503499   MME2000 
               2000021   LUTETIA_FIXED 
               2002867   STEINS_FIXED

 
-Restrictions
 
   1) This routine will work correctly if the kernel pool 
      contains no invalid frame specifications. See the 
      description of exception 4 above. Users must ensure 
      that no invalid frame specifications are introduced 
      into the kernel pool, either by loaded kernels or 
      by means of the kernel pool "put" APIs.         
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 22-MAY-2012 (NJB)

-Index_Entries
 
   fetch names of reference_frames from the kernel_pool 
 
-&
*/

{ /* Begin kplfrm_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "kplfrm_c" );

   /*
   Initialize the control area of the cell's data array
   if necessary. 
   */
   CELLINIT ( idset );

   /*
   Make the sure cell data type is SpiceInt. 
   */
   CELLTYPECHK ( CHK_STANDARD, "kplfrm_c", SPICE_INT, idset );

   /*
   Let the f2c'd routine do the work.
   */
   kplfrm_ ( ( integer  * ) &frmcls,
             ( integer  * ) (idset->base) );

   /*
   Sync the output cell. 
   */
   if ( !failed_c() )
   {
     zzsynccl_c ( F2C, idset ) ;
   }

   chkout_c ( "kplfrm_c" );

} /* End kplfrm_c */
