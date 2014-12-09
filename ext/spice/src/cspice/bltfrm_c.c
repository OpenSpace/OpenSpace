/*

-Procedure bltfrm_c ( Built-in frame IDs )

-Abstract
 
   Return a SPICE set containing the frame IDs of all built-in frames
   of a specified class.
 
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


   void bltfrm_c ( SpiceInt      frmcls,
                   SpiceCell   * idset  ) 
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   frmcls     I   Frame class. 
   idset      O   Set of ID codes of frames of the specified class. 
 
-Detailed_Input
 
   frmcls         is an integer code specifying the frame class or 
                  classes for which built-in frame ID codes are 
                  requested. `frmcls' may designate a single class or 
                  "all classes." 
 
                  The header file SpiceFrm.h declares parameters 
                  identifying frame classes. The supported values 
                  and corresponding meanings of `frmcls' are 
 
                     Parameter            Value    Meaning 
                     ===================  =====    ================== 
                     SPICE_FRMTYP_ALL       -1     All frame classes.
                     SPICE_FRMTYP_INERTL     1     Built-in inertial.
                     SPICE_FRMTYP_PCK        2     PCK-based frame.
                     SPICE_FRMTYP_CK         3     CK-based frame.
                     SPICE_FRMTYP_TK         4     Fixed offset ("text 
                                                   kernel") frame.
                     SPICE_FRMTYP_DYN        5     Dynamic frame.
 
-Detailed_Output
 
   idset          is a SPICE set containing the ID codes of all 
                  built-in reference frames of the specified class 
                  or classes. 
 
-Parameters
 
   See the header file SpiceFrm.h. 
 
-Exceptions
 
   1)  If the input frame class argument is not defined in SpiceFrm.h,
       the error is diagnosed by a routine in the call tree of this
       routine.
 
   2)  If the size of `idset' is too small to hold the requested frame ID
       set, the error is diagnosed by a routine in the call tree of
       this routine.
 
-Files
 
   None. 
 
-Particulars
 
   This routine has a counterpart  
 
      kplfrm_c 
 
   which fetches the frame IDs of all frames specified in the kernel 
   pool. 
    
-Examples
 
   1) Display the IDs and names of all SPICE built-in frames. 
      Group the outputs by frame class. Also fetch and display 
      the entire set of IDs and names using the parameter 
      SPICE_FRMTYP_ALL.
 
 
      Program source code: 
  

         #include <stdio.h>
         #include "SpiceUsr.h"

         int main()
         {
            /.
            Local parameters 
            ./    
            #define FRNMLN          33
            #define NFRAME          ( SPICE_NFRAME_NINERT +  \
                                      SPICE_NFRAME_NNINRT   )
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
            Fetch and display the frames of each class. 
            ./
            for ( i = 1;  i <= 6;  i++ )
            {
               if ( i < 6 )
               {
                  /.
                  Fetch the frames of class i.
                  ./
                  bltfrm_c ( i, &idset );

                  sprintf ( outlin, 
                            "Number of frames of class %ld: %ld",
                            (long) i,
                            (long) card_c(&idset)                );
               }
               else
               {
                  /.
                  Fetch IDs of all built-in frames. 
                  ./
                  bltfrm_c ( SPICE_FRMTYP_ALL, &idset );

                  sprintf ( outlin, 
                            "Number of built-in frames: %ld",
                            (long) card_c(&idset)                );
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
   against the N0064 CSPICE Toolkit, is shown below. Note that 
   the set of built-in frames, particularly the non-inertial 
   ones, will grow over time, so the output shown here may 
   be out of sync with that produced by a current SPICE Toolkit. 
 
   The output shown here has been abbreviated. 
 
 
      Number of frames of class 1: 21 
         Frame IDs and names 
                 1   J2000 
                 2   B1950
                 3   FK4 
                 4   DE-118 
                 5   DE-96 
                 6   DE-102 
                 7   DE-108 
                 8   DE-111 
                 9   DE-114 
                10   DE-122 
                11   DE-125 
                12   DE-130 
                13   GALACTIC 
                14   DE-200 
                15   DE-202 
                16   MARSIAU 
                17   ECLIPJ2000 
                18   ECLIPB1950 
                19   DE-140 
                20   DE-142 
                21   DE-143 
 
      Number of frames of class 2: 99 
         Frame IDs and names 
             10001   IAU_MERCURY_BARYCENTER 
             10002   IAU_VENUS_BARYCENTER 
 
                 ... 
 
             10100   IAU_ITOKAWA 
             13000   ITRF93 
 
      Number of frames of class 3: 0 
         Frame IDs and names 
 
      Number of frames of class 4: 1 
         Frame IDs and names 
             10081   EARTH_FIXED 
 
      Number of frames of class 5: 0 
         Frame IDs and names 
 
      Number of built-in frames: 121 
         Frame IDs and names 
                 1   J2000 
                 2   B1950
 
                 ... 
 
             10100   IAU_ITOKAWA 
             13000   ITRF93 
 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 22-MAY-2012 (NJB)

-Index_Entries
 
   fetch names of built-in reference frames 
 
-&
*/

{ /* Begin bltfrm_c */

 
   /*
   Participate in error tracing.
   */
   chkin_c ( "bltfrm_c" );

   /*
   Initialize the control area of the cell's data array
   if necessary. 
   */
   CELLINIT ( idset );

   /*
   Make the sure cell data type is SpiceInt. 
   */
   CELLTYPECHK ( CHK_STANDARD, "bltfrm_c", SPICE_INT, idset );

   /*
   Let the f2'd routine do the work.
   */
   bltfrm_ ( ( integer  * ) &frmcls,
             ( integer  * ) (idset->base) );

   /*
   Sync the output cell. 
   */
   if ( !failed_c() )
   {
     zzsynccl_c ( F2C, idset ) ;
   }

   chkout_c ( "bltfrm_c" );

} /* End bltfrm_c */
