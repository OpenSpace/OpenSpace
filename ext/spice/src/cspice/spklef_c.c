/*

-Procedure  spklef_c (  S/P Kernel, Load ephemeris file )

-Abstract

   Load an ephemeris file for use by the readers.  Return that file's 
   handle, to be used by other SPK routines to refer to the file.

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

   SPK

-Keywords

   None.

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   

   void spklef_c ( ConstSpiceChar * filename,
                   SpiceInt       * handle    )


/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   fname      I   Name of the file to be loaded.
   handle     O   Loaded file's handle.
 
-Detailed_Input

   fname          is a string containing the name of the file to be 
                  loaded.
 
-Detailed_Output

   handle         is an integer handle assigned to the file upon 
                  loading.   Almost every other SPK routine will 
                  subsequently use this number to refer to the file.

-Parameters

   None.

-Exceptions

   The parameter FTSIZE referenced below is defined in the header file
   cspicelimits.h.

   1) If an attempt is made to load more files than is specified
      by the parameter FTSIZE, the error "SPICE(SPKFILETABLEFULL)" is
      signalled.

   2) If an attempt is made to open more DAF files than is specified
      by the parameter FTSIZE, an error is signalled by a routine that 
      this routine calls.

-Files

   A file specified by fname, to be loaded.  The file is assigned a
   handle by spklef_c, which will be used by most other routines to
   refer to it.

-Particulars

   Loading an SPK file make the file's data accessible to the CSPICE
   SPK readers spkezr_c and spkez_c.  
   
   The maximum number of SPK files that may be loaded at any time is
   given by the parameter FTSIZE, which is defined in the header file
   cspicelimits.h.  After this limit it reached, it is necessary to
   unload an SPK file before another can be loaded.  The function
   spkuef_c is provided to unload files from the SPK system.
   
-Examples

   1)  Load a planetary ephemeris SPK; then look up a series of
       geometric states of the Earth relative to the solar system 
       barycenter, referenced to the J2000 frame.
       
       
            #define        MAXITR        100
            #define        ET0           -315576000.0
            #define        STEP          3600.0
            
            #define        ABCORR        "NONE"
            #define        FRAME         "J2000"
            #define        OBSERVER      "SOLAR SYSTEM BARYCENTER"
            #define        SPK           "de403.bsp"
            #define        TARGET        "EARTH"

            SpiceInt       handle;
            SpiceInt       i;
            
            SpiceDouble    et;
            SpiceDouble    lt;
            SpiceDouble    state [6];
            
            
         /.
            Load the spk file.
         ./
            spklef_c ( SPK, &handle );
         
         /.
            Step through a series of epochs, looking up a state vector
            at each one.
         ./ 
            for ( i = 0;  i < MAXITR;  i++ )
            {
               et  =  ET0 + i*STEP;
               
               spkezr_c ( TARGET,    et,     FRAME,  ABCORR, 
                          OBSERVER,  state,  &lt             );
            
               printf( "\net = %20.10f\n\n",                 et       );
               printf( "J2000 x-position (km):   %20.10f\n", state[0] );
               printf( "J2000 y-position (km):   %20.10f\n", state[1] );
               printf( "J2000 z-position (km):   %20.10f\n", state[2] );
               printf( "J2000 x-velocity (km/s): %20.10f\n", state[3] );
               printf( "J2000 y-velocity (km/s): %20.10f\n", state[4] );
               printf( "J2000 z-velocity (km/s): %20.10f\n", state[5] );
            }
       
-Restrictions

   None.

-Literature_References

   NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
   User's Guide"

-Author_and_Institution

   N.J. Bachman    (JPL)
   K.R. Gehringer  (JPL)
   J.M. Lynch      (JPL)
   R.E. Thurman    (JPL)
   I.M. Underwood  (JPL)
   E.D. Wright     (JPL)
   B.V. Semenov    (JPL)

-Version

   -CSPICE Version 2.0.3, 04-FEB-2008   (BVS)

      Removed duplicate header section '-Exceptions'.

   -CSPICE Version 2.0.2, 16-JAN-2008   (EDW)

      Corrected typos in header titles:
      
      Detailed Input to Detailed_Input
      Detailed Output to Detailed_Output
      
   -CSPICE Version 2.0.1, 10-NOV-2006   (EDW)

      Added Keywords and Parameters section headers. 
      Reordered section headers.

   -CSPICE Version 2.0.0, 08-FEB-1998 (NJB)  
   
       Input argument filename changed to type ConstSpiceChar *.
       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly.  String checks are now done using
       the macro CHKFSTR. 
       
   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB) (EDW)
      
-Index_Entries

   load spk ephemeris file

-&
*/


{  /* Begin spklef_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "spklef_c" );


   /*
   Check the input string filename to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spklef_c", filename );
   
   
   /*
   Call the f2c'd Fortran routine.
   */
   spklef_ ( ( char     * )  filename, 
             ( integer  * )  handle, 
             ( ftnlen     )  strlen(filename) );


   chkout_c ( "spklef_c" );

} /* end spklef_c */
