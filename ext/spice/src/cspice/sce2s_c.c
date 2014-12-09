/*

-Procedure      sce2s_c ( ET to SCLK string )

-Abstract
 
   Convert an epoch specified as ephemeris seconds past J2000 (ET) to a
   character string representation of a spacecraft clock value (SCLK).
 
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
 
   SCLK 
   TIME 
 
-Keywords
 
   CONVERSION 
   TIME 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void sce2s_c ( SpiceInt        sc, 
                  SpiceDouble     et, 
                  SpiceInt        lenout,
                  SpiceChar     * sclkch  ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   sc         I   NAIF spacecraft clock ID code. 
   et         I   Ephemeris time, specified as seconds past J2000. 
   lenout     I   Maximum length of output string.
   sclkch     O   An SCLK string. 
 
-Detailed_Input
 
   sc             is a NAIF ID code for a spacecraft clock whose 
                  reading at the epoch specified by `et' is desired. 
 
   et             is an epoch, specified as ephemeris seconds past 
                  J2000 TDB. 
                  
   lenout         is the maximum number of characters that can be
                  accommodated in the output string.  This count
                  includes room for the terminating null character. For
                  example, if the maximum allowed length of the output
                  string, including the terminating null, is 25
                  characters, then `lenout' should be set to 25.

                  In order to choose an appropriate value of `lenout',
                  you can examine an SCLK kernel for the clock specified 
                  by `sc'.  The format of string representations of
                  the clock's values is specified by kernel variables
                  associated with the clock.  See Examples below for
                  further information.
 
-Detailed_Output
 
   sclkch         is a character string representation of the
                  spacecraft clock value that corresponds to `et', for
                  the spacecraft clock specified by the input argument
                  `sc'. `sclkch' is an absolute spacecraft clock value,
                  so a partition number is included in the string. The
                  format of `sclkch' is specified in the SCLK kernel
                  for the clock `sc'.  A general discussion of
                  spacecraft clock string formats is available in the
                  SCLK Required Reading.

-Parameters
 
   None. 
 
-Exceptions
 
   1)  This routine assumes that an SCLK kernel appropriate to the
       spacecraft clock identified by the input argument SC has been
       loaded.  If an SCLK kernel has not been loaded, does not contain
       all of the required data, or contains invalid data, error
       diagnoses will be performed by routines in the call tree of this
       routine.  The output argument `sclkch' will not be modified.
 
   2)  When using an SCLK kernel that maps SCLK to a time system other
       than ET (also called barycentric dynamical time---"TDB"), it is
       necessary to have a leapseconds kernel loaded at the time this
       routine is called.  If a leapseconds kernel is required for
       conversion between SCLK and ET but is not loaded, the error will
       be diagnosed by routines called by this routine. The output
       argument `sclkch' will not be modified.
 
       The time system to which an SCLK kernel maps SCLK epochs is
       indicated by the variable SCLK_TIME_SYSTEM_nn in the kernel,
       where nn is the negative of the NAIF integer code for the
       spacecraft. The time system used in a kernel is TDB if and only
       if the variable is assigned the value 1.
 
   3)  If the input ET value is not representable in the spacecraft
       clock string format for the spacecraft clock identified by `sc',
       the error will be diagnosed by routines in the call tree of this
       routine.  The output argument `sclkch' will not be modified.
 
   4)  If the declared length of the output argument `sclkch' is too
       short to contain the output spacecraft clock string produced by
       this routine, the error will be diagnosed by routines in the
       call tree of this routine.  The output argument `sclkch' may
       contain a portion of the truncated string.
 
-Files
 
   1)  An SCLK kernel appropriate to the spacecraft clock identified 
       by SC must be loaded at the time this routine is called. 
 
   2)  If the SCLK kernel used with this routine does not map SCLK 
       directly to barycentric dynamical time, a leapseconds kernel 
       must be loaded at the time this routine is called.
 
-Particulars
 
   This routine is provided as a convenience; it is simply shorthand 
   for the code fragment 
 
      sce2t_c  ( sc,  et,      &sclkdp ); 
      scdecd_c ( sc,  sclkdp,   sclkch ); 
  
-Examples
 
   The numerical results shown for these examples may differ across
   platforms.  The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1)  Determine the length of Galileo spacecraft clock strings.

       Examine a Galileo SCLK kernel.  There you'll find the
       kernel variable assignments

          SCLK01_MODULI_77          = ( 16777215 91 10 8 )
          SCLK01_OFFSETS_77         = (        0  0  0 0 )

       Each field of the clock string contains values ranging
       from the offset value to M-1, where M is the corresponding
       modulus.  So the Galileo clock fields have maximum values

          16777214 90 9 7

       representing the partition number by the symbol "pp" and
       the field delimiter character by the symbol "D", we see   
       that the GLL SCLK format is

          pp/xxxxxxxxDxxDxDx

       This string has length 18 characters.  Accounting for the
       terminating null character, the value of `lenout' should
       be set to at least 19.

       Note:  the delimiter character is determined by the integer
       code assignment

          SCLK01_OUTPUT_DELIM_77    = (                2 )

       The SCLK Required Reading indicates that 2 is the SCLK kernel
       code for the colon character.


   2)  Find the Galileo SCLK value corresponding to the ET value 

          -322452420.5593641.

       We can use the program below:

          #include <stdio.h>
          #include "SpiceUsr.h"

          int main()
          {
             #define SCLKLEN         30
     
             /.
             The spacecraft ID code for the Galileo orbiter 
             is -77.  This is the code for the Galileo spacecraft
             clock as well.
             ./
             #define GLL             -77

             SpiceChar               sclkch[SCLKLEN];
             SpiceDouble             et;
             

             /.
             Start out by loading the SCLK kernel.  In your own 
             program, you must use the name of a real SCLK kernel. 
             The name shown here is fictitious. 
             ./
             furnsh_c ( "gllsclk.ker" ); 

             /.
             Load a leapseconds kernel in case it is needed for 
             SCLK-to-ET conversion.  Depending on the SCLK kernel 
             used, it may not be necessary to load this file; it's 
             just a simple, reliable way of making sure that the 
             leapseconds kernel constants are available if we need 
             them.  Again, a fictitious name is used. 
             ./
             furnsh_c ( "leapseconds.ker" ); 

             et = -322452420.5593641; 

             sce2s_c ( GLL, et, SCLKLEN, sclkch ); 

             printf ( "ET        = %25.17e\n"
                      "GLL SCLK  =  %s\n", 
                      et,
                      sclkch                    );

             return ( 0 );

          }

 
       The output will be 
 
          ET        =  -3.22452420559364080e+08 
          GLL SCLK  =  1/00010001:44:2:0
 

 
   3)  Convert the UTC time 
 
          August 25 1989 4:00:00 
 
       to a Voyager 2 SCLK value. 

       To perform this conversion, we could use the program below.

          #include <stdio.h>
          #include "SpiceUsr.h"

          int main()
          {
             /.
             The spacecraft ID code for the Voyager 2 spacecraft 
             is -32.  This is the code for the Voyager 2 spacecraft
             clock as well.
             ./
             #define VGR2            -32
             #define SCLKLEN         30

             SpiceChar               sclkch[SCLKLEN];
             SpiceDouble             et;
             
             /.
             Load SCLK and leapseconds kernels.  
             ./
             furnsh_c ( "vgr2sclk.ker"    ); 
             furnsh_c ( "leapseconds.ker" ); 

             /.
             Find the Voyager 2 SCLK string corresponding to the 
             specified UTC time. 
             ./ 
             str2et_c ( "Aug 25 1989 4:00:00",  &et              ); 
             sce2s_c  ( VGR2,       et,         SCLKLEN,  sclkch ); 

             printf ( "ET         = %25.17e\n"
                      "VGR2 SCLK  =  %s\n", 
                      et,
                      sclkch                    );

             return ( 0 );
          }

 
       The output will be 

          ET         =  -3.26707143817267537e+08
          VGR2 SCLK  =  4/11390:22:012


-Restrictions
 
   None.
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   C.H. Acton     (JPL)
   N.J. Bachman   (JPL) 
   B.V. Semenov   (JPL)
 
-Version
 
   -CSPICE Version 1.1.2, 21-JAN-2014 (BVS)

      Fixed an incomplete sentence in the item 2) in the header 
      section Files.

   -CSPICE Version 1.1.1, 29-JUL-2003 (NJB) (CHA)

      Various header changes were made to improve clarity and 
      more fully explain the routine's functionality.

   -CSPICE Version 1.1.0, 09-FEB-1998 (NJB)

      Re-implemented routine without dynamically allocated, temporary 
      strings.  Updated the Exceptions header section.
 
   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

      Based on SPICELIB Version 1.2.0, 10-APR-1992 (NJB) (WLT)
      
-Index_Entries
 
   ephemeris time to spacecraft_clock string 
 
-&
*/

{ /* Begin sce2s_c */

   
   /*
   Participate in error tracing.
   */
   chkin_c ( "sce2s_c");


   /*
   Make sure the output sclkch has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "sce2s_c", sclkch, lenout );
   
   
   /*
   Do the conversion.
   */
   sce2s_ ( ( integer    * ) &sc, 
            ( doublereal * ) &et, 
            ( char       * ) sclkch, 
            ( ftnlen       ) lenout-1 );
   
   /*
   Convert sclkch to a null-terminated C string. 
   */
   F2C_ConvertStr ( lenout, sclkch );


   chkout_c ( "sce2s_c");

} /* End sce2s_c */
