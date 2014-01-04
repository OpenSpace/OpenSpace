/*

-Procedure      scdecd_c ( Decode spacecraft clock )

-Abstract
 
   Convert double precision encoding of spacecraft clock time into 
   a character representation. 
 
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
 
-Keywords
 
   CONVERSION 
   TIME 
 
*/   
   
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void scdecd_c ( SpiceInt       sc, 
                   SpiceDouble    sclkdp, 
                   SpiceInt       lenout,
                   SpiceChar    * sclkch  ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   sc         I   NAIF spacecraft identification code. 
   sclkdp     I   Encoded representation of a spacecraft clock count.
   lenout     I   Maximum allowed length of output SCLK string. 
   sclkch     O   Character representation of a clock count. 
   MXPART     P   Maximum number of spacecraft clock partitions. 
 
-Detailed_Input
 
   sc         is the NAIF integer code of the spacecraft whose 
              clock's time is being decoded. 
 
   sclkdp     is the double precision encoding of a clock time in 
              units of ticks since the spacecraft clock start time. 
              This value does reflect partition information. 
 
              An analogy may be drawn between a spacecraft clock 
              and a standard wall clock. The number of ticks 
              corresponding to the wall clock string 
 
                              hh:mm:ss 
 
              would be the number of seconds represented by that 
              time. 
 
              For example: 
 
                    Clock string      Number of ticks 
                    ------------      --------------- 
                      00:00:10              10 
                      00:01:00              60 
                      00:10:00             600 
                      01:00:00            3600 
 
              If sclkdp contains a fractional part the result 
              is the same as if sclkdp had been rounded to the 
              nearest whole number. 
 
 
   lenout     is the maximum number of characters that can be 
              accommodated in the output string.  This count 
              includes room for the terminating null character.
              For example, if the maximum allowed length of the 
              output string, including the terminating null, is 25
              characters, then lenout should be set to 25.
 
-Detailed_Output
 
   sclkch     is the character representation of the clock count. 
              The exact form that sclkch takes depends on the 
              spacecraft. 
 
              Nevertheless, sclkch will have the following general 
              format: 
 
                           "pp/sclk_string" 
 
              "pp" is an integer greater than or equal to one and 
              represents a "partition number". 
 
              Each mission is divided into some number of partitions. 
              A new partition starts when the spacecraft clock 
              resets, either to zero, or to some other 
              value. Thus, the first partition for any mission 
              starts with launch, and ends with the first clock 
              reset. The second partition starts immediately when 
              the first stopped, and so on. 
 
              In order to be completely unambiguous about a 
              particular time, you need to specify a partition number 
              along with the standard clock string. 
 
              Information about when partitions occur for different 
              missions is contained in a spacecraft clock kernel 
              file which needs to be loaded into the kernel pool 
              before calling scdecd_c. 
 
              The routine scpart_c may be used to read the partition 
              start and stop times, in encoded units of ticks, from 
              the kernel file. 
 
              Since the end time of one partition is coincident with 
              the begin time of the next, two different time strings 
              with different partition numbers can encode into the 
              same value. 
 
              For example, if partition 1 ends at time t1, and 
              partition 2 starts at time t2, then 
 
                 "1/t1" and "2/t2" 
 
              will be encoded into the same value, say X. scdecd_c 
              always decodes such values into the latter of the 
              two partitions. In this example, 
 
                 scdecd_c ( x, sc, MAXLEN, clkstr ) 
 
              will result in 
 
                 clkstr = "2/t2". 
 
 
              "sclk_string" is a spacecraft specific clock string, 
              typically consisting of a number of components 
              separated by delimiters. 
 
              Using Galileo as an example, the full format is 
 
                 wwwwwwww:xx:y:z 
 
              where z is a mod-8 counter (values 0-7) which 
              increments approximately once every 8 1/3 ms., y is a 
              mod-10 counter (values 0-9) which increments once 
              every time z turns over, i.e., approximately once every 
              66 2/3 ms., xx is a mod-91 (values 0-90) counter 
              which increments once every time y turns over, i.e., 
              once every 2/3 seconds. wwwwwwww is the Real-Time Image 
              Count (RIM), which increments once every time xx turns 
              over, i.e., once every 60 2/3 seconds. The roll-over 
              expression for the RIM is 16777215, which corresponds 
              to approximately 32 years. 
 
              wwwwwwww, xx, y, and z are referred to interchangeably 
              as the fields or components of the spacecraft clock. 
              SCLK components may be separated by any of these five 
              characters: " "  ":"  ","  "-"  "." 
              The delimiter used is determined by a kernel pool 
              variable and can be adjusted by the user. 
 
              Some spacecraft clock components have offset, or 
              starting, values different from zero.  For example, 
              with an offset value of 1, a mod 20 counter would 
              cycle from 1 to 20 instead of from 0 to 19. 
 
              See the SCLK required reading for a detailed 
              description of the Voyager and Mars Observer clock 
              formats. 
 
 
-Parameters
 
   MXPART     is the maximum number of spacecraft clock partitions 
              expected in the kernel file for any one spacecraft. 
              MXPART is currently set to 9999. 

-Exceptions
 
   1) If kernel variables required by this routine are unavailable, 
      the error will be diagnosed by routines called by this routine. 
      sclkch will be returned as a blank string in this case. 
 
   2) If the number of partitions in the kernel file for spacecraft 
      SC exceeds the parameter MXPART, the error 
      SPICE(TOOMANYPARTS) is signaled.  sclkch will be returned 
      as a blank string in this case. 
 
   3) If the encoded value does not fall in the boundaries of the 
      mission, the error SPICE(VALUEOUTOFRANGE) is signaled. 
      sclkch will be returned as a blank string in this case. 
 
   4) If the output string pointer is null, the error SPICE(NULLPOINTER)
      is signaled.
      
   5) If the output string has length less than two characters, it 
      is too short to contain one character of output data plus a null  
      terminator, so it cannot be passed to the underlying Fortran 
      routine.  In this event, the error SPICE(STRINGTOOSHORT) is
      signaled.
      
   6) If the length of sclkch (indicated by lenout) is at least two
      characters but not large enough to contain the output clock 
      string, the error SPICE(SCLKTRUNCATED) is signaled either by the
      underlying Fortran routine or by a routine called by that routine.
      On output sclkch will contain a portion of the truncated clock 
      string. 
 
   
-Files
 
   A kernel file containing spacecraft clock partition information 
   for the desired spacecraft must be loaded, using the routine 
   furnsh_c, before calling this routine. 
 
-Particulars
 
   In general, it is difficult to compare spacecraft clock counts 
   numerically since there are too many clock components for a 
   single comparison.  The routine scencd_c provides a method of 
   assigning a single double precision number to a spacecraft's 
   clock count, given one of its character representations. 
 
   This routine performs the inverse operation to scencd_c, converting 
   an encoded double precision number to character format. 
 
   To convert the number of ticks since the start of the mission to 
   a clock format character string, scdecd_c: 
 
      1) Determines the spacecraft clock partition that TICKS falls 
         in. 
 
      2) Subtracts off the number of ticks occurring in previous 
         partitions, to get the number of ticks since the beginning 
         of the current partition. 
 
      3) Converts the resulting ticks to clock format and forms the 
         string 
 
            "partition_number/clock_string" 
 
 
-Examples
 
   Double precision encodings of spacecraft clock counts are used to 
   tag pointing data in the C-kernel. 

   In the following example, pointing for a sequence of images from 
   the Voyager 2 narrow angle camera is requested from the C-kernel 
   using an array of character spacecraft clock counts as input. 
   The clock counts attached to the output are then decoded to 
   character and compared with the input strings. 
 
      #include <stdio.h>
      #include "SpiceUsr.h"
      
      void main()
      {
         /. 
         The instrument we want pointing for is the Voyager 2 
         narrow angle camera.  The reference frame we want is 
         J2000. The spacecraft is Voyager 2. 
         ./
         
         #define  SC         -32
         #define  INST       -32001
         #define  REF        "J2000"
         #define  CK         "/kernels/voyager2/ck/vg2_jup_qmw_na.bc"
         #define  SCLK       "/kernels/voyager2/sclk/vg200004.tsc"
         #define  NPICS      4 
         #define  CLKTOL     "0:01:001"
         #define  MAXLEN     30
      
         SpiceBoolean       found;
      
         SpiceChar          sclkin [4][25] = { {"2 / 20538:39:768"}, 
                                               {"2 / 20543:21:768"}, 
                                               {"2 / 20550:37"    }, 
                                               {"2 / 20561:59"    } };      
         SpiceChar          sclkout[25];
      
         SpiceDouble        tol;
         SpiceDouble        timein;
         SpiceDouble        timeout; 
         SpiceDouble        cmat   [3][3];
      
         SpiceInt           handle;
         SpiceInt           i;
      
      
         /.
         Load the appropriate files. We need 
         
            1) CK file containing pointing data. 
            2) Spacecraft clock kernel file, for scencd_c and SCDECD. 
         ./
         
         cklpf_c  ( CK,  &handle );
         furnsh_c ( SCLK         ); 
      
      
         /.
         Convert the tolerance string to ticks. 
         ./
         sctiks_c ( SC, CLKTOL, &tol ); 
      
      
         for ( i = 0;  i < NPICS;  i++ )
         {
            scencd_c ( SC,   sclkin[i], &timein ); 
      
            ckgp_c   ( INST,  timein,    tol,    REF, 
                       cmat,  &timeout,  &found       );
      
            scdecd_c ( SC, timeout, MAXLEN, sclkout );  
      
            if ( found )
            {
               printf ( "\n"
                        "Input  s/c clock count: %s\n"
                        "Output s/c clock count: %s\n"
                        "Output C-Matrix:          \n"
                        "%25.16f   %25.16f   %25.16f\n"   
                        "%25.16f   %25.16f   %25.16f\n"   
                        "%25.16f   %25.16f   %25.16f\n"
                        "\n",   
                        sclkin[i],
                        sclkout,
                        cmat[0][0],  cmat[0][1],  cmat[0][2],
                        cmat[1][0],  cmat[1][1],  cmat[1][2],
                        cmat[2][0],  cmat[2][1],  cmat[2][2] );
            }
            else
            {
               printf ( "\n"
                        "Input  s/c clock count: %s\n"
                        "No pointing found.\n",   
                        sclkin[i]                     );
            }
         }
      
      }

 
   The output from such a program might look like: 
 
 
          Input  s/c clock count:  2 / 20538:39:768 
          Output s/c clock count:  2/20538:39:768 
          Output C-Matrix:  "first C-matrix" 
 
          Input  s/c clock count:  2 / 20543:21:768 
          Output s/c clock count:  2/20543:22:768 
          Output C-Matrix:  "second C-matrix" 
 
          Input  s/c clock count:  2 / 20550:37 
          Output s/c clock count:  2/20550:36:768 
          Output C-Matrix:  "third C-matrix" 
 
          Input  s/c clock count:  2 / 20561:59 
          Output s/c clock count:  2/20561:58:768 
          Output C-Matrix:  "fourth C-matrix" 
 
 
-Restrictions
 
   1) Assumes that an SCLK kernel file appropriate for the clock 
      designated by SC is loaded in the kernel pool at the time 
      this routine is called. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman (JPL)
   J.M. Lynch   (JPL) 
   R.E. Thurman (JPL) 
 
-Version

   -CSPICE Version 1.2.0, 11-FEB-2008   (NJB)

      Definition of constant macro MXPART was deleted.
      Documentation was updated to reflect current
      MXPART value of 9999.

   -CSPICE Version 1.1.2, 14-AUG-2006   (EDW)

      Replace mention of ldpool_c with furnsh_c.
 
   -CSPICE Version 1.1.1, 26-MAR-2003 (NJB) 

       Fixed description of exception (6):  replaced "lenout-1"
       with "lenout."  Corrected spelling of "signaled."

   -CSPICE Version 1.1.0, 09-FEB-1998 (NJB)

      Re-implemented routine without dynamically allocated, temporary 
      strings.  Added output string length and pointer checks.
 
   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

-Index_Entries
 
   decode spacecraft_clock 
 
-&
*/

{ /* Begin scdecd_c */


   /*
   Participate in error handling
   */
   chkin_c ( "scdecd_c");

   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "scdecd_c", sclkch, lenout );
   

   /*
   Decode the encoded SCLK value.
   */
   scdecd_ (  ( integer    * ) &sc,
              ( doublereal * ) &sclkdp,
              ( char       * ) sclkch,
              ( ftnlen       ) lenout-1  ); 
                
   /*
   Convert the Fortran string to a C string by placing a null
   after the last non-blank character.  This operation is valid
   whether or not the SPICELIB routine signaled an error.
   */
   F2C_ConvertStr ( lenout, sclkch );
   
              
   chkout_c ( "scdecd_c");


} /* End scdecd_c */
