/*

-Procedure      sctiks_c ( Convert spacecraft clock string to ticks. )

-Abstract
 
   Convert a spacecraft clock format string to number of "ticks". 
 
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
   #include "SpiceZmc.h"


   void sctiks_c ( SpiceInt           sc, 
                   ConstSpiceChar   * clkstr, 
                   SpiceDouble      * ticks   ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   sc         I   NAIF spacecraft identification code. 
   clkstr     I   Character representation of a spacecraft clock. 
   ticks      O   Number of ticks represented by the clock string. 
 
-Detailed_Input
 
   sc         is the NAIF ID number for the spacecraft whose clock 
              string is being converted. 
 
   clkstr     is a character string representing a spacecraft clock 
              time, WITHOUT PARTITION NUMBER. 
 
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
              SCLK components may be separated by any of the 
              following characters: " "  "."  ":"  ","  "-" 
              Any number of spaces may separate the components and 
              the delimiters. The presence of the RIM component 
              is required. Successive components may be omitted, and 
              in such cases are assumed to represent zero values. 
 
              Values for the individual components may exceed the 
              maximum expected values. For instance, "0:0:0:9" is 
              an acceptable Galileo clock string, and will convert 
              to the same number of ticks as "0:0:1:1". 
 
              Consecutive delimiters containing no intervening digits 
              are treated as if they delimit zero components. 
 
              Trailing zeros should always be included to match the 
              length of the counter.  For example, a Galileo clock 
              count of "25684.90" should not be represented as 
              "25684.9". 
 
              Some spacecraft clock components have offset, or 
              starting, values different from zero.  For example, 
              with an offset value of 1, a mod 20 counter would 
              cycle from 1 to 20 instead of from 0 to 19. 
 
              See the SCLK required reading for a detailed 
              description of the Voyager and Mars Observer clock 
              formats. 
 
 
-Detailed_Output
 
   ticks      is the number of ticks represented by the spacecraft 
              clock string. A tick is defined to be the smallest 
              time increment expressible by the spacecraft clock. 
 
              An analogy may be drawn between a spacecraft clock 
              and a standard wall clock, measuring hours, minutes 
              and seconds. The number of ticks represented by the 
              wall clock string 
                                 
                 hh:mm:ss 
 
              would be the number of seconds represented by that 
              time. 
 
              For example: 
 
                 00:00:10  would convert to 10 
                 00:01:00  would convert to 60 
                 00:10:00  would convert to 600 
                 01:00:00  would convert to 3600 
                 01:01:00  would convert to 3660 
 
              See the Examples section below for examples for 
              actual spacecraft clocks. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the spacecraft clock type is not supported then the 
       error SPICE(NOTSUPPORTED) is signalled. 
 
   2)  If any of the extracted clock components cannot be parsed as 
       integers, or the string has too many components, or the value 
       of one of the components is less than the offset value, then 
       the error is diagnosed by routines called by this routine. 
 
   3)  Invalid spacecraft ID's are not diagnosed. 
 
-Files
 
   None. 
 
-Particulars
 
   Note the important difference between scencd_c and sctiks_c. scencd_c 
   converts a clock string to the number of ticks it represents 
   since the beginning of the mission, and so uses partition 
   information. sctiks_c just converts to absolute ticks. 
 
-Examples
 
   sctiks_c is used as part of the process of encoding spacecraft clock 
   by scencd_c, though sctiks_c does not process any partition informa- 
   tion. 
 
   Another use of sctiks_c, however, is to convert a clock measurement 
   to ticks for use as a tolerance for the CK reader ckgp_c. 
 
 
      /.
      Get the pointing from a CK file of the VGR 1 narrow angle 
      image corresponding to a particular SCLK count. 
 
      Load the CK file and the kernel file containing SCLK 
      partition information for scencd_c. 
      ./
      cklpf_c  ( "vgr1na.ck", &handle ); 
      furnsh_c ( "sclk.ker"           ); 
 
      /.
      Get the right ID numbers. 
      ./
      sc    = -31; 
      instr = -31001; 
 
      /.
      The SCLK string includes a partition number. Pictures are 
      never shuttered at intervals smaller than 1 MOD60 count 
      from each other. So use 1 MOD60 count as the time 
      tolerance. 
      ./
      clkstr = "1/20556:14:768"; 
      tolstr = "      0:01:000"; 

      /.
      Encode the clock string and the tolerance. 
      ./
      scencd_c ( sc, clkstr, &sclk ); 
      sctiks_c ( sc, tolstr, &tol  ); 
 
      /.
      Get the pointing from the C-kernel. 
      ./
      ckgp_c ( instr, sclk, tol, ref, cmat, &clkout, &found ); 
 
 
 
    Below are some examples illustrating various clock string inputs 
    and the resulting outputs for the Galileo spacecraft. See the 
    SCLK required reading for a detailed description of the Galileo 
    clock format. 
 
       CLKSTR                TICKS 
       ----------------      -------------------- 
       "0:0:0:1"             1 
       "0:0:1"               8 
       "0:1"                 80 
       "1"                   7280 
       "1 0 0 0"             7280 
       "1,0,0,0"             7280 
       "1:90"                14480 
       "1:9"                 8000 
       "1:09"                8000 
       "0-0-10"              80   |--  Third component is supposed 
       "0-1-0"               80   |    to be a mod-10 count. 
       "0/1/0"               Error: "/" is not an accepted delimiter. 
       "1: 00 : 0 : 1"       7281 
       "1:::1"               7281 
       "1.1.1.1.1"           Error: Too many components 
       "1.1.1.1."            Error: The last delimiter signals that 
                                    a fifth component will follow. 
 
 
       The following examples are for the Voyager 2 spacecraft. Note 
       that the last component of the Voyager clock has an offset 
       value of 1. 
 
       CLKSTR                TICKS 
       ----------------      -------------------- 
        "0.0.001"              0 
        "0:0:002"              1 
        "0:01"                 800 
        "1"                    48000 
        "1.0"                  48000 
        "1.0.0"                Error: The 3rd component is never 0. 
        "0.0:100"              99 
        "0-60-1"               48000 
        "1-1-1"                48800 
        "1-1-2"                48801 
 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   J.M. Lynch     (JPL) 
   R.E. Thurman   (JPL) 
 
-Version

   -CSPICE Version 1.1.1, 14-AUG-2006   (EDW)

      Replace mention of ldpool_c with furnsh_c.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)  
   
       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly.  String checks are now done using
       the macro CHKFSTR.
       
   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)
   
      Based on SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)

-Index_Entries
 
   convert spacecraft_clock string to ticks 
 
-&
*/

{ /* Begin sctiks_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "sctiks_c");

   
   /*
   Check the input string to make sure the pointer
   is non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "sctiks_c", clkstr );
   
   
   /*
   Do the conversion.
   */
   sctiks_ ( ( integer    * ) &sc,
             ( char       * ) clkstr,
             ( doublereal * ) ticks,
             ( ftnlen       ) strlen(clkstr) );
             
             
   chkout_c ( "sctiks_c");

} /* End sctiks_c */
