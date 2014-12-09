/*

-Procedure      scpart_c ( Spacecraft Clock Partition Information )

-Abstract
 
   Get spacecraft clock partition information from a spacecraft 
   clock kernel file. 
 
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
 
   TIME 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void scpart_c ( SpiceInt        sc, 
                   SpiceInt      * nparts, 
                   SpiceDouble   * pstart, 
                   SpiceDouble   * pstop  ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   sc         I   NAIF spacecraft identification code. 
   nparts     O   The number of spacecraft clock partitions. 
   pstart     O   Array of partition start times. 
   pstop      O   Array of partition stop times. 
   MXPART     P   Maximum number of partitions. 
 
-Detailed_Input
 
   sc         is the NAIF ID for the spacecraft whose clock partition 
              information is being requested. 
 
-Detailed_Output
 
   nparts     is the number of spacecraft clock time partitions 
              described in the kernel file for spacecraft SC. 
 
   pstart     is an array containing nparts partition start times
              represented as double precision, encoded SCLK ("ticks").
              The values contained in pstart are whole numbers.
 
   pstop      is an array containing nparts partition end times
              represented as double precision, encoded SCLK ("ticks").
              The values contained in pstop are whole numbers.
 
-Parameters
 
   MXPART     is the maximum number of partitions for any 
              spacecraft clock. MXPART is currently set to
              9999.

-Exceptions
 
   1)  If the kernel variables containing the spacecraft clock 
       partition start and stop times have not been loaded in the 
       kernel pool, the error will be diagnosed by routines called 
       by this routine. 
 
   2)  If the number of start and stop times are different then 
       the error SPICE(NUMPARTSUNEQUAL) is signalled. 
 
-Files
 
   An SCLK kernel containing spacecraft clock partition start 
   and stop times for the spacecraft clock indicated by SC must 
   be loaded into the kernel pool before this routine may be called.
 
-Particulars
 
   scpart_c looks for two variables in the kernel pool for each 
   spacecraft's partition information. If sc = -nn, then the names of 
   the variables are 
 
      "SCLK_PARTITION_START_nn" 
      "SCLK_PARTITION_END_nn"
 
   The start and stop times returned are in units of "ticks." 
 
-Examples
 
   1)  The following program fragment finds and prints out partition 
       start and stop times in clock format for the Galileo mission. 
       In this example, Galileo partition times are assumed to be 
       in the kernel file sclk.ker. 
 
          #include <stdio.h>
          #include "SpiceUsr.h"
          
          void main()
          {
             #define   CLKLEN        30
             #define   SC            -32
             #define   MXPART        9999
             
             SpiceChar               start  [ CLKLEN ];
             SpiceChar               stop   [ CLKLEN ];
             SpiceDouble             pstart [ MXPART ];
             SpiceDouble             pstop  [ MXPART ];

             SpiceInt                nparts;
             SpiceInt                i;


             furnsh_c ( "sclk.ker" );
    
             scpart_c ( SC, &nparts, pstart, pstop ); 
     
             for ( i = 0;  i < nparts;  i++ ) 
             {
                scfmt_c ( SC, pstart[ i ], CLKLEN, start ); 
                scfmt_c ( SC, pstop [ i ], CLKLEN, stop  ); 
    
                printf  ( "\n"
                          "Partition %d:\n" 
                          "Start = %s\n" 
                          "Stop  = %s\n" 
                          "\n",
                          i,
                          start,
                          stop                );
             } 
          }
          
          
 
-Restrictions
 
   1) This routine assumes that an SCLK kernel appropriate to the 
      spacecraft identified by SC has been loaded into the kernel 
      pool. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
   J.M. Lynch     (JPL) 
   R.E. Thurman   (JPL) 
 
-Version

   -CSPICE Version 1.1.1, 19-MAR-2014   (NJB)

      Minor header comment updates were made.

   -CSPICE Version 1.1.0, 11-FEB-2008   (NJB)

      Definition of constant macro MXPART was deleted.
      Documentation was updated to reflect current
      MXPART value of 9999.

   -CSPICE Version 1.0.1, 14-AUG-2006   (EDW)

      Replace mention of ldpool_c with furnsh_c.

   -CSPICE Version 1.0.0, 08-FEB-1998 (NJB)

      Based on SPICELIB Version 1.1.0, 22-MAR-1993 (JML)
      
-Index_Entries
 
   spacecraft_clock partition information 
 
-&
*/

{ /* Begin scpart_c */

   /*
   Participate in error handling
   */
   chkin_c ( "scpart_c");

   /*
   Unlike most of the wrappers, this one reads the
   partition data directly into the callers' buffers.
   
   We rely on the scpart_ to check for an excessive 
   partition count.
   */

   scpart_ ( ( integer     * ) &sc,
             ( integer     * ) nparts,
             ( doublereal  * ) pstart,
             ( doublereal  * ) pstop   );
             



   chkout_c ( "scpart_c");


} /* End scpart_c */
