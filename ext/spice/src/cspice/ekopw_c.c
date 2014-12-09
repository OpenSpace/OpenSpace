/*

-Procedure ekopw_c ( EK, open file for writing )

-Abstract
 
   Open an existing E-kernel file for writing. 
 
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
 
   EK 
 
-Keywords
 
   EK 
   FILES 
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void ekopw_c ( ConstSpiceChar  * fname,
                  SpiceInt        * handle ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   fname      I   Name of EK file. 
   handle     O   Handle attached to EK file. 
 
-Detailed_Input
 
   fname          is the name of an existing E-kernel file to be 
                  opened for write access. 
 
-Detailed_Output
 
   handle         is the DAS file handle of the EK designate by 
                  fname.  This handle is used to identify the file 
                  to other EK routines. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the indicated file cannot be opened, the error will be 
       diagnosed by routines called by this routine.  The new file 
       will be deleted. 
 
   2)  If the indicated file has the wrong architecture version, the 
       error will be diagnosed by routines called by this routine. 
 
   3)  If an I/O error occurs while reading or writing the indicated 
       file, the error will be diagnosed by routines called by this 
       routine. 
 
-Files
 
   See the EK Required Reading for a discussion of the EK file 
   format. 
 
-Particulars
 
   This routine should be used to open an EK file for write access. 
 
   Opening an EK file with this routine makes the EK accessible to 
   the following CSPICE EK access routines, all of which modify 
   the target EK file: 
 
      Begin segment: 
 
         ekbseg_c 
 
      Append, insert, delete records: 
 
         ekappr_c 
         ekinsr_c
         ekdelr_c
         
      Add column entries: 
 
         ekacec_c 
         ekaced_c 
         ekacei_c 
 
      Update existing column entries: 
 
         ekucec_c
         ekuced_c 
         ekucei_c 
 
      Execute fast write: 
 
         ekifld_c 
         ekffld_c 
         ekaclc_c
         ekacld_c 
         ekacli_c 
 
   An EK opened for write access is also accessible for reading. 
   The file may be accessed by the CSPICE EK readers 
 
         ekrcec_c 
         ekrced_c
         ekrcei_c
 
      and summary routines: 
 
         eknseg_c 
         ekssum_c
 
 
   An EK opened for write access cannot be queried.  To make an EK 
   available to the EK query system, the file must be loaded via 
   EKLEF, rather than by this routine.  See the EK Required Reading 
   for further information. 
 
-Examples
 
   1)  Open the file MY.EK for write access: 
 
          ekopw_c ( "my.ek", &handle ); 
 
-Restrictions
 
   1)  No more than FTSIZE DAS files may be opened simultaneously. 
       See dasfm_ for the value of FTSIZE. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.1, 09-JAN-2002 (NJB)

      Documentation change:  instances of the phrase "fast load"
      were replaced with "fast write."

   -CSPICE Version 1.0.0, 25-MAY-1999 (NJB)

-Index_Entries
 
   open EK for writing 
 
-&
*/

{ /* Begin ekopw_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekopw_c" );


   /*
   Check the file name string.  The pointer must be non-null
   and the string length must be at least 1.
   */
   CHKFSTR ( CHK_STANDARD, "ekopw_c", fname );


   /*
   Call the f2c'd routine.
   */
   ekopw_ (  ( char      * ) fname,
             ( integer   * ) handle,
             ( ftnlen      ) strlen(fname) );
             

   chkout_c ( "ekopw_c" );

} /* End ekopw_c */
