/*

-Procedure ekopn_c ( EK, open new file )

-Abstract
 
   Open a new E-kernel file and prepare the file for writing. 
 
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
   NAIF_IDS 
   TIME 
 
-Keywords
 
   EK 
   FILES 
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void ekopn_c ( ConstSpiceChar    * fname,
                  ConstSpiceChar    * ifname,
                  SpiceInt            ncomch,
                  SpiceInt          * handle ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   fname      I   Name of EK file. 
   ifname     I   Internal file name. 
   ncomch     I   The number of characters to reserve for comments. 
   handle     O   Handle attached to new EK file. 
 
-Detailed_Input
 
   fname          is the name of a new E-kernel file to be created. 
 
   ifname         is the internal file name of a new E-kernel.  The 
                  internal file name may be up to 60 characters in 
                  length, not including the null terminator.
 
   ncomch         is the amount of space, measured in characters, to 
                  be allocated in the comment area when the new EK 
                  file is created.  It is not necessary to allocate 
                  space in advance in order to add comments, but 
                  doing so may greatly increase the efficiency with 
                  which comments may be added.  Making room for 
                  comments after data has already been added to the 
                  file involves moving the data, and thus is slower. 
 
                  ncomch must be greater than or equal to zero. 
 
-Detailed_Output
 
   handle         is the EK handle of the file designated by fname. 
                  This handle is used to identify the file to other 
                  EK routines. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If NCOMCH is less than zero, the error SPICE(INVALIDCOUNT) 
       will be signalled.  No file will be created. 
 
   2)  If IFNAME is invalid, the error will be diagnosed by routines 
       called by this routine. 
 
   3)  If the indicated file cannot be opened, the error will be 
       diagnosed by routines called by this routine.  The new file 
       will be deleted. 
 
   4)  If an I/O error occurs while reading or writing the indicated 
       file, the error will be diagnosed by routines called by this 
       routine. 
 
-Files
 
   See the EK Required Reading for a discussion of the EK file 
   format. 
 
-Particulars
 
   This routine operates by side effects:  it opens and prepares 
   an EK for addition of data. 
 
-Examples
 
   1)  Open a new EK file with name "my.ek" and internal file 
       name "test ek/1995-JUL-17": 
 
       ekopn_c ( "my.ek",  "test ek/1995-JUL-17",  &handle  );
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 31-MAR-1998 (NJB)
   
      Based on SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)

-Index_Entries
 
   open new E-kernel 
   open new EK 
 
-&
*/

{ /* Begin ekopn_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekopn_c" );

   /*
   Check the file name to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekopn_c", fname );


   /*
   Check the internal file name to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekopn_c", ifname );

   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.
   */
   ekopn_ ( ( char     * ) fname, 
            ( char     * ) ifname,
            ( integer  * ) &ncomch,
            ( integer  * ) handle,
            ( ftnlen     ) strlen(fname),
            ( ftnlen     ) strlen(ifname) );

   chkout_c ( "ekopn_c" );

} /* End ekopn_c */
