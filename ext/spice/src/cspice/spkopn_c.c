/*

-Procedure spkopn_c ( SPK, open new file. )

-Abstract
 
   Create a new SPK file, returning the handle of the opened file. 
 
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
 
   SPK 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void spkopn_c ( ConstSpiceChar * name,
                   ConstSpiceChar * ifname,
                   SpiceInt         ncomch,
                   SpiceInt       * handle  ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   name       I   The name of the new SPK file to be created. 
   ifname     I   The internal filename for the SPK file. 
   ncomch     I   The number of characters to reserve for comments. 
   handle     O   The handle of the opened SPK file. 
 
-Detailed_Input
 
   name     The name of the new SPK file to be created. 
 
   ifname   The internal filename for the SPK file that is being 
            created. The internal filename may be up to 60 characters 
            long. If you do not have any conventions for tagging your 
            files, an internal filename of "SPK_file" is perfectly 
            acceptable. You may also leave it blank if you like. 
 
   ncomch   This is the space, measured in characters, to be 
            initially set aside for the comment area when a new SPK 
            file is opened. The amount of space actually set aside 
            may be greater than the amount requested, due to the 
            manner in which comment records are allocated in an SPK 
            file. However, the amount of space set aside for comments 
            will always be at least the amount that was requested. 
 
            The value of ncomch should be greater than or equal to 
            zero, i.e., 0 <= ncomch. A negative value, should one 
            occur, will be assumed to be zero. 
 
-Detailed_Output
 
   handle   The handle of the opened SPK file. If an error occurs 
            when opening the file, the value of this variable should 
            not be used, as it will not represent a valid handle. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the value of ncomch is negative, a value of zero (0) will 
      be used for the number of comment characters to be set aside 
      for comments. 
 
   2) If an error occurs while attempting to open a CK file, the 
      value of handle will not represent a valid file handle. 
 
   3) If any input string pointers are null, the error 
      SPICE(NULLPOINTER) will be signaled.
      
   4) If any input strings have length zero, the error 
      SPICE(EMPTYSTRING) will be signaled.
 
-Files
 
   See arguments name and handle. 
 
-Particulars
 
   Open a new SPK file, reserving room for comments if requested. 
 
-Examples
 
   Suppose that you want to create a new SPK file called 'new.spk' 
   that contains a single type 5 SPK segment and has room for at 
   least 5000 comment characters. The following code fragment should 
   take care of this for you, assuming that all of the variables 
   passed to the SPK type 5 segment writer have appropriate values 
   and no errors occur. 
 
      #include "SpiceUsr.h"
         .
         .
         . 
      name   = "new.spk";
      ifname = "test spk file";
 
      spkopn_c ( name, ifname, 5000,  &handle ); 
      spkw05_c ( handle, objid, cntrid, cframe, etbeg, 
                 etend, segmid, cntrgm, nstate, state, 
                 epoch                                 );
      spkcls_c ( handle );
 
-Restrictions
 
   None. 
 
-Author_and_Institution
 
   F.S. Turner        (JPL)
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.1.0, 20-APR-2005 (NJB)

       Bug fix:  address, rather than value, of `ncomch' is now 
       passed to spkopn_.

       Header comments indicating that `ncomch' is not used have
       been deleted.
       
   -CSPICE Version 1.0.0, 16-MAR-1999 (FST)

-Index_Entries
 
   open a new spk file 
 
-&
*/

{ /* Begin spkopn_c */

   /*
   Participate in error tracing.
   */

   chkin_c ( "spkopn_c" );

   /*
   Check the input string name to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkopn_c", name );
   
   /*
   Check the input string ifname to make sure the pointer is 
   non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkopn_c", ifname );

   /*
   Call the f2c'd Fortran routine.
   */
   spkopn_ ( ( char    * )  name,
             ( char    * )  ifname,
             ( integer * )  &ncomch,
             ( integer * )  handle,
             ( ftnlen    )  strlen(name),
             ( ftnlen    )  strlen(ifname) );

   chkout_c ( "spkopn_c" );

} /* End spkopn_c */
